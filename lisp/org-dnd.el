;;; org-dnd.el --- Org DnD support                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Visuwesh <visuweshm@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides tooling to handle drag and drop in Org mode.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ol)

;;;; Customization variables

(defcustom org-yank-image-save-method 'attach
  "Method to save images yanked from clipboard and dropped to Emacs.
It can be the symbol `attach' to add it as an attachment, or a
directory name to copy/cut the image to that directory, or a
function that will be called without arguments and should return the
directory name, as a string."
  :group 'org
  :package-version '(Org . "9.8")
  :type '(choice (const :tag "Add it as attachment" attach)
                 (directory :tag "Save it in directory")
                 (function :tag "Save it in a directory returned from the function call"))
  :safe (lambda (x) (eq x 'attach)))

(defcustom org-yank-image-file-name-function #'org-yank-image-autogen-filename
  "Function to generate filename for image yanked from clipboard.
By default, this autogenerates a filename based on the current
time.
It is called with no arguments and should return a string without
any extension which is used as the filename."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(radio (function-item :doc "Autogenerate filename"
                               org-yank-image-autogen-filename)
                (function-item :doc "Ask for filename"
                               org-yank-image-read-filename)
                function))

(defcustom org-yank-dnd-method 'ask
  "Action to perform on the dropped and the pasted files.
When the value is the symbol,
  . `attach' -- attach dropped/pasted file
  . `open' -- visit/open dropped/pasted file in Emacs
  . `file-link' -- insert file: link to dropped/pasted file
  . `ask' -- ask what to do out of the above."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "Attach" attach)
                 (const :tag "Open/Visit file" open)
                 (const :tag "Insert file: link" file-link)
                 (const :tag "Ask what to do" ask)))

(defcustom org-yank-dnd-default-attach-method nil
  "Default attach method to use when DND action is unspecified.
This attach method is used when the DND action is `private'.
This is also used when `org-yank-image-save-method' is nil.
When nil, use `org-attach-method'."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "Default attach method" nil)
                 (const :tag "Copy" cp)
                 (const :tag "Move" mv)
                 (const :tag "Hard link" ln)
                 (const :tag "Symbolic link" lns)))

;;;; Functions

(defun org-setup-yank-dnd-handlers ()
  "Setup the `yank-media' and DND handlers for buffer."
  (let ((handler (if (>= emacs-major-version 30)
                     #'org--dnd-multi-local-file-handler
                   #'org--dnd-local-file-handler)))
    (setq-local dnd-protocol-alist
                (append
                 (list (cons "^file:///" handler)
                       (cons "^file:/[^/]" handler)
                       (cons "^file:[^/]" handler))
                 dnd-protocol-alist)))
  (when (fboundp 'yank-media-handler)
    (yank-media-handler "image/.*" #'org--image-yank-media-handler)
    ;; Looks like different DEs go for different handler names,
    ;; https://larsee.com/blog/2019/05/clipboard-files/.
    (yank-media-handler "x/special-\\(?:gnome\\|KDE\\|mate\\)-files"
                        #'org--copied-files-yank-media-handler)
    (yank-media-handler "application/x-libreoffice-tsvc"
                        #'org--libreoffice-table-handler))
  (when (boundp 'x-dnd-direct-save-function)
    (setq-local x-dnd-direct-save-function #'org--dnd-xds-function)))

(defun org-yank-image-autogen-filename ()
  "Autogenerate filename for image in clipboard."
  (format-time-string "clipboard-%Y%m%dT%H%M%S.%6N"))

(defun org-yank-image-read-filename ()
  "Read filename for image in clipboard."
  (read-string "Basename for image file without extension: "))

(declare-function org-attach-attach "org-attach" (file &optional visit-dir method))

(defun org--image-yank-media-handler (mimetype data)
  "Save image DATA of mime-type MIMETYPE and insert link at point.
It is saved as per `org-yank-image-save-method'.  The name for the
image is prompted and the extension is automatically added to the
end."
  (cl-assert (fboundp 'mailcap-mime-type-to-extension)) ; Emacs >=29
  (cl-assert (fboundp 'file-name-with-extension)) ; Emacs >=28
  (let* ((ext (symbol-name
               (with-no-warnings ; Suppress warning in Emacs <29
                 (mailcap-mime-type-to-extension mimetype))))
         (iname (funcall org-yank-image-file-name-function))
         (filename (with-no-warnings ; Suppress warning in Emacs <28
                     (file-name-with-extension iname ext)))
         (dirname (cond ((eq org-yank-image-save-method 'attach) temporary-file-directory)
                        ((stringp org-yank-image-save-method) org-yank-image-save-method)
                        ((functionp org-yank-image-save-method)
                         (let ((retval (funcall org-yank-image-save-method)))
                           (when (not (stringp retval))
                             (user-error
                              "`org-yank-image-save-method' did not return a string: %S"
                              retval))
                           retval))
                        (t (user-error
                            "Unknown value of `org-yank-image-save-method': %S"
                            org-yank-image-save-method))))
         (absname (expand-file-name
                   filename
                   dirname)))
    (when (and (not (eq org-yank-image-save-method 'attach))
               (not (file-directory-p dirname)))
      (make-directory dirname t))
    (when (file-exists-p absname)
      (if (y-or-n-p
           (format "Yank target %s already exists.  Overwrite?"
                   absname))
          (delete-file absname)
        (error "Yank target already exists: %s" absname)))
    ;; DATA is a raw image.  Tell Emacs to write it raw, without
    ;; trying to auto-detect the coding system.
    (let ((coding-system-for-write 'emacs-internal))
      (with-temp-file absname
        (insert data)))
    (insert
     (if (not (eq org-yank-image-save-method 'attach))
         (org-link-make-string-for-buffer (concat "file:" absname))
       (progn
         (require 'org-attach)
         (apply #'org-link-make-string-for-buffer
                (org-attach-attach absname nil 'mv)))))))

;; I cannot find a spec for this but
;; https://indigo.re/posts/2021-12-21-clipboard-data.html and pcmanfm
;; suggests that this is the format.
(defun org--copied-files-yank-media-handler (_mimetype data)
  "Handle copied or cut files from file manager.
They are handled as per `org-yank-dnd-method'.
DATA is a string where the first line is the operation to
perform: copy or cut.  Rest of the lines are file: links to the
concerned files."
  ;; pcmanfm adds a null byte at the end for some reason.
  (let* ((data (split-string data "[\0\n\r]" t))
         (files (cdr data))
         (action (if (equal (car data) "cut")
                     'copy
                   'move))
         (sep (if (= (length files) 1) "" " ")))
    (dolist (f files)
      (if (file-readable-p f)
          (org--dnd-local-file-handler f action sep)
        (message "File `%s' is not readable, skipping" f)))))

(declare-function org-table-convert-region "org-table" (beg0 end0 &optional separator))
(defun org--libreoffice-table-handler (_mimetype data)
  "Insert LibreOffice Calc table DATA as an Org table.
DATA is in the TSV format."
  (defvar org-table-convert-region-max-lines)
  ;; Some LibreOffice versions have the null byte in the selection.
  ;; It should be safe to remove it.
  (when (string-search "\0" data)
    (setq data (string-replace "\0" "" data)))
  (let ((orig-buf (current-buffer)))
    (with-temp-buffer
      (decode-coding-string data 'undecided nil (current-buffer))
      (let ((tmp (current-buffer))
            (nlines (count-lines (point-min) (point-max))))
        (when (> nlines org-table-convert-region-max-lines)
          (unless (yes-or-no-p
                   (format "Inserting large table with %d lines, more than `org-table-convert-region-max-lines'.  Continue? "
                           nlines))
            (user-error "Table is larger than limit `org-table-convert-region-max-lines'")))
        ;; User has chosen to ignore the limit.
        (let ((org-table-convert-region-max-lines most-positive-fixnum))
          (org-table-convert-region (point-min) (point-max)))
        (with-current-buffer orig-buf
          (insert-buffer-substring tmp))))))

(declare-function mailcap-file-name-to-mime-type "mailcap" (file-name))
(defvar org-attach-method)

(defun org--dnd-rmc (prompt choices)
  "Display a menu or dialog and select with PROMPT among CHOICES.
PROMPT is the prompt string.  CHOICES is a list of choices.  Each
choice is a list of (key description value).  VALUE from the selected
choice is returned."
  (if (null (and
             ;; Emacs <=28 does not have `use-dialog-box-p'.
             (fboundp 'use-dialog-box-p)
             (use-dialog-box-p)))
      (progn
        (setq choices
              (mapcar
               (pcase-lambda (`(,key ,message ,val))
                 ;; `read-multiple-choice' expects VAL to be a long
                 ;; description of the choice - string or nil.  Move VAL
                 ;; further, so that it is not seen by the extended
                 ;; help in `read-multiple-choice'.
                 (list key message nil val))
               choices))
        (nth 3 (read-multiple-choice prompt choices)))
    (setq choices
          (mapcar
           (pcase-lambda (`(_key ,message ,val))
             (cons (capitalize message) val))
           choices))
    (x-popup-menu t (list prompt (cons "" choices)))))

(defun org--dnd-multi-local-file-handler (urls action)
  "Handle file URLS as per ACTION.
URLS is a list of file URL."
  (let ((sep (if (= (length urls) 1) "" " ")))
    (dolist (u urls)
      (org--dnd-local-file-handler u action sep))))

(put 'org--dnd-multi-local-file-handler 'dnd-multiple-handler t)

(defun org--dnd-local-file-handler (url action &optional separator)
  "Handle file URL as per ACTION.
SEPARATOR is the string to insert after each link.  It may be nil
in which case, space is inserted."
  (unless separator
    (setq separator " "))
  (let ((method (if (eq org-yank-dnd-method 'ask)
                    (org--dnd-rmc
                     "What to do with file?"
                     '((?a "attach" attach)
                       (?o "open" open)
                       (?f "insert file: link" file-link)))
                  org-yank-dnd-method)))
    (pcase method
      (`attach (org--dnd-attach-file url action separator))
      (`open (dnd-open-local-file url action))
      (`file-link
       (let ((filename (dnd-get-local-file-name url)))
         (insert (org-link-make-string (concat "file:" filename)) separator))))))

(defun org--dnd-attach-file (url action separator)
  "Attach filename given by URL using method pertaining to ACTION.
If ACTION is `move', use `mv' attach method.
If `copy', use `cp' attach method.
If `ask', ask the user.
If `private', use the method denoted in `org-yank-dnd-default-attach-method'.
The action `private' is always returned.

SEPARATOR is the string to insert after each link."
  (require 'mailcap)
  (require 'org-attach)
  (let* ((filename (dnd-get-local-file-name url))
         (mimetype (mailcap-file-name-to-mime-type filename))
         (separatep (and (string-prefix-p "image/" mimetype)
                         (not (eq 'attach org-yank-image-save-method))))
         (method (pcase action
                   ('copy 'cp)
                   ('move 'mv)
                   ('ask (org--dnd-rmc
                          "Attach using method"
                          '((?c "copy" cp)
                            (?m "move" mv)
                            (?l "hard link" ln)
                            (?s "symbolic link" lns))))
                   ('private (or org-yank-dnd-default-attach-method
                                 org-attach-method)))))
    (if separatep
        (progn
          (unless (file-directory-p org-yank-image-save-method)
            (make-directory org-yank-image-save-method t))
          (funcall
           (pcase method
             ('cp #'copy-file)
             ('mv #'rename-file)
             ('ln #'add-name-to-file)
             ('lns #'make-symbolic-link))
           filename
           (expand-file-name (file-name-nondirectory filename)
                             org-yank-image-save-method)))
      (org-attach-attach filename nil method))
    (insert
     (org-link-make-string
      (concat (if separatep
                  "file:"
                "attachment:")
              (if separatep
                  (expand-file-name (file-name-nondirectory filename)
                                    org-yank-image-save-method)
                (file-name-nondirectory filename))))
     separator)
    'private))

(declare-function org-attach-dir "org-attach" (&optional create-if-not-exists-p no-fs-check))
(defvar-local org--dnd-xds-method nil
  "The method to use for dropped file.")
(defun org--dnd-xds-function (need-name filename)
  "Handle file with FILENAME dropped via XDS protocol.
When NEED-NAME is t, FILENAME is the base name of the file to be
saved.
When NEED-NAME is nil, the drop is complete."
  (if need-name
      (let ((method (if (eq org-yank-dnd-method 'ask)
                        (org--dnd-rmc
                         "What to do with dropped file?"
                         '((?a "attach" attach)
                           (?o "open" open)
                           (?f "insert file: link" file-link)))
                      org-yank-dnd-method)))
        (setq-local org--dnd-xds-method method)
        (pcase method
          (`attach (expand-file-name filename (org-attach-dir 'create)))
          (`open (expand-file-name (make-temp-name "emacs.") temporary-file-directory))
          (`file-link (read-file-name "Write file to: " nil nil nil filename))))
    (pcase org--dnd-xds-method
      (`attach (insert (org-link-make-string
                        (concat "attachment:" (file-name-nondirectory filename)))))
      (`file-link (insert (org-link-make-string (concat "file:" filename))))
      (`open (find-file filename)))
    (setq-local org--dnd-xds-method nil)))

(provide 'org-dnd)

;;; org-dnd.el ends here
