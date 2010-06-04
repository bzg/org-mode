;;; org-mime.el --- org html export for text/html MIME emails

;; Copyright (C) 2010 Eric Schulte

;; Author: Eric Schulte
;; Keywords: mime, mail, email, html
;; Homepage: http://orgmode.org/worg/org-contrib/org-mime.php
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; WYSWYG, html mime composition using org-mode
;;
;; For mail composed using the orgstruct-mode minor mode, this
;; provides a function for converting all or part of your mail buffer
;; to embedded html as exported by org-mode.  Call `org-mime-htmlize'
;; in a message buffer to convert either the active region or the
;; entire buffer to html.
;;
;; Similarly the `org-mime-org-buffer-htmlize' function can be called
;; from within an org-mode buffer to convert the buffer to html, and
;; package the results into an email handling with appropriate MIME
;; encoding.
;;
;; you might want to bind this to a key with something like the
;; following message-mode binding
;; 
;;   (add-hook 'message-mode-hook
;;             (lambda ()
;;               (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
;;
;; and the following org-mode binding
;; 
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

;;; Code:
(require 'cl)

(defcustom org-mime-default-header
  "#+OPTIONS: latex:t\n"
  "Default header to control html export options, and ensure
  first line isn't assumed to be a title line."
  :group 'org-mime
  :type 'string)

(defcustom org-mime-library 'mml
  "Library to use for marking up MIME elements."
  :group 'org-mime
  :type '(choice 'mml 'semi 'vm))

(defcustom org-mime-preserve-breaks t
  "Used as temporary value of `org-export-preserve-breaks' during
  mime encoding."
  :group 'org-mime
  :type 'boolean)

(defcustom org-mime-fixedwith-wrap
  "<pre style=\"font-family: courier, monospace;\">\n%s</pre>\n"
  "Format string used to wrap a fixedwidth HTML email."
  :group 'org-mime
  :type 'string)

(defcustom org-mime-html-hook nil
  "Hook to run over the html buffer before attachment to email.
  This could be used for example to post-process html elements."
  :group 'org-mime
  :type 'hook)

;; example hook, for setting a dark background in <pre style="background-color: #EEE;"> elements
(defun org-mime-change-element-style (element style)
  "Set new default htlm style for <ELEMENT> elements in exported html."
  (while (re-search-forward (format "<%s" element) nil t)
    (replace-match (format "<%s style=\"%s\"" element style))))

(defun org-mime-change-class-style (class style)
  "Set new default htlm style for objects with classs=CLASS in
exported html."
  (while (re-search-forward (format "class=\"%s\"" class) nil t)
    (replace-match (format "class=\"%s\" style=\"%s\"" class style))))

;; ;; example addition to `org-mime-html-hook' adding a dark background
;; ;; color to <pre> elements
;; (add-hook 'org-mime-html-hook
;;           (lambda ()
;;             (org-mime-change-element-style
;;              "pre" (format "color: %s; background-color: %s;"
;;                            "#E6E1DC" "#232323"))
;; 	    (org-mime-change-class-style
;;              "verse" "border-left: 2px solid gray; padding-left: 4px;")))

(defun org-mime-file (ext path id)
  "Markup a file for attachment."
  (case org-mime-library
    ('mml (format
           "<#part type=\"%s\" filename=\"%s\" id=\"<%s>\">\n<#/part>\n"
           ext path id))
    ('semi (concat
            (format
             "--[[%s\nContent-Disposition: inline;\nContent-ID: <%s>][base64]]\n"
             ext id)
            (base64-encode-string
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (binary-insert-encoded-file path)
               (buffer-string)))))
    ('vm "?")))

(defun org-mime-multipart (plain html)
  "Markup a multipart/alternative with text/plain and text/html
  alternatives."
  (case org-mime-library
    ('mml (format (concat "<#multipart type=alternative><#part type=text/plain>"
                          "%s<#part type=text/html>%s<#/multipart>\n")
                  plain html))
    ('semi (concat
            "--" "<<alternative>>-{\n"
            "--" "[[text/plain]]\n" plain
            "--" "[[text/html]]\n"  html
            "--" "}-<<alternative>>\n"))
    ('vm "?")))

(defun org-mime-replace-images (str current-file)
  "Replace images in html files with cid links."
  (let (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (format
         "src=\"cid:%s\""
         (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                          (match-string 1 text)))
                (path (expand-file-name
                       url (file-name-directory current-file)))
                (ext (file-name-extension path))
                (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
           (add-to-list 'html-images
                        (org-mime-file (concat "image/" ext) path id))
           id)))
      str)
     html-images)))

(defun org-mime-htmlize (arg)
  "Export a portion of an email body composed using `mml-mode' to
html using `org-mode'.  If called with an active region only
export that region, otherwise export the entire body."
  (interactive "P")
  (let* ((region-p (org-region-active-p))
         (html-start (or (and region-p (region-beginning))
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward mail-header-separator)
                           (+ (point) 1))))
         (html-end (or (and region-p (region-end))
                       ;; TODO: should catch signature...
                       (point-max)))
         (raw-body (buffer-substring html-start html-end))
         (tmp-file (make-temp-name (expand-file-name "mail" temporary-file-directory)))
         (body (org-mime-org-export "org" raw-body tmp-file))
         ;; because we probably don't want to skip part of our mail
         (org-export-skip-text-before-1st-heading nil)
         ;; because we probably don't want to export a huge style file
         (org-export-htmlize-output-type 'inline-css)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks org-mime-preserve-breaks)
         ;; to hold attachments for inline html images
         (html-and-images
          (org-mime-replace-images
           (org-mime-org-export "html" raw-body tmp-file)
           tmp-file))
         (html-images (unless arg (cdr html-and-images)))
         (html (org-mime-apply-html-hook
                (if arg
                    (format org-mime-fixedwith-wrap body)
                  (car html-and-images)))))
    (delete-region html-start html-end)
    (save-excursion
      (goto-char html-start)
      (insert (org-mime-multipart body html)
              (mapconcat 'identity html-images "\n")))))

(defun org-mime-org-export (fmt body tmp-file)
  "Org-Export BODY to format FMT with the file name set to
TMP-FILE during export."
  (save-excursion
    (with-temp-buffer
      (insert org-mime-default-header)
      (insert body)
      (write-file tmp-file)
      (org-load-modules-maybe)
      (unless org-local-vars
        (setq org-local-vars (org-get-local-variables)))
      (substring
       (eval ;; convert to fmt -- mimicing `org-run-like-in-org-mode'
        (list 'let org-local-vars 
              (list (intern (concat "org-export-as-" fmt))
                    nil nil nil ''string t)))
       (if (string= fmt "org") (length org-mime-default-header) 0)))))

(defun org-mime-apply-html-hook (html)
  (if org-mime-html-hook
      (with-temp-buffer
        (insert html)
        (goto-char (point-min))
        (run-hooks 'org-mime-html-hook)
        (buffer-string))
    html))

(defun org-mime-org-buffer-htmlize ()
  "Export the current org-mode buffer to HTML using
`org-export-as-html' and package the results into an email
handling with appropriate MIME encoding."
  (interactive)
  (require 'reporter)
  (let* ((region-p (org-region-active-p))
         (current-file (buffer-file-name (current-buffer)))
         (html-start (or (and region-p (region-beginning))
                         (save-excursion
                           (goto-char (point-min)))))
         (html-end (or (and region-p (region-end))
                       (point-max)))
         (body (org-export-as-org nil nil nil 'string t))
         (org-link-file-path-type 'absolute)
         ;; because we probably don't want to export a huge style file
         (org-export-htmlize-output-type 'inline-css)
         ;; to hold attachments for inline html images
         (html-and-images (org-mime-replace-images
                           (org-export-as-html nil nil nil 'string t)
                           current-file))
         (html-images (cdr html-and-images))
         (html (org-mime-apply-html-hook (car html-and-images))))
    ;; dump the exported html into a fresh message buffer
    (reporter-compose-outgoing)
    (goto-char (point-max))
    (insert (org-mime-multipart body html)
            (mapconcat 'identity html-images "\n"))))

(provide 'org-mime)