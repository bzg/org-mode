;;; org-persist.el --- Persist data across Emacs sessions         -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2021 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92 at gmail dot com>
;; Keywords: cache, storage

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
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
;;
;; This file implements persistant data storage across Emacs sessions.
;; Both global and buffer-local data can be stored.

;;; Code:

(require 'org-compat)
(require 'org-id)

(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-at-heading-p "org" (&optional invisible-not-ok))

(defvar org-persist-path (org-file-name-concat user-emacs-directory "org-persist/")
  "Directory where the data is stored.")

(defvar org-persist-index-file "index"
  "File name used to store the data index.")

(defvar org-persist-before-write-hook nil
  "Abnormal hook ran before saving data for a single variable in a buffer.
The hook must accept the same arguments as `org-persist-write'.
The hooks will be evaluated until a hook returns non-nil.
If any of the hooks return non-nil, do not save the data.")

(defvar org-persist-before-read-hook nil
  "Abnormal hook ran before reading data for a single variable in a buffer.
The hook must accept the same arguments as `org-persist-read'.
The hooks will be evaluated until a hook returns non-nil.
If any of the hooks return non-nil, do not read the data.")

(defvar org-persist-after-read-hook nil
  "Abnormal hook ran after reading data for a single variable in a buffer.
The hook must accept the same arguments as `org-persist-read'.")

(defvar org-persist--index nil
  "Global index.

The index is a list of plists.  Each plist contains information about
a data variable.  Each plist contains the following properties:

  - `:variable'    list of variables to be stored in single file
  - `:persist-file': data file name
  - `:path':       buffer file path, if any
  - `:inode':      buffer file inode, if any
  - `:hash':       buffer hash, if any")

(defun org-persist--get-index (var &optional buffer)
  "Return plist used to store VAR in BUFFER.
When BUFFER is nil, return plist for global VAR."
  (let* ((buffer-file (when buffer (buffer-file-name (or (buffer-base-buffer buffer)
                                                         buffer))))
         (inode (when buffer-file (file-attribute-inode-number (file-attributes buffer-file)))))
    (let ((result (seq-find (lambda (plist)
                              (and (or (memq var (plist-get plist :variable))
                                       (eq var (plist-get plist :variable)))
                                   (or (equal inode (plist-get plist :inode))
                                       (equal buffer-file (plist-get plist :path)))))
                            org-persist--index)))
      (when result
        (unless (equal buffer-file (plist-get result :path))
          (setf result (plist-put result :path buffer-file))))
      (unless result
        (push (list :variable (if (listp var) var (list var))
                    :persist-file (replace-regexp-in-string "^.." "\\&/" (org-id-uuid))
                    :path buffer-file
                    :inode inode
                    :hash (when buffer (secure-hash 'md5 buffer)))
              org-persist--index)
        (setf result (car org-persist--index)))
      result)))

(defun org-persist--read-index ()
  "Read `org-persist--index'"
  (unless org-persist--index
    (when (file-exists-p (org-file-name-concat org-persist-path org-persist-index-file))
      (with-temp-buffer
        (insert-file-contents (org-file-name-concat org-persist-path org-persist-index-file))
        (setq org-persist--index (read (current-buffer)))))))

(cl-defun org-persist-register (var &optional buffer &key inherit)
  "Register VAR in BUFFER to be persistent.
Optional key INHERIT make VAR dependent on another variable.  Such
dependency means that data shared between variables will be preserved
(see elisp#Circular Objects)."
  (unless org-persist--index (org-persist--read-index))
  (when inherit
    (let ((inherited-index (org-persist--get-index inherit buffer)))
      (unless (memq var (plist-get inherited-index :variable))
        (push var (plist-get inherited-index :variable)))))
  (org-persist--get-index var buffer)
  (when buffer
    (add-hook 'kill-buffer-hook #'org-persist-write-all-buffer 1000 'local)))

(defun org-persist-unregister (var &optional buffer)
  "Unregister VAR in BUFFER to be persistent.
When BUFFER is `all', unregister VAR in all buffers."
  (unless org-persist--index (org-persist--read-index))
  (setq org-persist--index
        (seq-remove
         (lambda (plist)
           (when (and (memq var (plist-get plist :variable))
                      (or (eq buffer 'all)
                          (eq (buffer-file-name
                               (or (buffer-base-buffer buffer)
                                   buffer))
                              (plist-get plist :path))))
             (if (length> (plist-get plist :variable) 1)
                 (progn
                   (setq plist
                         (plist-put plist :variable
                                    (delq var (plist-get plist :variable))))
                   ;; Do not remove the index though.
                   nil)
               (let ((persist-file (org-file-name-concat org-persist-path (plist-get plist :persist-file))))
                 (delete-file persist-file)
                 (when (directory-empty-p (file-name-directory persist-file))
                   (delete-directory (file-name-directory persist-file))))
               'delete-from-index)))
         org-persist--index))
  (org-persist-gc))

(defun org-persist-write (var &optional buffer)
  "Save buffer-local data in BUFFER for VAR."
  (unless (and buffer (not (get-buffer buffer)))
    (unless (listp var) (setq var (list var)))
    (with-current-buffer (or buffer (current-buffer))
      (let ((index (org-persist--get-index var buffer)))
        (setf index (plist-put index :hash (when buffer (secure-hash 'md5 buffer))))
        (let ((print-circle t)
              print-level
              print-length
              print-quoted
              (print-escape-control-characters t)
              (print-escape-nonascii t)
              (print-continuous-numbering t)
              print-number-table)
          (unless (seq-find (lambda (v)
                              (run-hook-with-args-until-success 'org-persist-before-write-hook v buffer))
                            (plist-get index :variable))
            (unless (file-exists-p org-persist-path)
              (make-directory org-persist-path))
            (with-temp-file (org-file-name-concat org-persist-path org-persist-index-file)
              (prin1 org-persist--index (current-buffer)))
            (let ((file (org-file-name-concat org-persist-path (plist-get index :persist-file)))
                  (data (mapcar (lambda (s) (cons s (symbol-value s)))
                                (plist-get index :variable))))
              (unless (file-exists-p (file-name-directory file))
                (make-directory (file-name-directory file) t))
              (with-temp-file file
                (prin1 data (current-buffer))))))))))

(defun org-persist-write-all (&optional buffer)
  "Save all the persistent data."
  (dolist (index org-persist--index)
    (when (or (not (plist-get index :path))
              (and (get-file-buffer (plist-get index :path))
                   (or (not buffer)
                       (equal (buffer-file-name (or (buffer-base-buffer buffer)
                                                    buffer))
                              (plist-get index :path)))))
      (org-persist-write (plist-get index :variable)
              (when (plist-get index :path)
                (get-file-buffer (plist-get index :path)))))))

(defun org-persist-write-all-buffer ()
  "Call `org-persist-write-all' in current buffer."
  (org-persist-write-all (current-buffer)))

(defun org-persist-read (var &optional buffer)
  "Restore VAR data in BUFFER."
  (let* ((index (org-persist--get-index var buffer))
         (persist-file (org-file-name-concat org-persist-path (plist-get index :persist-file)))
         (data nil))
    (when (and (file-exists-p persist-file)
               (or (not buffer)
                   (equal (secure-hash 'md5 buffer) (plist-get index :hash))))
      (unless (seq-find (lambda (v)
                          (run-hook-with-args-until-success 'org-persist-before-read-hook v buffer))
                        (plist-get index :variable))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8)
                (read-circle t))
            (insert-file-contents persist-file))
          ;; FIXME: Reading sometimes fails to read circular objects.
          ;; I suspect that it happens when we have object reference
          ;; #N# read before object definition #N=.  If it is really
          ;; #so, it should be Emacs bug - either in `read' or in
          ;; #`prin1'.  Meanwhile, just fail silently when `read'
          ;; #fails to parse the saved cache object.
          (condition-case err
              (setq data (read (current-buffer)))
            (error
             (warn "Emacs reader failed to read data for %S:%S. The error was: %S"
                   (or buffer "global") var (error-message-string err))
             (setq data nil))))
        (with-current-buffer (or buffer (current-buffer))
          (cl-loop for var1 in (plist-get index :variable)
                   do
                   (when (alist-get var1 data)
                     (setf (symbol-value var1) (alist-get var1 data)))
                   (run-hook-with-args 'org-persist-after-read-hook var1 buffer)))))))

(defun org-persist-read-all (&optional buffer)
  "Restore all the persistent data in BUFFER."
  (unless org-persist--index (org-persist--read-index))
  (dolist (index org-persist--index)
    (when (equal (buffer-file-name (or (buffer-base-buffer buffer)
                                       buffer))
                 (plist-get index :path))
      (org-persist-read (plist-get index :variable) buffer))))

(defun org-persist-read-all-buffer ()
  "Call `org-persist-read-all' in current buffer."
  (org-persist-read-all (current-buffer)))

(defun org-persist-gc ()
  "Remove stored data for not existing files or unregistered variables."
  (let (new-index)
    (dolist (index org-persist--index)
      (when-let ((file (plist-get index :path))
                 (persist-file (org-file-name-concat
                                org-persist-path
                                (plist-get index :persist-file))))
        (if (file-exists-p file)
            (push index new-index)
          (when (file-exists-p persist-file)
            (delete-file persist-file)
            (when (directory-empty-p (file-name-directory persist-file))
              (delete-directory (file-name-directory persist-file)))))))
    (setq org-persist--index (nreverse new-index))))

(add-hook 'kill-emacs-hook #'org-persist-gc)
(add-hook 'kill-emacs-hook #'org-persist-write-all 1000)
(add-hook 'after-init-hook #'org-persist-read-all)

(provide 'org-persist)

;;; org-persist.el ends here
