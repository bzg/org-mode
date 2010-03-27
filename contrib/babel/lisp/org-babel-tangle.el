;;; org-babel-tangle.el --- Extract source code from org-mode files

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
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

;; Extract the code from source blocks out into raw source-code files.

;;; Code:
(require 'org-babel)

(defvar org-babel-tangle-langs nil
  "List of languages supported by `org-babel-tangle'.  The first
element of each language's list is a string indicating the name
of the language, the second element should be the file extension
of the language, an optional third element the shebang(#!)  line
to use when writing out the language to file, and an optional
fourth element is a flag which when true indicates that the
language does not support comments.")

(defun org-babel-load-file (file)
  "Load the contents of the Emacs Lisp source code blocks in the
org-mode formatted FILE.  This function will first export the
source code using `org-babel-tangle' and then load the resulting
file using `load-file'."
  (flet ((age (file)
              (time-to-seconds
               (time-subtract (current-time)
                              (sixth (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (let* ((base-name (file-name-sans-extension file))
           (exported-file (concat base-name ".el")))
      ;; tangle if the org-mode file is newer than the elisp file
      (unless (and (file-exists-p exported-file) (> (age file) (age exported-file)))
        (org-babel-tangle-file file exported-file "emacs-lisp"))
      (load-file exported-file)
      (message "loaded %s" exported-file))))

(defun org-babel-tangle-file (file &optional target-file lang)
  "Extract the bodies of all source code blocks in FILE with
`org-babel-tangle'.  Optional argument TARGET-FILE can be used to
specify a default export file for all source blocks.  Optional
argument LANG can be used to limit the exported source code
blocks by language."
  (interactive "fFile to tangle: \nP")
  (save-window-excursion (find-file file) (org-babel-tangle target-file lang)))

(defun org-babel-tangle (&optional target-file lang)
  "Extract the bodies of all source code blocks from the current
file into their own source-specific files.  Optional argument
TARGET-FILE can be used to specify a default export file for all
source blocks.  Optional argument LANG can be used to limit the
exported source code blocks by language."
  (interactive)
  (save-buffer)
  (save-excursion
    (let ((block-counter 0)
          path-collector)
      (mapc ;; map over all languages
       (lambda (by-lang)
         (let* ((lang (car by-lang))
                (specs (cdr by-lang))
                (lang-f (intern (concat
                                 (or (and (cdr (assoc lang org-src-lang-modes))
                                          (symbol-name
                                           (cdr (assoc lang org-src-lang-modes))))
                                     lang)
                                 "-mode")))
                (lang-specs (cdr (assoc lang org-babel-tangle-langs)))
                (ext (first lang-specs))
                (she-bang (second lang-specs))
                (commentable (and (fboundp lang-f) (not (third lang-specs))))
                she-banged)
           (mapc
            (lambda (spec)
              (flet ((get-spec (name)
                               (cdr (assoc name (third spec)))))
                (let* ((tangle (get-spec :tangle))
                       (she-bang (if (> (length (get-spec :shebang)) 0)
                                     (get-spec :shebang)
                                   she-bang))
                       (base-name (or (cond
                                       ((string= "yes" tangle)
                                        (file-name-sans-extension (buffer-file-name)))
                                       ((string= "no" tangle) nil)
                                       ((> (length tangle) 0) tangle))
                                      target-file))
                       (file-name (when base-name
                                    ;; decide if we want to add ext to base-name
                                    (if (and ext (string= "yes" tangle))
                                        (concat base-name "." ext) base-name))))
                  ;; ;; debugging
                  ;; (message
                  ;;  "tangle=%S base-name=%S file-name=%S she-bang=%S commentable=%s"
                  ;;  tangle base-name file-name she-bang commentable)
                  (when file-name
                    ;; delete any old versions of file
                    (when (and (file-exists-p file-name)
                               (not (member file-name path-collector)))
                      (delete-file file-name))
                    ;; drop source-block to file
                    (with-temp-buffer
                      (if (fboundp lang-f) (funcall lang-f))
                      (when (and she-bang (not (member file-name she-banged)))
                        (insert (concat she-bang "\n"))
                        (setq she-banged (cons file-name she-banged)))
                      (org-babel-spec-to-string spec)
		      ;; We avoid append-to-file as it does not work with tramp.
		      (let ((content (buffer-string)))
			(with-temp-buffer
			  (if (file-exists-p file-name)
			      (insert-file-contents file-name))
			  (goto-char (point-max))
			  (insert content)
			  (write-region nil nil file-name))))
                    ;; update counter
                    (setq block-counter (+ 1 block-counter))
                    (add-to-list 'path-collector file-name)))))
            specs)))
       (org-babel-tangle-collect-blocks lang))
      (message "tangled %d code block%s" block-counter
               (if (= block-counter 1) "" "s"))
      path-collector)))

(defun org-babel-tangle-clean ()
  "Call this function inside of a source-code file generated by
`org-babel-tangle' to remove all comments inserted automatically
by `org-babel-tangle'.  Warning, this comment removes any lines
containing constructs which resemble org-mode file links or noweb
references."
  (interactive)
  (goto-char (point-min))
  (while (or (re-search-forward "\\[\\[file:.*\\]\\[.*\\]\\]" nil t)
             (re-search-forward "<<[^[:space:]]*>>" nil t))
    (delete-region (save-excursion (move-beginning-of-line 1) (point))
                   (save-excursion (move-end-of-line 1) (forward-char 1) (point)))))

(defun org-babel-tangle-collect-blocks (&optional lang)
  "Collect all source blocks in the current org-mode file.
Return an association list of source-code block specifications of
the form used by `org-babel-spec-to-string' grouped by language.
Optional argument LANG can be used to limit the collected source
code blocks by language."
  (let ((block-counter 0) blocks)
    (org-babel-map-source-blocks (buffer-file-name)
      (setq block-counter (+ 1 block-counter))
      (let* ((link (progn (call-interactively 'org-store-link)
                          (org-babel-clean-text-properties (car (pop org-stored-links)))))
             (info (org-babel-get-src-block-info))
             (source-name (intern (or (fifth info)
                                      (format "block-%d" block-counter))))
             (src-lang (first info))
             (body (org-babel-expand-noweb-references info))
             (params (third info))
             (spec (list link source-name params body (third (cdr (assoc src-lang org-babel-tangle-langs)))))
             by-lang)
        (unless (string= (cdr (assoc :tangle params)) "no") ;; maybe skip
          (unless (and lang (not (string= lang src-lang))) ;; maybe limit by language
            ;; add the spec for this block to blocks under it's language
            (setq by-lang (cdr (assoc src-lang blocks)))
            (setq blocks (delq (assoc src-lang blocks) blocks))
            (setq blocks (cons (cons src-lang (cons spec by-lang)) blocks))))))
    ;; ensure blocks in the correct order
    (setq blocks
          (mapcar (lambda (by-lang) (cons (car by-lang) (reverse (cdr by-lang)))) blocks))
    ;; blocks should contain all source-blocks organized by language
    ;; (message "blocks=%S" blocks) ;; debugging
    blocks))

(defun org-babel-spec-to-string (spec)
  "Insert the source-code specified by SPEC into the current
source code file.  This function uses `comment-region' which
assumes that the appropriate major-mode is set.  SPEC has the
form

  (link source-name params body)"
  (flet ((insert-comment (text)
                         (when commentable
                           (insert "\n")
                           (comment-region (point) (progn (insert text) (point)))
                           (move-end-of-line nil)
                           (insert "\n"))))
    (let ((link (first spec))
          (source-name (second spec))
          (body (fourth spec))
          (commentable (not (if (> (length (cdr (assoc :comments (third spec)))) 0)
                                (string= (cdr (assoc :comments (third spec))) "no")
                              (fifth spec)))))
      (insert-comment (format "[[%s][%s]]" (org-link-escape link) source-name))
      (insert (format "%s" (org-babel-chomp body)))
      (insert-comment (format "%s ends here" source-name)))))

(provide 'org-babel-tangle)
;;; org-babel-tangle.el ends here
