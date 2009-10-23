;;; org-babel-exp.el --- Exportation of org-babel source blocks

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

;; for more information see the comments in org-babel.el

;;; Code:
(require 'org-babel)
(require 'org-exp-blocks)
(org-export-blocks-add-block '(src org-babel-exp-src-blocks nil))
(add-to-list 'org-export-interblocks '(src org-babel-exp-inline-src-blocks))

(defun org-babel-exp-src-blocks (body &rest headers)
  "Process src block for export.  Depending on the 'export'
headers argument in replace the source code block with...

both ---- display the code and the results

code ---- the default, display the code inside the block but do
          not process

results - just like none only the block is run on export ensuring
          that it's results are present in the org-mode buffer

none ----- do not display either code or results upon export"
  (interactive)
  (unless headers (error "org-babel can't process a source block without knowing the source code"))
  (message "org-babel-exp processing...")
  (let* ((lang (car headers))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang)))
         (params (org-babel-merge-params
                  org-babel-default-header-args
                  (if (boundp lang-headers) (eval lang-headers) nil)
                  (org-babel-params-from-properties)
                  (org-babel-parse-header-arguments
                   (mapconcat #'identity (cdr headers) " ")))))
    (org-babel-exp-do-export lang body params)))

(defun org-babel-exp-inline-src-blocks (start end)
  "Process inline src blocks between START and END for export.
See `org-babel-exp-src-blocks' for export options, currently the
options and are taken from `org-babel-defualt-inline-header-args'."
  (interactive)
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward org-babel-inline-src-block-regexp end t))
      (let* ((info (save-match-data (org-babel-parse-inline-src-block-match)))
             (replacement (save-match-data
                            (org-babel-exp-do-export
                             (first info) (second info) (third info) t))))
        ;; (message "%s -> %s" (second info) replacement) ;; debugging
        (setf end (+ end (- (length replacement)
                            (+ 6 (length (first info)) (length (second info))))))
        (replace-match replacement t t)))))

(defun org-babel-exp-do-export (lang body params &optional inline)
  (case (intern (or (cdr (assoc :exports params)) "code"))
    ('none "")
    ('code (org-babel-exp-code body lang params inline))
    ('results (org-babel-exp-results body lang params inline))
    ('both (concat (org-babel-exp-code body lang params inline)
                   "\n\n"
                   (org-babel-exp-results body lang params inline)))))

(defun org-babel-exp-code (body lang params &optional inline)
  (if inline
      (format "=%s=" body)
    (format "#+BEGIN_SRC %s\n%s%s\n#+END_SRC" lang body
            (if (string-match "\n$" body) "" "\n"))))

(defun org-babel-exp-results (body lang params &optional inline)
  (let* ((params
          ;; lets ensure that we lookup references in the original file
          (mapcar (lambda (pair)
                    (if (and (eq (car pair) :var)
                             (string-match org-babel-ref-split-regexp (cdr pair)))
                        `(:var . ,(concat (match-string 1 (cdr pair))
                                          "=" org-current-export-file
                                          ":" (match-string 2 (cdr pair))))
                      pair)) params))
         (raw (org-babel-execute-src-block
               nil (list lang body params) '(("results" . "silent")))))
    (if (and (stringp raw) (= 0 (length raw)))
        "=(no results)=" (format "=%S=" raw))))

(provide 'org-babel-exp)
;;; org-babel-exp.el ends here
