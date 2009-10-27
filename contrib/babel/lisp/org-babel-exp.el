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
(add-to-list 'org-export-interblocks '(lob org-babel-exp-lob-one-liners))

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
    (org-babel-exp-do-export lang body params 'block)))

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
                             (first info) (second info) (third info) 'inline))))
        (setq end (+ end (- (length replacement) (length (match-string 1)))))
        (replace-match replacement t t nil 1)))))

(defun org-babel-exp-lob-one-liners (start end)
  "Process #+lob (Library of Babel) calls between START and END for export.
See `org-babel-exp-src-blocks' for export options. Currently the
options are taken from `org-babel-default-header-args'."
  (interactive)
  (let (replacement)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (re-search-forward org-babel-lob-one-liner-regexp nil t))
	(setq replacement
	      (save-match-data
		(org-babel-exp-do-export
		 "emacs-lisp" "results"
		 (org-babel-merge-params
		  org-babel-default-header-args
		  (org-babel-parse-header-arguments
		   (org-babel-clean-text-properties
		    (concat ":var results="
			    (mapconcat #'identity (org-babel-lob-get-info) " ")))))
		 'lob)))
	(setq end (+ end (- (length replacement) (length (match-string 0)))))
	(replace-match replacement t t)))))

(defun org-babel-exp-do-export (lang body params type)
  (case (intern (or (cdr (assoc :exports params)) "code"))
    ('none "")
    ('code (org-babel-exp-code lang body params type))
    ('results (org-babel-exp-results lang body params type))
    ('both (concat (org-babel-exp-code lang body params type)
                   "\n\n"
                   (org-babel-exp-results lang body params type)))))

(defun org-babel-exp-code (lang body params type)
    (case type
      ('inline (format "=%s=" body))
      ('block (format "#+BEGIN_SRC %s\n%s%s\n#+END_SRC" lang body
		      (if (string-match "\n$" body) "" "\n")))
      ('lob (save-excursion
	      (re-search-backward org-babel-lob-one-liner-regexp)
	      (format "#+BEGIN_SRC org-babel-lob\n%s\n#+END_SRC"
                      (first (org-babel-lob-get-info)))))))

(defun org-babel-exp-results (lang body params type)
  (let ((params
         ;; lets ensure that we lookup references in the original file
         (mapcar (lambda (pair)
                   (if (and (eq (car pair) :var)
                            (string-match org-babel-ref-split-regexp (cdr pair)))
                       `(:var . ,(concat (match-string 1 (cdr pair))
                                         "=" org-current-export-file
                                         ":" (match-string 2 (cdr pair))))
                     pair)) params)))
    (case type
      ('inline
        (let ((raw (org-babel-execute-src-block
                    nil (list lang body params) '((:results . "silent"))))
              (result-params (split-string (cdr (assoc :results params)))))
          (cond ;; respect the value of the :results header argument
           ((member "file" result-params)
            (org-babel-result-to-file raw))
           ((or (member "raw" result-params) (member "org" result-params))
            raw)
           ((member "code" result-params)
            (format "src_%s{%s}" lang raw))
           (t
            (if (stringp raw)
		(if (= 0 (length raw)) "=(no results)="
		  (format "=%s=" raw))
	      (format "=%S=" raw))))))
      ('block
          (save-excursion ;; org-exp-blocks places us at the end of the block
            (re-search-backward org-babel-src-block-regexp nil t)
            (org-babel-execute-src-block
             nil nil (org-babel-merge-params params '((:results . "replace")))) ""))
      ('lob
          (save-excursion
            (re-search-backward org-babel-lob-one-liner-regexp nil t)
            (org-babel-execute-src-block
             nil (list lang body (org-babel-merge-params params '((:results . "replace"))))) "")))))

(provide 'org-babel-exp)
;;; org-babel-exp.el ends here
