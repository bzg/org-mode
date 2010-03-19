;;; org-babel-exp.el --- Exportation of org-babel source blocks

;; Copyright (C) 2009 Eric Schulte, Dan Davison

;; Author: Eric Schulte, Dan Davison
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

(defvar org-babel-function-def-export-keyword "function"
  "When exporting a source block function, this keyword will
appear in the exported version in the place of source name
line. A source block is considered to be a source block function
if the source name is present and is followed by a parenthesized
argument list. The parentheses may be empty or contain
whitespace. An example is the following which generates n random
(uniform) numbers.

#+source: rand(n)
#+begin_src R
  runif(n)
#+end_src
")

(defvar org-babel-function-def-export-indent 4
  "When exporting a source block function, the block contents
will be indented by this many characters. See
`org-babel-function-def-export-name' for the definition of a
source block function.")

(defvar obe-marker nil)

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
  (message "org-babel-exp processing...")
  (when (member (first headers) org-babel-interpreters)
    (save-excursion
      (goto-char (match-beginning 0))
      (org-babel-exp-do-export (org-babel-get-src-block-info) 'block))))

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
                            (org-babel-exp-do-export info 'inline))))
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
		 (list "emacs-lisp" "results"
		       (org-babel-merge-params
			org-babel-default-header-args
			(org-babel-parse-header-arguments
			 (org-babel-clean-text-properties
			  (concat ":var results="
				  (mapconcat #'identity (org-babel-lob-get-info) " "))))))
		 'lob)))
	(setq end (+ end (- (length replacement) (length (match-string 0)))))
	(replace-match replacement t t)))))

(defun org-babel-exp-do-export (info type)
  (case (intern (or (cdr (assoc :exports (third info))) "code"))
    ('none "")
    ('code (org-babel-exp-code info type))
    ('results (org-babel-exp-results info type))
    ('both (concat (org-babel-exp-code info type)
                   "\n\n"
                   (org-babel-exp-results info type)))))

(defun org-babel-exp-code (info type)
  (let ((lang (first info))
        (body (second info))
        (switches (fourth info))
        (name (fifth info))
        (args (mapcar #'cdr
                      (remove-if-not (lambda (el) (eq :var (car el))) (third info)))))
    (case type
      ('inline (format "=%s=" body))
      ('block
          (let ((str (format "#+BEGIN_SRC %s %s\n%s%s#+END_SRC\n" lang switches body
                             (if (string-match "\n$" body) "" "\n"))))
            (when name (add-text-properties 0 (length str)
                                           (list 'org-caption
                                                 (format "%s(%s)"
                                                         name (mapconcat #'identity args ", ")))
                                           str))
           str))
      ('lob
       (let ((call-line (and (string-match "results=" (car args))
                             (substring (car args) (match-end 0)))))
         (cond
          ((eq backend 'html)
           (format "\n#+HTML: <label class=\"org-src-name\">%s</label>\n" call-line))
          ((t (format ": %s\n" call-line)))))))))

(defun org-babel-exp-results (info type)
  (let ((lang (first info))
	(body (second info))
	(params
         ;; lets ensure that we lookup references in the original file
         (mapcar (lambda (pair)
                   (if (and org-current-export-file
                            (eq (car pair) :var)
                            (string-match org-babel-ref-split-regexp (cdr pair)))
                       `(:var . ,(concat (match-string 1 (cdr pair))
                                         "=" org-current-export-file
                                         ":" (match-string 2 (cdr pair))))
                     pair))
		 (third info))))
    (case type
      ('inline
        (let ((raw (org-babel-execute-src-block
                    nil info '((:results . "silent"))))
              (result-params (split-string (cdr (assoc :results params)))))
          (cond ;; respect the value of the :results header argument
           ((member "file" result-params)
            (org-babel-result-to-file raw))
           ((or (member "raw" result-params) (member "org" result-params))
            (format "%s" raw))
           ((member "code" result-params)
            (format "src_%s{%s}" lang raw))
           (t
            (if (stringp raw)
		(if (= 0 (length raw)) "=(no results)="
		  (format "=%s=" raw))
	      (format "=%S=" raw))))))
      ('block
          (org-babel-execute-src-block
           nil nil (org-babel-merge-params params '((:results . "replace"))))
        "")
      ('lob
          (save-excursion
            (re-search-backward org-babel-lob-one-liner-regexp nil t)
            (org-babel-execute-src-block
             nil (list lang body (org-babel-merge-params params '((:results . "replace"))))) "")))))

(provide 'org-babel-exp)
;;; org-babel-exp.el ends here
