;;; litorgy.el --- literate programming in org-mode

;; Copyright (C) 2009 Eric Schulte, Dan Davison, Austin F. Frank

;; Author: Eric Schulte, Dan Davison, Austin F. Frank
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

;; See rorg.org in the parent directory for more information

;;; Code:
(require 'org)

(defun litorgy-execute-src-block-maybe ()
  "Detect if this is context for a litorgical src-block and if so
then run `litorgy-execute-src-block'."
  (interactive)
  (let ((info (litorgy-get-src-block-info)))
    (if info (progn (litorgy-execute-src-block current-prefix-arg info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'litorgy-execute-src-block-maybe)

(defvar litorgy-inline-header-args '((:results . "silent") (:exports . "results"))
  "Default arguments to use when evaluating an inline source block.")

(defvar litorgy-src-block-regexp nil
  "Regexp used to test when inside of a litorgical src-block")

(defvar litorgy-inline-src-block-regexp nil
  "Regexp used to test when on an inline litorgical src-block")

(defun litorgy-set-interpreters (var value)
  (set-default var value)
  (setq litorgy-src-block-regexp
	(concat "#\\+begin_src \\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)[ \t]*"
                "\\([ \t]+\\([^\n]+\\)\\)?\n" ;; match header arguments
                "\\([^\000]+?\\)#\\+end_src"))
  (setq litorgy-inline-src-block-regexp
	(concat "src_\\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)"
                "\\(\\|\\[\\(.*\\)\\]\\)"
                "{\\([^\n]+\\)}")))

(defun litorgy-add-interpreter (interpreter)
  "Add INTERPRETER to `litorgy-interpreters' and update
`litorgy-src-block-regexp' appropriately."
  (unless (member interpreter litorgy-interpreters)
    (setq litorgy-interpreters (cons interpreter litorgy-interpreters))
    (litorgy-set-interpreters 'litorgy-interpreters litorgy-interpreters)))

(defcustom litorgy-interpreters '()
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are

R          Evaluate R code
emacs-lisp Evaluate Emacs Lisp code and display the result
sh         Pass command to the shell and display the result
perl       The perl interpreter
python     The python interpreter
ruby       The ruby interpreter

The source block regexp `litorgy-src-block-regexp' is updated
when a new interpreter is added to this list through the
customize interface.  To add interpreters to this variable from
lisp code use the `litorgy-add-interpreter' function."
  :group 'litorgy
  :set 'litorgy-set-interpreters
  :type '(set :greedy t
              (const "R")
	      (const "emacs-lisp")
              (const "sh")
	      (const "perl")
	      (const "python")
	      (const "ruby")))

;;; functions
(defun litorgy-execute-src-block (&optional arg info params)
  "Execute the current source code block, and dump the results
into the buffer immediately following the block.  Results are
commented by `org-toggle-fixed-width-section'.  With optional
prefix don't dump results into buffer but rather return the
results in raw elisp (this is useful for automated execution of a
source block).

Optionally supply a value for INFO in the form returned by
`litorgy-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the source code block."
  (interactive)
  (let* ((info (or info (litorgy-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (params (org-combine-plists (third info) params))
         (cmd (intern (concat "litorgy-execute:" lang)))
         result)
    ;; (message (format "params=%S" params)) ;; debugging statement
    (unless (member lang litorgy-interpreters)
      (error "Language is not in `litorgy-interpreters': %s" lang))
    (setq result (funcall cmd body params))
    ;; possibly force result into a vector
    (if (and (not (listp result)) (cdr (assoc :results params))
             (member "vector" (split-string (cdr (assoc :results params)))))
        (setq result (list result)))
    (if arg
        (message (format "%S" result))
      (litorgy-insert-result result (cdr (assoc :results params))))
    result))

(defun litorgy-eval-buffer (&optional arg)
  "Replace EVAL snippets in the entire buffer."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward litorgy-regexp nil t)
      (litorgy-eval-src-block arg))))

(defun litorgy-eval-subtree (&optional arg)
  "Replace EVAL snippets in the entire subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (litorgy-eval-buffer)
    (widen)))

(defun litorgy-get-src-block-name ()
  "Return the name of the current source block if one exists"
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (litorgy-where-is-src-block-head))
      (if (save-excursion (forward-line -1)
                          (looking-at "#\\+srcname:[ \f\t\n\r\v]*\\([^ \f\t\n\r\v]+\\)"))
          (litorgy-clean-text-properties (match-string 1))))))

(defun litorgy-get-src-block-info ()
  "Return the information of the current source block as a list
of the following form.  (language body header-arguments-alist)"
  (let ((case-fold-search t) head)
    (if (setq head (litorgy-where-is-src-block-head))
        (save-excursion (goto-char head) (litorgy-parse-src-block-match))
      (if (save-excursion ;; inline source block
            (re-search-backward "[ \f\t\n\r\v]" nil t)
            (forward-char 1)
            (looking-at litorgy-inline-src-block-regexp))
          (litorgy-parse-inline-src-block-match)
        nil)))) ;; indicate that no source block was found

(defun litorgy-parse-src-block-match ()
  (list (litorgy-clean-text-properties (match-string 1))
        (litorgy-clean-text-properties (match-string 4))
        (litorgy-parse-header-arguments (litorgy-clean-text-properties (or (match-string 3) "")))))

(defun litorgy-parse-inline-src-block-match ()
  (list (litorgy-clean-text-properties (match-string 1))
        (litorgy-clean-text-properties (match-string 4))
        (org-combine-plists litorgy-inline-header-args
                            (litorgy-parse-header-arguments (litorgy-clean-text-properties (or (match-string 3) ""))))))

(defun litorgy-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (delq nil
        (mapcar
         (lambda (arg) (if (string-match "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]*\\([^ \f\t\n\r\v]+.*\\)" arg)
                           (cons (intern (concat ":" (match-string 1 arg))) (match-string 2 arg))))
         (split-string (concat " " arg-string) "[ \f\t\n\r\v]+:" t))))

(defun litorgy-where-is-src-block-head ()
  "Return the point at the beginning of the current source
block.  Specifically at the beginning of the #+BEGIN_SRC line.
If the point is not on a source block then return nil."
  (let ((initial (point)) top bottom)
    (or
     (save-excursion ;; on a #+srcname: line
       (beginning-of-line 1)
       (and (looking-at "#\\+srcname") (forward-line 1)
            (looking-at litorgy-src-block-regexp)
            (point)))
     (save-excursion ;; on a #+begin_src line
       (beginning-of-line 1)
       (and (looking-at litorgy-src-block-regexp)
            (point)))
     (save-excursion ;; inside a src block
       (and
        (re-search-backward "#\\+begin_src" nil t) (setq top (point))
        (re-search-forward "#\\+end_src" nil t) (setq bottom (point))
        (< top initial) (< initial bottom)
        (goto-char top) (looking-at litorgy-src-block-regexp)
        (point))))))

(defun litorgy-find-named-result (name)
  "Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "#\\+resname:[ \t]*" (regexp-quote name)) nil t)
      (move-beginning-of-line 1) (point))))

(defun litorgy-where-is-src-block-result ()
  "Return the point at the beginning of the result of the current
source block.  Specifically at the beginning of the #+RESNAME:
line.  If no result exists for this block then create a
#+RESNAME: line following the source block."
  (save-excursion
    (goto-char (litorgy-where-is-src-block-head))
    (let ((name (litorgy-get-src-block-name)) end head)
      (or (and name (message name) (litorgy-find-named-result name))
          (and (re-search-forward "#\\+end_src" nil t)
               (progn (move-end-of-line 1) (forward-char 1) (setq end (point))
                      (or (progn ;; either an unnamed #+resname: line already exists
                            (re-search-forward "[^ \f\t\n\r\v]" nil t)
                            (move-beginning-of-line 1) (looking-at "#\\+resname:"))
                          (progn ;; or we need to back up and make one ourselves
                            (goto-char end) (open-line 2) (forward-char 1)
                            (insert "#+resname:") (move-beginning-of-line 1) t)))
               (point))))))

(defun litorgy-insert-result (result &optional insert)
  "Insert RESULT into the current buffer after the end of the
current source block.  With optional argument INSERT controls
insertion of results in the org-mode file.  INSERT can take the
following values...

t ------ the default options, simply insert the results after the
         source block
         
replace - insert results after the source block replacing any
          previously inserted results

silent -- no results are inserted"
  (if insert (setq insert (split-string insert)))
  (if (stringp result)
      (progn
        (setq result (litorgy-clean-text-properties result))
        (if (member "file" insert) (setq result (litorgy-result-to-file result))))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and insert (member "replace" insert)) (litorgy-remove-result))
  (if (= (length result) 0)
      (message "no result returned by source block")
    (if (and insert (member "silent" insert))
        (progn (message (format "%S" result)) result)
      (when (and (stringp result) ;; ensure results end in a newline
                 (not (or (string-equal (substring result -1) "\n")
                          (string-equal (substring result -1) "\r"))))
        (setq result (concat result "\n")))
      (save-excursion
        (goto-char (litorgy-where-is-src-block-result)) (forward-line 1)
        (if (stringp result) ;; assume the result is a table if it's not a string
            (if (member "file" insert)
                (insert result)
              (litorgy-examplize-region (point) (progn (insert result) (point))))
          (progn
            (insert
             (concat (orgtbl-to-orgtbl
                      (if (consp (car result)) result (list result))
                      '(:fmt (lambda (cell) (format "%S" cell)))) "\n"))
            (forward-line -1)
            (org-cycle))))
      (message "finished"))))

(defun litorgy-result-to-org-string (result)
  "Return RESULT as a string in org-mode format.  This function
relies on `litorgy-insert-result'."
  (with-temp-buffer (litorgy-insert-result result) (buffer-string)))

(defun litorgy-remove-result ()
  "Remove the result of the current source block."
  (save-excursion
    (goto-char (litorgy-where-is-src-block-result)) (forward-line 1)
    (delete-region (point)
                   (save-excursion
                     (if (org-at-table-p)
                         (org-table-end)
                       (while (if (looking-at "\\(: \\|\\[\\[\\)")
                                  (progn (while (looking-at "\\(: \\|\\[\\[\\)")
                                           (forward-line 1)) t))
                         (forward-line 1))
                       (forward-line -1)
                       (point))))))

(defun litorgy-result-to-file (result)
  "Return an `org-mode' link with the path being the value or
RESULT, and the display being the `file-name-nondirectory' if
non-nil."
  (let ((name (file-name-nondirectory result)))
    (concat "[[" result (if name (concat "][" name "]]") "]]"))))

(defun litorgy-examplize-region (beg end)
  "Comment out region using the ': ' org example quote."
  (interactive "*r")
  (let ((size (abs (- (line-number-at-pos end)
		      (line-number-at-pos beg)))))
    (if (= size 0)
	(let ((result (buffer-substring beg end)))
	  (delete-region beg end)
	  (insert (concat ": " result)))
      (save-excursion
        (goto-char beg)
        (dotimes (n size)
          (move-beginning-of-line 1) (insert ": ") (forward-line 1))))))

(defun litorgy-clean-text-properties (text)
  "Strip all properties from text return."
  (set-text-properties 0 (length text) nil text) text)

(defun litorgy-read (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like a list (meaning it starts with a
'(') then read it as lisp, otherwise return it unmodified as a
string.

This is taken almost directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (if (litorgy-number-p cell)
          (string-to-number cell)
        (if (or (equal "(" (substring cell 0 1))
                (equal "'" (substring cell 0 1)))
            (read cell)
          (progn (set-text-properties 0 (length cell) nil cell) cell)))
    cell))

(defun litorgy-number-p (string)
  "Return t if STRING represents a number"
  (string-match "^[[:digit:]]*\\.?[[:digit:]]*$" string))

(defun litorgy-chomp (string &optional regexp)
  "Remove any trailing space or carriage returns characters from
STRING.  Default regexp used is \"[ \f\t\n\r\v]\" but can be
overwritten by specifying a regexp as a second argument."
  (while (string-match "[ \f\t\n\r\v]" (substring results -1))
    (setq results (substring results 0 -1)))
  results)

(provide 'litorgy)
;;; litorgy.el ends here
