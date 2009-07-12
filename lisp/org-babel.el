;;; org-babel.el --- facilitating communication between programming languages and people

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

;; See org-babel.org in the parent directory for more information

;;; Code:
(require 'org)

(defun org-babel-execute-src-block-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-execute-src-block current-prefix-arg info) t) nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-src-block-maybe)

(defun org-babel-pop-to-session-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-pop-to-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-pop-to-session current-prefix-arg info) t) nil)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defvar org-babel-default-header-args '((:session . "none"))
  "Default arguments to use when evaluating a source block.")

(defvar org-babel-default-inline-header-args '((:results . "silent") (:exports . "results"))
  "Default arguments to use when evaluating an inline source block.")

(defvar org-babel-src-block-regexp nil
  "Regexp used to test when inside of a org-babel src-block")

(defvar org-babel-inline-src-block-regexp nil
  "Regexp used to test when on an inline org-babel src-block")

(defun org-babel-named-src-block-regexp-for-name (name)
  "Regexp used to match named src block."
  (concat "#\\+srcname:[ \t]*" (regexp-quote name) "[ \t\n]*"
	  org-babel-src-block-regexp))

(defun org-babel-set-interpreters (var value)
  (set-default var value)
  (setq org-babel-src-block-regexp
	(concat "#\\+begin_src \\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)[ \t]*"
                "\\([ \t]+\\([^\n]+\\)\\)?\n" ;; match header arguments
                "\\([^\000]+?\\)#\\+end_src"))
  (setq org-babel-inline-src-block-regexp
	(concat "src_\\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)"
                "\\(\\|\\[\\(.*\\)\\]\\)"
                "{\\([^\n]+\\)}")))

(defun org-babel-add-interpreter (interpreter)
  "Add INTERPRETER to `org-babel-interpreters' and update
`org-babel-src-block-regexp' appropriately."
  (unless (member interpreter org-babel-interpreters)
    (setq org-babel-interpreters (cons interpreter org-babel-interpreters))
    ;; (add-to-list 'org-babel-session-defaults (cons interpreter (format "org-babel-%s" interpreter)))
    (org-babel-set-interpreters 'org-babel-interpreters org-babel-interpreters)))

(defcustom org-babel-interpreters '()
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are

R          Evaluate R code
emacs-lisp Evaluate Emacs Lisp code and display the result
sh         Pass command to the shell and display the result
perl       The perl interpreter
python     The python interpreter
ruby       The ruby interpreter
babel      A degenerate source block (no body) to implement library-of-babel calls

The source block regexp `org-babel-src-block-regexp' is updated
when a new interpreter is added to this list through the
customize interface.  To add interpreters to this variable from
lisp code use the `org-babel-add-interpreter' function."
  :group 'org-babel
  :set 'org-babel-set-interpreters
  :type '(set :greedy t
              (const "R")
	      (const "emacs-lisp")
              (const "sh")
	      (const "perl")
	      (const "python")
	      (const "ruby")
  	      (const "babel")))

;;; functions
(defun org-babel-pop-to-session (&optional arg info)
  "Pop to the session of the current source-code block.  If
called with a prefix argument then evaluate the header arguments
for the source block before entering the session.  Copy the body
of the source block to the kill ring."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (params (third info))
         (session (cdr (assoc :session params))))
    (unless (member lang org-babel-interpreters)
      (error "Language is not in `org-babel-interpreters': %s" lang))
    ;; copy body to the kill ring
    (with-temp-buffer (insert (org-babel-trim body)) (copy-region-as-kill (point-min) (point-max)))
    ;; if called with a prefix argument, then process header arguments
    (if arg (funcall (intern (concat "org-babel-prep-session:" lang)) session params))
    ;; just to the session using pop-to-buffer
    (pop-to-buffer (funcall (intern (format "org-babel-%s-initiate-session" lang)) session))
    (move-end-of-line 1)))

(defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block, and dump the results
into the buffer immediately following the block.  Results are
commented by `org-toggle-fixed-width-section'.  With optional
prefix don't dump results into buffer but rather return the
results in raw elisp (this is useful for automated execution of a
source block).

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the source code block."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (params (org-combine-plists params (third info)))
         (cmd (intern (concat "org-babel-execute:" lang)))
         result)
    ;; (message "params=%S" params) ;; debugging statement
    (unless (member lang org-babel-interpreters)
      (error "Language is not in `org-babel-interpreters': %s" lang))
    (setq result (funcall cmd body params))
    ;; possibly force result into a vector
    (if (and (not (listp result)) (cdr (assoc :results params))
             (member "vector" (split-string (cdr (assoc :results params)))))
        (setq result (list result)))
    (if arg
        (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
      (org-babel-insert-result result (cdr (assoc :results params))))
    result))

(defun org-babel-eval-buffer (&optional arg)
  "Replace EVAL snippets in the entire buffer."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-regexp nil t)
      (org-babel-eval-src-block arg))))

(defun org-babel-eval-subtree (&optional arg)
  "Replace EVAL snippets in the entire subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (org-babel-eval-buffer)
    (widen)))

(defun org-babel-get-src-block-name ()
  "Return the name of the current source block if one exists"
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (org-babel-where-is-src-block-head))
      (if (save-excursion (forward-line -1)
                          (looking-at "#\\+srcname:[ \f\t\n\r\v]*\\([^ \f\t\n\r\v]+\\)"))
          (org-babel-clean-text-properties (match-string 1))))))

(defun org-babel-get-src-block-info ()
  "Return the information of the current source block as a list
of the following form.  (language body header-arguments-alist)"
  (let ((case-fold-search t) head)
    (if (setq head (org-babel-where-is-src-block-head))
        (save-excursion (goto-char head) (org-babel-parse-src-block-match))
      (if (save-excursion ;; inline source block
            (re-search-backward "[ \f\t\n\r\v]" nil t)
            (forward-char 1)
            (looking-at org-babel-inline-src-block-regexp))
          (org-babel-parse-inline-src-block-match)
        nil)))) ;; indicate that no source block was found

(defmacro org-babel-map-source-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE."
  (declare (indent 1))
  `(save-window-excursion
     (find-file ,file) (goto-char (point-min))
     (while (re-search-forward org-babel-src-block-regexp nil t)
       (goto-char (match-beginning 0))
       (save-match-data ,@body)
       (goto-char (match-end 0)))))

(defun org-babel-parse-src-block-match ()
  (list (org-babel-clean-text-properties (match-string 1))
        (org-babel-strip-protective-comas (org-babel-clean-text-properties (match-string 4)))
        (org-combine-plists org-babel-default-header-args
                            (org-babel-parse-header-arguments (org-babel-clean-text-properties (or (match-string 3) ""))))))

(defun org-babel-parse-inline-src-block-match ()
  (list (org-babel-clean-text-properties (match-string 1))
        (org-babel-strip-protective-comas (org-babel-clean-text-properties (match-string 4)))
        (org-combine-plists org-babel-default-inline-header-args
                            (org-babel-parse-header-arguments (org-babel-clean-text-properties (or (match-string 3) ""))))))

(defun org-babel-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (delq nil
        (mapcar
         (lambda (arg) (if (string-match "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)" arg)
                           (cons (intern (concat ":" (match-string 1 arg))) (org-babel-chomp (match-string 2 arg)))
                         (cons (intern (concat ":" arg)) nil)))
         (split-string (concat " " arg-string) "[ \f\t\n\r\v]+:" t))))

(defun org-babel-where-is-src-block-head ()
  "Return the point at the beginning of the current source
block.  Specifically at the beginning of the #+BEGIN_SRC line.
If the point is not on a source block then return nil."
  (let ((initial (point)) top bottom)
    (or
     (save-excursion ;; on a #+srcname: line
       (beginning-of-line 1)
       (and (looking-at "#\\+srcname") (forward-line 1)
            (looking-at org-babel-src-block-regexp)
            (point)))
     (save-excursion ;; on a #+begin_src line
       (beginning-of-line 1)
       (and (looking-at org-babel-src-block-regexp)
            (point)))
     (save-excursion ;; inside a src block
       (and
        (re-search-backward "#\\+begin_src" nil t) (setq top (point))
        (re-search-forward "#\\+end_src" nil t) (setq bottom (point))
        (< top initial) (< initial bottom)
        (goto-char top) (looking-at org-babel-src-block-regexp)
        (point))))))

(defun org-babel-goto-named-source-block (&optional name)
  "Go to a named source-code block."
  (interactive "ssource-block name: ")
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-show-context))
      (message "source-code block '%s' not found in this buffer" name))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by
#+srcname NAME, or nil if no such block exists. Set match data
according to org-babel-named-src-block-regexp."
  (save-excursion
    (let ((case-fold-search t)
	  (regexp (org-babel-named-src-block-regexp-for-name name)) msg)
      (goto-char (point-min))
      (when (or (re-search-forward regexp nil t)
                (re-search-backward regexp nil t))
        (match-beginning 0)))))

(defun org-babel-find-named-result (name)
  "Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "#\\+resname:[ \t]*" (regexp-quote name)) nil t)
      (move-beginning-of-line 1) (point))))

(defun org-babel-where-is-src-block-result ()
  "Return the point at the beginning of the result of the current
source block.  Specifically at the beginning of the #+RESNAME:
line.  If no result exists for this block then create a
#+RESNAME: line following the source block."
  (save-excursion
    (goto-char (org-babel-where-is-src-block-head))
    (let ((name (org-babel-get-src-block-name)) end head)
      (or (and name (message name) (org-babel-find-named-result name))
          (and (re-search-forward "#\\+end_src" nil t)
               (progn (move-end-of-line 1)
		      (if (eobp) (insert "\n") (forward-char 1))
		      (setq end (point))
                      (or (progn ;; either an unnamed #+resname: line already exists
                            (re-search-forward "[^ \f\t\n\r\v]" nil t)
                            (move-beginning-of-line 1) (looking-at "#\\+resname:"))
                          (progn ;; or we need to back up and make one ourselves
                            (goto-char end) (open-line 2) (forward-char 1)
                            (insert (concat "#+resname:" (if name (concat " " name))))
                            (move-beginning-of-line 1) t)))
               (point))))))

(defun org-babel-insert-result (result &optional insert)
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
        (setq result (org-babel-clean-text-properties result))
        (if (member "file" insert) (setq result (org-babel-result-to-file result))))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and insert (member "replace" insert)) (org-babel-remove-result))
  (if (= (length result) 0)
      (message "no result returned by source block")
    (if (and insert (member "silent" insert))
        (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result))) result)
      (when (and (stringp result) ;; ensure results end in a newline
                 (not (or (string-equal (substring result -1) "\n")
                          (string-equal (substring result -1) "\r"))))
        (setq result (concat result "\n")))
      (save-excursion
        (goto-char (org-babel-where-is-src-block-result)) (forward-line 1)
        (if (stringp result) ;; assume the result is a table if it's not a string
            (if (member "file" insert)
                (insert result)
              (org-babel-examplize-region (point) (progn (insert result) (point))))
          (progn
            (insert
             (concat (orgtbl-to-orgtbl
                      (if (consp (car result)) result (list result))
                      '(:fmt (lambda (cell) (format "%S" cell)))) "\n"))
            (forward-line -1)
            (org-cycle))))
      (message "finished"))))

(defun org-babel-result-to-org-string (result)
  "Return RESULT as a string in org-mode format.  This function
relies on `org-babel-insert-result'."
  (with-temp-buffer (org-babel-insert-result result) (buffer-string)))

(defun org-babel-remove-result ()
  "Remove the result of the current source block."
  (interactive)
  (save-excursion
    (goto-char (org-babel-where-is-src-block-result)) (forward-line 1)
    (delete-region (point) (org-babel-result-end))))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results"
  (save-excursion
    (if (org-at-table-p)
        (org-table-end)
      (while (if (looking-at "\\(: \\|\\[\\[\\)")
                 (progn (while (looking-at "\\(: \\|\\[\\[\\)")
                          (forward-line 1)) t))
        (forward-line 1))
      (forward-line -1)
      (point))))

(defun org-babel-result-to-file (result)
  "Return an `org-mode' link with the path being the value or
RESULT, and the display being the `file-name-nondirectory' if
non-nil."
  (let ((name (file-name-nondirectory result)))
    (concat "[[" result (if name (concat "][" name "]]") "]]"))))

(defun org-babel-examplize-region (beg end)
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

(defun org-babel-clean-text-properties (text)
  "Strip all properties from text return."
  (set-text-properties 0 (length text) nil text) text)

(defun org-babel-strip-protective-comas (body)
  "Strip protective comas from bodies of source blocks."
  (replace-regexp-in-string "^,#" "#" body))

(defun org-babel-read (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like lisp (meaning it starts with a
\"(\" or a \"'\") then read it as lisp, otherwise return it
unmodified as a string.

This is taken almost directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (or (org-babel-number-p cell)
          (if (or (equal "(" (substring cell 0 1))
                  (equal "'" (substring cell 0 1)))
              (read cell)
            (progn (set-text-properties 0 (length cell) nil cell) cell)))
    cell))

(defun org-babel-number-p (string)
  "Return t if STRING represents a number"
  (if (string-match "^[[:digit:]]*\\.?[[:digit:]]*$" string)
      (string-to-number string)))

(defun org-babel-import-elisp-from-file (file-name)
  "Read the results located at FILE-NAME into an elisp table.  If
the table is trivial, then return it as a scalar."
  (let (result)
    (with-temp-buffer
      (condition-case nil
          (progn
            (org-table-import file-name nil)
            (delete-file file-name)
            (setq result (mapcar (lambda (row)
                                   (mapcar #'org-babel-string-read row))
                                 (org-table-to-lisp))))
        (error nil))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
          (if (consp (car result))
              (if (null (cdr (car result)))
                  (caar result)
                result)
            (car result))
        result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around strings in exported R values."
  (org-babel-read (or (and (stringp cell)
                           (string-match "\\\"\\(.+\\)\\\"" cell)
                           (match-string 1 cell))
                      cell)))

(defun org-babel-reverse-string (string)
  (apply 'string (reverse (string-to-list string))))

(defun org-babel-chomp (string &optional regexp)
  "Remove any trailing space or carriage returns characters from
STRING.  Default regexp used is \"[ \f\t\n\r\v]\" but can be
overwritten by specifying a regexp as a second argument."
  (while (and (> (length string) 0) (string-match "[ \f\t\n\r\v]" (substring string -1)))
    (setq string (substring string 0 -1)))
  string)

(defun org-babel-trim (string &optional regexp)
  "Like `org-babel-chomp' only it runs on both the front and back of the string"
  (org-babel-chomp (org-babel-reverse-string
                    (org-babel-chomp (org-babel-reverse-string string) regexp)) regexp))

(provide 'org-babel)
;;; org-babel.el ends here
