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

(defadvice org-edit-special (around org-babel-prep-session-for-edit activate)
  "Prepare the current source block's session according to it's
header arguments before editing in an org-src buffer.  This
function is called when `org-edit-special' is called with a
prefix argument from inside of a source-code block."
  (when current-prefix-arg
    (let* ((info (org-babel-get-src-block-info))
           (lang (first info))
           (params (third info))
           (session (cdr (assoc :session params))))
      (when (and info session) ;; if we are in a source-code block which has a session
        (funcall (intern (concat "org-babel-prep-session:" lang)) session params))))
  ad-do-it)

(defadvice org-open-at-point (around org-babel-open-at-point activate)
  "If `point' is on a source code block, then open that block's
results with `org-babel-open-src-block-results', otherwise defer
to `org-open-at-point'."
  (interactive "P")
  (or (call-interactively #'org-babel-open-src-block-result) ad-do-it))

(defun org-babel-load-in-session-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-load-in-session current-prefix-arg info) t) nil)))

(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

(defun org-babel-pop-to-session-maybe ()
  "Detect if this is context for a org-babel src-block and if so
then run `org-babel-pop-to-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-pop-to-session current-prefix-arg info) t) nil)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code"))
  "Default arguments to use when evaluating a source block.")

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "silent") (:exports . "results"))
  "Default arguments to use when evaluating an inline source block.")

(defvar org-babel-src-block-regexp nil
  "Regexp used to test when inside of a org-babel src-block")

(defvar org-babel-inline-src-block-regexp nil
  "Regexp used to test when on an inline org-babel src-block")

(defvar org-babel-min-lines-for-block-output 10
  "If number of lines of output is equal to or exceeds this
  value, the output is placed in a
  #+begin_example...#+end_example block. Otherwise the output is
  marked as literal by inserting colons at the starts of the
  lines. This variable only takes effect if the :results output
  option is in effect.")

(defun org-babel-named-src-block-regexp-for-name (name)
  "Regexp used to match named src block."
  (concat "#\\+srcname:[ \t]*" (regexp-quote name) "[ \t\n]*"
	  (substring org-babel-src-block-regexp 1)))

(defun org-babel-set-interpreters (var value)
  (set-default var value)
  (setq org-babel-src-block-regexp
	(concat "^[ \t]*#\\+begin_src[ \t]+\\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)[ \t]*"
                "\\([ \t]+\\([^\n]+\\)\\)?\n" ;; match header arguments
                "\\([^\000]+?\\)#\\+end_src"))
  (setq org-babel-inline-src-block-regexp
	(concat "[ \f\t\n\r\v]\\(src_"                ;; (1)   replacement target
		"\\("                                 ;; (2)   lang
		(mapconcat 'regexp-quote value "\\|")
		"\\)"
                "\\(\\|\\[\\(.*\\)\\]\\)"             ;; (3,4) (unused, headers)
                "{\\([^\f\n\r\v]+\\)}"                ;; (5)   body
		"\\)")))

(defun org-babel-add-interpreter (interpreter)
  "Add INTERPRETER to `org-babel-interpreters' and update
`org-babel-src-block-regexp' appropriately."
  (unless (member interpreter org-babel-interpreters)
    (setq org-babel-interpreters (cons interpreter org-babel-interpreters))
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
	      (const "ruby")))

;;; functions
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
  ;; (message "supplied params=%S" params) ;; debugging
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (params (org-babel-merge-params
                  (third info) (org-babel-get-src-block-function-args) params))
         (body (if (assoc :noweb params)
                   (org-babel-expand-noweb-references info) (second info)))
         (processed-params (org-babel-process-params params))
         (result-params (third processed-params))
         (result-type (fourth processed-params))
         (cmd (intern (concat "org-babel-execute:" lang)))
         result)
    ;; (message "params=%S" params) ;; debugging statement
    ;; (message "vars=%S" (second processed-params)) ;; debugging statement
    (unless (member lang org-babel-interpreters)
      (error "Language is not in `org-babel-interpreters': %s" lang))
    (when arg (setq result-params (cons "silent" result-params)))
    (setq result (multiple-value-bind (session vars result-params result-type) processed-params
                   (funcall cmd body params)))
    (if (eq result-type 'value)
        (setq result (org-babel-process-value-result result result-params)))
    (org-babel-insert-result result result-params)
    result))

(defun org-babel-load-in-session (&optional arg info)
  "Load the body of the current source-code block.  Evaluate the
header arguments for the source block before entering the
session.  After loading the body this pops open the session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (params (third info))
         (session (cdr (assoc :session params))))
    (unless (member lang org-babel-interpreters)
      (error "Language is not in `org-babel-interpreters': %s" lang))
    ;; if called with a prefix argument, then process header arguments
    (pop-to-buffer (funcall (intern (concat "org-babel-load-session:" lang)) session body params))
    (move-end-of-line 1)))

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

(defun org-babel-open-src-block-result (&optional re-run)
  "If `point' is on a src block then open the results of the
source code block, otherwise return nil.  With optional prefix
argument RE-RUN the source-code block is evaluated even if
results already exist."
  (interactive "P")
  (when (org-babel-get-src-block-info)
    (save-excursion
      ;; go to the results, if there aren't any then run the block
      (goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
                     (progn (org-babel-execute-src-block)
                            (org-babel-where-is-src-block-result))))
      (move-end-of-line 1) (forward-char 1)
      ;; open the results
      (if (looking-at org-bracket-link-regexp)
          ;; file results
          (org-open-at-point)
        (let ((results (org-babel-read-result)))
          (flet ((echo-res (result)
                           (if (stringp result) result (format "%S" result))))
            (pop-to-buffer (get-buffer-create "org-babel-results"))
            (delete-region (point-min) (point-max))
            (if (listp results)
                ;; table result
                (insert (orgtbl-to-generic results '(:sep "\t" :fmt echo-res)))
              ;; scalar result
              (insert (echo-res results))))))
      t)))

(defun org-babel-process-value-result (result result-params)
  "Process returned value for insertion in buffer.

Currently, this function forces to table output if :results
table or :results vector has been supplied.

  You can see below the various fragments of results-processing
code that were present in the language-specific files. Out of
those fragments, I've moved the org-babel-python-table-or-results
and org-babel-import-elisp-from-file functionality into the
org-babel-*-evaluate functions. I think those should only be used
in the :results value case, as in the 'output case we are not
concerned with creating elisp versions of results. "

  (if (and (or (member "vector" result-params)
               (member "table" result-params))
           (not (listp result)))
      (list (list result))
    result))

(defun org-babel-execute-buffer (&optional arg)
  "Replace EVAL snippets in the entire buffer."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (goto-char (match-beginning 0))
      (org-babel-execute-src-block arg)
      (goto-char (match-end 0)))))

(defun org-babel-execute-subtree (&optional arg)
  "Replace EVAL snippets in the entire subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (org-babel-execute-buffer)
    (widen)))

(defun org-babel-get-src-block-name ()
  "Return the name of the current source block if one exists.

This function is analogous to org-babel-lob-get-info. For both
functions, after they are called, (match-string 1) matches the
function name, and (match-string 3) matches the function
arguments inside the parentheses. I think perhaps these functions
should be renamed to bring out this similarity, perhaps involving
the word 'call'.

Currently the function `org-babel-get-src-block-function-args'
relies on the match-data from a match in this function.  I think
splitting a match and the use of it's data is bad form, and we
should re-work these two functions, perhaps combining them into
one function which returns more data than just the name. [Eric]"
  (let ((case-fold-search t)
	(head (org-babel-where-is-src-block-head)))
    (if head
	(save-excursion
	  (goto-char head)
	  (if (save-excursion
		(forward-line -1)
                ;; the second match of this regexp is used later to
                ;; find arguments in the "functional" style, where
                ;; they are passed as part of the source name line
		(looking-at "#\\+srcname:[ \t]*\\([^ ()\f\t\n\r\v]+\\)\\(\(\\(.*\\)\)\\|\\)"))
	      (org-babel-clean-text-properties (match-string 1)))))))

(defun org-babel-get-src-block-info ()
  "Return the information of the current source block as a list
of the following form.  (language body header-arguments-alist)"
  (let ((case-fold-search t) head)
    (if (setq head (org-babel-where-is-src-block-head))
        (save-excursion (goto-char head) (org-babel-parse-src-block-match))
      (if (save-excursion ;; inline source block
            (re-search-backward "[ \f\t\n\r\v]" nil t)
            (looking-at org-babel-inline-src-block-regexp))
          (org-babel-parse-inline-src-block-match)
        nil)))) ;; indicate that no source block was found

(defun org-babel-get-src-block-function-args ()
  (when (org-babel-get-src-block-name)
    (mapcar (lambda (ref) (cons :var ref))
	    (org-babel-ref-split-args (match-string 3)))))

(defmacro org-babel-map-source-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE."
  (declare (indent 1))
  `(let ((visited-p (get-buffer (file-name-nondirectory ,file))))
     (save-window-excursion
       (find-file ,file) (goto-char (point-min))
       (while (re-search-forward org-babel-src-block-regexp nil t)
         (goto-char (match-beginning 0))
         (save-match-data ,@body)
         (goto-char (match-end 0))))
     (unless visited-p (kill-buffer (file-name-nondirectory file)))))

(defun org-babel-params-from-properties ()
  "Return an association list of any source block params which
may be specified in the properties of the current outline entry."
  (save-match-data
    (delq nil
          (mapcar
           (lambda (header-arg)
             (let ((val (or (org-entry-get (point) header-arg 'selective)
			    (cdr (assoc header-arg org-file-properties)))))
               (when val
                 ;; (message "param-from-property %s=%s" header-arg val) ;; debugging statement
                 (cons (intern (concat ":" header-arg)) val))))
           '("exports" "results" "session" "tangle" "var")))))

(defun org-babel-parse-src-block-match ()
  (let* ((lang (org-babel-clean-text-properties (match-string 1)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang)))
         (body (org-babel-clean-text-properties (match-string 4)))
	 (preserve-indentation org-src-preserve-indentation))
    (list lang
          ;; get src block body removing properties, protective commas, and indentation
          (with-temp-buffer
            (save-match-data
              (insert (org-babel-strip-protective-commas body))
	      (unless preserve-indentation (org-do-remove-indentation))
              (buffer-string)))
	  (org-babel-merge-params
	   org-babel-default-header-args
           (org-babel-params-from-properties)
	   (if (boundp lang-headers) (eval lang-headers) nil)
	   (org-babel-parse-header-arguments (org-babel-clean-text-properties (or (match-string 3) "")))))))

(defun org-babel-parse-inline-src-block-match ()
  (let* ((lang (org-babel-clean-text-properties (match-string 2)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang))))
    (list lang
          (org-babel-strip-protective-commas (org-babel-clean-text-properties (match-string 5)))
          (org-babel-merge-params
           org-babel-default-inline-header-args
           (org-babel-params-from-properties)
           (if (boundp lang-headers) (eval lang-headers) nil)
           (org-babel-parse-header-arguments (org-babel-clean-text-properties (or (match-string 4) "")))))))

(defun org-babel-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (if (> (length arg-string) 0)
      (delq nil
	    (mapcar
	     (lambda (arg)
	       (if (string-match "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)" arg)
		   (cons (intern (concat ":" (match-string 1 arg)))
			 (org-babel-chomp (match-string 2 arg)))
		 (cons (intern (concat ":" arg)) nil)))
	     (split-string (concat " " arg-string) "[ \f\t\n\r\v]+:" t)))))

(defun org-babel-process-params (params)
  "Parse params and resolve references.

Return a list (session vars result-params result-type). These are
made available to the org-babel-execute:LANG functions via
multiple-value-bind."
  (let* ((session (cdr (assoc :session params)))
	 (vars (org-babel-ref-variables params))
	 (result-params (split-string (or (cdr (assoc :results params)) "")))
	 (result-type (cond ((member "output" result-params) 'output)
			    ((member "value" result-params) 'value)
			    (t 'value))))
    (list session vars result-params result-type)))

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
        (goto-char top) (move-beginning-of-line 1)
        (looking-at org-babel-src-block-regexp)
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
    (when (re-search-forward ;; ellow end-of-buffer in following regexp?
	   (concat "#\\+resname:[ \t]*" (regexp-quote name) "[ \t\n\f\v\r]") nil t)
      (move-beginning-of-line 0) (point))))

(defun org-babel-where-is-src-block-result (&optional insert)
  "Return the point at the beginning of the result of the current
source block.  Specifically at the beginning of the #+RESNAME:
line.  If no result exists for this block then create a
#+RESNAME: line following the source block."
  (save-excursion
    (let* ((on-lob-line (progn (beginning-of-line 1)
			       (looking-at org-babel-lob-one-liner-regexp)))
	   (name (if on-lob-line (first (org-babel-lob-get-info)) (org-babel-get-src-block-name)))
	   (head (unless on-lob-line (org-babel-where-is-src-block-head))) end)
      (when head (goto-char head))
      (or (and name (org-babel-find-named-result name))
          (and (or on-lob-line (re-search-forward "#\\+end_src" nil t))
               (progn (move-end-of-line 1)
		      (if (eobp) (insert "\n") (forward-char 1))
		      (setq end (point))
                      (or (and (not name)
			       (progn ;; either the unnamed #+resname: line already exists
				 (re-search-forward "[^ \f\t\n\r\v]" nil t)
				 (move-beginning-of-line 1) (looking-at "#\\+resname:\n")))
			  ;; or (with optional insert) we need to back up and make one ourselves
                          (when insert
                            (goto-char end) (open-line 2) (forward-char 1)
                            (insert (concat "#+resname:" (if name (concat " " name)) "\n"))
                            (move-beginning-of-line 0) t)))
               (point))))))

(defun org-babel-read-result ()
  "Read the result at `point' into emacs-lisp."
  (cond
   ((org-at-table-p) (org-babel-read-table))
   ((looking-at ": ")
    (let ((result-string
           (org-babel-trim
            (mapconcat (lambda (line) (if (and (> (length line) 1)
                                               (string= ": " (substring line 0 2)))
                                          (substring line 2)
                                        line))
                       (split-string
                        (buffer-substring (point) (org-babel-result-end)) "[\r\n]+")
                       "\n"))))
      (or (org-babel-number-p result-string) result-string)))
   ((looking-at "^#\\+RESNAME:")
    (save-excursion (forward-line 1) (org-babel-read-result)))))

(defun org-babel-read-table ()
  "Read the table at `point' into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar #'org-babel-read row)))
          (org-table-to-lisp)))

(defun org-babel-insert-result (result &optional insert)
  "Insert RESULT into the current buffer after the end of the
current source block.  With optional argument INSERT controls
insertion of results in the org-mode file.  INSERT can take the
following values...

replace - (default option) insert results after the source block
          replacing any previously inserted results

silent -- no results are inserted

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org-mode file syntax

raw ----- results are added directly to the org-mode file.  This
          is a good option if you code block will output org-mode
          formatted text.

org ----- this is the same as the 'raw' option

html ---- results are added inside of a #+BEGIN_HTML block.  This
          is a good option if you code block will output html
          formatted text.

latex --- results are added inside of a #+BEGIN_LATEX block.
          This is a good option if you code block will output
          latex formatted text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a #+BEGIN_SRC block with the source-code
          language set appropriately."
  (if (stringp result)
      (progn
        (setq result (org-babel-clean-text-properties result))
        (if (member "file" insert) (setq result (org-babel-result-to-file result))))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and insert (member "replace" insert) (not (member "silent" insert)))
      (org-babel-remove-result))
  (if (= (length result) 0)
      (if (member "value" result-params)
	  (message "No result returned by source block")
	(message "Source block produced no output"))
    (if (and insert (member "silent" insert))
        (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result))) result)
      (when (and (stringp result) ;; ensure results end in a newline
                 (not (or (string-equal (substring result -1) "\n")
                          (string-equal (substring result -1) "\r"))))
        (setq result (concat result "\n")))
      (save-excursion
	(let ((existing-result (org-babel-where-is-src-block-result t)))
	  (when existing-result (goto-char existing-result) (forward-line 1)))
        (cond
         ;; assume the result is a table if it's not a string
         ((not (stringp result))
          (insert (concat (orgtbl-to-orgtbl
                           (if (and (listp (car result)) (listp (cdr (car result))))
                               result (list result))
                           '(:fmt (lambda (cell) (format "%S" cell)))) "\n"))
          (forward-line -1) (org-cycle))
         ((member "file" insert)
          (insert result))
         ((member "html" insert)
          (insert (format "#+BEGIN_HTML\n%s#+END_HTML\n" result)))
         ((member "latex" insert)
          (insert (format "#+BEGIN_LaTeX\n%s#+END_LaTeX\n" result)))
         ((member "code" insert)
          (insert (format "#+BEGIN_SRC %s\n%s#+END_SRC\n" lang result)))
         ((or (member "raw" insert) (member "org" insert))
          (save-excursion (insert result)) (if (org-at-table-p) (org-cycle)))
         (t
          (org-babel-examplize-region (point) (progn (insert result) (point))))))
      (message "finished"))))

(defun org-babel-result-to-org-string (result)
  "Return RESULT as a string in org-mode format.  This function
relies on `org-babel-insert-result'."
  (with-temp-buffer (org-babel-insert-result result) (buffer-string)))

(defun org-babel-remove-result ()
  "Remove the result of the current source block."
  (interactive)
  (save-excursion
    (goto-char (org-babel-where-is-src-block-result t)) (forward-line 1)
    (delete-region (point) (org-babel-result-end))))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results"
  (save-excursion
    (if (org-at-table-p)
        (progn (goto-char (org-table-end)) (point))
      (let ((case-fold-search t))
        (cond
         ((looking-at "#\\+begin_latex")
          (search-forward "#+end_latex" nil t)
          (forward-line 1))
         ((looking-at "#\\+begin_html")
          (search-forward "#+end_html" nil t)
          (forward-line 1))
         ((looking-at "#\\+begin_example")
          (search-forward "#+end_example" nil t)
          (forward-line 1))
         ((looking-at "#\\+begin_src")
          (search-forward "#+end_src" nil t)
          (forward-line 1))
         (t (progn (while (looking-at "\\(: \\|\\[\\[\\)")
                     (forward-line 1))))))
      (point))))

(defun org-babel-result-to-file (result)
  "Return an `org-mode' link with the path being the value or
RESULT, and the display being the `file-name-nondirectory' if
non-nil."
  (concat "[[file:" result "]]"))

(defun org-babel-examplize-region (beg end)
  "Comment out region using the ': ' org example quote."
  (interactive "*r")
  (let ((size (abs (- (line-number-at-pos end)
		      (line-number-at-pos beg)))))
    (save-excursion
      (cond ((= size 0)
	     (error "This should be impossible: a newline was appended to result if missing")
	     (let ((result (buffer-substring beg end)))
	       (delete-region beg end)
	       (insert (concat ": " result))))
	    ((< size org-babel-min-lines-for-block-output)
	     (goto-char beg)
	     (dotimes (n size)
	       (move-beginning-of-line 1) (insert ": ") (forward-line 1)))
	    (t
	     (goto-char beg)
	     (insert "#+begin_example\n")
	     (forward-char (- end beg))
	     (insert "#+end_example\n"))))))

(defun org-babel-merge-params (&rest plists)
  "Combine all parameter association lists in PLISTS.  Later
elements of PLISTS override the values of previous element.  This
takes into account some special considerations for certain
parameters when merging lists."
  (let ((results-exclusive-groups
	 '(("file" "vector" "table" "scalar" "raw" "org" "html" "latex" "code" "pp")
	   ("replace" "silent")
	   ("output" "value")))
	(exports-exclusive-groups
	 '(("code" "results" "both" "none")))
	params results exports tangle vars var ref)
    (flet ((e-merge (exclusive-groups &rest result-params)
                    ;; maintain exclusivity of mutually exclusive parameters
                    (let (output)
                      (mapc (lambda (new-params)
                              (mapc (lambda (new-param)
                                      (mapc (lambda (exclusive-group)
                                              (when (member new-param exclusive-group)
                                                (mapcar (lambda (excluded-param)
                                                          (setq output (delete excluded-param output)))
                                                        exclusive-group)))
                                            exclusive-groups)
                                      (setq output (org-uniquify (cons new-param output))))
                                    new-params))
                            result-params)
                      output)))
      (mapc (lambda (plist)
              (mapc (lambda (pair)
                      (case (car pair)
                        (:var
                         ;; we want only one specification per variable
                         (when (string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*=[ \t]*\\([^\f\n\r\v]+\\)$" (cdr pair))
                           ;; TODO: When is this not true?
                           (setq var (intern (match-string 1 (cdr pair)))
                                 ref (match-string 2 (cdr pair))
                                 vars (cons (cons var ref) (assq-delete-all var vars)))))
                        (:results
                         (setq results
			       (e-merge results-exclusive-groups results (split-string (cdr pair)))))
			(:file
			 (when (cdr pair)
			   (setq results (e-merge results-exclusive-groups results '("file")))
			   (unless (or (member "both" exports) (member "none" exports))
			     (setq exports (e-merge exports-exclusive-groups exports '("results"))))
			   (setq params (cons pair (assq-delete-all (car pair) params)))))
                        (:exports
                         (setq exports (e-merge exports-exclusive-groups
                                                exports (split-string (cdr pair)))))
                        (:tangle
                         (setq tangle (e-merge '(("yes" "no"))
                                               tangle (split-string (cdr pair)))))
                        (t ;; replace: this covers e.g. :session
                         (setq params (cons pair (assq-delete-all (car pair) params))))))
                    plist))
            plists))
    (setq vars (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) vars))
    (while vars (setq params (cons (cons :var (pop vars)) params)))
    (cons (cons :tangle (mapconcat 'identity tangle " "))
          (cons (cons :exports (mapconcat 'identity exports " "))
                (cons (cons :results (mapconcat 'identity results " "))
                      params)))))

(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "This function expands Noweb style references in the body of
the current source-code block.  For example the following
reference would be replaced with the body of the source-code
block named 'example-block' (assuming the '#' character starts a
comment) .

# <<example-block>>

This function must be called from inside of the buffer containing
the source-code block which holds BODY."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info)))
         (lang (first info))
         (body (second info))
         (new-body "") index source-name)
    (flet ((nb-add (text)
                   (setq new-body (concat new-body text))))
      (with-temp-buffer
        (insert body) (goto-char (point-min))
        (funcall (intern (concat (or (and (cdr (assoc lang org-src-lang-modes))
                                          (symbol-name
                                           (cdr (assoc lang org-src-lang-modes))))
                                     lang) "-mode")))
        (setq index (point))
        (while (and (re-search-forward "<<\\(.+\\)>>" nil t))
          (save-match-data (setf source-name (match-string 1)))
          ;; add interval to new-body
          (goto-char (match-end 0)) (move-end-of-line nil)
          (nb-add (buffer-substring index (point)))
          (setq index (point))
          ;; if found, add body of referenced source-block
          (nb-add (save-excursion
                    (set-buffer parent-buffer)
                    (let ((point (org-babel-find-named-block source-name)))
                      (if point
                          (save-excursion
                            (goto-char point)
                            (concat "\n" (org-babel-expand-noweb-references
                                          (org-babel-get-src-block-info))))
                        "")))))
        (nb-add (buffer-substring index (point-max)))))
    new-body))

(defun org-babel-clean-text-properties (text)
  "Strip all properties from text return."
  (set-text-properties 0 (length text) nil text) text)

(defun org-babel-strip-protective-commas (body)
  "Strip protective commas from bodies of source blocks."
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
  (if (and (string-match "^-?[[:digit:]]*\\.?[[:digit:]]*$" string)
           (= (match-end 0) (length string)))
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
        (error nil)))
    (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
	(if (consp (car result))
	    (if (null (cdr (car result)))
		(caar result)
	      result)
	  (car result))
      result)))

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
