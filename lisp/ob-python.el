;;; ob-python.el --- Babel Functions for Python      -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
;; Maintainer: Jack Kamm <jackkamm@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

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

;; Org-Babel support for evaluating python source code.

;;; Code:
(require 'ob)
(require 'org-macs)
(require 'python)

(declare-function py-shell "ext:python-mode" (&rest args))
(declare-function py-toggle-shells "ext:python-mode" (arg))
(declare-function py-send-string-no-output "ext:python-mode" (strg &optional process buffer-name))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

(defvar org-babel-default-header-args:python '())

(defcustom org-babel-python-command "python"
  "Name of the command for executing Python code."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-babel
  :type 'string)

(defcustom org-babel-python-mode
  (if (featurep 'python-mode) 'python-mode 'python)
  "Preferred python mode for use in running python interactively.
This will typically be either `python' or `python-mode'."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defcustom org-babel-python-hline-to "None"
  "Replace hlines in incoming tables with this when translating to python."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-babel-python-None-to 'hline
  "Replace `None' in python tables with this before returning."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defun org-babel-execute:python (body params)
  "Execute a block of Python code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((org-babel-python-command
	  (or (cdr (assq :python params))
	      org-babel-python-command))
	 (session (org-babel-python-initiate-session
		   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (return-val (when (and (eq result-type 'value) (not session))
		       (cdr (assq :return params))))
	 (preamble (cdr (assq :preamble params)))
         (full-body
	  (org-babel-expand-body:generic
	   (concat body (if return-val (format "\nreturn %s" return-val) ""))
	   params (org-babel-variable-assignments:python params)))
         (result (org-babel-python-evaluate
		  session full-body result-type result-params preamble)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

(defun org-babel-prep-session:python (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references."
  (let* ((session (org-babel-python-initiate-session session))
	 (var-lines
	  (org-babel-variable-assignments:python params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session))
	    var-lines))
    session))

(defun org-babel-load-session:python (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:python session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:python (params)
  "Return a list of Python statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-python-var-to-python (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-python-var-to-python (var)
  "Convert an elisp value to a python variable.
Convert an elisp value, VAR, into a string of python source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-python-var-to-python var ", ") "]")
    (if (eq var 'hline)
	org-babel-python-hline-to
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-python-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (eq el 'None)
                            org-babel-python-None-to el))
                res)
      res)))

(defvar org-babel-python-buffers '((:default . "*Python*")))

(defun org-babel-python-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-python-buffers)))

(defun org-babel-python-with-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	name
      (format "*%s*" name))))

(defun org-babel-python-without-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	(substring name 1 (- (length name) 1))
      name)))

(defvar py-default-interpreter)
(defvar py-which-bufname)
(defvar python-shell-buffer-name)
(defun org-babel-python-initiate-session-by-key (&optional session)
  "Initiate a python session.
If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (py-buffer (org-babel-python-session-buffer session))
	   (cmd (if (member system-type '(cygwin windows-nt ms-dos))
		    (concat org-babel-python-command " -i")
		  org-babel-python-command)))
      (cond
       ((eq 'python org-babel-python-mode) ; python.el
	(unless py-buffer
	  (setq py-buffer (org-babel-python-with-earmuffs session)))
	(let ((python-shell-buffer-name
	       (org-babel-python-without-earmuffs py-buffer)))
	  (run-python cmd)
	  (sleep-for 0 10)))
       ((and (eq 'python-mode org-babel-python-mode)
	     (fboundp 'py-shell)) ; python-mode.el
	(require 'python-mode)
	;; Make sure that py-which-bufname is initialized, as otherwise
	;; it will be overwritten the first time a Python buffer is
	;; created.
	(py-toggle-shells py-default-interpreter)
	;; `py-shell' creates a buffer whose name is the value of
	;; `py-which-bufname' with '*'s at the beginning and end
	(let* ((bufname (if (and py-buffer (buffer-live-p py-buffer))
			    (replace-regexp-in-string ;; zap surrounding *
			     "^\\*\\([^*]+\\)\\*$" "\\1" py-buffer)
			  (concat "Python-" (symbol-name session))))
	       (py-which-bufname bufname))
	  (setq py-buffer (org-babel-python-with-earmuffs bufname))
	  (py-shell nil nil t org-babel-python-command py-buffer nil nil t nil)))
       (t
	(error "No function available for running an inferior Python")))
      (setq org-babel-python-buffers
	    (cons (cons session py-buffer)
		  (assq-delete-all session org-babel-python-buffers)))
      session)))

(defun org-babel-python-initiate-session (&optional session _params)
  "Create a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (org-babel-python-session-buffer
     (org-babel-python-initiate-session-by-key session))))

(defvar org-babel-python-eoe-indicator "'org_babel_python_eoe'"
  "A string to indicate that evaluation has completed.")

(defconst org-babel-python-wrapper-method
  "
def main():
%s

open('%s', 'w').write( str(main()) )")
(defconst org-babel-python-pp-wrapper-method
  "
import pprint
def main():
%s

open('%s', 'w').write( pprint.pformat(main()) )")

(defconst org-babel-python--exec-tmpfile "\
with open('%s') as f:
    exec(compile(f.read(), f.name, 'exec'))"
  "Template for Python session command with output results.

Has a single %s escape, the tempfile containing the source code
to evaluate.")

(defconst org-babel-python--eval-ast "\
import ast
with open('%s') as f:
    __org_babel_python_ast = ast.parse(f.read())

__org_babel_python_final = __org_babel_python_ast.body[-1]

if isinstance(__org_babel_python_final, ast.Expr):
    __org_babel_python_ast.body = __org_babel_python_ast.body[:-1]
    exec(compile(__org_babel_python_ast, '<string>', 'exec'))
    __org_babel_python_final = eval(compile(ast.Expression(
        __org_babel_python_final.value), '<string>', 'eval'))
    with open('%s', 'w') as f:
        if %s:
            import pprint
            f.write(pprint.pformat(__org_babel_python_final))
        else:
            f.write(str(__org_babel_python_final))
else:
    exec(compile(__org_babel_python_ast, '<string>', 'exec'))
    __org_babel_python_final = None"
  "Template for Python session command with value results.

Has three %s escapes to be filled in:
1. Tempfile containing source to evaluate.
2. Tempfile to write results to.
3. Whether to pretty print, \"True\" or \"False\".")

(defun org-babel-python-evaluate
  (session body &optional result-type result-params preamble)
  "Evaluate BODY as Python code."
  (if session
      (org-babel-python-evaluate-session
       session body result-type result-params)
    (org-babel-python-evaluate-external-process
     body result-type result-params preamble)))

(defun org-babel-python-evaluate-external-process
    (body &optional result-type result-params preamble)
  "Evaluate BODY in external python process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((raw
         (pcase result-type
           (`output (org-babel-eval org-babel-python-command
				    (concat preamble (and preamble "\n")
					    body)))
           (`value (let ((tmp-file (org-babel-temp-file "python-")))
		     (org-babel-eval
		      org-babel-python-command
		      (concat
		       preamble (and preamble "\n")
		       (format
			(if (member "pp" result-params)
			    org-babel-python-pp-wrapper-method
			  org-babel-python-wrapper-method)
			(with-temp-buffer
			  (python-mode)
			  (insert body)
			  (goto-char (point-min))
			  (while (not (eobp))
			    (unless (python-syntax-context 'string)
			      (python-indent-shift-right (line-beginning-position)
							 (line-end-position)))
			   (forward-line 1))
			 (buffer-string))
			(org-babel-process-file-name tmp-file 'noquote))))
		     (org-babel-eval-read-file tmp-file))))))
    (org-babel-result-cond result-params
      raw
      (org-babel-python-table-or-string (org-trim raw)))))

(defun org-babel-python-evaluate-session
    (session body &optional result-type result-params)
  "Pass BODY to the Python process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let* ((python-shell-buffer-name (org-babel-python-without-earmuffs session))
	 (send-wait (lambda () (comint-send-input nil t) (sleep-for 0 5)))
	 (input-body (lambda (body)
		       (dolist (line (split-string body "[\r\n]"))
			 (insert line)
			 (funcall send-wait))
		       (funcall send-wait)))
	 (tmp-src-file (org-babel-temp-file "python-"))
         (results
	  (progn
	    (with-temp-file tmp-src-file (insert body))
            (pcase result-type
	      (`output
	       (let ((src-str (format org-babel-python--exec-tmpfile
				      (org-babel-process-file-name
				       tmp-src-file 'noquote))))
		 (if (eq 'python-mode org-babel-python-mode)
		     (py-send-string-no-output
		      src-str (get-buffer-process session) session)
		   (python-shell-send-string-no-output src-str))))
              (`value
               (let* ((tmp-results-file (org-babel-temp-file "python-"))
		      (body (format org-babel-python--eval-ast
				    (org-babel-process-file-name
				     tmp-src-file 'noquote)
				    (org-babel-process-file-name
				     tmp-results-file 'noquote)
				    (if (member "pp" result-params)
					"True" "False"))))
		 (org-babel-comint-with-output
                     (session org-babel-python-eoe-indicator nil body)
                   (let ((comint-process-echoes nil))
                     (funcall input-body body)
                     (funcall send-wait) (funcall send-wait)
                     (insert org-babel-python-eoe-indicator)
                     (funcall send-wait)))
		 (org-babel-eval-read-file tmp-results-file)))))))
    (unless (string= (substring org-babel-python-eoe-indicator 1 -1) results)
      (org-babel-result-cond result-params
	results
        (org-babel-python-table-or-string results)))))

(defun org-babel-python-read-string (string)
  "Strip \\='s from around Python string."
  (if (and (string-prefix-p "'" string)
	   (string-suffix-p "'" string))
      (substring string 1 -1)
    string))

(provide 'ob-python)

;;; ob-python.el ends here
