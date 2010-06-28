;;; ob-python.el --- org-babel functions for python evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation

;; Author: Eric Schulte, Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating python source code.

;;; Code:
(require 'ob)
(require 'ob-tangle)
(require 'ob-comint)
(require (if (featurep 'xemacs) 'python-mode 'python))

(add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

(defvar org-babel-default-header-args:python '())

(defun org-babel-expand-body:python (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (concat
   (mapconcat ;; define any variables
    (lambda (pair)
      (format "%s=%s"
              (car pair)
              (org-babel-python-var-to-python (cdr pair))))
    (nth 1 (or processed-params (org-babel-process-params params))) "\n")
   "\n" (org-babel-trim body) "\n"))

(defun org-babel-execute:python (body params)
  "Execute a block of Python code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Python source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-python-initiate-session (first processed-params)))
         (result-params (nth 2 processed-params))
         (result-type (nth 3 processed-params))
         (full-body (org-babel-expand-body:python
                     body params processed-params)) ;; then the source block body
         (result (org-babel-python-evaluate session full-body result-type)))
    (or (cdr (assoc :file params))
        (org-babel-reassemble-table
         result
         (org-babel-pick-name (nth 4 processed-params) (cdr (assoc :colnames params)))
         (org-babel-pick-name (nth 5 processed-params) (cdr (assoc :rownames params)))))))

(defun org-babel-prep-session:python (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-python-initiate-session session))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar ;; define any variables
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-python-var-to-python (cdr pair))))
                     vars)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session)) var-lines))
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

(defun org-babel-python-var-to-python (var)
  "Convert an elisp var into a string of python source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-python-var-to-python var ", ") "]")
    (if (equal var 'hline) "None" (format "%S" var))))

(defun org-babel-python-table-or-string (results)
  "If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  ((lambda (res)
     (if (listp res)
	 (mapcar (lambda (el) (if (equal el 'None) 'hline el)) res)
       res))
   (org-babel-read
   (if (or (string-match "^\\[.+\\]$" results)
	   (string-match "^(.+)$" results))
       (org-babel-read
        (concat "'"
                (replace-regexp-in-string
                 "\\[" "(" (replace-regexp-in-string
                            "\\]" ")" (replace-regexp-in-string
                                       ", " " " (replace-regexp-in-string
                                                 "'" "\"" results t))))))
     results))))

(defvar org-babel-python-buffers '(:default . nil))

(defun org-babel-python-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-python-buffers)))

(defun org-babel-python-initiate-session-by-key (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (python-buffer (org-babel-python-session-buffer session)))
      (cond
       ((fboundp 'run-python) ; python.el
	(run-python))
       ((fboundp 'py-shell) ; python-mode.el
	;; `py-shell' creates a buffer whose name is the value of
	;; `py-which-bufname' with '*'s at the beginning and end
	(let* ((bufname (if python-buffer
			    (replace-regexp-in-string "^\\*\\([^*]+\\)\\*$" "\\1" python-buffer) ; zap surrounding *
			  (concat "Python-" (symbol-name session))))
	       (py-which-bufname bufname)) ; avoid making a mess with buffer-local
	  (py-shell)
	  (setq python-buffer (concat "*" bufname "*"))))
       (t
	(error "No function available for running an inferior python.")))
	
      (setq org-babel-python-buffers (cons (cons session python-buffer)
					   (assq-delete-all session org-babel-python-buffers)))
      session)))

(defun org-babel-python-initiate-session (&optional session params)
  "Create a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (org-babel-python-session-buffer (org-babel-python-initiate-session-by-key session))))

(defvar org-babel-python-last-value-eval "_"
  "When evaluated by Python this returns the return value of the last statement.")
(defvar org-babel-python-pp-last-value-eval
  '("results = _"
    "import pprint"
    "org_babel_pp = pprint.PrettyPrinter()"
    "org_babel_pp.pprint(results)")
  "When evaluated by Python this pretty prints the value of the last statement.")
(defvar org-babel-python-eoe-indicator "'org_babel_python_eoe'"
  "Used to indicate that evaluation is has completed.")
(defvar org-babel-python-wrapper-method
  "
def main():
%s

open('%s', 'w').write( str(main()) )")
(defvar org-babel-python-pp-wrapper-method
  "
import pprint
def main():
%s

open('%s', 'w').write( pprint.pformat(main()) )")

(defun org-babel-python-evaluate (buffer body &optional result-type)
  "Pass BODY to the Python process in BUFFER.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (if (not session)
      ;; external process evaluation
      (save-excursion
        (case result-type
          (output
           (with-temp-buffer
             (insert body)
             ;; (message "buffer=%s" (buffer-string)) ;; debugging
             (org-babel-shell-command-on-region (point-min) (point-max) "python" 'current-buffer 'replace)
             (buffer-string)))
          (value
           (let* ((tmp-file (make-temp-file "org-babel-python-results-")) exit-code
		  (stderr
		   (with-temp-buffer
		     (insert
		      (format
		       (if (member "pp" result-params)
			   org-babel-python-pp-wrapper-method
			 org-babel-python-wrapper-method)
		       (mapconcat
			(lambda (line) (format "\t%s" line))
			(split-string
			 (org-remove-indentation (org-babel-trim body)) "[\r\n]") "\n")
		       tmp-file))
		     ;; (message "buffer=%s" (buffer-string)) ;; debugging
		     (setq exit-code (org-babel-shell-command-on-region
				      (point-min) (point-max) "python" nil 'replace (current-buffer)))
		     (buffer-string))))
	     (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
             (let ((raw (with-temp-buffer
			  (insert-file-contents (org-babel-maybe-remote-file tmp-file))
			  (buffer-string))))
               (if (or (member "code" result-params) (member "pp" result-params))
                   raw
                 (org-babel-python-table-or-string raw)))))))
    ;; comint session evaluation
    (org-babel-comint-in-buffer buffer
      (let* ((raw (org-babel-comint-with-output
		      (buffer org-babel-python-eoe-indicator t full-body)
                    ;; for some reason python is fussy, and likes enters after every input
		    (let ((comint-process-echoes nil))
		      (mapc (lambda (statement) (insert statement) (comint-send-input))
			    (split-string (org-babel-trim body) "[\r\n]+"))
		      (comint-send-input) (comint-send-input)
		      (if (member "pp" result-params)
			  (mapc (lambda (statement) (insert statement) (comint-send-input))
				org-babel-python-pp-last-value-eval)
			(insert org-babel-python-last-value-eval))
		      (comint-send-input) (comint-send-input)
		      (insert org-babel-python-eoe-indicator)
		      (comint-send-input))))
	     (raw (apply #'append ; split further
			 (mapcar #'(lambda (r)
				     (split-string r "[\r\n]+"))
				 raw)))
             (results (delete org-babel-python-eoe-indicator
                              (cdr (member org-babel-python-eoe-indicator
                                           (mapcar #'org-babel-trim raw))))))
        (unless (or (member "code" result-params) (member "pp" result-params))
          (setq results (mapcar #'org-babel-python-read-string results)))
        (case result-type
	  (output (org-babel-trim (mapconcat #'identity (reverse (cdr results)) "\n")))
	  (value
           (if (or (member "code" result-params) (member "pp" result-params))
               (car results)
             (org-babel-python-table-or-string (org-babel-trim (car results))))))))))

(defun org-babel-python-read-string (string)
  "Strip 's from around python string"
  (if (string-match "^'\\([^\000]+\\)'$" string)
      (match-string 1 string)
    string))

(provide 'ob-python)

;; arch-tag: f19b6c3d-dfcb-4a1a-9ce0-45ade1ebc212

;;; ob-python.el ends here
