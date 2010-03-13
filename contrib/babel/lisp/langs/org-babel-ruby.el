;;; org-babel-ruby.el --- org-babel functions for ruby evaluation

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

;; Org-Babel support for evaluating ruby source code.

;;; Requirements:

;; - ruby and irb executables :: http://www.ruby-lang.org/
;; 
;; - ruby-mode :: Can be installed through ELPA, or from
;;   http://github.com/eschulte/rinari/raw/master/util/ruby-mode.el
;;   
;; - inf-ruby mode :: Can be installed through ELPA, or from
;;   http://github.com/eschulte/rinari/raw/master/util/inf-ruby.el

;;; Code:
(require 'org-babel)
(require 'inf-ruby)

(org-babel-add-interpreter "ruby")

(add-to-list 'org-babel-tangle-langs '("ruby" "rb" "#!/usr/bin/env ruby"))

(defun org-babel-execute:ruby (body params)
  "Execute a block of Ruby code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Ruby source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-ruby-initiate-session (first processed-params)))
         (vars (second processed-params))
         (result-params (third processed-params))
         (result-type (fourth processed-params))
         (full-body (concat
		    (mapconcat ;; define any variables
		     (lambda (pair)
		       (format "%s=%s"
			       (car pair)
			       (org-babel-ruby-var-to-ruby (cdr pair))))
		     vars "\n") "\n" body "\n")) ;; then the source block body
         (result (org-babel-ruby-evaluate session full-body result-type)))
    (or (cdr (assoc :file params)) result)))

(defun org-babel-prep-session:ruby (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  ;; (message "params=%S" params) ;; debugging
  (let* ((session (org-babel-ruby-initiate-session session))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar ;; define any variables
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-ruby-var-to-ruby (cdr pair))))
                     vars)))
    (org-babel-comint-in-buffer session
      (sit-for .5) (goto-char (point-max))
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))
    session))

(defun org-babel-load-session:ruby (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:ruby session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-ruby-var-to-ruby (var)
  "Convert an elisp var into a string of ruby source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-ruby-var-to-ruby var ", ") "]")
    (format "%S" var)))

(defun org-babel-ruby-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               ", " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(defun org-babel-ruby-initiate-session (&optional session params)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (let ((session-buffer (save-window-excursion (run-ruby nil session) (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
          session-buffer
        (sit-for .5)
        (org-babel-ruby-initiate-session session)))))

(defvar org-babel-ruby-last-value-eval "_"
  "When evaluated by Ruby this returns the return value of the last statement.")
(defvar org-babel-ruby-pp-last-value-eval "require 'pp'; pp(_)"
  "When evaluated by Ruby this pretty prints value of the last statement.")
(defvar org-babel-ruby-eoe-indicator ":org_babel_ruby_eoe"
  "Used to indicate that evaluation is has completed.")
(defvar org-babel-ruby-wrapper-method
  "
def main()
%s
end
results = main()
File.open('%s', 'w'){ |f| f.write((results.class == String) ? results : results.inspect) }
")
(defvar org-babel-ruby-pp-wrapper-method
  "
require 'pp'
def main()
%s
end
results = main()
File.open('%s', 'w') do |f|
  $stdout = f
  pp results
end
")

(defun org-babel-ruby-evaluate (buffer body &optional result-type)
  "Pass BODY to the Ruby process in BUFFER.  If RESULT-TYPE equals
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
             (org-babel-shell-command-on-region (point-min) (point-max) "ruby" 'current-buffer 'replace)
             (buffer-string)))
          (value
           (let* ((tmp-file (make-temp-file "ruby-functional-results")) exit-code
		  (stderr
		   (with-temp-buffer
		     (insert (format (if (member "pp" result-params)
					 org-babel-ruby-pp-wrapper-method
				       org-babel-ruby-wrapper-method) body tmp-file))
		     ;; (message "buffer=%s" (buffer-string)) ;; debugging
		     (setq exit-code
			   (org-babel-shell-command-on-region (point-min) (point-max) "ruby" nil 'replace (current-buffer)))
		     (buffer-string))))
	     (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
             (let ((raw (with-temp-buffer
			  (insert-file-contents (org-babel-maybe-remote-file tmp-file))
			  (buffer-string))))
               (if (or (member "code" result-params) (member "pp" result-params))
                   raw
                 (org-babel-ruby-table-or-string raw)))))))
    ;; comint session evaluation
    (let* ((full-body
	    (mapconcat
	     #'org-babel-chomp
	     (list body (if (member "pp" result-params)
                            org-babel-ruby-pp-last-value-eval
                          org-babel-ruby-last-value-eval)
                   org-babel-ruby-eoe-indicator) "\n"))
           (raw (org-babel-comint-with-output buffer org-babel-ruby-eoe-indicator t
                  (insert full-body) (comint-send-input nil t)))
           (results (cdr (member org-babel-ruby-eoe-indicator
                                 (reverse (mapcar #'org-babel-ruby-read-string
                                                  (mapcar #'org-babel-trim raw)))))))
      (case result-type
        (output (mapconcat #'identity (reverse (cdr results)) "\n"))
        (value
         (if (or (member "code" result-params) (member "pp" result-params))
             (car results)
           (org-babel-ruby-table-or-string (car results))))))))

(defun org-babel-ruby-read-string (string)
  "Strip \\\"s from around ruby string"
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(provide 'org-babel-ruby)
;;; org-babel-ruby.el ends here
