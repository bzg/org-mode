;;; org-babel-octave.el --- org-babel functions for octave and matlab evaluation

;; Copyright (C) Dan Davison

;; Author: Dan Davison
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

;;; Requirements:

;; octave
;; octave-mode.el and octave-inf.el come with GNU emacs

;;; Code:
(require 'org-babel)
(require 'octave-inf)

(org-babel-add-interpreter "octave")
(add-to-list 'org-babel-tangle-langs '("octave" "m" "#!/usr/bin/env octave"))

(defvar org-babel-octave-shell-command "octave -q"
  "Shell command to use to run octave as an external process.")

(defun org-babel-execute:octave (body params &optional matlabp)
  "Execute a block of octave code with org-babel."
  (message (format "executing %s source code block" (if matlabp "matlab" "octave")))
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         (session (funcall (intern (format "org-babel-%s-initiate-session" lang))
			   (first processed-params) params))
         (vars (second processed-params))
         (result-params (third processed-params))
         (result-type (fourth processed-params))
	 (out-file (cdr (assoc :file params)))
	 (augmented-body (concat
			  ;; prepend code to define all arguments passed to the code block
			  ;; (may not be appropriate for all languages)
			  (mapconcat
			   (lambda (pair)
			     (format "%s=%s"
				     (car pair)
				     (org-babel-octave-var-to-octave (cdr pair))))
			   vars "\n") "\n" body "\n"))
	 (result (org-babel-octave-evaluate session augmented-body result-type matlabp)))
    (or out-file result)))

(defun org-babel-octave-var-to-octave (var)
  "Convert an emacs-lisp variable into an octave variable.
Converts an emacs-lisp variable into a string of octave code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-octave-var-to-octave var ", ") "]")
    (format "%S" var)))

(defun org-babel-prep-session:octave (session params &optional matlabp)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-octave-initiate-session session params matlabp))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-octave-var-to-octave (cdr pair))))
                     vars)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (move-end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-octave-initiate-session (&optional session params matlabp)
  "Create octave inferior process buffer.
If there is not a current inferior-process-buffer in SESSION
then create. Return the initialized session."
  (unless (string= session "none")
    (let ((session (or session (if matlabp "*Inferior Matlab*" "*Inferior Octave*"))))
      (if (org-babel-comint-buffer-livep session) session
	(save-window-excursion
	  (if matlabp (matlab-shell) (run-octave))
	  (rename-buffer (if (bufferp session) (buffer-name session)
			   (if (stringp session) session (buffer-name)))) (current-buffer))))))

(defvar org-babel-octave-wrapper-method
   "%s
if ischar(ans)
   fid = fopen('%s', 'w')
   fprintf(fid, ans)
   fprintf(fid, '\\n')
   fclose(fid)
else
   save -ascii %s ans
end")

(defvar org-babel-octave-eoe-indicator "\'org_babel_eoe\'")

(defvar org-babel-octave-eoe-output "ans = org_babel_eoe")

(defun org-babel-octave-evaluate (session body result-type lang)
  "Pass BODY to the octave process.
If RESULT-TYPE equals 'output then return the outputs of the
statements in BODY, if RESULT-TYPE equals 'value then return the
value of the last statement in BODY, as elisp."
  (if session
    (org-babel-octave-evaluate-session session body result-type matlabp)
    (org-babel-octave-evaluate-external-process body result-type matlabp)))

(defun org-babel-octave-evaluate-external-process (body result-type matlabp)
  (let ((cmd (if matlabp org-babel-matlab-shell-command org-babel-octave-shell-command)))
    (save-excursion
      (case result-type
	(output
	 (with-temp-buffer
	   (insert body)
	   (org-babel-shell-command-on-region (point-min) (point-max) cmd 'current-buffer 'replace)
	   (buffer-string)))
	(value
	 (let* ((tmp-file (make-temp-file "org-babel-results-")) exit-code
		(stderr
		 (with-temp-buffer
		   (insert (format org-babel-octave-wrapper-method body tmp-file tmp-file))
		   (setq exit-code (org-babel-shell-command-on-region
				    (point-min) (point-max) cmd nil 'replace (current-buffer)))
		   (buffer-string))))
	   (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
	   (org-babel-octave-import-elisp-from-file (org-babel-maybe-remote-file tmp-file))))))))

(defun org-babel-octave-evaluate-session (session body result-type &optional matlabp)
  (let* ((tmp-file (make-temp-file "org-babel-results-"))
	 (full-body
	  (case result-type
	    (output
	     (mapconcat
	      #'org-babel-chomp
	      (list body org-babel-octave-eoe-indicator) "\n"))
	    (value
	     (mapconcat
	      #'org-babel-chomp
	      (list (format org-babel-octave-wrapper-method body tmp-file tmp-file) org-babel-octave-eoe-indicator) "\n"))))
	 (raw (org-babel-comint-with-output session
		  (if matlabp org-babel-octave-eoe-indicator org-babel-octave-eoe-output) t
		(insert full-body) (comint-send-input nil t))) results)
    (case result-type
      (value
       (org-babel-octave-import-elisp-from-file (org-babel-maybe-remote-file tmp-file)))
      (output
       (progn
	 (setq results
	       (if matlabp
		   (cdr (reverse (delq "" (mapcar #'org-babel-octave-read-string
						  (mapcar #'org-babel-trim raw)))))
		 (cdr (member org-babel-octave-eoe-output
			      (reverse (mapcar #'org-babel-octave-read-string
					       (mapcar #'org-babel-trim raw)))))))
	 (mapconcat #'identity (reverse results) "\n"))))))

(defun org-babel-octave-import-elisp-from-file (file-name)
  "Import data written to file by octave.
This removes initial blank and comment lines and then calls
`org-babel-import-elisp-from-file'."
  (let ((temp-file (make-temp-file "org-babel-results-")) beg end)
    (with-temp-file temp-file
      (insert-file-contents file-name)
      (re-search-forward "^[ \t]*[^# \t]" nil t)
      (if (< (setq beg (point-min))
	     (setq end (point-at-bol)))
	  (delete-region beg end)))
    (org-babel-import-elisp-from-file temp-file)))

(defun org-babel-octave-read-string (string)
  "Strip \\\"s from around octave string"
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(provide 'org-babel-octave)
;;; org-babel-octave.el ends here
