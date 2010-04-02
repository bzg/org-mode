;;; org-babel-R.el --- org-babel functions for R code evaluation

;; Copyright (C) 2009 Eric Schulte

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, R, statistics
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

;; Org-Babel support for evaluating R code

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "R")

(add-to-list 'org-babel-tangle-langs '("R" "R" "#!/usr/bin/env Rscript"))

(defun org-babel-execute:R (body params)
  "Execute a block of R code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing R source code block...")
  (save-excursion
    (let* ((processed-params (org-babel-process-params params))
           (result-type (fourth processed-params))
           (session (org-babel-R-initiate-session (first processed-params) params))
           (vars (second processed-params))
	   (column-names-p (and (cdr (assoc :colnames params))
				(string= "yes" (cdr (assoc :colnames params)))))
	   (out-file (cdr (assoc :file params)))
	   (augmented-body
	    (concat
	     (if out-file (concat (org-babel-R-construct-graphics-device-call out-file params) "\n") "")
	     (mapconcat ;; define any variables
	      (lambda (pair) (org-babel-R-assign-elisp (car pair) (cdr pair))) vars "\n")
	     "\n" body "\n" (if out-file "dev.off()\n" "")))
	   (result (org-babel-R-evaluate session augmented-body result-type column-names-p)))
      (or out-file result))))

(defun org-babel-prep-session:R (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-R-initiate-session session params))
         (vars (org-babel-ref-variables params)))
    (mapc (lambda (pair) (org-babel-R-assign-elisp session (car pair) (cdr pair))) vars)
    session))

(defun org-babel-load-session:R (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:R session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-R-assign-elisp (name value)
  "Read the elisp VALUE into a variable named NAME."
  (if (listp value)
      (let ((transition-file (make-temp-file "org-babel-R-import")))
        ;; ensure VALUE has an orgtbl structure (depth of at least 2)
        (unless (listp (car value)) (setq value (list value)))
        (with-temp-file (org-babel-maybe-remote-file transition-file)
          (insert (orgtbl-to-tsv value '(:fmt org-babel-R-quote-tsv-field)))
          (insert "\n"))
        (format "%s <- read.table(\"%s\", header=%s, sep=\"\\t\", as.is=TRUE)"
                name transition-file (if (eq (second value) 'hline) "TRUE" "FALSE")))
    (format "%s <- %s" name (org-babel-R-quote-tsv-field value))))

(defun org-babel-R-initiate-session (session params)
  "If there is not a current R process then create one."
  (unless (string= session "none")
    (let ((session (or session "*R*"))
	  (ess-ask-for-ess-directory (not (cdr (assoc :dir params)))))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (R)
	  (rename-buffer (if (bufferp session) (buffer-name session)
			   (if (stringp session) session (buffer-name)))) (current-buffer))))))

(defun org-babel-R-construct-graphics-device-call (out-file params)
  "Construct the call to the graphics device"
  (let ((devices
	 '((:bmp . "bmp")
	   (:jpg . "jpeg")
	   (:jpeg . "jpeg")
	   (:tiff . "tiff")
	   (:png . "png")
	   (:svg . "svg")
	   (:pdf . "pdf")
	   (:ps . "postscript")
	   (:postscript . "postscript")))
	(allowed-args '(:width :height :bg :units :pointsize
			       :antialias :quality :compression :res :type
			       :family :title :fonts :version :paper :encoding
			       :pagecentre :colormodel :useDingbats :horizontal))
	(device (and (string-match ".+\\.\\([^.]+\\)" out-file) (match-string 1 out-file)))
	(extra-args (cdr (assq :R-dev-args params))) filearg args)
    (setq device (or (and device (cdr (assq (intern (concat ":" device)) devices))) "png"))
    (setq filearg (if (member device '("pdf" "postscript" "svg")) "file" "filename"))
    (setq args (mapconcat (lambda (pair)
			    (if (member (car pair) allowed-args)
				(format ",%s=%s" (substring (symbol-name (car pair)) 1) (cdr pair)) ""))
			  params ""))
    (format "%s(%s=\"%s\"%s%s%s)\n" device filearg out-file args (if extra-args "," "") (or extra-args ""))))

(defvar org-babel-R-eoe-indicator "'org_babel_R_eoe'")
(defvar org-babel-R-eoe-output "[1] \"org_babel_R_eoe\"")
(defvar org-babel-R-wrapper-method "main <- function ()\n{\n%s\n}
write.table(main(), file=\"%s\", sep=\"\\t\", na=\"nil\",row.names=FALSE, col.names=%s, quote=FALSE)")

(defun org-babel-R-evaluate (session body result-type column-names-p)
  "Pass BODY to the R process in SESSION.  If RESULT-TYPE equals
'output then return a list of the outputs of the statements in
BODY, if RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (if (not session)
      ;; external process evaluation
      (case result-type
	(output
	 (with-temp-buffer
	   (insert body)
	   (org-babel-shell-command-on-region (point-min) (point-max) "R --slave --no-save" 'current-buffer 'replace)
	   (buffer-string)))
	(value
	 (let* ((tmp-file (make-temp-file "R-out-functional-results")) exit-code
		(stderr
		 (with-temp-buffer
		   (insert (format org-babel-R-wrapper-method
				   body tmp-file (if column-names-p "TRUE" "FALSE")))
		   (setq exit-code (org-babel-shell-command-on-region
				    (point-min) (point-max) "R --no-save" nil 'replace (current-buffer)))
		   (buffer-string))))
	   (if (> exit-code 0) (org-babel-error-notify exit-code stderr))
	   (org-babel-R-process-value-result
	    (org-babel-import-elisp-from-file (org-babel-maybe-remote-file tmp-file))
	    column-names-p))))
    ;; comint session evaluation
    (org-babel-comint-in-buffer session
      (let* ((tmp-file (make-temp-file "org-babel-R"))
	     (full-body
	      (case result-type
		(value
		 (mapconcat #'org-babel-chomp (list body
						    (format "write.table(.Last.value, file=\"%s\", sep=\"\\t\", na=\"nil\",row.names=FALSE, col.names=%s, quote=FALSE)" tmp-file (if column-names-p "TRUE" "FALSE"))
						    org-babel-R-eoe-indicator) "\n"))
		(output
		 (mapconcat #'org-babel-chomp (list body org-babel-R-eoe-indicator) "\n"))))
	     (raw (org-babel-comint-with-output session org-babel-R-eoe-output nil
                    (insert full-body) (inferior-ess-send-input)))
	     (comint-prompt-regexp
	      (concat "^\\("
		      inferior-ess-primary-prompt
		      "\\|"
		      inferior-ess-secondary-prompt
		      "\\)*"))
	     broke results)
        (case result-type
          (value (org-babel-R-process-value-result
		  (org-babel-import-elisp-from-file
		   (org-babel-maybe-remote-file tmp-file))
		  column-names-p))
          (output
	   (flet ((extractor
		   (el)
		   (if (or broke
			   (and (string-match (regexp-quote org-babel-R-eoe-output) el)
				(setq broke t)))
		       nil
		     (if (= (length el) 0)
			 nil
		       (if (string-match comint-prompt-regexp el)
			   (substring el (match-end 0))
			 el)))))
	     (mapconcat
	      #'identity
	      (delete nil (mapcar #'extractor (mapcar #'org-babel-chomp raw))) "\n"))))))))

(defun org-babel-R-process-value-result (result column-names-p)
  "R-specific processing of return value prior to return to org-babel.

Currently, insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))
  

(provide 'org-babel-R)
;;; org-babel-R.el ends here
