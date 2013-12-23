;;; ob-J.el --- org-babel functions for J evaluation

;; Copyright (C) 2011-2013 Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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

;; Session interaction depends on `j-console'.

;;; Code:
(require 'ob)

(defun org-babel-expand-body:J (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body.
PROCESSED-PARAMS isn't used yet."
  (org-babel-J-interleave-echos-except-functions body))

(defun org-babel-J-interleave-echos (body)
  "Interleave echo'' between each source line of BODY."
  (mapconcat #'identity (split-string body "\n") "\necho''\n"))

(defun org-babel-J-interleave-echos-except-functions (body)
  "Interleave echo'' between source lines of BODY that aren't functions."
  (if (obj-string-match-m "\\(?:^\\|\n\\)[^\n]*\\(?:1\\|2\\|3\\|4\\) : 0\n.*)\\(?:\n\\|$\\)" body)
      (let ((s1 (substring body 0 (match-beginning 0)))
	    (s2 (match-string 0 body))
	    (s3 (substring body (match-end 0))))
	(concat
	 (org-babel-J-interleave-echos s1)
	 "\necho''\n"
	 s2
	 (org-babel-J-interleave-echos-except-functions s3)))
    (org-babel-J-interleave-echos body)))

(defun org-babel-execute:J (body params)
  "Execute a block of J code BODY.
PARAMS are given by org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing J source code block")
  (let* ((processed-params (org-babel-process-params params))
	 (sessionp (cdr (assoc :session params)))
         (session (org-babel-j-initiate-session sessionp))
         (vars (second processed-params))
         (result-params (third processed-params))
         (result-type (fourth processed-params))
         (full-body (org-babel-expand-body:J
                     body params processed-params))
	 (tmp-script-file (org-babel-temp-file "J-src")))
    (org-babel-J-strip-whitespace
     (if (string= sessionp "none")
	 (progn
	   (with-temp-file tmp-script-file
	     (insert full-body))
	   (org-babel-eval (format "jconsole < %s" tmp-script-file) ""))
       (org-babel-J-eval-string full-body)))))

(defun org-babel-J-eval-string (str)
  "Sends STR to the `j-console-cmd' session and exectues it."
  (let ((session (j-console-ensure-session)))
    (with-current-buffer (process-buffer session)
      (goto-char (point-max))
      (insert (format "\n%s\n" str))
      (let ((beg (point)))
	(comint-send-input)
	(sit-for .1)
	(buffer-substring-no-properties
	 beg (point-max))))))

(defun org-babel-J-strip-whitespace (str)
  "Remove whitespace from jconsole output STR."
  (let ((strs (split-string str "\n" t))
	out cur s)
    (while (setq s (pop strs))
      (if (string-match "^ *$" s)
	  (progn (push (nreverse cur) out)
		 (setq cur))
	(push s cur)))
    (mapconcat #'org-babel-J-print-block
	       (delq nil (nreverse out))
	       "\n\n")))

(defun org-babel-J-print-block (x)
  "Prettify jconsole output X."
  (if (= 1 (length x))
      (obj-strip-leading-ws (car x))
    ;; assume only first row is misaligned
    (let ((n1 (obj-match-second-space (car x)))
	  (n2 (obj-match-second-space (cadr x))))
      (setcar
       x
       (if (and n1 n2)
	   (substring (car x) (- n1 n2))
	 (obj-strip-leading-ws (car x))))
      (mapconcat #'identity x "\n"))))

(defun obj-match-second-space (s)
  "Return position of second space in S or nil."
  (and (string-match "^ *[^ ]+\\( \\)" s)
       (match-beginning 1)))

(defun obj-strip-leading-ws (s)
  "String leading whitespace from S."
  (and (string-match "^ *\\([^ ].*\\)" s)
       (match-string 1 s)))

(defun obj-string-match-m (regexp string &optional start)
  "Like `sting-match', only .* includes newlines too."
  (string-match
   (replace-regexp-in-string "\\.\\*" "[\0-\377[:nonascii:]]*" regexp)
   string
   start))

(defun org-babel-j-initiate-session (&optional session)
  "Initiate a J session.
SESSION is a parameter given by org-babel."
  (unless (string= session "none")
    (require 'j-console)
    (j-console-ensure-session)))

(provide 'ob-J)

;;; ob-J.el ends here
