;;; ob-C.el --- org-babel functions for C and similar languages

;; Copyright (C) 2010-2014 Free Software Foundation, Inc.

;; Author: Eric Schulte, Thierry Banel
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

;; Org-Babel support for evaluating C, C++, D code.
;;
;; very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:
(require 'ob)
(require 'cc-mode)
(eval-when-compile
  (require 'cl))

(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-remove-indentation "org" (code &optional n))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("C++" . "cpp"))
(add-to-list 'org-babel-tangle-lang-exts '("D" . "d"))

(defvar org-babel-default-header-args:C '())

(defvar org-babel-C-compiler "gcc"
  "Command used to compile a C source code file into an
executable.")

(defvar org-babel-C++-compiler "g++"
  "Command used to compile a C++ source code file into an
executable.")

(defvar org-babel-D-compiler "rdmd"
  "Command used to compile and execute a D source code file.")

(defvar org-babel-c-variant nil
  "Internal variable used to hold which type of C (e.g. C or C++ or D)
is currently being evaluated.")

(defun org-babel-execute:cpp (body params)
  "Execute BODY according to PARAMS.
This function calls `org-babel-execute:C++'."
  (org-babel-execute:C++ body params))

(defun org-babel-execute:C++ (body params)
  "Execute a block of C++ code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'cpp)) (org-babel-C-execute body params)))

;;(defun org-babel-expand-body:C++ (body params) ;; unused
;;  "Expand a block of C++ code with org-babel according to it's
;;header arguments (calls `org-babel-C-expand')."
;;  (let ((org-babel-c-variant 'cpp)) (org-babel-C-expand body params)))

(defun org-babel-execute:D (body params)
  "Execute a block of D code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'd)) (org-babel-C-execute body params)))

;; (defun org-babel-expand-body:D (body params) ;; unused
;;  "Expand a block of D code with org-babel according to it's
;;header arguments (calls `org-babel-C-expand')."
;;  (let ((org-babel-c-variant 'd)) (org-babel-C-expand body params)))

(defun org-babel-execute:C (body params)
  "Execute a block of C code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'c)) (org-babel-C-execute body params)))

;; (defun org-babel-expand-body:c (body params) ;; unused
;;  "Expand a block of C code with org-babel according to it's
;;header arguments (calls `org-babel-C-expand')."
;;  (let ((org-babel-c-variant 'c)) (org-babel-C-expand body params)))

(defun org-babel-C-execute (body params)
  "This function should only be called by `org-babel-execute:C'
or `org-babel-execute:C++' or `org-babel-execute:D'."
  (let* ((tmp-src-file (org-babel-temp-file
			"C-src-"
			(cond
			 ((equal org-babel-c-variant 'c  ) ".c"  )
			 ((equal org-babel-c-variant 'cpp) ".cpp")
			 ((equal org-babel-c-variant 'd  ) ".d"  ))))
	 (tmp-bin-file (org-babel-temp-file "C-bin-" org-babel-exeext)) ;; not used for D
	 (cmdline (cdr (assoc :cmdline params)))
	 (cmdline (if cmdline (concat " " cmdline) ""))
	 (flags (cdr (assoc :flags params)))
	 (flags (mapconcat 'identity
			   (if (listp flags) flags (list flags)) " "))
	 (full-body
	  (cond ((equal org-babel-c-variant 'c  ) (org-babel-C-expand-C   body params))
		((equal org-babel-c-variant 'cpp) (org-babel-C-expand-C++ body params))
		((equal org-babel-c-variant 'd  ) (org-babel-C-expand-D   body params)))))
    (with-temp-file tmp-src-file (insert full-body))
    (if (memq org-babel-c-variant '(c cpp)) ;; no separate compilation for D
	(org-babel-eval
	 (format "%s -o %s %s %s"
		 (cond
		  ((equal org-babel-c-variant 'c  ) org-babel-C-compiler)
		  ((equal org-babel-c-variant 'cpp) org-babel-C++-compiler))
		 (org-babel-process-file-name tmp-bin-file)
		 flags
		 (org-babel-process-file-name tmp-src-file)) ""))
    (let ((results
	   (org-babel-trim
	    (org-remove-indentation
	     (org-babel-eval
	      (cond ((memq org-babel-c-variant '(c cpp))
		     (concat tmp-bin-file cmdline))
		    ((equal org-babel-c-variant 'd)
		     (format "%s %s %s %s"
			     org-babel-D-compiler
			     flags
			     (org-babel-process-file-name tmp-src-file)
			     cmdline)))
	      "")))))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assoc :result-params params))
	 (org-babel-read results t)
	 (let ((tmp-file (org-babel-temp-file "c-")))
	   (with-temp-file tmp-file (insert results))
	   (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
	(cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
	(cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))
    ))

(defun org-babel-C-expand-C++ (body params)
  "Expand a block of C or C++ code with org-babel according to
it's header arguments."
  (org-babel-C-expand-C body params))

(defun org-babel-C-expand-C (body params)
  "Expand a block of C or C++ code with org-babel according to
it's header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	(main-p (not (string= (cdr (assoc :main params)) "no")))
	(includes (or (cdr (assoc :includes params))
		      (org-babel-read (org-entry-get nil "includes" t))))
	(defines (org-babel-read
		  (or (cdr (assoc :defines params))
		      (org-babel-read (org-entry-get nil "defines" t))))))
    (mapconcat 'identity
	       (list
		;; includes
		(mapconcat
		 (lambda (inc) (format "#include %s" inc))
		 (if (listp includes) includes (list includes)) "\n")
		;; defines
		(mapconcat
		 (lambda (inc) (format "#define %s" inc))
		 (if (listp defines) defines (list defines)) "\n")
		;; variables
		(mapconcat 'org-babel-C-var-to-C vars "\n")
		;; body
		(if main-p
		    (org-babel-C-ensure-main-wrap body)
		  body) "\n") "\n")))

(defun org-babel-C-expand-D (body params)
  "Expand a block of D code with org-babel according to
it's header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	(main-p (not (string= (cdr (assoc :main params)) "no")))
	(imports (or (cdr (assoc :imports params))
		     (org-babel-read (org-entry-get nil "imports" t)))))
    (mapconcat 'identity
	       (list
		"module mmm;"
		;; imports
		(mapconcat
		 (lambda (inc) (format "import %s;" inc))
		 (if (listp imports) imports (list imports)) "\n")
		;; variables
		(mapconcat 'org-babel-C-var-to-C vars "\n")
		;; body
		(if main-p
		    (org-babel-C-ensure-main-wrap body)
		  body) "\n") "\n")))

(defun org-babel-C-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "^[ \t]*[intvod]+[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "int main() {\n%s\nreturn 0;\n}\n" body)))

(defun org-babel-prep-session:C (session params)
  "This function does nothing as C is a compiled language with no
support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:C (session body params)
  "This function does nothing as C is a compiled language with no
support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-C-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-C-val-to-C-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  (cond
   ((integerp val) '("int" "%d"))
   ((floatp val) '("double" "%f"))
   ((or (listp val) (vectorp val))
    (lexical-let ((type (org-babel-C-val-to-C-list-type val)))
      (list (car type)
	    (lambda (val)
	      (cons
	       (format "[%d]%s"
		       (length val)
		       (car (org-babel-C-format-val type (elt val 0))))
	       (concat (if (equal org-babel-c-variant 'd) "[ " "{ ")
		       (mapconcat (lambda (v)
				    (cdr (org-babel-C-format-val type v)))
				  val
				  ", ")
		       (if (equal org-babel-c-variant 'd) " ]" " }")))))))
   (t ;; treat unknown types as string
    (list
     (if (equal org-babel-c-variant 'd) "string" "const char*")
     "\"%s\""))))

(defun org-babel-C-val-to-C-list-type (val)
  "Determine the C array type of a VAL."
  (let (type)
    (mapc
     #'(lambda (i)
	 (let* ((tmp-type (org-babel-C-val-to-C-type i))
		(type-name (car type))
		(tmp-type-name (car tmp-type)))
	   (when (and type (not (string= type-name tmp-type-name)))
	     (if (and (member type-name '("int" "double" "int32_t"))
		      (member tmp-type-name '("int" "double" "int32_t")))
		 (setq tmp-type '("double" "" "%f"))
	       (error "Only homogeneous lists are supported by C.  You can not mix %s and %s"
		      type-name
		      tmp-type-name)))
	   (setq type tmp-type)))
     val)
    type))

(defun org-babel-C-var-to-C (pair)
  "Convert an elisp val into a string of C code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
	(val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-C-val-to-C-type val))
	   (type (car type-data))
	   (formated (org-babel-C-format-val type-data val))
	   (suffix (car formated))
	   (data (cdr formated)))
      (format "%s %s%s = %s;"
	      type
	      var
	      suffix
	      data))))

(provide 'ob-C)

;;; ob-C.el ends here
