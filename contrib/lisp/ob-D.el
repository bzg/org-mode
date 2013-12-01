;;; ob-D.el --- org-babel functions for the D language

;; Copyright (C) 2013 Thierry Banel

;; Author: Thierry Banel, derived from the Eric Schulte work
;; Keywords: literate programming, reproducible research

;; This file is NOT (yet) part of GNU Emacs.

;; ob-D.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ob-D.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; the GNU General Public License can be obtained here:
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Org-Babel support for evaluating Digital Mars D Language code.
;; The D language home page is here:
;; http://dlang.org/

;;; Code:
(require 'ob)
(require 'ob-eval)

(declare-function org-entry-get "org"
                  (pom property &optional inherit literal-nil))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("D" . "D"))

(defvar org-babel-default-header-args:D '())

(defvar org-babel-D-compiler "rdmd"
  "Command used to compile and run a D source code file into an
  executable.")

(defun org-babel-execute:D (body params)
  "Execute a block of D code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (org-babel-D-execute body params))

(defun org-babel-D-execute (body params)
  "This function should only be called by `org-babel-execute:D'"
  (let* ((tmp-src-file (org-babel-temp-file "Dsrc" ".d"))
         (cmdline (cdr (assoc :cmdline params)))
         (flags (cdr (assoc :flags params)))
         (rdmd (format "%s %s %s"
                       org-babel-D-compiler
                       (mapconcat 'identity
                                  (if (listp flags) flags (list flags)) " ")

                       ;; On Unix, keep directory separator
                       (org-babel-process-file-name tmp-src-file)))

         (full-body (org-babel-D-expand body params)))
    (with-temp-file tmp-src-file (insert full-body))

    (org-babel-eval rdmd "")))

(defun org-babel-D-expand (body params)
  "Expand a block of D code with org-babel according to
its header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
        (colname-names (cdr (car (org-babel-get-header params :colname-names))))
        (main-p (not (string= (cdr (assoc :main params)) "no")))
        (imports (mapcar #'cdr (org-babel-get-header params :import))))
    (mapconcat 'identity
               (list
                "module aaa;\n"
                ;; imports
                (mapconcat
                 (lambda (inc) (format "import %s;" inc))
                 imports "\n")
                ;; variables
                (mapconcat 'org-babel-D-var-to-D vars "\n")
                (mapconcat 'org-babel-D-colnames-to-D colname-names "\n")
                ;; body
                (if main-p
                    (org-babel-D-ensure-main-wrap body)
                  body) "\n") "\n")))

(defun org-babel-D-ensure-main-wrap (body)
  "Wrap body in a \"main\" function call if none exists."
  (if (string-match "^[ \t]*[intvod]+[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "int main() {\n%s\nreturn(0);\n}\n" body)))

(defun org-babel-prep-session:D (session params)
  "This function does nothing as D is a compiled language with no
support for sessions"
  (error "D is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:D (session body params)
  "This function does nothing as D is a compiled language with no
support for sessions"
  (error "D is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-D-var-to-D (pair)
  "Convert an elisp value into a string of D code specifying a variable
of the same value."
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (cond
     ((integerp val)
      (format "int %S = %S;" var val))
     ((floatp val)
      (format "double %S = %S;" var val))
     ((stringp val)
      (format "string %S = \"%s\";" var val))
     ((listp val)
      (if (assoc var colname-names) ()
        (setq colname-names
              (cons (cons
                     var
                     (let ((i 0)) (mapcar (lambda (x) (setq i (1+ i)) (format
                                                                  "$%s" i))
                                          (car val))))
                    colname-names)))
      (format "string[][] %S = [\n[%s]];" var
              (mapconcat (lambda (row)
                           (if (listp row)
                               (mapconcat (lambda (v) (format "\"%s\"" v))
                                          row
                                          ",")))
                         val
                         "],\n[")))
     (t
      (format "u32 %S = %S;" var val)))))

(defun org-babel-D-colnames-to-D (pair)
  "Convert an elisp list of header table into a D vector
specifying a variable with the name of the table"
  (let ((table (car pair))
        (headers (cdr pair)))
    (format "string[] %S_headers = [%s];"
            table
            (mapconcat (lambda (h) (format "%S" h)) headers ","))))

(provide 'ob-D)

;;; ob-D.el ends here
