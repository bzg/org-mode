;;; org-babel-C.el --- org-babel functions for C and similar languages

;; Copyright (C) 2010 Eric Schulte

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

;; Org-Babel support for evaluating C code.
;;
;; very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:
(require 'org-babel)
(require 'cc-mode)

(org-babel-add-interpreter "C")
(add-to-list 'org-babel-tangle-langs '("C" "c" nil))

(org-babel-add-interpreter "cpp")
(add-to-list 'org-babel-tangle-langs '("cpp" "cpp" nil))

(org-babel-add-interpreter "c++")
(add-to-list 'org-babel-tangle-langs '("c++" "cpp" nil))

(defvar org-babel-C-compiler "gcc"
  "Command used to compile a C source code file into an
  executable.")

(defun org-babel-execute:cpp (body params)
  (org-babel-execute:C body params))

(defun org-babel-execute:c++ (body params)
  (org-babel-execute:C body params))

(defun org-babel-execute:C (body params)
  "Execute a block of C commands with org-babel.  This
function is called by `org-babel-execute-src-block'."
  (message "executing C source code block")
  (let* ((processed-params (org-babel-process-params params))
         (tmp-src-file (make-temp-file "org-babel-C-src" nil ".c"))
         (tmp-bin-file (make-temp-file "org-babel-C-bin"))
         (tmp-out-file (make-temp-file "org-babel-C-out"))
         (flags (cdr (assoc :flags params)))
         (vars (second processed-params))
         (includes (cdr (assoc :includes params)))
         (defines (cdr (assoc :defines params)))
         (full-body (concat
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
                     "\n" body "\n\n"))
         (bin (progn
                (with-temp-file tmp-src-file (insert full-body))
                (shell-command
                 (format "%s -o %s %s %s"
                         org-babel-C-compiler tmp-bin-file
                         (mapconcat 'identity (if (listp flags) flags (list flags)) " ")
                         tmp-src-file))
                tmp-bin-file)))
    (org-babel-read (org-babel-trim (shell-command-to-string bin)))))

(defun org-babel-prep-session:C (session params)
  "C is a compiled languages -- no support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:C (session body params)
  "C is a compiled languages -- no support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-C-var-to-C (pair)
  "Convert an elisp val into a string of C code specifying a var
of the same value.  TODO list support."
  (let* ((var (car pair))
         (val (cdr pair))
         (type (cond
                ((integerp val) "int")
                ((floatp val) "double")
                ((characterp val) "char")
                ((stringp val) (format "char[%d]" (length val)))
                (t "u32"))))
    (format "%s %S = %S;" type var val)))

(provide 'org-babel-C)
;;; org-babel-C.el ends here
