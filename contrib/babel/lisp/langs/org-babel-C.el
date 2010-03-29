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

(org-babel-add-interpreter "c++")
(add-to-list 'org-babel-tangle-langs '("c++" "cpp" nil))

(defvar org-babel-C-compiler "gcc"
  "Command used to compile a C source code file into an
  executable.")

(defvar org-babel-c++-compiler "g++"
  "Command used to compile a c++ source code file into an
  executable.")

(defun org-babel-execute:cpp (body params)
  (org-babel-execute:C body params))

(defun org-babel-execute:c++ (body params)
    "Execute a block of C++ code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (let ((c-variant 'cpp)) (org-babel-C-execute body params)))

(defun org-babel-execute:C (body params)
  "Execute a block of C code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (let ((c-variant 'c)) (org-babel-C-execute body params)))

(defun org-babel-C-execute (body params)
  "This should only be called by `org-babel-execute:C' or
`org-babel-execute:c++'."
  (message "executing C source code block")
  (let* ((processed-params (org-babel-process-params params))
         (tmp-src-file (make-temp-file "org-babel-C-src" nil
                                       (case c-variant
                                         ('c ".c")
                                         ('cpp ".cpp"))))
         (tmp-bin-file (make-temp-file "org-babel-C-bin"))
         (tmp-out-file (make-temp-file "org-babel-C-out"))
         (cmdline (cdr (assoc :cmdline params)))
         (flags (cdr (assoc :flags params)))
         (vars (second processed-params))
         (includes (org-babel-read
                    (or (cdr (assoc :includes params))
                        (org-entry-get nil "includes" t))))
         (defines (org-babel-read
                   (or (cdr (assoc :includes params))
                       (org-entry-get nil "defines" t))))
         (full-body (mapconcat 'identity
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
                      "\n" (org-babel-C-ensure-main-wrap body) "\n") "\n"))
         (error-buf (get-buffer-create "*Org-Babel Error Output*"))
         (compile
          (progn
            (with-temp-file tmp-src-file (insert full-body))
            (with-temp-buffer
              (org-babel-shell-command-on-region
               (point-min) (point-max)
               (format "%s -o %s %s %s"
                       (case c-variant
                         ('c org-babel-C-compiler)
                         ('cpp org-babel-c++-compiler))
                       tmp-bin-file
                       (mapconcat 'identity
                                  (if (listp flags) flags (list flags)) " ")
                       tmp-src-file)
               (current-buffer) 'replace error-buf)))))
    (if (= compile 0)
        (org-babel-read
         (org-babel-trim
          (with-temp-buffer
            (org-babel-shell-command-on-region
             (point-min) (point-max)
             (concat tmp-bin-file (if cmdline (concat " " cmdline) ""))
             (current-buffer) 'replace)
            (buffer-string))))
      (progn
        (with-current-buffer error-buf
          (goto-char (point-max))
          (insert (concat "\n\n--body--\n" full-body))
          (goto-char (point-min)))
        (display-buffer error-buf) nil))))

(defun org-babel-C-ensure-main-wrap (body)
  "Wrap body in a \"main\" function call if none exists."
  (if (string-match "^[ \t]*[intvod]+[ \t]*main[ \t]*(.*)" body)
      body
    (format "int main() {\n%s\n}\n" body)))

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
     ((or (characterp val))
      (format "char %S = '%S';" var val))
     ((stringp val)
      (format "char %S[%d] = \"%s\";"
              var (+ 1 (length val)) val))
     (t
      (format "u32 %S = %S;" var val)))))


(provide 'org-babel-C)
;;; org-babel-C.el ends here
