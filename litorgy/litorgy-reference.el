;;; litorgy-reference.el --- litorgical functions for referencing external data

;; Copyright (C) 2009 Eric Schulte, Dan Davison, Austin F. Frank

;; Author: Eric Schulte, Dan Davison, Austin F. Frank
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

;; Functions for referencing data from the header arguments of a
;; litorgical block.  The syntax of such a reference should be
;;
;;   #+VAR: variable-name=file:resource-id
;;
;; - variable-name :: the name of the variable to which the value
;;                    will be assigned
;;                    
;; - file :: path to the file containing the resource, or omitted if
;;           resource is in the current file
;;
;; - resource-id :: the id or name of the resource, or 'previous' to
;;                  grab the previous table, or 'next' to grab the
;;                  next table
;;
;; So an example of a simple src block referencing table data in the
;; same file would be
;;
;; #+var: table previous
;; #+begin_src emacs-lisp
;; (message table)
;; #+end_src
;;

;;; Code:
(require 'litorgy)

(defun litorgy-reference-variables (params)
  "Takes a parameter alist, and return an alist of variable
names, and the string representation of the related value."
  (mapcar #'litorgy-reference-parse
   (delq nil (mapcar (lambda (pair) (if (= (car pair) :var) (cdr pair))) params))))

(defun litorgy-reference-parse (reference)
  "Parse a reference to an external resource returning a list
with two elements.  The first element of the list will be the
name of the variable, and the second will be an emacs-lisp
representation of the value of the variable."
  (save-excursion
    (if (string-match "(.+)=(.+)" reference)
      (let ((var (match-string 1 reference))
            (ref (match-string 2 reference)))
        (when (string-match "(.+):(.+)" reference)
          (find-file (match-string 1 reference))
          (setf ref (match-string 2 reference)))
        ;; follow the reference in the current file
        (case ref
          ("previous"
           )
          ("next")
          (t ))
        ))))

(provide 'litorgy-reference)
;;; litorgy-reference.el ends here
