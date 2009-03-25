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

(defun litorgy-read-cell (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like a list (meaning it starts with a
'(') then read it as lisp, otherwise return it unmodified as a
string.

This is taken almost directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (let ((out (string-to-number cell)))
	(if (equal out 0)
	    (if (or (equal "(" (substring cell 0 1))
                    (equal "'" (substring cell 0 1)))
                (read cell)
	      (if (string-match "^\\(+0\\|-0\\|0\\)$" cell)
		  0
		(progn (set-text-properties 0 (length cell) nil cell)
		       cell)))
	  out))
    cell))

(defun litorgy-reference-variables (params)
  "Takes a parameter alist, and return an alist of variable
names, and the string representation of the related value."
  (mapcar #'litorgy-reference-parse
   (delq nil (mapcar (lambda (pair) (if (eq (car pair) :var) (cdr pair))) params))))

(defun litorgy-reference-parse (reference)
  "Parse a reference to an external resource returning a list
with two elements.  The first element of the list will be the
name of the variable, and the second will be an emacs-lisp
representation of the value of the variable."
  (save-excursion
    (if (string-match "\\(.+\\)=\\(.+\\)" reference)
        (let ((var (match-string 1 reference))
              (ref (match-string 2 reference))
              direction)
          (when (string-match "\\(.+\\):\\(.+\\)" reference)
            (find-file (match-string 1 reference))
            (setf ref (match-string 2 reference)))
          (cons (intern var)
                (progn
                  (cond ;; follow the reference in the current file
                   ((string= ref "previous") (setq direction -1))
                   ((string= ref "next") (setq direction 1))
                   (t
                    (goto-char (point-min))
                    (setq direction 1)
                    (unless (re-search-forward
                             (concat "^#\\+TBLNAME:[ \t]*" (regexp-quote ref) "[ \t]*$") nil t)
                      (setq id-loc (org-id-find name-or-id 'marker)
                            buffer (marker-buffer id-loc)
                            loc (marker-position id-loc))
                      (move-marker id-loc nil))))
                  (while (not (org-at-table-p))
                    (forward-line direction)
                    (if (or (= (point) (point-min)) (= (point) (point-max)))
                        (error "no table found")))
                  (mapcar (lambda (row)
                            (mapcar #'litorgy-read-cell row))
                          (org-table-to-lisp))))))))

(provide 'litorgy-reference)
;;; litorgy-reference.el ends here
