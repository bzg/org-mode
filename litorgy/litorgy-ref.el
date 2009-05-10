;;; litorgy-ref.el --- litorgical functions for referencing external data

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

(defun litorgy-ref-variables (params)
  "Takes a parameter alist, and return an alist of variable
names, and the emacs-lisp representation of the related value."
  (mapcar #'litorgy-ref-parse
   (delq nil (mapcar (lambda (pair) (if (eq (car pair) :var) (cdr pair))) params))))

(defun litorgy-ref-parse (assignment)
  "Parse a variable ASSIGNMENT in a header argument.  If the
right hand side of the assignment has a literal value return that
value, otherwise interpret as a reference to an external resource
and find it's value using `litorgy-ref-resolve-reference'.
Return a list with two elements.  The first element of the list
will be the name of the variable, and the second will be an
emacs-lisp representation of the value of the variable."
  (message (format "assignment=%S" assignment))
  (if (string-match "\\(.+?\\)=\\(.+\\)" assignment)
      (let ((var (match-string 1 assignment))
            (ref (match-string 2 assignment)))
        (cons (intern var)
              (or (litorgy-ref-literal ref)
                  (litorgy-ref-resolve-reference ref))))))

(defun litorgy-ref-literal (ref)
  "Determine if the right side of a header argument variable
assignment is a literal value or is a reference to some external
resource.  If REF is literal then return it's value, otherwise
return nil."
  (let ((out (string-to-number ref)))
    (if (or (not (equal out 0)) (string= ref "0") (string= ref "0.0")) out ;; number
      (if (string-match "\"\\(.+\\)\"" ref) (read ref) ;; string
        nil)))) ;; reference

(defun litorgy-ref-resolve-reference (ref)
  "Resolve the reference and return it's value"
  (save-excursion
    (let (direction type args new-ref)
      ;; assign any arguments to pass to source block
      (when (string-match "\\(.+\\)\(\\(.+\\)\)" ref)
        (save-match-data
          (setq args (mapcar (lambda (ref) (cons :var ref))
                           (split-string (match-string 2 ref) ",[ \f\t\n\r\v]*"))))
        (setq ref (match-string 1 ref)))
      (when (string-match "\\(.+\\):\\(.+\\)" ref)
        (find-file (match-string 1 ref))
        (setf ref (match-string 2 ref)))
      (cond ;; follow the reference in the current file
       ((string= ref "previous") (setq direction -1))
       ((string= ref "next") (setq direction 1))
       (t
        (goto-char (point-min))
        (setq direction 1)
        (unless (let ((regexp (concat "^#\\+\\(TBL\\|SRC\\)NAME:[ \t]*"
                                      (regexp-quote ref) "[ \t]*$")))
                  (or (re-search-forward regexp nil t)
                      (re-search-backward regexp nil t)))
          ;; ;; TODO: allow searching for names in other buffers
          ;; (setq id-loc (org-id-find ref 'marker)
          ;;       buffer (marker-buffer id-loc)
          ;;       loc (marker-position id-loc))
          ;; (move-marker id-loc nil)
          (error (format "reference '%s' not found in this buffer" ref)))))
      (while (not (setq type (litorgy-ref-at-ref-p)))
        (forward-line direction)
        (beginning-of-line)
        (if (or (= (point) (point-min)) (= (point) (point-max)))
            (error "reference not found")))
      (case type
        ('table
         (mapcar (lambda (row)
                   (mapcar #'litorgy-read row))
                 (org-table-to-lisp)))
        ('source-block
         (litorgy-execute-src-block t nil args))))))

(defun litorgy-ref-at-ref-p ()
  "Return the type of reference located at point or nil of none
of the supported reference types are found.  Supported reference
types are tables and source blocks."
  (cond ((org-at-table-p) 'table)
        ((looking-at "^#\\+BEGIN_SRC") 'source-block)))

(provide 'litorgy-ref)
;;; litorgy-ref.el ends here
