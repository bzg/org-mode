;;; org-babel-ref.el --- org-babel functions for referencing external data

;; Copyright (C) 2009 Eric Schulte, Dan Davison

;; Author: Eric Schulte, Dan Davison
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
;; org-babel block.  The syntax of such a reference should be
;;
;;   #+VAR: variable-name=file:resource-id
;;
;; - variable-name :: the name of the variable to which the value
;;                    will be assigned
;;                    
;; - file :: path to the file containing the resource, or omitted if
;;           resource is in the current file
;;
;; - resource-id :: the id or name of the resource
;;
;; So an example of a simple src block referencing table data in the
;; same file would be
;;
;;  #+TBLNAME: sandbox
;;  | 1 |       2 | 3 |
;;  | 4 | org-babel | 6 |
;;
;;  #+begin_src emacs-lisp :var table=sandbox
;;  (message table)
;;  #+end_src
;;

;;; Code:
(require 'org-babel)

(defun org-babel-ref-variables (params)
  "Takes a parameter alist, and return an alist of variable
names, and the emacs-lisp representation of the related value."
  (let ((assignments
	 (delq nil (mapcar (lambda (pair) (if (eq (car pair) :var) (cdr pair))) params)))
	(other-params (assq-delete-all :var params)))
    (mapcar (lambda (assignment) (org-babel-ref-parse assignment other-params)) assignments)))

(defvar org-babel-ref-split-regexp
  "[ \f\t\n\r\v]*\\(.+?\\)[ \f\t\n\r\v]*=[ \f\t\n\r\v]*\\(.+\\)[ \f\t\n\r\v]*")

(defun org-babel-ref-parse (assignment params)
  "Parse a variable ASSIGNMENT in a header argument.  If the
right hand side of the assignment has a literal value return that
value, otherwise interpret as a reference to an external resource
and find it's value using `org-babel-ref-resolve-reference'.
Return a list with two elements.  The first element of the list
will be the name of the variable, and the second will be an
emacs-lisp representation of the value of the variable."
  (if (string-match org-babel-ref-split-regexp assignment)
      (let ((var (match-string 1 assignment))
            (ref (match-string 2 assignment)))
        (cons (intern var)
              (or (org-babel-ref-literal ref)
                  (org-babel-ref-resolve-reference ref params))))))

(defun org-babel-ref-literal (ref)
  "Determine if the right side of a header argument variable
assignment is a literal value or is a reference to some external
resource.  If REF is literal then return it's value, otherwise
return nil."
  (let ((out (org-babel-read ref)))
    (if (equal out ref)
        (if (string-match "^\".+\"$" ref)
            (read ref))
      out)))

(defun org-babel-ref-resolve-reference (ref params)
  "Resolve the reference and return its value"
  (save-excursion
    (let ((case-fold-search t)
          type args new-refere new-referent result lob-info split-file split-ref)
      ;; assign any arguments to pass to source block
      (when (string-match "^\\(.+?\\)\(\\(.*\\)\)$" ref)
        (setq new-refere (match-string 1 ref))
        (setq new-referent (match-string 2 ref))
        ;; (message "new-refere=%S, new-referent=%S" new-refere new-referent) ;; debugging
        (when (> (length new-refere) 0)
          (if (> (length new-referent) 0)
              (setq args (mapcar (lambda (ref) (cons :var ref))
                                 (org-babel-ref-split-args new-referent))))
          ;; (message "args=%S" args) ;; debugging
          (setq ref new-refere)))
      (when (string-match "^\\(.+\\):\\(.+\\)$" ref)
        (setq split-file (match-string 1 ref))
        (setq split-ref (match-string 2 ref))
        (find-file split-file) (setq ref split-ref))
      (goto-char (point-min))
      (if (let ((result_regexp (concat "^#\\+\\(TBL\\|RES\\)NAME:[ \t]*"
                                       (regexp-quote ref) "[ \t]*$"))
                (regexp (concat "^#\\+SRCNAME:[ \t]*"
                                (regexp-quote ref) "\\(\(.*\)\\)?" "[ \t]*$")))
            ;; goto ref in the current buffer
            (or (and (not args)
                     (or (re-search-forward result_regexp nil t)
                         (re-search-backward result_regexp nil t)))
                (re-search-forward regexp nil t)
                (re-search-backward regexp nil t)
                ;; check the Library of Babel
                (setq lob-info (cdr (assoc (intern ref) org-babel-library-of-babel)))))
          (unless lob-info (goto-char (match-beginning 0)))
        ;; ;; TODO: allow searching for names in other buffers
        ;; (setq id-loc (org-id-find ref 'marker)
        ;;       buffer (marker-buffer id-loc)
        ;;       loc (marker-position id-loc))
        ;; (move-marker id-loc nil)
        (progn (message (format "reference '%s' not found in this buffer" ref))
               (error (format "reference '%s' not found in this buffer" ref))))
      (if lob-info
          (setq type 'lob)
        (while (not (setq type (org-babel-ref-at-ref-p)))
          (forward-line 1)
          (beginning-of-line)
          (if (or (= (point) (point-min)) (= (point) (point-max)))
              (error "reference not found"))))
      (setq params (org-babel-merge-params params args))
      (setq result
	    (case type
	      ('results-line (org-babel-read-result))
	      ('table (org-babel-read-table))
	      ('source-block (org-babel-execute-src-block t nil params))
	      ('lob (org-babel-execute-src-block t lob-info params))))
      (if (symbolp result) (format "%S" result) result))))

(defun org-babel-ref-split-args (arg-string)
  "Split ARG-STRING into top-level arguments of balanced parenthesis."
  (let ((index 0) (depth 0) (buffer "") holder return)
    ;; crawl along string, splitting at any ","s which are on the top level
    (while (< index (length arg-string))
      (setq holder (substring arg-string index (+ 1 index)))
      (setq buffer (concat buffer holder))
      (setq index (+ 1 index))
      (cond
       ((string= holder ",")
        (when (= depth 0)
          (setq return (reverse (cons (substring buffer 0 -1) return)))
          (setq buffer "")))
       ((string= holder "(") (setq depth (+ depth 1)))
       ((string= holder ")") (setq depth (- depth 1)))))
    (mapcar #'org-babel-trim (reverse (cons buffer return)))))

(defun org-babel-ref-at-ref-p ()
  "Return the type of reference located at point or nil if none
of the supported reference types are found.  Supported reference
types are tables and source blocks."
  (cond ((org-at-table-p) 'table)
        ((looking-at "^#\\+BEGIN_SRC") 'source-block)
        ((looking-at "^#\\+RESNAME:") 'results-line)))

(provide 'org-babel-ref)
;;; org-babel-ref.el ends here
