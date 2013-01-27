;;; ox-org.el --- Org Back-End for Org Export Engine

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: org, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an Org back-end for Org exporter.  Since
;; its usage is mainly internal, it doesn't provide any interactive
;; function.

;;; Code:
(require 'ox)

(org-export-define-backend org
  ((babel-call . org-org-identity)
   (bold . org-org-identity)
   (center-block . org-org-identity)
   (clock . org-org-identity)
   (code . org-org-identity)
   (diary-sexp . org-org-identity)
   (drawer . org-org-identity)
   (dynamic-block . org-org-identity)
   (entity . org-org-identity)
   (example-block . org-org-identity)
   (fixed-width . org-org-identity)
   (footnote-definition . org-org-identity)
   (footnote-reference . org-org-identity)
   (headline . org-org-headline)
   (horizontal-rule . org-org-identity)
   (inline-babel-call . org-org-identity)
   (inline-src-block . org-org-identity)
   (inlinetask . org-org-identity)
   (italic . org-org-identity)
   (item . org-org-identity)
   (keyword . org-org-keyword)
   (latex-environment . org-org-identity)
   (latex-fragment . org-org-identity)
   (line-break . org-org-identity)
   (link . org-org-identity)
   (node-property . org-org-identity)
   (paragraph . org-org-identity)
   (plain-list . org-org-identity)
   (planning . org-org-identity)
   (property-drawer . org-org-identity)
   (quote-block . org-org-identity)
   (quote-section . org-org-identity)
   (radio-target . org-org-identity)
   (section . org-org-identity)
   (special-block . org-org-identity)
   (src-block . org-org-identity)
   (statistics-cookie . org-org-identity)
   (strike-through . org-org-identity)
   (subscript . org-org-identity)
   (superscript . org-org-identity)
   (table . org-org-identity)
   (table-cell . org-org-identity)
   (table-row . org-org-identity)
   (target . org-org-identity)
   (timestamp . org-org-identity)
   (underline . org-org-identity)
   (verbatim . org-org-identity)
   (verse-block . org-org-identity)))

(defun org-org-identity (blob contents info)
  "Transcode BLOB element or object back into Org syntax."
  (funcall
   (intern (format "org-element-%s-interpreter" (org-element-type blob)))
   blob contents))

(defun org-org-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax."
  (unless (plist-get info :with-todo-keywords)
    (org-element-put-property headline :todo-keyword nil))
  (unless (plist-get info :with-tags)
    (org-element-put-property headline :tags nil))
  (unless (plist-get info :with-priority)
    (org-element-put-property headline :priority nil))
  (org-element-headline-interpreter headline contents))

(defun org-org-keyword (keyword contents info)
  "Transcode KEYWORD element back into Org syntax.
Ignore keywords targeted at other export back-ends."
  (unless (member (org-element-property :key keyword)
		  (mapcar
		   (lambda (block-cons)
		     (and (eq (cdr block-cons) 'org-element-export-block-parser)
			  (car block-cons)))
		   org-element-block-name-alist))
    (org-element-keyword-interpreter keyword nil)))


(provide 'ox-org)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-org.el ends here
