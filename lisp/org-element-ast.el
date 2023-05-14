;;; org-element-ast.el --- Abstract syntax tree for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>
;; Keywords: data, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements Org abstract syntax tree (AST) data structure.
;;
;; Only the most generic aspect of the syntax tree are considered
;; below.  The fine details of Org syntax are implemented elsewhere.
;;
;; Org AST is composed of nested syntax nodes.
;; Within actual Org syntax, the nodes can be either headings,
;; elements, or objects.  However, historically, we often call syntax
;; nodes simply "elements", unless the context requires clarification
;; about the node type.  In particular, many functions below will have
;; naming pattern `org-element-X', implying `org-element-node-X' --
;; they will apply to all the node types, not just to elements.
;;
;; 1. Syntax nodes
;; ------------------
;; Each Org syntax node can be represented as a string or list.
;;
;; The main node representation follows the pattern
;; (TYPE PROPERTIES CONTENTS), where
;;   TYPE is a symbol describing the node type.
;;   PROPERTIES is the property list attached to it.
;;   CONTENTS is a list of child syntax nodes contained within the
;;            current node, when applicable.
;;
;;; For example, "*bold text*  " node can be represented as
;;
;;    (bold (:begin 1 :end 14 :post-blank 2 ...) "bold text")
;;
;; TYPE can be any symbol, including symbol not explicitly defined by
;; Org syntax.  If TYPE is not a part of the syntax, the syntax
;; node is called "pseudo element/object", but otherwise considered a
;; valid part of Org syntax tree.  Search "Pseudo objects and
;; elements" in lisp/ox-latex.el for an example of using pseudo
;; elements.
;;
;; PROPERTIES is a property list (:property1 value1 :property2 value2 ...)
;; holding properties and value.
;;
;; `:standard-properties', `:parent', `:deferred', and `:secondary'
;; properties are treated specially in the code below.
;;
;; `:standard-properties' holds an array with
;; `org-element--standard-properties' values, in the same order.  The
;; values in the array have priority over the same properties
;; specified in the property list.  You should not rely on the value
;; of `org-element--standard-propreties' in the code.
;; `:standard-properties' may or may not be actually present in
;; PROPERTIES.  It is mostly used to speed up property access in
;; performance-critical code, as most of the code requesting property
;; values by constant name is inlined.
;;
;; The previous example can also be presented in more compact form as:
;;
;;    (bold (:standard-properties [1 10 ... 2 ...]) "bold text")
;;
;; Using an array allows faster access to frequently used properties.
;;
;; `:parent' holds the containing node, for a child node within the
;; AST.  It may or may not be present in PROPERTIES.
;;
;; `:secondary' holds a list of properties that may contain extra AST
;; nodes, in addition to the node contents.
;;
;; `deferred' property describes how to update not-yet-calculated
;; properties on request.
;;
;;
;; Syntax node can also be represented by a string.  Strings always
;; represent syntax node of `plain-text' type with contents being nil
;; and properties represented as string properties at position 0.
;; `:standard-properties' are not considered for `plain-text' nodes as
;; `plain-text' nodes tend to hold much fewer properties.
;;
;; In the above example, `plain-text' node "bold text" is more
;; accurately represented as
;;
;;    #("bold text" 0 9 (:parent (bold ...)))
;;
;; with :parent property value pointing back to the containing `bold'
;; node.
;;
;; `anonymous' syntax node is represented as a list with `car'
;; containing another syntax node.  Such node has nil type, does not
;; have properties, and its contents is a list of the contained syntax
;; node.  `:parent' property of the contained nodes point back to the
;; list itself, except when `anonymous' node holds secondary value
;; (see below), in which case the `:parent' property is set to be the
;; containing node in the AST.
;;
;; Any node representation other then described above is not
;; considered as Org syntax node.
;;
;; 2. Deferred values
;; ------------------
;; Sometimes, it is computationally expensive or even not possible to
;; calculate property values when creating an AST node.  The value
;; calculation can be deferred to the time the value is requested.
;;
;; Property values and contained nodes may have a special value of
;; `org-element-deferred' type.  Such values are computed dynamically.
;; Either every time the property value is requested or just the first
;; time.  In the latter case, the `org-element-deferred' property
;; value is auto-replaced with the dynamically computed result.
;;
;; Sometimes, even property names (not just property values) cannot, or
;; should not be computed in advance.  If a special property
;; `:deferred' has the value of `org-element-deferred-type', it is
;; first resolved for side effects of setting the missing properties.
;; The resolved value is re-assigned to the `:deferred' property.
;;
;; Note that `org-element-copy' unconditionally resolves deferred
;; properties.  This is useful to generate pure (in functional sense)
;; AST.
;;
;; The properties listed in `org-element--standard-properties', except
;; `:deferred' and `:parent' are never considered to have deferred value.
;; This constraint makes org-element API significantly faster.
;;
;; 3. Org document representation
;; ------------------------------
;; Document AST is represented by nested Org syntax nodes.
;;
;; Each node in the AST can hold the contained node in its CONTENTS or
;; as values of properties.
;;
;; For example, (bold (...) "bold text") `bold' node contains
;; `plain-text' node in CONTENTS.
;;
;; The containing node is called "parent node".
;;
;; The contained nodes held inside CONTENTS are called "child nodes".
;; They must have their `:parent' property set to the containing
;; parent node.
;;
;; The contained nodes can also be held as property values.  Such
;; nodes are called "secondary nodes".  Only certain properties
;; can contribute to AST - the property names listed as the value of
;; special property `:secondary'
;;
;; For example,
;;
;;   (headline ((:secondary (:title)
;;               :title (#("text" 0 4 (:parent (headline ...)))))))
;;
;; is a parent headline node containing "text" secondary string node
;; inside `:title' property.  Note that `:title' is listed in
;; `:secondary' value.
;;
;; The following example illustrates an example AST for Org document:
;;
;; ---- Org document --------
;; * Heading with *bold* text
;; Paragraph.
;; ---- end -----------------
;;
;; (org-data (...) ; `org-data' node.
;;   (headline
;;     (
;;      ;; `:secondary' property lists property names that contain other
;;      ;; syntax tree nodes.
;;
;;      :secondary (:title)
;;
;;      ;; `:title' property is set to anonymous node containing:
;;      ;; `plain-text', `bold', `plain-text'.
;;
;;      :title ("Heading with " (bold (:post-blank 1 ...) "bold") "text"))
;;
;;      ;; `headline' contents
;;     (section (...)
;;       (paragraph
;;         ;; `:parent' property set to the containing section.
;;         (:parent (section ...))
;;         ;; paragraph contents is a `plain-text' node.
;;         "Paragraph1."))))
;;
;; Try calling M-: (org-element-parse-buffer) on the above example Org
;; document to explore a more complete version of Org AST.

;;; Code:

(require 'org-macs)
(require 'inline) ; load indentation rules

(defsubst org-element-type (element)
  "Return type of ELEMENT.

The function returns the type of the element or object provided.
It can also return the following special value:
  `plain-text'       for a string
  `org-data'         for a complete document
  nil                in any other case."
  (cond
   ((not (consp element)) (and (stringp element) 'plain-text))
   ((symbolp (car element)) (car element))))

(defun org-element-secondary-p (object)
  "Non-nil when OBJECT directly belongs to a secondary string.
Return value is the property name, as a keyword, or nil."
  (let* ((parent (org-element-property :parent object))
	 (properties (cdr (assq (org-element-type parent)
				org-element-secondary-value-alist))))
    (catch 'exit
      (dolist (p properties)
	(and (memq object (org-element-property p parent))
	     (throw 'exit p))))))

(defsubst org-element-property (property element)
  "Extract the value from the PROPERTY of an ELEMENT."
  (if (stringp element) (get-text-property 0 property element)
    (plist-get (nth 1 element) property)))

(defsubst org-element-put-property (element property value)
  "In ELEMENT set PROPERTY to VALUE.
Return modified element."
  (if (stringp element) (org-add-props element nil property value)
    (setcar (cdr element) (plist-put (nth 1 element) property value))
    element))

(defsubst org-element-contents (element)
  "Extract contents from an ELEMENT."
  (cond ((not (consp element)) nil)
	((symbolp (car element)) (nthcdr 2 element))
	(t element)))

(defsubst org-element-set-contents (element &rest contents)
  "Set ELEMENT's contents to CONTENTS.
Return ELEMENT."
  (cond ((null element) contents)
	((not (symbolp (car element)))
         (if (not (listp element))
             ;; Non-element.
             contents
           ;; Anonymous element (el1 el2 ...)
           (setcar element (car contents))
           (setcdr element (cdr contents))
           element))
	((cdr element) (setcdr (cdr element) contents) element)
	(t (nconc element contents))))

(defsubst org-element-adopt-elements (parent &rest children)
  "Append elements to the contents of another element.

PARENT is an element or object.  CHILDREN can be elements,
objects, or a strings.

The function takes care of setting `:parent' property for CHILD.
Return parent element."
  (declare (indent 1))
  (if (not children) parent
    ;; Link every child to PARENT. If PARENT is nil, it is a secondary
    ;; string: parent is the list itself.
    (dolist (child children)
      (when child
        (org-element-put-property child :parent (or parent children))))
    ;; Add CHILDREN at the end of PARENT contents.
    (when parent
      (apply #'org-element-set-contents
	     parent
	     (nconc (org-element-contents parent) children)))
    ;; Return modified PARENT element.
    (or parent children)))

(defun org-element-extract-element (element)
  "Extract ELEMENT from parse tree.
Remove element from the parse tree by side-effect, and return it
with its `:parent' property stripped out."
  (let ((parent (org-element-property :parent element))
	(secondary (org-element-secondary-p element)))
    (if secondary
        (org-element-put-property
	 parent secondary
	 (delq element (org-element-property secondary parent)))
      (apply #'org-element-set-contents
	     parent
	     (delq element (org-element-contents parent))))
    ;; Return ELEMENT with its :parent removed.
    (org-element-put-property element :parent nil)))

(defun org-element-insert-before (element location)
  "Insert ELEMENT before LOCATION in parse tree.
LOCATION is an element, object or string within the parse tree.
Parse tree is modified by side effect."
  (let* ((parent (org-element-property :parent location))
	 (property (org-element-secondary-p location))
	 (siblings (if property (org-element-property property parent)
		     (org-element-contents parent)))
	 ;; Special case: LOCATION is the first element of an
	 ;; independent secondary string (e.g. :title property).  Add
	 ;; ELEMENT in-place.
	 (specialp (and (not property)
			(eq siblings parent)
			(eq (car parent) location))))
    ;; Install ELEMENT at the appropriate LOCATION within SIBLINGS.
    (cond (specialp)
	  ((or (null siblings) (eq (car siblings) location))
	   (push element siblings))
	  ((null location) (nconc siblings (list element)))
	  (t
	   (let ((index (cl-position location siblings)))
	     (unless index (error "No location found to insert element"))
	     (push element (cdr (nthcdr (1- index) siblings))))))
    ;; Store SIBLINGS at appropriate place in parse tree.
    (cond
     (specialp (setcdr parent (copy-sequence parent)) (setcar parent element))
     (property (org-element-put-property parent property siblings))
     (t (apply #'org-element-set-contents parent siblings)))
    ;; Set appropriate :parent property.
    (org-element-put-property element :parent parent)))

(defun org-element-set-element (old new)
  "Replace element or object OLD with element or object NEW.
The function takes care of setting `:parent' property for NEW."
  ;; Ensure OLD and NEW have the same parent.
  (org-element-put-property new :parent (org-element-property :parent old))
  (dolist (p org-element--cache-element-properties)
    (when (org-element-property p old)
      (org-element-put-property new p (org-element-property p old))))
  (if (or (memq (org-element-type old) '(plain-text nil))
	  (memq (org-element-type new) '(plain-text nil)))
      ;; We cannot replace OLD with NEW since one of them is not an
      ;; object or element.  We take the long path.
      (progn (org-element-insert-before new old)
	     (org-element-extract-element old))
    ;; Since OLD is going to be changed into NEW by side-effect, first
    ;; make sure that every element or object within NEW has OLD as
    ;; parent.
    (dolist (blob (org-element-contents new))
      (org-element-put-property blob :parent old))
    ;; Transfer contents.
    (apply #'org-element-set-contents old (org-element-contents new))
    ;; Overwrite OLD's properties with NEW's.
    (setcar (cdr old) (nth 1 new))
    ;; Transfer type.
    (setcar old (car new))))

(defun org-element-create (type &optional props &rest children)
  "Create a new element of type TYPE.
Optional argument PROPS, when non-nil, is a plist defining the
properties of the element.  CHILDREN can be elements, objects or
strings."
  (apply #'org-element-adopt-elements (list type props) children))

(defun org-element-copy (datum)
  "Return a copy of DATUM.
DATUM is an element, object, string or nil.  `:parent' property
is cleared and contents are removed in the process."
  (when datum
    (let ((type (org-element-type datum)))
      (pcase type
	(`org-data (list 'org-data nil))
	(`plain-text (substring-no-properties datum))
	(`nil (copy-sequence datum))
	(_
         (let ((element-copy (list type (plist-put (copy-sequence (nth 1 datum)) :parent nil))))
           ;; We cannot simply return the copies property list.  When
           ;; DATUM is i.e. a headline, it's property list (`:title'
           ;; in case of headline) can contain parsed objects.  The
           ;; objects will contain `:parent' property set to the DATUM
           ;; itself.  When copied, these inner `:parent' property
           ;; values will contain incorrect object decoupled from
           ;; DATUM.  Changes to the DATUM copy will not longer be
           ;; reflected in the `:parent' properties.  So, we need to
           ;; reassign inner `:parent' properties to the DATUM copy
           ;; explicitly.
           (org-element-map element-copy (cons 'plain-text org-element-all-objects)
             (lambda (obj) (when (equal datum (org-element-property :parent obj))
                        (org-element-put-property obj :parent element-copy))))
           element-copy))))))

(defun org-element-lineage (datum &optional types with-self)
  "List all ancestors of a given element or object.

DATUM is an object or element.

Return ancestors from the closest to the farthest.  When optional
argument TYPES is a list of symbols, return the first element or
object in the lineage whose type belongs to that list instead.

When optional argument WITH-SELF is non-nil, lineage includes
DATUM itself as the first element, and TYPES, if provided, also
apply to it.

When DATUM is obtained through `org-element-context' or
`org-element-at-point', only ancestors from its section can be
found.  There is no such limitation when DATUM belongs to a full
parse tree."
  (let ((up (if with-self datum (org-element-property :parent datum)))
	ancestors)
    (while (and up (not (memq (org-element-type up) types)))
      (unless types (push up ancestors))
      (setq up (org-element-property :parent up)))
    (if types up (nreverse ancestors))))

(provide 'org-element-ast)
;;; org-element-ast.el ends here
