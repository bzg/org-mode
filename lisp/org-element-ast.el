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

;;;; Syntax node type

(defun org-element-type (node &optional anonymous)
  "Return type of NODE.

The function returns the type of the node provided.
It can also return the following special value:
  `plain-text'       for a string
  nil                in any other case.

When optional argument ANONYMOUS is non-nil, return symbol `anonymous'
when NODE is an anonymous node."
  (declare (pure t))
  (cond
   ((stringp node) 'plain-text)
   ((null node) nil)
   ((not (consp node)) nil)
   ((symbolp (car node)) (car node))
   ((and anonymous (car node) (org-element-type (car node) t))
    'anonymous)
   (t nil)))

(defun org-element-secondary-p (node)
  "Non-nil when NODE directly belongs to a secondary node.
Return value is the containing property name, as a keyword, or nil."
  (declare (pure t))
  (let* ((parent (org-element-property :parent node))
	 (properties (org-element-property :secondary parent))
         val)
    (catch 'exit
      (dolist (p properties)
        (setq val (org-element-property-1 p parent))
	(when (or (eq node val) (memq node val))
	  (throw 'exit p))))))

;;;; Deferred values

(cl-defstruct (org-element-deferred
               (:constructor nil)
               (:constructor org-element-deferred-create
                             ( auto-undefer-p function &rest arg-value
                               &aux (args arg-value)))
               (:constructor org-element-deferred-create-alias
                             ( keyword &optional auto-undefer-p
                               &aux
                               (function #'org-element-property-2)
                               (args (list keyword))))
               (:constructor org-element-deferred-create-list
                             ( args &optional auto-undefer-p
                               &aux
                               (function #'org-element--deferred-resolve-list)))
               (:type vector) :named)
  "Dynamically computed value.

The value can be obtained by calling FUNCTION with containing syntax
node as first argument and ARGS list as remainting arguments.

If the function throws `:org-element-deferred-retry' signal, assume
that the syntax node has been modified by side effect and retry
retrieving the value that was previously deferred.

AUTO-UNDEFER slot flags if the property value should be replaced upon
resolution.  Some functions may ignore this flag."
  function args auto-undefer-p)

(defsubst org-element--deferred-resolve-once (deferred-value &optional node)
  "Resolve DEFERRED-VALUE for NODE.
Throw `:org-element-deferred-retry' if NODE has been modified and we
need to re-read the value again."
  (apply (org-element-deferred-function deferred-value)
         node
         (org-element-deferred-args deferred-value)))

(defsubst org-element--deferred-resolve (value &optional node force-undefer)
  "Resolve VALUE for NODE recursively.
Return a cons cell of the resolved value and the value to store.
When no value should be stored, return `org-element-ast--nil' as cdr.
When FORCE-UNDEFER is non-nil, resolve all the deferred values, ignoring
their `auto-undefer-p' slot.

Throw `:org-element-deferred-retry' if NODE has been modified and we
need to re-read the value again."
  (let ((value-to-store 'org-element-ast--nil) undefer)
    (while (org-element-deferred-p value)
      (setq undefer (or force-undefer (org-element-deferred-auto-undefer-p value))
            value (org-element--deferred-resolve-once value node))
      (when undefer (setq value-to-store value)))
    (cons value value-to-store)))

(defsubst org-element--deferred-resolve-force (value &optional node)
  "Resolve VALUE for NODE recursively, ignoring `auto-undefer-p'.
Return the resolved value.

Throw `:org-element-deferred-retry' if NODE has been modified and we
need to re-read the value again."
  (car (org-element--deferred-resolve value node 'force)))

(defsubst org-element--deferred-resolve-list (node &rest list)
  "Unconditionally resolve all the deferred values in LIST for NODE.
Return a new list with all the values resolved.

Throw `:org-element-deferred-retry' if NODE has been modified and we
need to re-read the value again."
  (mapcar
   (lambda (value)
     (if (org-element-deferred-p value)
         (org-element--deferred-resolve-force value node)
       value))
   list))

;;;; Object properties

(eval-and-compile ; make available during inline expansion

  (defconst org-element--standard-properties
    '( :begin :end :contents-begin :contents-end
       :post-blank :post-affiliated :secondary
       :cached :org-element--cache-sync-key
       :robust-begin :robust-end
       :mode :granularity :true-level
       :parent :deferred :structure :buffer)
    "Standard properties stored in every syntax node structure.
These properties are stored in an array pre-allocated every time a new
object is created.  Two exceptions are `anonymous' and `plain-text'
node types.")

  (defconst org-element--standard-properties-idxs
    (let (plist)
      (seq-do-indexed
       (lambda (property idx)
         (setq plist (plist-put plist property idx)))
       org-element--standard-properties)
      plist)
    "Property list holding standard indexes for `org-element--standard-properties'."))

(define-inline org-element--property-idx (property)
  "Return standard property index or nil."
  (declare (pure t))
  (if (inline-const-p property)
      (plist-get
       org-element--standard-properties-idxs
       (inline-const-val property))
    (inline-quote (plist-get
                   org-element--standard-properties-idxs
                   ,property))))

(define-inline org-element--parray (node)
  "Return standard property array for NODE."
  (declare (pure t))
  (inline-letevals (node)
    (inline-quote
     (pcase (org-element-type ,node)
       (`nil nil)
       ;; Do not use property array for strings - they usually hold
       ;; `:parent' property and nothing more.
       (`plain-text nil)
       (_
        ;; (type (:standard-properties val ...) ...)
        (if (eq :standard-properties (car (nth 1 ,node)))
            (cadr (nth 1 ,node))
          ;; Non-standard order.  Go long way.
          (plist-get (nth 1 ,node) :standard-properties)))))))

(define-inline org-element--plist-property (property node &optional dflt)
  "Extract the value for PROPERTY from NODE's property list.
Ignore standard property array."
  (declare (pure t))
  (inline-letevals (property node dflt)
    (inline-quote
     (pcase (org-element-type ,node)
       (`nil ,dflt)
       (`plain-text
        (or (get-text-property 0 ,property ,node)
            (when ,dflt
              (if (plist-member (text-properties-at 0 ,node) ,property)
                  nil ,dflt))))
       (_
        (or (plist-get (nth 1 ,node) ,property)
            (when ,dflt
              (if (plist-member (nth 1 ,node) ,property)
                  nil ,dflt))))))))

(define-inline org-element-property-1 (property node &optional dflt)
  "Extract the value for PROPERTY of an NODE.
Do not resolve deferred values.
If PROPERTY is not present, return DFLT."
  (declare (pure t))
  (let ((idx (and (inline-const-p property)
                  (org-element--property-idx property))))
    (if idx
        (inline-letevals (node)
          (inline-quote
           (if-let ((parray (org-element--parray ,node)))
               (pcase (aref parray ,idx)
                 (`org-element-ast--nil ,dflt)
                 (val val))
             ;; No property array exists.  Fall back to `plist-get'.
             (org-element--plist-property ,property ,node ,dflt))))
      (inline-letevals (node property)
        (inline-quote
         (let ((idx (org-element--property-idx ,property)))
           (if-let ((parray (and idx (org-element--parray ,node))))
               (pcase (aref parray idx)
                 (`org-element-ast--nil ,dflt)
                 (val val))
             ;; No property array exists.  Fall back to `plist-get'.
             (org-element--plist-property ,property ,node ,dflt))))))))

(define-inline org-element--put-parray (node &optional parray)
  "Initialize standard property array in NODE.
Return the array or nil when NODE is `plain-text'."
  (inline-letevals (node parray)
    (inline-quote
     (let ((parray ,parray))
       (unless (or parray (memq (org-element-type ,node) '(plain-text nil)))
         (setq parray (make-vector ,(length org-element--standard-properties) nil))
         ;; Copy plist standard properties back to parray.
         (let ((stdplist org-element--standard-properties-idxs))
           (while stdplist
             (aset parray (cadr stdplist)
                   (org-element--plist-property (car stdplist) ,node))
             (setq stdplist (cddr stdplist))))
         (setcar (cdr ,node)
                 (nconc (list :standard-properties parray)
                        (cadr ,node)))
         parray)))))

(define-inline org-element-put-property (node property value)
  "In NODE, set PROPERTY to VALUE.
Return modified NODE."
  (let ((idx (and (inline-const-p property)
                  (org-element--property-idx property))))
    (if idx
        (inline-letevals (node value)
          (inline-quote
           (if (eq 'plain-text (org-element-type ,node))
               ;; Special case: Do not use parray for plain-text.
               (org-add-props ,node nil ,property ,value)
             (let ((parray
                    (or (org-element--parray ,node)
                        (org-element--put-parray ,node))))
               (when parray (aset parray ,idx ,value))
               ,node))))
      (inline-letevals (node property value)
        (inline-quote
         (let ((idx (org-element--property-idx ,property)))
           (if (and idx (not (eq 'plain-text (org-element-type ,node))))
               (when-let
                   ((parray
                     (or (org-element--parray ,node)
                         (org-element--put-parray ,node))))
                 (aset parray idx ,value))
             (pcase (org-element-type ,node)
               (`nil nil)
               (`plain-text
                (org-add-props ,node nil ,property ,value))
               (_
                ;; Note that `plist-put' adds new elements at the end,
                ;; thus keeping `:standard-properties' as the first element.
                (setcar (cdr ,node) (plist-put (nth 1 ,node) ,property ,value)))))
           ,node))))))

(defun org-element--property (property node &optional dflt force-undefer)
  "Extract the value from the PROPERTY of a NODE.
Return DFLT when PROPERTY is not present.
When FORCE-UNDEFER is non-nil, unconditionally resolve deferred
properties, replacing their values in NODE."
  (let ((value (org-element-property-1 property node 'org-element-ast--nil)))
    ;; PROPERTY not present.
    (when (and (eq 'org-element-ast--nil value)
               (org-element-deferred-p
                (org-element-property-1 :deferred node)))
      ;; If :deferred has `org-element-deferred' type, resolve it for
      ;; side-effects, and re-assign the new value.
      (org-element--property :deferred node nil 'force-undefer)
      ;; Try to retrieve the value again.
      (setq value (org-element-property-1 property node dflt)))
    ;; Deferred property.  Resolve it recursively.
    (when (org-element-deferred-p value)
      (let ((retry t) (firstiter t))
        (while retry
          (if firstiter (setq firstiter nil) ; avoid extra call to `org-element-property-1'.
            (setq value (org-element-property-1 property node 'org-element-ast--nil)))
          (catch :org-element-deferred-retry
            (pcase-let
                ((`(,resolved . ,value-to-store)
                  (org-element--deferred-resolve value node force-undefer)))
              (setq value resolved)
              ;; Store the resolved property value, if needed.
              (unless (eq value-to-store 'org-element-ast--nil)
                (org-element-put-property node property value-to-store)))
            ;; Finished resolving.
            (setq retry nil)))))
    ;; Return the resolved value.
    (if (eq value 'org-element-ast--nil) dflt value)))

(define-inline org-element-property (property node &optional dflt force-undefer)
  "Extract the value from the PROPERTY of a NODE.
Return DFLT when PROPERTY is not present.
When FORCE-UNDEFER is non-nil, unconditionally resolve deferred
properties, replacing their values in NODE.

Note: The properties listed in `org-element--standard-properties',
except `:deferred', may not be resolved."
  (if (and (inline-const-p property)
           (not (memq (inline-const-val property) '(:deferred :parent)))
           (org-element--property-idx (inline-const-val property)))
      ;; This is an important optimization, making common org-element
      ;; API calls much faster.
      (inline-quote (org-element-property-1 ,property ,node ,dflt))
    (inline-quote (org-element--property ,property ,node ,dflt ,force-undefer))))

;;;; Node contents.

(defsubst org-element-contents (node)
  "Extract contents from NODE.
Do not resolve deferred values."
  (declare (pure t))
  (cond ((not (consp node)) nil)
	((symbolp (car node)) (nthcdr 2 node))
	(t node)))

(defsubst org-element-set-contents (node &rest contents)
  "Set NODE's contents to CONTENTS.
Return modified NODE.
If NODE cannot have contents, return CONTENTS."
  (pcase (org-element-type node t)
    (`plain-text contents)
    ((guard (null node)) contents)
    ;; Anonymous node.
    (`anonymous
     (setcar node (car contents))
     (setcdr node (cdr contents))
     node)
    ;; Node with type.
    (_ (setf (cddr node) contents)
       node)))
;;;; AST modification

(defalias 'org-element-adopt-elements #'org-element-adopt)
(defun org-element-adopt (parent &rest children)
  "Append CHILDREN to the contents of PARENT.

PARENT is a syntax node.  CHILDREN can be elements, objects, or
strings.

If PARENT is nil, create a new anonymous node containing CHILDREN.

The function takes care of setting `:parent' property for each child.
Return the modified PARENT."
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

(defalias 'org-element-extract-element #'org-element-extract)
(defun org-element-extract (node)
  "Extract NODE from parse tree.
Remove NODE from the parse tree by side-effect, and return it
with its `:parent' property stripped out."
  (let ((parent (org-element-property :parent node))
	(secondary (org-element-secondary-p node)))
    (if secondary
        (org-element-put-property
	 parent secondary
	 (delq node (org-element-property secondary parent)))
      (apply #'org-element-set-contents
	     parent
	     (delq node (org-element-contents parent))))
    ;; Return NODE with its :parent removed.
    (org-element-put-property node :parent nil)))

(defun org-element-insert-before (node location)
  "Insert NODE before LOCATION in parse tree.
LOCATION is an element, object or string within the parse tree.
Parse tree is modified by side effect."
  (let* ((parent (org-element-property :parent location))
	 (property (org-element-secondary-p location))
	 (siblings (if property (org-element-property property parent)
		     (org-element-contents parent)))
	 ;; Special case: LOCATION is the first element of an
	 ;; independent secondary string (e.g. :title property).  Add
	 ;; NODE in-place.
	 (specialp (and (not property)
			(eq siblings parent)
			(eq (car parent) location))))
    ;; Install NODE at the appropriate LOCATION within SIBLINGS.
    (cond (specialp)
	  ((or (null siblings) (eq (car siblings) location))
	   (push node siblings))
	  ((null location) (nconc siblings (list node)))
	  (t
	   (let ((index (cl-position location siblings)))
	     (unless index (error "No location found to insert node"))
	     (push node (cdr (nthcdr (1- index) siblings))))))
    ;; Store SIBLINGS at appropriate place in parse tree.
    (cond
     (specialp (setcdr parent (copy-sequence parent)) (setcar parent node))
     (property (org-element-put-property parent property siblings))
     (t (apply #'org-element-set-contents parent siblings)))
    ;; Set appropriate :parent property.
    (org-element-put-property node :parent parent)))

(defalias 'org-element-set-element #'org-element-set)
(defun org-element-set (old new &optional keep-props)
  "Replace element or object OLD with element or object NEW.
When KEEP-PROPS is non-nil, keep OLD values of the listed property
names.

Return the modified element.

The function takes care of setting `:parent' property for NEW."
  ;; Ensure OLD and NEW have the same parent.
  (org-element-put-property new :parent (org-element-property :parent old))
  ;; Handle KEEP-PROPS.
  (dolist (p keep-props)
    (org-element-put-property new p (org-element-property p old)))
  (let ((old-type (org-element-type old))
        (new-type (org-element-type new)))
    (if (or (eq old-type 'plain-text)
	    (eq new-type 'plain-text))
        ;; We cannot replace OLD with NEW since strings are not mutable.
        ;; We take the long path.
        (progn (org-element-insert-before new old)
	       (org-element-extract old))
      ;; Since OLD is going to be changed into NEW by side-effect, first
      ;; make sure that every element or object within NEW has OLD as
      ;; parent.
      (dolist (blob (org-element-contents new))
        (org-element-put-property blob :parent old))
      ;; Both OLD and NEW are lists.
      (setcar old (car new))
      (setcdr old (cdr new))))
  old)

(defun org-element-create (type &optional props &rest children)
  "Create a new syntax node of TYPE.
Optional argument PROPS, when non-nil, is a plist defining the
properties of the node.  CHILDREN can be elements, objects or
strings.

When TYPE is `plain-text', CHILDREN must contain a single node -
string.  Alternatively, TYPE can be a string.  When TYPE is nil or
`anonymous', PROPS must be nil."
  (cl-assert (plistp props))
  ;; Assign parray.
  (when (and props (not (stringp type)) (not (eq type 'plain-text)))
    (let ((node (list 'dummy props)))
      (org-element--put-parray node)
      (setq props (nth 1 node))
      ;; Remove standard properties from PROPS plist by side effect.
      (let ((ptail props))
        (while ptail
          (if (not (and (keywordp (car ptail))
                        (org-element--property-idx (car ptail))))
              (setq ptail (cddr ptail))
            (if (null (cddr ptail)) ; last property
                (setq props (nbutlast props 2)
                      ptail nil)
              (setcar ptail (nth 2 ptail))
              (setcdr ptail (seq-drop ptail 3))))))))
  (pcase type
    ((or `nil `anonymous)
     (cl-assert (null props))
     (apply #'org-element-adopt nil children))
    (`plain-text
     (cl-assert (length= children 1))
     (org-add-props (car children) props))
    ((pred stringp)
     (if props (org-add-props type props) type))
    (_ (apply #'org-element-adopt (list type props) children))))

(defun org-element-copy (datum &optional keep-contents)
  "Return a copy of DATUM.
DATUM is an element, object, string or nil.  `:parent' property
is cleared and contents are removed in the process.
Secondary objects are also copied and their `:parent' is re-assigned.

When optional argument KEEP-CONTENTS is non-nil, do not remove the
contents.  Instead, copy the children recursively, updating their
`:parent' property.

As a special case, `anonymous' nodes do not have their contents
removed.  The contained children are copied recursively, updating
their `:parent' property to the copied `anonymous' node.

When DATUM is `plain-text', all the properties are removed."
  (pcase (org-element-type datum t)
    ((guard (null datum)) nil)
    (`plain-text (substring-no-properties datum))
    (`nil (error "Not an Org syntax node: %S" datum))
    (`anonymous
     (let* ((node-copy (copy-sequence datum))
            (tail node-copy))
       (while tail
         (setcar tail (org-element-copy (car tail) t))
         (org-element-put-property (car tail) :parent node-copy)
         (setq tail (cdr tail)))
       node-copy))
    (_
     (let ((node-copy (copy-sequence datum)))
       ;; Copy `:standard-properties'
       (when-let ((parray (org-element-property-1 :standard-properties node-copy)))
         (org-element-put-property node-copy :standard-properties (copy-sequence parray)))
       ;; Clear `:parent'.
       (org-element-put-property node-copy :parent nil)
       ;; We cannot simply return the copied property list.  When
       ;; DATUM is i.e. a headline, it's property list `:title' can
       ;; contain parsed objects.  The objects will contain
       ;; `:parent' property set to the DATUM itself.  When copied,
       ;; these inner `:parent' property values will contain
       ;; incorrect object decoupled from DATUM.  Changes to the
       ;; DATUM copy will no longer be reflected in the `:parent'
       ;; properties.  So, we need to reassign inner `:parent'
       ;; properties to the DATUM copy explicitly.
       (dolist (secondary-prop (org-element-property :secondary node-copy))
         (when-let ((secondary-value (org-element-property secondary-prop node-copy)))
           (setq secondary-value (org-element-copy secondary-value t))
           (if (org-element-type secondary-value)
               (org-element-put-property secondary-value :parent node-copy)
             (dolist (el secondary-value)
               (org-element-put-property el :parent node-copy)))
           (org-element-put-property node-copy secondary-prop secondary-value)))
       (when keep-contents
         (let ((contents (org-element-contents node-copy)))
           (while contents
             (setcar contents (org-element-copy (car contents) t))
             (setq contents (cdr contents)))))
       node-copy))))

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
`org-element-at-point', and org-element-cache is disabled, only
ancestors from its section can be found.  There is no such limitation
when DATUM belongs to a full parse tree."
  (let ((up (if with-self datum (org-element-property :parent datum)))
	ancestors)
    (while (and up (not (memq (org-element-type up) types)))
      (unless types (push up ancestors))
      (setq up (org-element-property :parent up)))
    (if types up (nreverse ancestors))))

(provide 'org-element-ast)
;;; org-element-ast.el ends here
