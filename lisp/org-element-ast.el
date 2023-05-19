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
