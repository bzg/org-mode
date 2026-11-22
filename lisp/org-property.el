;;; org-property.el --- Org property API                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements Org mode headline properties.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(declare-function org-inlinetask-in-task-p "org-inlinetask")
(require 'org-move)
(require 'org-duration)
(require 'org-tags)
(require 'org-timestamp)
(require 'org-indent-static)

(declare-function org-get-heading "org")
(declare-function org-clock-update-mode-line "org-clock")
(defvar org-complex-heading-regexp)
(defvar org-todo-line-regexp)
(defvar org-priority-default)
(defvar org-todo-keywords-1)
(declare-function org-entry-blocked-p "org")
(declare-function org-get-category "org")
(declare-function org-todo "org")
(declare-function org-priority "org")
(declare-function org-schedule "org")
(declare-function org-deadline "org")
(defvar org-priority-lowest)
(defvar org-priority-highest)
(declare-function org-heading-components "org")
(declare-function org-columns-compute "org-colview")
(declare-function org-columns-get-format-and-top-level "org-colview")

(defgroup org-properties nil
  "Options concerning properties in Org mode."
  :tag "Org Properties"
  :group 'org)

(defvar org-property-format)

(defcustom org-properties-postprocess-alist nil
  "Alist of properties and functions to adjust inserted values.
Elements of this alist must be of the form

  ([string] [function])

where [string] must be a property name and [function] must be a
lambda expression: this lambda expression must take one argument,
the value to adjust, and return the new value as a string.

For example, this element will allow the property \"Remaining\"
to be updated wrt the relation between the \"Effort\" property
and the clock summary:

 ((\"Remaining\" (lambda(value)
                   (let ((clocksum (org-clock-sum-current-item))
                         (effort (org-duration-to-minutes
                                   (org-entry-get (point) \"Effort\"))))
                     (org-duration-from-minutes (- effort clocksum))))))"
  :group 'org-properties
  :version "24.1"
  :type '(alist :key-type (string     :tag "Property")
		:value-type (function :tag "Function")))

(defcustom org-use-property-inheritance nil
  "Non-nil means properties apply also for sublevels.

This setting is chiefly used during property searches.  Turning it on can
cause significant overhead when doing a search, which is why it is not
on by default.

When nil, only the properties directly given in the current entry count.
When t, every property is inherited.  The value may also be a list of
properties that should have inheritance, or a regular expression matching
properties that should be inherited.

However, note that some special properties use inheritance under special
circumstances (not in searches).  Examples are CATEGORY, ARCHIVE, COLUMNS,
and the properties ending in \"_ALL\" when they are used as descriptor
for valid values of a property.

Note for programmers:
When querying an entry with `org-entry-get', you can control if inheritance
should be used.  By default, `org-entry-get' looks only at the local
properties.  You can request inheritance by setting the inherit argument
to t (to force inheritance) or to `selective' (to respect the setting
in this variable)."
  :group 'org-properties
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific properties" (string :tag "Property"))
	  (regexp :tag "Properties matched by regexp")))

(defconst org-effort-property "Effort"
  "The property that is being used to keep track of effort estimates.
Effort estimates given in this property need to be in the format
defined in org-duration.el.")

(defun org-property-inherit-p (property)
  "Return a non-nil value if PROPERTY should be inherited."
  (cond
   ((eq org-use-property-inheritance t) t)
   ((not org-use-property-inheritance) nil)
   ((stringp org-use-property-inheritance)
    (string-match org-use-property-inheritance property))
   ((listp org-use-property-inheritance)
    (member-ignore-case property org-use-property-inheritance))
   (t (error "Invalid setting of `org-use-property-inheritance'"))))

(defcustom org-property-separators nil
  "An alist to control how properties are combined.

The car of each item should be either a list of property names or
a regular expression, while the cdr should be the separator to
use when combining that property.

If an alist item cannot be found that matches a given property, a
single space will be used as the separator."
  :group 'org-properties
  :package-version '(Org . "9.6")
  :type '(alist :key-type (choice (repeat :tag "Properties" string)
                                  (string :tag "Regular Expression"))
                :value-type (restricted-sexp :tag "Separator"
                                             :match-alternatives (stringp)
                                             :value " ")))

(defun org--property-get-separator (property)
  "Get the separator to use for combining PROPERTY."
  (or
   (catch 'separator
     (dolist (spec org-property-separators)
       (if (listp (car spec))
           (if (member property (car spec))
               (throw 'separator (cdr spec)))
         (if (string-match-p (car spec) property)
             (throw 'separator (cdr spec))))))
   " "))

(defconst org-global-properties-fixed
  '(("VISIBILITY_ALL" . "folded children content all")
    ("CLOCK_MODELINE_TOTAL_ALL" . "current today repeat all auto"))
  "List of property/value pairs that can be inherited by any entry.

These are fixed values, for the preset properties.  The user variable
that can be used to add to this list is `org-global-properties'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.  If the value represents
multiple items like an \"_ALL\" property, separate the items by
spaces.")

(defcustom org-global-properties nil
  "List of property/value pairs that can be inherited by any entry.

This list will be combined with the constant `org-global-properties-fixed'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.

Buffer local properties are added either by a document property drawer

:PROPERTIES:
:NAME: VALUE
:END:

or by adding lines like

#+PROPERTY: NAME VALUE"
  :group 'org-properties
  :type '(repeat
	  (cons (string :tag "Property")
		(string :tag "Value"))))

(defvar-local org-keyword-properties nil
  "List of property/value pairs inherited by any entry.

Valid for the current buffer.  This variable is populated from
PROPERTY keywords.

Note that properties are defined also in property drawers.
Properties defined there take precedence over properties defined
as keywords.")

(defconst org-special-properties
  '("ALLTAGS" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE" "FILE"
    "ITEM" "PRIORITY" "SCHEDULED" "TAGS" "TIMESTAMP" "TIMESTAMP_IA" "TODO")
  "The special properties valid in Org mode.
These are properties that are not defined in the property drawer,
but in some other way.")

(defconst org-default-properties
  '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION" "CUSTOM_ID"
    "LOCATION" "LOGGING" "COLUMNS" "VISIBILITY"
    "TABLE_EXPORT_FORMAT" "TABLE_EXPORT_FILE"
    "EXPORT_OPTIONS" "EXPORT_TEXT" "EXPORT_FILE_NAME"
    "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE" "UNNUMBERED"
    "ORDERED" "NOBLOCKING" "COOKIE_DATA" "LOG_INTO_DRAWER" "REPEAT_TO_STATE"
    "CLOCK_MODELINE_TOTAL" "STYLE" "HTML_CONTAINER_CLASS"
    "ORG-IMAGE-ACTUAL-WIDTH")
  "Some properties that are used by Org mode for various purposes.
Being in this list makes sure that they are offered for completion.")

(defun org--valid-property-p (property)
  "Non-nil when string PROPERTY is a valid property name."
  (not
   (or (equal property "")
       (string-match-p "\\s-" property))))

(defun org--update-property-plist (key val props)
  "Associate KEY to VAL in alist PROPS.
Modifications are made by side-effect.  Return new alist."
  (let* ((appending (string= (substring key -1) "+"))
	 (key (if appending (substring key 0 -1) key))
	 (old (assoc-string key props t)))
    (if (not old) (cons (cons key val) props)
      (setcdr old (if appending (concat (cdr old) " " val) val))
      props)))

(defun org-get-property-block (&optional beg force)
  "Return the (beg . end) range of the body of the property drawer.
BEG is the beginning of the current subtree or the beginning of
the document if before the first headline.  If it is not given,
it will be found.  If the drawer does not exist, create it if
FORCE is non-nil, or return nil."
  (org-with-wide-buffer
   (let ((beg (cond (beg (goto-char beg))
		    ((or (not (featurep 'org-inlinetask))
			 (org-inlinetask-in-task-p))
		     (org-back-to-heading-or-point-min t) (point))
		    (t (org-with-limited-levels
			(org-back-to-heading-or-point-min t))
		       (point)))))
     ;; Move point to its position according to its positional rules.
     (cond ((org-before-first-heading-p)
	    (while (and (org-at-comment-p) (bolp)) (forward-line)))
	   (t (forward-line)
	      (when (looking-at-p org-planning-line-re) (forward-line))))
     (cond ((looking-at org-property-drawer-re)
	    (forward-line)
	    (cons (point) (progn (goto-char (match-end 0))
				 (line-beginning-position))))
	   (force
	    (goto-char beg)
	    (org-insert-property-drawer)
	    (let ((pos (save-excursion (re-search-forward org-property-drawer-re)
				       (line-beginning-position))))
	      (cons pos pos)))))))

(defun org-property-action ()
  "Do an action on properties."
  (interactive nil org-mode)
  (message "Property Action:  [s]et  [d]elete  [D]elete globally  [c]ompute")
  (let ((c (read-char-exclusive)))
    (cl-case c
      (?s (call-interactively #'org-set-property))
      (?d (call-interactively #'org-delete-property))
      (?D (call-interactively #'org-delete-property-globally))
      (?c (call-interactively #'org-compute-property-at-point))
      (otherwise (user-error "No such property action %c" c)))))

(defun org-inc-effort ()
  "Increment the value of the effort property in the current entry."
  (interactive nil org-mode)
  (org-set-effort t))

(defvar org-clock-effort)       ; Defined in org-clock.el.
(defvar org-clock-current-task) ; Defined in org-clock.el.
(defun org-set-effort (&optional increment value)
  "Set the effort property of the current entry.
If INCREMENT is non-nil, set the property to the next allowed
value.  Otherwise, if optional argument VALUE is provided, use
it.  Eventually, prompt for the new value if none of the previous
variables is set."
  (interactive "P" org-mode)
  (let* ((allowed (org-property-get-allowed-values nil org-effort-property t))
	 (current (org-entry-get nil org-effort-property))
	 (value
	  (cond
	   (increment
	    (unless allowed (user-error "Allowed effort values are not set"))
	    (or (caadr (member (list current) allowed))
		(user-error "Unknown value %S among allowed values" current)))
	   (value
	    (if (stringp value) value
	      (error "Invalid effort value: %S" value)))
	   (t
	    (let ((must-match
		   (and allowed
			(not (get-text-property 0 'org-unrestricted
					      (caar allowed))))))
	      (completing-read "Effort: " allowed nil must-match))))))
    ;; Test whether the value can be interpreted as a duration before
    ;; inserting it in the buffer:
    (org-duration-to-minutes value)
    ;; Maybe update the effort value:
    (unless (equal current value)
      (org-entry-put nil org-effort-property value))
    (when (equal (org-get-heading t t t t)
		 (bound-and-true-p org-clock-current-task))
      (setq org-clock-effort value)
      (org-clock-update-mode-line))
    (message "%s is now %s" org-effort-property value)))

(defun org-entry-properties (&optional epom which)
  "Get all properties of the current entry.

When EPOM is a buffer position, marker, or element, get all properties
from the entry there instead.

This includes the TODO keyword, the tags, time strings for
deadline, scheduled, and clocking, and any additional properties
defined in the entry.

If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass.  If WHICH is
a string, only get that property.

Return value is an alist.  Keys are properties, as upcased
strings."
  (org-with-point-at epom
    (when (and (derived-mode-p 'org-mode)
	       (org-back-to-heading-or-point-min t))
      (catch 'exit
	(let* ((beg (point))
	       (specific (and (stringp which) (upcase which)))
	       (which (cond ((not specific) which)
			    ((member specific org-special-properties) 'special)
			    (t 'standard)))
	       props)
	  ;; Get the special properties, like TODO and TAGS.
	  (when (memq which '(nil all special))
	    (when (or (not specific) (string= specific "CLOCKSUM"))
	      (let ((clocksum (get-text-property (point) :org-clock-minutes)))
		(when clocksum
		  (push (cons "CLOCKSUM" (org-duration-from-minutes clocksum))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "CLOCKSUM_T"))
	      (let ((clocksumt (get-text-property (point)
						  :org-clock-minutes-today)))
		(when clocksumt
		  (push (cons "CLOCKSUM_T"
			      (org-duration-from-minutes clocksumt))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ITEM"))
	      (let ((case-fold-search nil))
		(when (looking-at org-complex-heading-regexp)
		  (push (cons "ITEM"
			      (let ((title (match-string-no-properties 4)))
				(if (org-string-nw-p title)
				    (org-remove-tabs title)
				  "")))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TODO"))
	      (let ((case-fold-search nil))
		(when (and (looking-at org-todo-line-regexp) (match-end 2))
		  (push (cons "TODO" (match-string-no-properties 2)) props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "PRIORITY"))
	      (push (cons "PRIORITY"
			  (if (looking-at org-priority-regexp)
			      (match-string-no-properties 2)
			    (org-priority-to-string org-priority-default)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "FILE"))
	      (push (cons "FILE" (buffer-file-name (buffer-base-buffer)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TAGS"))
	      (let ((tags (org-get-tags nil t)))
		(when tags
		  (push (cons "TAGS" (org-make-tag-string tags))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ALLTAGS"))
	      (let ((tags (org-get-tags)))
		(when tags
		  (push (cons "ALLTAGS" (org-make-tag-string tags))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "BLOCKED"))
	      (push (cons "BLOCKED" (if (org-entry-blocked-p) "t" "")) props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("CLOSED" "DEADLINE" "SCHEDULED")))
	      (forward-line)
	      (when (looking-at-p org-planning-line-re)
		(end-of-line)
		(let ((bol (line-beginning-position))
		      ;; Backward compatibility: time keywords used to
		      ;; be configurable (before 8.3).  Make sure we
		      ;; get the correct keyword.
		      (key-assoc `(("CLOSED" . ,org-closed-string)
				   ("DEADLINE" . ,org-deadline-string)
				   ("SCHEDULED" . ,org-scheduled-string))))
		  (dolist (pair (if specific (list (assoc specific key-assoc))
				  key-assoc))
		    (save-excursion
		      (when (search-backward (cdr pair) bol t)
			(goto-char (match-end 0))
			(skip-chars-forward " \t")
			(and (looking-at org-ts-regexp-both)
			     (push (cons (car pair)
					 (match-string-no-properties 0))
				   props)))))))
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("TIMESTAMP" "TIMESTAMP_IA")))
	      (let ((find-ts
		     (lambda (end ts)
		       ;; Fix next timestamp before END.  TS is the
		       ;; list of timestamps found so far.
		       (let ((ts ts)
			     (regexp (cond
				      ((string= specific "TIMESTAMP")
				       org-ts-regexp)
				      ((string= specific "TIMESTAMP_IA")
				       org-ts-regexp-inactive)
				      ((assoc "TIMESTAMP_IA" ts)
				       org-ts-regexp)
				      ((assoc "TIMESTAMP" ts)
				       org-ts-regexp-inactive)
				      (t org-ts-regexp-both))))
			 (catch 'next
			   (while (re-search-forward regexp end t)
			     (backward-char)
			     (let ((object (org-element-context)))
			       ;; Accept to match timestamps in node
			       ;; properties, too.
			       (when (org-element-type-p
                                      object '(node-property timestamp))
				 (let ((type
					(org-element-property :type object)))
				   (cond
				    ((and (memq type '(active active-range))
					  (not (equal specific "TIMESTAMP_IA")))
				     (unless (assoc "TIMESTAMP" ts)
				       (push (cons "TIMESTAMP"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))
				    ((and (memq type '(inactive inactive-range))
					  (not (string= specific "TIMESTAMP")))
				     (unless (assoc "TIMESTAMP_IA" ts)
				       (push (cons "TIMESTAMP_IA"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))))
				 ;; Both timestamp types are found,
				 ;; move to next part.
				 (when (= (length ts) 2) (throw 'next ts)))))
			   ts)))))
		(goto-char beg)
		;; First look for timestamps within headline.
		(let ((ts (funcall find-ts (line-end-position) nil)))
		  (if (= (length ts) 2) (setq props (nconc ts props))
		    ;; Then find timestamps in the section, skipping
		    ;; planning line.
		    (let ((end (save-excursion (outline-next-heading))))
		      (forward-line)
		      (when (looking-at-p org-planning-line-re) (forward-line))
		      (setq props (nconc (funcall find-ts end ts) props))))))))
	  ;; Get the standard properties, like :PROP:.
	  (when (memq which '(nil all standard))
	    ;; If we are looking after a specific property, delegate
	    ;; to `org-entry-get', which is faster.  However, make an
	    ;; exception for "CATEGORY", since it can be also set
	    ;; through keywords (i.e. #+CATEGORY).
	    (if (and specific (not (equal specific "CATEGORY")))
		(let ((value (org-entry-get beg specific nil t)))
		  (throw 'exit (and value (list (cons specific value)))))
	      (let ((range (org-get-property-block beg)))
		(when range
		  (let ((end (cdr range)) seen-base)
		    (goto-char (car range))
		    ;; Unlike to `org--update-property-plist', we
		    ;; handle the case where base values is found
		    ;; after its extension.  We also forbid standard
		    ;; properties to be named as special properties.
		    (while (re-search-forward org-property-re end t)
		      (let* ((key (upcase (match-string-no-properties 2)))
			     (extendp (string-match-p "\\+\\'" key))
			     (key-base (if extendp (substring key 0 -1) key))
			     (value (match-string-no-properties 3)))
			(cond
			 ((member-ignore-case key-base org-special-properties))
			 (extendp
			  (setq props
				(org--update-property-plist key value props)))
			 ((member key seen-base))
			 (t (push key seen-base)
			    (let ((p (assoc-string key props t)))
			      (if p (setcdr p (concat value " " (cdr p)))
				(push (cons key value) props))))))))))))
	  (unless (assoc "CATEGORY" props)
	    (push (cons "CATEGORY" (org-get-category beg)) props)
	    (when (string= specific "CATEGORY") (throw 'exit props)))
	  ;; Return value.
	  props)))))

(defun org--property-local-values (property literal-nil &optional epom)
  "Return value for PROPERTY in current entry or at EPOM.
EPOM can be point, marker, or syntax node.

Value is a list whose car is the base value for PROPERTY and cdr
a list of accumulated values.  Return nil if neither is found in
the entry.  Also return nil when PROPERTY is set to \"nil\",
unless LITERAL-NIL is non-nil."
  (setq epom
        (org-element-lineage
         (org-element-at-point epom)
         '(headline inlinetask org-data)
         'with-self))
  (let* ((base-value  (org-element-property (intern (concat ":" (upcase property)    )) epom))
         (extra-value (org-element-property (intern (concat ":" (upcase property) "+")) epom))
         (extra-value (if (listp extra-value) extra-value (list extra-value)))
         (value (if literal-nil (cons base-value extra-value)
                  (cons (org-not-nil base-value) (org-not-nil extra-value)))))
    (and (not (equal value '(nil))) value)))

(defun org--property-global-or-keyword-value (property literal-nil)
  "Return value for PROPERTY as defined by global properties or by keyword.
Return value is a string.  Return nil if property is not set
globally or by keyword.  Also return nil when PROPERTY is set to
\"nil\", unless LITERAL-NIL is non-nil."
  (let ((global
         (or
          (let ((main-value
                 (org-element-property
                  (intern (format ":%s" property))
                  (org-element-org-data)))
                (extra-value
                 (org-element-property
                  (intern (format ":%s+" property))
                  (org-element-org-data))))
            (when (or main-value extra-value)
              (mapconcat
               #'identity
               (delq nil (cons main-value extra-value))
               (org--property-get-separator property))))
	  (cdr (or (assoc-string property org-global-properties t)
	           (assoc-string property org-global-properties-fixed t))))))
    (if literal-nil global (org-not-nil global))))

(defun org-entry-get (epom property &optional inherit literal-nil)
  "Get value of PROPERTY for entry or content at EPOM.

EPOM is an element, marker, or buffer position.

If INHERIT is non-nil and the entry does not have the property,
then also check higher levels of the hierarchy.  If INHERIT is
the symbol `selective', use inheritance only if the setting in
`org-use-property-inheritance' selects PROPERTY for inheritance.

If the property is present but empty, the return value is the
empty string.  If the property is not present at all, nil is
returned.  In any other case, return the value as a string.
Search is case-insensitive.

If LITERAL-NIL is set, return the string value \"nil\" as
a string, do not interpret it as the list atom nil.  This is used
for inheritance when a \"nil\" value can supersede a non-nil
value higher up the hierarchy."
  (cond
   ((member-ignore-case property (cons "CATEGORY" org-special-properties))
    ;; We need a special property.  Use `org-entry-properties' to
    ;; retrieve it, but specify the wanted property.
    (cdr (assoc-string property (org-entry-properties epom property))))
   ((and inherit
	 (or (not (eq inherit 'selective)) (org-property-inherit-p property)))
    (org-entry-get-with-inheritance property literal-nil epom))
   (t
    (let* ((local (org--property-local-values property literal-nil epom))
	   (value (and local (mapconcat #'identity
                                        (delq nil local)
                                        (org--property-get-separator property)))))
      (if literal-nil value (org-not-nil value))))))

(defun org-property-or-variable-value (var &optional inherit)
  "Check if there is a property fixing the value of VAR.
If yes, return this value.  If not, return the current value of the variable."
  (let ((prop (org-entry-get nil (symbol-name var) inherit)))
    (if (and prop (stringp prop) (string-match "\\S-" prop))
	(read prop)
      (symbol-value var))))

(defun org-entry-delete (epom property)
  "Delete PROPERTY from entry at element, point, or marker EPOM.
Accumulated properties, i.e. PROPERTY+, are also removed.  Return
non-nil when a property was removed."
  (org-with-point-at epom
    (pcase (org-get-property-block)
      (`(,begin . ,origin)
       (let* ((end (copy-marker origin))
	      (re (org-re-property
		   (concat (regexp-quote property) "\\+?") t t)))
	 (goto-char begin)
	 (while (re-search-forward re end t)
	   (delete-region (match-beginning 0) (line-beginning-position 2)))
	 ;; If drawer is empty, remove it altogether.
	 (when (= begin end)
	   (delete-region (line-beginning-position 0)
			  (line-beginning-position 2)))
	 ;; Return non-nil if some property was removed.
	 (prog1 (/= end origin) (set-marker end nil))))
      (_ nil))))

;; Multi-values properties are properties that contain multiple values
;; These values are assumed to be single words, separated by whitespace.
(defun org-entry-add-to-multivalued-property (epom property value)
  "Add VALUE to the words in the PROPERTY in entry at EPOM.
EPOM is an element, marker, or buffer position."
  (let* ((old (org-entry-get epom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (unless (member value values)
      (setq values (append values (list value)))
      (org-entry-put epom property (mapconcat #'identity values " ")))))

(defun org-entry-remove-from-multivalued-property (epom property value)
  "Remove VALUE from words in the PROPERTY in entry at EPOM.
EPOM is an element, marker, or buffer position."
  (let* ((old (org-entry-get epom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (when (member value values)
      (setq values (delete value values))
      (org-entry-put epom property (mapconcat #'identity values " ")))))

(defun org-entry-member-in-multivalued-property (epom property value)
  "Is VALUE one of the words in the PROPERTY in EPOM?
EPOM is an element, marker, or buffer position."
  (let* ((old (org-entry-get epom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (member value values)))

(defun org-entry-get-multivalued-property (pom property)
  "Return a list of values in a multivalued property."
  (let* ((value (org-entry-get pom property))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defun org-entry-put-multivalued-property (epom property &rest values)
  "Set multivalued PROPERTY at EPOM to VALUES.
VALUES should be a list of strings.  Spaces will be protected.
EPOM is an element, marker, or buffer position."
  (org-entry-put epom property (mapconcat #'org-entry-protect-space values " "))
  (let* ((value (org-entry-get epom property))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defun org-entry-protect-space (s)
  "Protect spaces and newline in string S."
  (while (string-match " " s)
    (setq s (replace-match "%20" t t s)))
  (while (string-match "\n" s)
    (setq s (replace-match "%0A" t t s)))
  s)

(defun org-entry-restore-space (s)
  "Restore spaces and newline in string S."
  (while (string-match "%20" s)
    (setq s (replace-match " " t t s)))
  (while (string-match "%0A" s)
    (setq s (replace-match "\n" t t s)))
  s)

(defvar org-entry-property-inherited-from (make-marker)
  "Marker pointing to the entry from where a property was inherited.
Each call to `org-entry-get-with-inheritance' will set this marker to the
location of the entry where the inheritance search matched.  If there was
no match, the marker will point nowhere.
Note that also `org-entry-get' calls this function, if the INHERIT flag
is set.")

(defun org-entry-get-with-inheritance (property &optional literal-nil epom)
  "Get PROPERTY of entry or content at EPOM, search higher levels if needed.
EPOM can be a point, marker, or syntax node.
The search will stop at the first ancestor which has the property defined.
If the value found is \"nil\", return nil to show that the property
should be considered as undefined (this is the meaning of nil here).
However, if LITERAL-NIL is set, return the string value \"nil\" instead."
  (move-marker org-entry-property-inherited-from nil)
  (let (values found-inherited?)
    (org-element-lineage-map
        (org-element-at-point epom)
        (lambda (el)
          (pcase-let ((`(,val . ,val+)
                       ;; Force LITERAL-NIL t.
                       ;; Note that the return value may be taken from
                       ;; cache and should not be modified by side effect.
                       (org--property-local-values property t el)))
            (if (not val)
                ;; PROPERTY+
                (prog1 nil ; keep looking for PROPERTY
                  (when val+ (setq values (append (delete nil val+) values))))
              (setq values (cons val (append (delete nil val+) values)))
              (move-marker
               org-entry-property-inherited-from
               (org-element-begin el)
               (org-element-property :buffer el))
              ;; Found inherited direct PROPERTY.
              (setq found-inherited? t))))
      '(inlinetask headline org-data)
      'with-self 'first-match)
    ;; Consider global properties, if we found no PROPERTY (or maybe
    ;; only PROPERTY+).
    (unless found-inherited?
      (when-let* ((global (org--property-global-or-keyword-value
                           property t)))
        (setq values (cons global values))))
    (when values
      (setq values (mapconcat
                    #'identity values
                    (org--property-get-separator property))))
    (if literal-nil values (org-not-nil values))))

(defvar org-property-changed-functions nil
  "Hook called when the value of a property has changed.
Each hook function should accept two arguments, the name of the property
and the new value.")

(defun org-entry-put (epom property value)
  "Set PROPERTY to VALUE for entry at EPOM.

EPOM is an element, marker, or buffer position.

If the value is nil, it is converted to the empty string.  If it
is not a string, an error is raised.  Also raise an error on
invalid property names.

PROPERTY can be any regular property (see
`org-special-properties').  It can also be \"TODO\",
\"PRIORITY\", \"SCHEDULED\" and \"DEADLINE\".

For the last two properties, VALUE may have any of the special
values \"earlier\" and \"later\".  The function then increases or
decreases scheduled or deadline date by one day."
  (cond ((null value) (setq value ""))
	((not (stringp value)) (error "Properties values should be strings"))
	((not (org--valid-property-p property))
	 (user-error "Invalid property name: \"%s\"" property)))
  (org-no-read-only
   (org-with-point-at epom
     (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
	 (org-back-to-heading-or-point-min t)
       (org-with-limited-levels (org-back-to-heading-or-point-min t)))
     (let ((beg (point)))
       (cond
        ((equal property "TODO")
	 (cond ((not (org-string-nw-p value)) (setq value 'none))
	       ((not (member value org-todo-keywords-1))
	        (user-error "\"%s\" is not a valid TODO state" value)))
	 (org-todo value)
	 (when org-auto-align-tags (org-align-tags)))
        ((equal property "PRIORITY")
	 (org-priority (if (org-string-nw-p value) (org-priority-to-value value) 'remove))
	 (when org-auto-align-tags (org-align-tags)))
        ((equal property "SCHEDULED")
	 (forward-line)
	 (if (and (looking-at-p org-planning-line-re)
		  (re-search-forward
		   org-scheduled-time-regexp (line-end-position) t))
	     (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		   ((string= value "later") (org-timestamp-change 1 'day))
		   ((string= value "") (org-schedule '(4)))
		   (t (org-schedule nil value)))
	   (if (member value '("earlier" "later" ""))
	       (call-interactively #'org-schedule)
	     (org-schedule nil value))))
        ((equal property "DEADLINE")
	 (forward-line)
	 (if (and (looking-at-p org-planning-line-re)
		  (re-search-forward
		   org-deadline-time-regexp (line-end-position) t))
	     (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		   ((string= value "later") (org-timestamp-change 1 'day))
		   ((string= value "") (org-deadline '(4)))
		   (t (org-deadline nil value)))
	   (if (member value '("earlier" "later" ""))
	       (call-interactively #'org-deadline)
	     (org-deadline nil value))))
        ((member property org-special-properties)
	 (error "The %s property cannot be set with `org-entry-put'" property))
        (t
         (org-fold-core-ignore-modifications
	   (let* ((range (org-get-property-block beg 'force))
	          (end (cdr range))
	          (case-fold-search t))
	     (goto-char (car range))
	     (if (re-search-forward (org-re-property property nil t) end t)
	         (progn (delete-region (match-beginning 0) (match-end 0))
		        (goto-char (match-beginning 0)))
	       (goto-char end)
	       (insert-and-inherit "\n")
	       (backward-char))
	     (insert-and-inherit ":" property ":")
	     (when value (insert-and-inherit " " value))
	     (org-indent-line))))))
     (run-hook-with-args 'org-property-changed-functions property value))))

(defun org-buffer-property-keys (&optional specials defaults columns)
  "Get all property keys in the current buffer.

When SPECIALS is non-nil, also list the special properties that
reflect things like tags and TODO state.

When DEFAULTS is non-nil, also include properties that has
special meaning internally: ARCHIVE, CATEGORY, SUMMARY,
DESCRIPTION, LOCATION, and LOGGING and others.

When COLUMNS in non-nil, also include property names given in
COLUMN formats in the current buffer."
  (let ((case-fold-search t)
	(props (append
		(and specials org-special-properties)
		(and defaults (cons org-effort-property org-default-properties))
		;; Get property names from #+PROPERTY keywords as well
		(mapcar (lambda (s)
			  (nth 0 (split-string s)))
                        (org-element-property :PROPERTY (org-element-org-data))))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-property-start-re nil t)
       (catch :skip
	 (let ((range (org-get-property-block)))
	   (unless range (throw :skip nil))
	   (goto-char (car range))
	   (let ((begin (car range))
		 (end (cdr range)))
	     ;; Make sure that found property block is not located
	     ;; before current point, as it would generate an infloop.
	     ;; It can happen, for example, in the following
	     ;; situation:
	     ;;
	     ;; * Headline
	     ;;   :PROPERTIES:
	     ;;   ...
	     ;;   :END:
	     ;; *************** Inlinetask
	     ;; #+BEGIN_EXAMPLE
	     ;; :PROPERTIES:
	     ;; #+END_EXAMPLE
	     ;;
	     (if (< begin (point)) (throw :skip nil) (goto-char begin))
	     (while (< (point) end)
	       (let ((p (progn (looking-at org-property-re)
			       (match-string-no-properties 2))))
		 ;; Only add true property name, not extension symbol.
		 (push (if (not (string-match-p "\\+\\'" p)) p
			 (substring p 0 -1))
		       props))
	       (forward-line))))
	 (outline-next-heading)))
     (when columns
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*\\(?:#\\+\\|:\\)COLUMNS:" nil t)
	 (let ((element (org-element-at-point)))
	   (when (org-element-type-p element '(keyword node-property))
	     (let ((value (org-element-property :value element))
		   (start 0))
	       (while (string-match "%[0-9]*\\([[:alnum:]_-]+\\)\\(([^)]+)\\)?\
\\(?:{[^}]+}\\)?"
				    value start)
		 (setq start (match-end 0))
		 (let ((p (match-string-no-properties 1 value)))
		   (unless (member-ignore-case p org-special-properties)
		     (push p props))))))))))
    (sort (delete-dups
	   (append props
		   ;; for each xxx_ALL property, make sure the bare
		   ;; xxx property is also included
		   (delq nil (mapcar (lambda (p)
				       (and (string-match-p "._ALL\\'" p)
					    (substring p 0 -4)))
				     props))))
	  (lambda (a b) (string< (upcase a) (upcase b))))))

(defun org-property-values (key)
  "List all non-nil values of property KEY in current buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search t)
	 (re (org-re-property key))
	 values)
     (while (re-search-forward re nil t)
       (push (org-entry-get (point) key) values))
     (delete-dups values))))

(defun org-insert-property-drawer ()
  "Insert a property drawer into the current entry.
Do nothing if the drawer already exists.  The newly created
drawer is immediately hidden."
  (org-with-wide-buffer
   ;; Set point to the position where the drawer should be inserted.
   (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
       (org-back-to-heading-or-point-min t)
     (org-with-limited-levels (org-back-to-heading-or-point-min t)))
   (if (org-before-first-heading-p)
       (while (and (org-at-comment-p) (bolp)) (forward-line))
     (forward-line)
     (when (looking-at-p org-planning-line-re) (forward-line)))
   (unless (looking-at-p org-property-drawer-re)
     ;; Make sure we start editing a line from current entry, not from
     ;; next one.  It prevents extending text properties or overlays
     ;; belonging to the latter.
     (when (and (bolp) (> (point) (point-min))) (backward-char))
     (let ((begin (if (bobp) (point) (1+ (point))))
	   (inhibit-read-only t))
       (unless (bobp) (insert "\n"))
       (insert ":PROPERTIES:\n:END:")
       (org-fold-region (line-end-position 0) (point) t 'drawer)
       (when (or (eobp) (= begin (point-min))) (insert "\n"))
       (org-indent-region begin (point))))))

(defvar org-property-set-functions-alist nil
  "Property set function alist.
Each entry should have the following format:

 (PROPERTY . READ-FUNCTION)

The read function will be called with the same argument as
`org-completing-read'.")

(defun org-set-property-function (property)
  "Get the function that should be used to set PROPERTY.
This is computed according to `org-property-set-functions-alist'."
  (or (cdr (assoc property org-property-set-functions-alist))
      'org-completing-read))

(defun org-read-property-value (property &optional epom default)
  "Read value for PROPERTY, as a string.
When optional argument EPOM is non-nil, completion uses additional
information, i.e., allowed or existing values at element, point, or
marker EPOM.
Optional argument DEFAULT provides a default value for PROPERTY."
  (let* ((completion-ignore-case t)
	 (allowed
	  (or (org-property-get-allowed-values nil property 'table)
	      (and epom (org-property-get-allowed-values epom property 'table))))
	 (current (org-entry-get nil property))
	 (prompt (format "%s value%s: "
			 property
			 (if (org-string-nw-p current)
			     (format " [%s]" current)
			   "")))
	 (set-function (org-set-property-function property))
         (default (cond
                   ((not allowed) default)
                   ((member default allowed) default)
                   (t nil))))
    (org-trim
     (if allowed
	 (funcall set-function
		  prompt allowed nil
		  (not (get-text-property 0 'org-unrestricted (caar allowed)))
		  default nil default)
       (let ((all (mapcar #'list
			  (append (org-property-values property)
				  (and epom
				       (org-with-point-at epom
					 (org-property-values property)))))))
	 (funcall set-function prompt all nil nil default nil current))))))

(defvar org-last-set-property nil)
(defvar org-last-set-property-value nil)
(defun org-read-property-name ()
  "Read a property name."
  (let ((completion-ignore-case t)
	(default-prop (or (and (org-at-property-p)
			       (match-string-no-properties 2))
			  org-last-set-property)))
    (org-completing-read
     (concat "Property"
	     (if default-prop (concat " [" default-prop "]") "")
	     ": ")
     (mapcar #'list (org-buffer-property-keys nil t t))
     nil nil nil nil default-prop)))

(defun org-set-property-and-value (use-last)
  "Allow setting [PROPERTY]: [value] direction from prompt.
When use-default, don't even ask, just use the last
\"[PROPERTY]: [value]\" string from the history."
  (interactive "P" org-mode)
  (let* ((completion-ignore-case t)
	 (pv (or (and use-last org-last-set-property-value)
		 (org-completing-read
		  "Enter a \"[Property]: [value]\" pair: "
		  nil nil nil nil nil
		  org-last-set-property-value)))
	 prop val)
    (when (string-match "^[ \t]*\\([^:]+\\):[ \t]*\\(.*\\)[ \t]*$" pv)
      (setq prop (match-string 1 pv)
	    val (match-string 2 pv))
      (org-set-property prop val))))

(defun org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.

When called interactively, this will prompt for a property name, offering
completion on existing and default properties.  And then it will prompt
for a value, offering completion either on allowed values (via an inherited
xxx_ALL property) or on existing values in other instances of this property
in the current file.

Throw an error when trying to set a property with an invalid name."
  (interactive (list nil nil) org-mode)
  (let ((property (or property (org-read-property-name))))
    ;; `org-entry-put' also makes the following check, but this one
    ;; avoids polluting `org-last-set-property' and
    ;; `org-last-set-property-value' needlessly.
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (let ((value (or value (org-read-property-value property)))
	  (fn (cdr (assoc-string property org-properties-postprocess-alist t))))
      (setq org-last-set-property property)
      (setq org-last-set-property-value (concat property ": " value))
      ;; Possibly postprocess the inserted value:
      (when fn (setq value (funcall fn value)))
      (unless (equal (org-entry-get nil property) value)
	(org-entry-put nil property value)))))

(defun org-find-property (property &optional value)
  "Find first entry in buffer that sets PROPERTY.

When optional argument VALUE is non-nil, only consider an entry
if it contains PROPERTY set to this value.  If PROPERTY should be
explicitly set to nil, use string \"nil\" for VALUE.

Return position where the entry begins, or nil if there is no
such entry.  If narrowing is in effect, only search the visible
part of the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (org-re-property property nil (not value) value)))
      (catch 'exit
	(while (re-search-forward re nil t)
	  (when (if value (org-at-property-p)
		  (org-entry-get (point) property nil t))
	    (throw 'exit (progn (org-back-to-heading-or-point-min t)
				(point)))))))))

(defun org-delete-property (property)
  "In the current entry, delete PROPERTY."
  (interactive
   (let* ((completion-ignore-case t)
	  (cat (org-entry-get (point) "CATEGORY"))
	  (props0 (org-entry-properties nil 'standard))
	  (props (if cat props0
		   (delete `("CATEGORY" . ,(org-get-category)) props0)))
	  (prop (if (< 1 (length props))
		    (completing-read "Property: " props nil t)
		  (caar props))))
     (list prop))
   org-mode)
  (if (not property)
      (message "No property to delete in this entry")
    (org-entry-delete nil property)
    (message "Property \"%s\" deleted" property)))

(defun org-delete-property-globally (property)
  "Remove PROPERTY globally, from all entries.
This function ignores narrowing, if any."
  (interactive
   (let* ((completion-ignore-case t)
	  (prop (completing-read
		 "Globally remove property: "
		 (mapcar #'list (org-buffer-property-keys)))))
     (list prop))
   org-mode)
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((count 0)
	 (re (org-re-property (concat (regexp-quote property) "\\+?") t t)))
     (while (re-search-forward re nil t)
       (when (org-entry-delete (point) property) (cl-incf count)))
     (message "Property \"%s\" removed from %d entries" property count))))

(defvar org-columns-current-fmt-compiled) ; defined in org-colview.el

(defun org-compute-property-at-point ()
  "Compute the property at point.
This looks for an enclosing column format, extracts the operator and
then applies it to the property in the column format's scope."
  (interactive nil org-mode)
  (unless (org-at-property-p)
    (user-error "Not at a property"))
  (let ((prop (match-string-no-properties 2)))
    (org-columns-get-format-and-top-level)
    (unless (nth 3 (assoc-string prop org-columns-current-fmt-compiled t))
      (user-error "No operator defined for property %s" prop))
    (org-columns-compute prop)))

(defvar org-property-allowed-value-functions nil
  "Hook for functions supplying allowed values for a specific property.
The functions must take a single argument, the name of the property, and
return a flat list of allowed values.  If \":ETC\" is one of
the values, this means that these values are intended as defaults for
completion, but that other values should be allowed too.
The functions must return nil if they are not responsible for this
property.")

(defun org-property-get-allowed-values (epom property &optional table)
  "Get allowed values at EPOM for the property PROPERTY.
EPOM can be an element, marker, or buffer position.
When TABLE is non-nil, return an alist that can directly be used for
completion."
  (let (vals)
    (cond
     ((equal property "TODO")
      (setq vals (org-with-point-at epom
		   (append org-todo-keywords-1 '("")))))
     ((equal property "PRIORITY")
      (let ((n org-priority-lowest))
	(while (>= n org-priority-highest)
	  (push (org-priority-to-string n) vals)
	  (setq n (1- n)))))
     ((member property org-special-properties))
     ((setq vals (run-hook-with-args-until-success
		  'org-property-allowed-value-functions property)))
     (t
      (setq vals (org-entry-get epom (concat property "_ALL") 'inherit))
      (when (and vals (string-match "\\S-" vals))
	(setq vals (car (read-from-string (concat "(" vals ")"))))
	(setq vals (mapcar (lambda (x)
			     (cond ((stringp x) x)
				   ((numberp x) (number-to-string x))
				   ((symbolp x) (symbol-name x))
				   (t "???")))
			   vals)))))
    (when (member ":ETC" vals)
      (setq vals (remove ":ETC" vals))
      (org-add-props (car vals) '(org-unrestricted t)))
    (if table (mapcar #'list vals) vals)))

(defun org-property-previous-allowed-value (&optional _previous)
  "Switch to the next allowed value for this property."
  (interactive nil org-mode)
  (org-property-next-allowed-value t))

(defun org-property-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive nil org-mode)
  (unless (org-at-property-p)
    (user-error "Not at a property"))
  (let* ((prop (car (save-match-data (org-split-string (match-string 1) ":"))))
	 (key (match-string 2))
	 (value (match-string 3))
	 (allowed (or (org-property-get-allowed-values (point) key)
		      (and (member value  '("[ ]" "[-]" "[X]"))
			   '("[ ]" "[X]"))))
	 (heading (save-match-data (nth 4 (org-heading-components))))
	 nval)
    (unless allowed
      (user-error "Allowed values for this property have not been defined"))
    (when previous (setq allowed (reverse allowed)))
    (when (member value allowed)
      (setq nval (car (cdr (member value allowed)))))
    (setq nval (or nval (car allowed)))
    (when (equal nval value)
      (user-error "Only one allowed value for this property"))
    (org-at-property-p)
    (replace-match (concat " :" key ": " nval) t t)
    (org-indent-line)
    (forward-line 0)
    (skip-chars-forward " \t")
    (when (equal prop org-effort-property)
      (when (string= org-clock-current-task heading)
	(setq org-clock-effort nval)
	(org-clock-update-mode-line)))
    (run-hook-with-args 'org-property-changed-functions key nval)))

(provide 'org-property)

;;; org-property.el ends here
