(defun org-agenda-switch-to (&optional delete-other-windows)
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let ((cb (current-buffer))
	(line (org-current-line))
	(col (current-column))
	(buf (current-buffer))
	(pos (point)))
    (with-current-buffer (get-buffer-create "OrgAgendaGotoLog")
      (goto-char (point-max))
      (insert "--------------------------------------------------------\n")
      (insert (format "This command: %s\n" this-command))
      (insert (format "Last command: %s\n" last-command))
      (insert (format "Line/Column/Point: %d/%d/%d\n" line col pos))))
  (orglog-describe-char (point))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (and delete-other-windows (delete-other-windows))
    (widen)
    (goto-char pos)
    (when (eq major-mode 'org-mode)
      (org-show-context 'agenda)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil))))
    (let ((cb (current-buffer))
	  (pos (point)))
      (with-current-buffer (get-buffer-create "OrgAgendaGotoLog")
      (goto-char (point-max))
	(insert (format "Arrived: %s %d\n" cb pos))))))

(defun org-agenda-goto (&optional highlight)
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let ((cb (current-buffer))
	(line (org-current-line))
	(col (current-column))
	(buf (current-buffer))
	(pos (point)))
    (with-current-buffer (get-buffer-create "OrgAgendaGotoLog")
      (goto-char (point-max))
      (insert "--------------------------------------------------------\n")
      (insert (format "This command: %s\n" this-command))
      (insert (format "Last command: %s\n" last-command))
      (insert (format "Line/Column/Point: %d/%d/%d\n" line col pos))))
  (orglog-describe-char (point))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (goto-char pos)
    (when (eq major-mode 'org-mode)
      (org-show-context 'agenda)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))) ; show the next heading
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))
    (let ((cb (current-buffer))
	  (pos (point)))
      (with-current-buffer (get-buffer-create "OrgAgendaGotoLog")
	(goto-char (point-max))
	(insert (format "Arrived: %s %d\n" cb pos))))))


(defun orglog-describe-char (pos)
  "Describe the character after POS (interactively, the character after point).
The information includes character code, charset and code points in it,
syntax, category, how the character is encoded in a file,
character composition information (if relevant),
as well as widgets, buttons, overlays, and text properties."
  (interactive "d")
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let* ((char (char-after pos))
	 (charset (char-charset char))
	 (composition (find-composition pos nil nil t))
	 (component-chars nil)
	 (display-table (or (window-display-table)
			    buffer-display-table
			    standard-display-table))
	 (disp-vector (and display-table (aref display-table char)))
	 (multibyte-p enable-multibyte-characters)
	 (overlays (mapcar #'(lambda (o) (overlay-properties o))
			   (overlays-at pos)))
	 (char-description (if (not multibyte-p)
			       (single-key-description char)
			     (if (< char 128)
				 (single-key-description char)
			       (string-to-multibyte
				(char-to-string char)))))
	 (text-props-desc
	  (let ((tmp-buf (generate-new-buffer " *text-props*")))
	    (unwind-protect
		(progn
		  (describe-text-properties pos tmp-buf)
		  (with-current-buffer tmp-buf (buffer-string)))
	      (kill-buffer tmp-buf))))
	 item-list max-width unicode)

    (if (or (< char 256)
	    (memq 'mule-utf-8 (find-coding-systems-region pos (1+ pos)))
	    (get-char-property pos 'untranslated-utf-8))
	(setq unicode (or (get-char-property pos 'untranslated-utf-8)
			  (encode-char char 'ucs))))
    (setq item-list
	  `(("character"
	     ,(format "%s (%d, #o%o, #x%x%s)"
		      (apply 'propertize char-description
			     (text-properties-at pos))
		      char char char
		      (if unicode
			  (format ", U+%04X" unicode)
			"")))
	    ("charset"
	     ,`(insert-text-button
		,(symbol-name charset)
		'type 'help-character-set 'help-args '(,charset))
	     ,(format "(%s)" (charset-description charset)))
	    ("code point"
	     ,(let ((split (split-char char)))
		`(insert-text-button
		  ,(if (= (charset-dimension charset) 1)
		       (format "#x%02X" (nth 1 split))
		     (format "#x%02X #x%02X" (nth 1 split)
			     (nth 2 split)))
		  'action (lambda (&rest ignore)
			    (list-charset-chars ',charset)
			    (with-selected-window
				(get-buffer-window "*Character List*" 0)
			      (goto-char (point-min))
			      (forward-line 2) ;Skip the header.
			      (let ((case-fold-search nil))
				(search-forward ,(char-to-string char)
						nil t))))
		  'help-echo
		  "mouse-2, RET: show this character in its character set")))
	    ("syntax"
	     ,(let ((syntax (syntax-after pos)))
		(with-temp-buffer
		  (internal-describe-syntax-value syntax)
		  (buffer-string))))
	    ("category"
	     ,@(let ((category-set (char-category-set char)))
		 (if (not category-set)
		     '("-- none --")
		   (mapcar #'(lambda (x) (format "%c:%s"
						 x (category-docstring x)))
			   (category-set-mnemonics category-set)))))
	    ,@(let ((props (aref char-code-property-table char))
		    ps)
		(when props
		  (while props
		    (push (format "%s:" (pop props)) ps)
		    (push (format "%s;" (pop props)) ps))
		  (list (cons "Properties" (nreverse ps)))))
	    ("to input"
	     ,@(let ((key-list (and (eq input-method-function
					'quail-input-method)
				    (quail-find-key char))))
		 (if (consp key-list)
		     (list "type"
			   (mapconcat #'(lambda (x) (concat "\"" x "\""))
				      key-list " or ")
			   "with"
			   `(insert-text-button
			     ,current-input-method
			     'type 'help-input-method
			     'help-args '(,current-input-method))))))
	    ("buffer code"
	     ,(encoded-string-description
	       (string-as-unibyte (char-to-string char)) nil))
	    ("file code"
	     ,@(let* ((coding buffer-file-coding-system)
		      (encoded (encode-coding-char char coding)))
		 (if encoded
		     (list (encoded-string-description encoded coding)
			   (format "(encoded by coding system %S)" coding))
		   (list "not encodable by coding system"
			 (symbol-name coding)))))
	    ("display"
	     ,(cond
	       (disp-vector
		(setq disp-vector (copy-sequence disp-vector))
		(dotimes (i (length disp-vector))
		  (setq char (aref disp-vector i))
		  (aset disp-vector i
			(cons char (describe-char-display
				    pos (glyph-char char)))))
		(format "by display table entry [%s] (see below)"
			(mapconcat
			 #'(lambda (x)
			     (format "?%c" (glyph-char (car x))))
			 disp-vector " ")))
	       (composition
		(let ((from (car composition))
		      (to (nth 1 composition))
		      (next (1+ pos))
		      (components (nth 2 composition))
		      ch)
		  (setcar composition
			  (and (< from pos) (buffer-substring from pos)))
		  (setcar (cdr composition)
			  (and (< next to) (buffer-substring next to)))
		  (dotimes (i (length components))
		    (if (integerp (setq ch (aref components i)))
			(push (cons ch (describe-char-display pos ch))
			      component-chars)))
		  (setq component-chars (nreverse component-chars))
		  (format "composed to form \"%s\" (see below)"
			  (buffer-substring from to))))
	       (t
		(let ((display (describe-char-display pos char)))
		  (if (display-graphic-p (selected-frame))
		      (if display
			  (concat
			   "by this font (glyph code)\n"
			   (format "     %s (#x%02X)"
				   (car display) (cdr display)))
			"no font available")
		    (if display
			(format "terminal code %s" display)
		      "not encodable for terminal"))))))
	    ,@(let ((face
		     (if (not (or disp-vector composition))
			 (cond
			  ((and show-trailing-whitespace
				(save-excursion (goto-char pos)
						(looking-at "[ \t]+$")))
			   'trailing-whitespace)
			  ((and nobreak-char-display unicode (eq unicode '#xa0))
			   'nobreak-space)
			  ((and nobreak-char-display unicode (eq unicode '#xad))
			   'escape-glyph)
			  ((and (< char 32) (not (memq char '(9 10))))
			   'escape-glyph)))))
		(if face (list (list "hardcoded face"
				     `(insert-text-button
				       ,(symbol-name face)
				       'type 'help-face 'help-args '(,face))))))
	    ,@(let ((unicodedata (and unicode
				      (describe-char-unicode-data unicode))))
		(if unicodedata
		    (cons (list "Unicode data" " ") unicodedata)))))
    (setq max-width (apply #'max (mapcar #'(lambda (x)
					     (if (cadr x) (length (car x)) 0))
					 item-list)))
    (with-current-buffer (get-buffer-create "OrgAgendaGotoLog")
      (goto-char (point-max))
      (set-buffer-multibyte multibyte-p)
      (let ((formatter (format "%%%ds:" max-width)))
	(dolist (elt item-list)
	  (when (cadr elt)
	    (insert (format formatter (car elt)))
	    (dolist (clm (cdr elt))
	      (if (eq (car-safe clm) 'insert-text-button)
		  (progn (insert " ") (eval clm))
		(when (>= (+ (current-column)
			     (or (string-match "\n" clm)
				 (string-width clm))
			     1)
			  (window-width))
		  (insert "\n")
		  (indent-to (1+ max-width)))
		(insert " " clm)))
	    (insert "\n"))))
      
      (when overlays
	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward "character:[ \t\n]+")
	  (let* ((end (+ (point) (length char-description))))
	    (mapc #'(lambda (props)
		      (let ((o (make-overlay (point) end)))
			(while props
			  (overlay-put o (car props) (nth 1 props))
			  (setq props (cddr props)))))
		  overlays))))
      
      (when disp-vector
	(insert
	 "\nThe display table entry is displayed by ")
	(if (display-graphic-p (selected-frame))
	    (progn
	      (insert "these fonts (glyph codes):\n")
	      (dotimes (i (length disp-vector))
		(insert (glyph-char (car (aref disp-vector i))) ?:
			(propertize " " 'display '(space :align-to 5))
			(if (cdr (aref disp-vector i))
			    (format "%s (#x%02X)" (cadr (aref disp-vector i))
				    (cddr (aref disp-vector i)))
			  "-- no font --")
			"\n")
		(let ((face (glyph-face (car (aref disp-vector i)))))
		  (when face
		    (insert (propertize " " 'display '(space :align-to 5))
			    "face: ")
		    (insert (concat "`" (symbol-name face) "'"))
		    (insert "\n")))))
	  (insert "these terminal codes:\n")
	  (dotimes (i (length disp-vector))
	    (insert (car (aref disp-vector i))
		    (propertize " " 'display '(space :align-to 5))
		    (or (cdr (aref disp-vector i)) "-- not encodable --")
		    "\n"))))
      
      (when composition
	(insert "\nComposed")
	(if (car composition)
	    (if (cadr composition)
		(insert " with the surrounding characters \""
			(car composition) "\" and \""
			(cadr composition) "\"")
	      (insert " with the preceding character(s) \""
		      (car composition) "\""))
	  (if (cadr composition)
	      (insert " with the following character(s) \""
		      (cadr composition) "\"")))
	(insert " by the rule:\n\t("
		(mapconcat (lambda (x)
			     (format (if (consp x) "%S" "?%c") x))
			   (nth 2 composition)
			   " ")
		")")
	(insert  "\nThe component character(s) are displayed by ")
	(if (display-graphic-p (selected-frame))
	    (progn
	      (insert "these fonts (glyph codes):")
	      (dolist (elt component-chars)
		(insert "\n " (car elt) ?:
			(propertize " " 'display '(space :align-to 5))
			(if (cdr elt)
			    (format "%s (#x%02X)" (cadr elt) (cddr elt))
			  "-- no font --"))))
	  (insert "these terminal codes:")
	  (dolist (elt component-chars)
	    (insert "\n  " (car elt) ":"
		    (propertize " " 'display '(space :align-to 5))
		    (or (cdr elt) "-- not encodable --"))))
	(insert "\nSee the variable `reference-point-alist' for "
		"the meaning of the rule.\n"))
      
      (if text-props-desc (insert text-props-desc)))))
