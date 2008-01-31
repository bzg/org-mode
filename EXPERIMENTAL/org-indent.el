;; Need to hook into demotion, promotion functions:  Redo the entire subtree

;; Problem:  I have this huge list of overlays, should I be able
;; to reuse them?

; should be a mode...
(defconst org-indent-strings (make-vector 80 nil))
(loop for i from 0 to 79 do
      (aset org-indent-strings i (org-add-props (make-string i ?\ )
				     nil 'face '(:underline nil :background "grey95"))))

(defvar org-indent-overlays nil)
(make-variable-buffer-local 'org-indent-overlays)

(defun org-indent-initialize-buffer ()
  (interactive)
  (when (org-mode-p)
    (org-remove-indent-overlays)
    (let (n)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (< (point) (point-max))
	    (cond
	     ((looking-at outline-regexp)
	      (setq n (1+ (funcall outline-level))))
	     (n
	      (setq ov (org-make-overlay (1- (point)) (point)))
	      (org-overlay-put ov 'after-string (aref org-indent-strings n))
	      (org-overlay-put ov 'evaporate t)
	      (org-overlay-put ov 'rear-sticky nil)
	      (org-overlay-put ov 'front-sticky nil)
	      (org-overlay-put ov 'org-indent n)
	      (push ov org-indent-overlays)))
	    (beginning-of-line 2)))))))

(run-with-idle-timer 10 t 'org-indent-initialize-buffer)

(defun org-indent-refresh (start end)
  "Refresh indentation overlays in the range given."
  (save-excursion
    (goto-char start)
    (when (search-forward "\n" end t)
      (goto-char start)
      (beginning-of-line 0)
      (let ((n (or (get-char-property (max (point-min) (1- (point)))
				      'org-indent) 0))
	    ov e)
	(while (<= (point) end)
	  (mapc (lambda (x) (if (org-overlay-get x 'org-indent)
				(if (> (setq e (org-overlay-end x)) (point))
				    (org-move-overlay x (1- e) e)
				  (org-delete-overlay x)
				  (setq org-indent-overlays
					(delq x org-indent-overlays)))))
		(overlays-at (max (point-min) (1- (point)))))
	  (cond
	   ((looking-at outline-regexp)
	    (setq n (1+ (funcall outline-level))))
	   (n
	    (setq ov (org-make-overlay (1- (point)) (point)))
	    (org-overlay-put ov 'after-string (aref org-indent-strings n))
	    (org-overlay-put ov 'evaporate t)
	    (org-overlay-put ov 'rear-sticky nil)
	    (org-overlay-put ov 'front-sticky nil)
	    (org-overlay-put ov 'org-indent n)
	    (push ov org-indent-overlays)))
	  (beginning-of-line 2))))))
  
(defun org-remove-indent-overlays ()
  (interactive)
  (mapc 'org-delete-overlay org-indent-overlays)
  (setq org-indent-overlays nil))

(defun org-indent-after-change-function (start end length)
  "After change function for org-indent.
Notices when an insert has added some lines and adjusts
the line number extents accordingly."
  (if (= start end)
      () ;; this is a deletion
    (org-indent-refresh start end)))

(add-hook 'after-change-functions 'org-indent-after-change-function)

(defun org-fixup-indentation (diff)
  ""
  (save-excursion
    (let ((end (save-excursion (outline-next-heading)
			       (point-marker)))
	  (prohibit (if (> diff 0)
			"^\\S-"
		      (concat "^ \\{0," (int-to-string (- diff)) "\\}\\S-")))
	  col)
      (if (eq org-adapt-indentation 'virtual)
	  (org-indent-refresh (point) end)
	(unless (save-excursion (end-of-line 1)
				(re-search-forward prohibit end t))
	  (while (and (< (point) end)
		      (re-search-forward "^[ \t]+" end t))
	    (goto-char (match-end 0))
	    (setq col (current-column))
	    (if (< diff 0) (replace-match ""))
	    (indent-to (+ diff col))))
	(move-marker end nil)))))
  
(setq org-adapt-indentation 'virtual)