;;; Modified picture mode with extra functions and bindings
;; BUGS:  The mouse stuff does not really work reliable

;; What it really needs:
;; Picture areas should always start with ":"
;; Automatic recognize the region and use the right commands, also
;; extending the region.   
 
;; Picture mode
;; ------------
;; Simple ASCII drawings can be made in picture-mode.  You can toggle
;; picture mode with `C-c C-c' (unless you have turned it off with the
;; variable `org-enable-picture-mode').  See the picture-mode
;; documentation for details. Some additional bindings are provided by
;; org-mode:
;;
;;      M-up         M-7 M-8 M-9  \
;; M-left  M-right   M-u     M-o   }  Draw lines in keypad-like directions
;;     M-down        M-j M-k M-o  /
;;
;;    M--              Draw line from mark to point, set mark at end.
;;    S-mouse1         Freehand drawing with the mouse.
;;
(defcustom org-enable-picture-mode t
  "Non-nil means, C-c C-c switches to picture mode.
When nil, this command is disabled."
  :group 'org
  :type 'boolean)
(defun org-edit-picture ()
  "Switch to picture mode and save the value of `transient-mark-mode'.
Turn transient-mark-mode off while in picture-mode."
  (interactive)
  (if (not org-enable-picture-mode)
      (error 
       "Set variable `org-enable-picture-mode' to allow picture-mode."))
  ;; FIXME: This is not XEmacs compatible yet
  (set (make-local-variable 'org-transient-mark-mode)
       transient-mark-mode)
  (set (make-local-variable 'org-cursor-color)
       (frame-parameter nil 'cursor-color))
  (set (make-local-variable 'transient-mark-mode) nil)
  (set-cursor-color "red")
  (picture-mode)
  (message (substitute-command-keys
            "Type \\[org-picture-mode-exit] in this buffer to return it to Org mode.")))

(defun org-picture-mode-exit (&optional arg)
  "Turn off picture mode and restore `transient-mark-mode'."
  (interactive "P")
  (if (local-variable-p 'org-transient-mark-mode)
      (setq transient-mark-mode org-transient-mark-mode))
  (if (local-variable-p 'org-cursor-color)
      (set-cursor-color org-cursor-color))
  (if (fboundp 'deactivate-mark) (deactivate-mark))
  (if (fboundp 'zmacs-deactivate-region) (zmacs-deactivate-region))
  (picture-mode-exit))


(eval-after-load "picture"
  ' (progn
      (define-key picture-mode-map [(meta left)] (lambda (arg) (interactive "p") (org-picture-draw 4 arg)))
      (define-key picture-mode-map [(meta right)] (lambda (arg) (interactive "p") (org-picture-draw 6 arg)))
      (define-key picture-mode-map [(meta up)] (lambda (arg) (interactive "p") (org-picture-draw 8 arg)))
      (define-key picture-mode-map [(meta down)] (lambda (arg) (interactive "p") (org-picture-draw 2 arg)))
      (define-key picture-mode-map [(meta shift left)] (lambda (arg) (interactive "p") (org-picture-draw 7 arg)))
      (define-key picture-mode-map [(meta shift right)] (lambda (arg) (interactive "p") (org-picture-draw 3 arg)))
      (define-key picture-mode-map [(meta shift up)] (lambda (arg) (interactive "p") (org-picture-draw 9 arg)))
      (define-key picture-mode-map [(meta shift down)] (lambda (arg) (interactive "p") (org-picture-draw 1 arg)))

      (define-key picture-mode-map [(meta ?j)] (lambda (arg) (interactive "p") (org-picture-draw 1 arg)))
      (define-key picture-mode-map [(meta ?k)] (lambda (arg) (interactive "p") (org-picture-draw 2 arg)))
      (define-key picture-mode-map [(meta ?l)] (lambda (arg) (interactive "p") (org-picture-draw 3 arg)))
      (define-key picture-mode-map [(meta ?u)] (lambda (arg) (interactive "p") (org-picture-draw 4 arg)))
      (define-key picture-mode-map [(meta ?o)] (lambda (arg) (interactive "p") (org-picture-draw 6 arg)))
      (define-key picture-mode-map [(meta ?7)] (lambda (arg) (interactive "p") (org-picture-draw 7 arg)))
      (define-key picture-mode-map [(meta ?8)] (lambda (arg) (interactive "p") (org-picture-draw 8 arg)))
      (define-key picture-mode-map [(meta ?9)] (lambda (arg) (interactive "p") (org-picture-draw 9 arg)))
      (define-key picture-mode-map [(meta ?-)] 'org-picture-draw-line)
      (define-key picture-mode-map [mouse-2] 'org-picture-mouse-line-to-here)
      (define-key picture-mode-map [mouse-1] 'org-picture-mouse-set-point)
      (define-key picture-mode-map [(shift down-mouse-1)] 'org-picture-draw-with-mouse)
      (define-key picture-mode-map "\C-c\C-c" 'org-picture-mode-exit)))

(defun org-picture-draw (dir arg)
  "Draw ARG character into the direction given by DIR."
  (cond
   ((equal dir 1)
    (picture-movement-sw)
    (setq last-command-event ?/)  (picture-self-insert arg))
   ((equal dir 2)
    (picture-movement-down)
    (setq last-command-event ?|)  (picture-self-insert arg))
   ((equal dir 3)
    (picture-movement-se)
    (setq last-command-event ?\\) (picture-self-insert arg))
   ((equal dir 4)
    (picture-movement-left)
    (setq last-command-event ?-)  (picture-self-insert arg))
   ((equal dir 5))
   ((equal dir 6)
    (picture-movement-right)
    (setq last-command-event ?-)  (picture-self-insert arg))
   ((equal dir 7)
    (picture-movement-nw)
    (setq last-command-event ?\\) (picture-self-insert arg))
   ((equal dir 8)
    (picture-movement-up)
    (setq last-command-event ?|)  (picture-self-insert arg))
   ((equal dir 9)
    (picture-movement-ne)
    (setq last-command-event ?/)  (picture-self-insert arg)))
  (picture-movement-right))

(defun org-picture-draw-line (&optional beg end)
  "Draw a line from mark to point."
  (interactive)
  (unless (and beg end)
    (setq beg (mark 'force)
          end (point)))
  (let (x1 x2 y1 y2 n i Dx Dy dx dy char lp x y x1a y1a lastx lasty)
    (goto-char beg)
    (setq x1 (current-column) y1 (count-lines (point-min) (point)))
    (if (bolp) (setq y1 (1+ y1)))
    (goto-char end)
    (setq x2 (current-column) y2 (count-lines (point-min) (point)))
    (if (bolp) (setq y2 (1+ y2)))
    (setq Dx (- x2 x1) Dy (- y2 y1)
	  n (+ (abs Dx) (abs Dy))
	  n (sqrt (+ (* Dx Dx) (* Dy Dy)))
	  n (max (abs Dx) (abs Dy))
	  n (max (abs Dx) (abs Dy))
	  dx (/ (float Dx) (float n)) dy (/ (float Dy) (float n)))
    (setq x1a (floor (+ x1 (* 1. dx) .5))
          y1a (floor (+ y1 (* 1. dy) .5)))
    ;; Do the loop
    (setq i -1)
    (setq lastx x1a lasty y1a)
    (while (< i n)
      (setq i (1+ i)
	    x (floor (+ x1 (* (float i) dx) .5))
	    y (floor (+ y1 (* (float i) dy) .5)))
      (setq char (cond ((= lastx x) ?|) ((= lasty y) ?-)
                       ((> (* (- x lastx) (- y lasty)) 0) ?\\)
                       (t ?/))
            lastx x lasty y)
      (goto-line y)
      (move-to-column x t)
      (setq last-command-event char)
      (setq lp (point))
      (picture-self-insert 1))
    (goto-char lp)
    (set-mark lp)))

(defun org-picture-mouse-line-to-here (ev)
  "Draw a line from point to the click position."
  (interactive "e")
  (let* ((beg (move-marker (make-marker) (point))))
    (org-picture-mouse-set-point ev)
    (org-picture-draw-line beg (point))
    (move-marker beg nil)))

;; Draw with the mouse
(defun org-picture-mouse-set-point (ev)
  "Mouse-set-point, but force position."
  (interactive "e")
  (let* ((colrow (posn-col-row (event-end ev)))
	 (col (car colrow)) (line (cdr colrow))
	 (realline (1+ (+ (count-lines (point-min) (window-start)) line))))
    (goto-line realline)
    (while (and (eobp)
		(not (> (count-lines (point-min) (point-max)) realline)))
      (newline))
    (goto-line realline)
    (move-to-column col t)))

(defun org-picture-draw-with-mouse (ev)
  "Use the mouse like a brush and paint stars where it goes."
  (interactive "e")
  (let (lastcr cr)
    (track-mouse
      (catch 'exit
	(while t
	  (setq e (read-event))
	  (if (not (eq (car e) 'mouse-movement)) (throw 'exit nil))
	  (setq cr (posn-col-row (event-end e)))
	  (when (not (equal cr lastcr))
	    (setq lastcr cr)
	    (org-picture-mouse-set-point e)
	    (setq last-command-event ?*)
	    (save-excursion
	      (picture-self-insert 1))))))))
