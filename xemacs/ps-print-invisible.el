;;; ps-print-invisible.el - addon to ps-print package that deals
;;  with invisible text printing in xemacs

;; Author: Greg Chernov
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; Put ps-print-invisible.el on your load path.
;; (require 'ps-print-invisible)
;; ps-print-buffer-with-faces will not print invisible parts of the buffer.
;; Work with invisible extents/text properties only 
;; (xemacs hideshow and noutline packages). 

(defun ps-generate-postscript-with-faces (from to)
  ;; Some initialization...
  (setq ps-current-effect 0)

  ;; Build the reference lists of faces if necessary.
  (when (or ps-always-build-face-reference
	    ps-build-face-reference)
    (message "Collecting face information...")
    (ps-build-reference-face-lists))

  ;; Black/white printer.
  (setq ps-black-white-faces-alist nil)
  (and (eq ps-print-color-p 'black-white)
       (ps-extend-face-list ps-black-white-faces nil
			    'ps-black-white-faces-alist))

  ;; Generate some PostScript.
  (save-restriction
    (narrow-to-region from to)
    (ps-print-ensure-fontified from to)
    (let ((face 'default)
	  (position to))
      (cond
       ((memq ps-print-emacs-type '(xemacs lucid))
       ;; Build the list of extents...
       ;;(debug)
	(let ((a (cons 'dummy nil))
	      record type extent extent-list
	      (list-invisible (ps-print-find-invisible-xmas from to)))
	  (ps-x-map-extents 'ps-mapper nil from to a)
	  (setq a (sort (cdr a) 'car-less-than-car)
		extent-list nil)
	  
	  ;; Loop through the extents...
	  (while a
	    (setq record (car a)
		  position (car record)
		  
		  record (cdr record)
		  type (car record)
		  
		  record (cdr record)
		  extent (car record))
	    
	    ;; Plot up to this record.
	    ;; XEmacs 19.12: for some reason, we're getting into a
	    ;; situation in which some of the records have
	    ;; positions less than 'from'.  Since we've narrowed
	    ;; the buffer, this'll generate errors.  This is a hack,
	    ;; but don't call ps-plot-with-face unless from > point-min.
	    (and (>= from (point-min))
		 (ps-plot-with-face from (min position (point-max)) face))
	    
	    (cond
	     ((eq type 'push)
	      (and (or (ps-x-extent-face extent)
		       (extent-property extent 'invisible))
		   (setq extent-list (sort (cons extent extent-list)
					   'ps-extent-sorter))))
	     
	     ((eq type 'pull)
	      (setq extent-list (sort (delq extent extent-list)
				      'ps-extent-sorter))))
	    
	    
	    (setq face (if extent-list
			   (let ((prop (extent-property (car extent-list) 'invisible)))
			     (if (or (and (eq buffer-invisibility-spec t)
					  (not (null prop)))
				     (and (consp buffer-invisibility-spec)
					  (or (memq prop buffer-invisibility-spec)
					      (assq prop buffer-invisibility-spec))))
				 'emacs--invisible--face
			       (ps-x-extent-face (car extent-list))))
			 'default)
		  from position
		  a (cdr a)))))

       ((eq ps-print-emacs-type 'emacs)
	(let ((property-change from)
	      (overlay-change from)
	      (save-buffer-invisibility-spec buffer-invisibility-spec)
	      (buffer-invisibility-spec nil)
	      before-string after-string)
	  (while (< from to)
	    (and (< property-change to)	; Don't search for property change
					; unless previous search succeeded.
		 (setq property-change (next-property-change from nil to)))
	    (and (< overlay-change to)	; Don't search for overlay change
					; unless previous search succeeded.
		 (setq overlay-change (min (ps-e-next-overlay-change from)
					   to)))
	    (setq position (min property-change overlay-change)
		  before-string nil
		  after-string nil)
	    ;; The code below is not quite correct,
	    ;; because a non-nil overlay invisible property
	    ;; which is inactive according to the current value
	    ;; of buffer-invisibility-spec nonetheless overrides
	    ;; a face text property.
	    (setq face
		  (cond ((let ((prop (get-text-property from 'invisible)))
			   ;; Decide whether this invisible property
			   ;; really makes the text invisible.
			   (if (eq save-buffer-invisibility-spec t)
			       (not (null prop))
			     (or (memq prop save-buffer-invisibility-spec)
				 (assq prop save-buffer-invisibility-spec))))
			 'emacs--invisible--face)
			((get-text-property from 'face))
			(t 'default)))
	    (let ((overlays (ps-e-overlays-at from))
		  (face-priority -1))	; text-property
	      (while (and overlays
			  (not (eq face 'emacs--invisible--face)))
		(let* ((overlay (car overlays))
		       (overlay-invisible
			(ps-e-overlay-get overlay 'invisible))
		       (overlay-priority
			(or (ps-e-overlay-get overlay 'priority) 0)))
		  (and (> overlay-priority face-priority)
		       (setq before-string
			     (or (ps-e-overlay-get overlay 'before-string)
				 before-string)
			     after-string
			     (or (and (<= (ps-e-overlay-end overlay) position)
				      (ps-e-overlay-get overlay 'after-string))
				 after-string)
			     face-priority overlay-priority
			     face
			     (cond
			      ((if (eq save-buffer-invisibility-spec t)
				   (not (null overlay-invisible))
				 (or (memq overlay-invisible
					   save-buffer-invisibility-spec)
				     (assq overlay-invisible
					   save-buffer-invisibility-spec)))
			       'emacs--invisible--face)
			      ((ps-e-overlay-get overlay 'face))
			      (t face)
			      ))))
		(setq overlays (cdr overlays))))
	    ;; Plot up to this record.
	    (and before-string
		 (ps-plot-string before-string))
	    (ps-plot-with-face from position face)
	    (and after-string
		 (ps-plot-string after-string))
	    (setq from position)))))
      (ps-plot-with-face from to face))))


(defun ps-print-find-invisible-xmas (from to)
  (let ((list nil))
    (map-extents '(lambda (ex ignored)
		    (let ((prop (extent-property ex 'invisible)))
		      (if (or (and (eq buffer-invisibility-spec t)
				   (not (null prop)))
			      (or (memq prop buffer-invisibility-spec)
				  (assq prop buffer-invisibility-spec)))
			  (setq list (cons (list 
					      (extent-start-position ex)
					      (extent-end-position ex))
					     list))))
		    nil)
		 (current-buffer)
		 from to nil 'start-and-end-in-region 'invisible)
    (reverse list)))


(defun ps-mapper (extent list)
  ;;(debug)
  (let ((beg (ps-x-extent-start-position extent))
	(end (ps-x-extent-end-position extent))
	(inv-lst list-invisible)
	(found nil))
    (while (and inv-lst
		(not found))
      (let ((inv-beg (caar inv-lst))
	    (inv-end (cadar inv-lst)))
	(if (and (>= beg inv-beg)
		 (<= end inv-end)
		 (not (extent-property extent 'invisible)))
	    (setq found t))
	(setq inv-lst (cdr inv-lst))))
    (if (not found) 
	(nconc list
	       (list (list beg 'push extent)
		     (list end 'pull extent)))))
      nil)


(provide 'ps-print-invisible)


;;; ps-print-invisible.el ends here