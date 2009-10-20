;;; org-habit.el --- The habit tracking code for Org-mode

;; Copyright (C) 2009
;;   Free Software Foundation, Inc.

;; Author: John Wiegley <johnw at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.31trans
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the habit tracking code for Org-mode

(require 'org)
(require 'org-agenda)
(eval-when-compile
  (require 'cl)
  (require 'calendar))

(defgroup org-habit nil
  "Options concerning habit tracking in Org-mode."
  :tag "Org Habit"
  :group 'org-progress)

(defcustom org-habit-graph-column 40
  "The absolute column at which to insert habit consistency graphs.
Note that consistency graphs will overwrite anything else in the buffer."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-preceding-days 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-show-habits t
  "If non-nil, show habits in agenda buffers."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-habits-only-for-today t
  "If non-nil, only show habits on today's agenda, and not for future days.
Note that even when shown for future days, the graph is always
relative to the current effective time."
  :group 'org-habit
  :type 'boolean)

(defface org-habit-clear-face
  '((((background light)) (:background "slateblue"))
    (((background dark)) (:background "blue")))
  "Face for days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-clear-future-face
  '((((background light)) (:background "powderblue"))
    (((background dark)) (:background "midnightblue")))
  "Face for future days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-face
  '((((background light)) (:background "green"))
    (((background dark)) (:background "forestgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-ready-future-face
  '((((background light)) (:background "palegreen"))
    (((background dark)) (:background "darkgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-warning-face
  '((((background light)) (:background "yellow"))
    (((background dark)) (:background "gold")))
  "Face for days on which a task ought to be done."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-warning-future-face
  '((((background light)) (:background "palegoldenrod"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for days on which a task ought be done."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-face
  '((((background light)) (:background "yellow"))
    (((background dark)) (:background "gold")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-alert-future-face
  '((((background light)) (:background "palegoldenrod"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-face
  '((((background light)) (:background "red"))
    (((background dark)) (:background "firebrick")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-overdue-future-face
  '((((background light)) (:background "mistyrose"))
    (((background dark)) (:background "darkred")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)

(defun org-habit-duration-to-days (ts)
  (if (string-match "\\([0-9]+\\)\\([dwmy]\\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25))))))
    (error "Invalid duration string: %s" ts)))

(defun org-is-habit-p (&optional pom)
  (string= "habit" (org-entry-get (or pom (point)) "STYLE")))

(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
	   (scheduled-repeat (org-get-repeat "SCHEDULED"))
	   (sr-days (org-habit-duration-to-days scheduled-repeat))
	   (end (org-entry-end-position))
	   closed-dates deadline dr-days)
      (unless scheduled
	(error "Habit has no scheduled date"))
      (unless scheduled-repeat
	(error "Habit has no scheduled repeat period"))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	(setq dr-days (org-habit-duration-to-days
		       (match-string-no-properties 1 scheduled-repeat)))
	(if (<= dr-days sr-days)
	    (error "Habit's deadline repeat period is less than or equal to scheduled"))
	(setq deadline (time-add scheduled
				 (days-to-time (- dr-days sr-days)))))
      (org-back-to-heading t)
      (while (re-search-forward "- State \"DONE\".*\\[\\([^]]+\\)\\]" end t)
	(push (org-time-string-to-time (match-string-no-properties 1))
	      closed-dates))
      (list scheduled sr-days deadline dr-days closed-dates))))

(defsubst org-habit-scheduled (habit)
  (nth 0 habit))
(defsubst org-habit-scheduled-repeat (habit)
  (nth 1 habit))
(defsubst org-habit-deadline (habit)
  (nth 2 habit))
(defsubst org-habit-deadline-repeat (habit)
  (nth 3 habit))
(defsubst org-habit-done-dates (habit)
  (nth 4 habit))

(defun org-habit-get-faces (habit &optional moment scheduled-time donep)
  "Return faces for HABIT relative to MOMENT and SCHEDULED-TIME.
MOMENT defaults to the current time if it is nil.
SCHEDULED-TIME defaults to the habit's actual scheduled time if nil.

Habits are assigned colors on the following basis:
  Blue      Task is before the scheduled date.
  Green     Task is on or after scheduled date, but before the
            end of the schedule's repeat period.
  Yellow    If the task has a deadline, then it is after schedule's
            repeat period, but before the deadline.
  Orange    The task has reached the deadline day, or if there is
            no deadline, the end of the schedule's repeat period.
  Red       The task has gone beyond the deadline day or the
            schedule's repeat period."
  (unless moment (setq moment (current-time)))
  (let* ((scheduled (or scheduled-time (org-habit-scheduled habit)))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (scheduled-end (time-add scheduled (days-to-time s-repeat)))
	 (d-repeat (org-habit-deadline-repeat habit))
	 (deadline (if (and scheduled-time d-repeat)
		       (time-add scheduled-time
				 (days-to-time (- d-repeat s-repeat)))
		     (org-habit-deadline habit))))
    (cond
     ((time-less-p moment scheduled)
      '(org-habit-clear-face . org-habit-clear-future-face))
     ((time-less-p moment scheduled-end)
      '(org-habit-ready-face . org-habit-ready-future-face))
     ((and deadline
	   (time-less-p moment deadline))
      (if donep
	  '(org-habit-ready-face . org-habit-ready-future-face)
	'(org-habit-warning-face . org-habit-warning-future-face)))
     ((= (time-to-days moment)
	 (if deadline
	     (time-to-days deadline)
	   (time-to-days scheduled-end)))
      (if donep
	  '(org-habit-ready-face . org-habit-ready-future-face)
	'(org-habit-alert-face . org-habit-alert-future-face)))
     (t
      '(org-habit-overdue-face . org-habit-overdue-future-face)))))

(defun org-habit-build-graph (habit &optional starting current ending)
  "Build a color graph for the given HABIT, from STARTING to ENDING."
  (let ((done-dates (sort (org-habit-done-dates habit) 'time-less-p))
	(scheduled (org-habit-scheduled habit))
	(s-repeat (org-habit-scheduled-repeat habit))
	(day starting)
	(current-days (time-to-days current))
	last-done-date
	(graph (make-string (1+ (- (time-to-days ending)
				   (time-to-days starting))) ?\ ))
	(index 0))
    (if done-dates
	(while (time-less-p (car done-dates) starting)
	  (setq last-done-date (car done-dates)
		done-dates (cdr done-dates))))
    (while (time-less-p day ending)
      (let* ((now-days (time-to-days day))
	     (in-the-past-p (< now-days current-days))
	     (todayp (= now-days current-days))
	     (donep (and done-dates
			 (= now-days (time-to-days (car done-dates)))))
	     (faces (if (and in-the-past-p
			     (not last-done-date)
			     (not (time-less-p scheduled current)))
			'(org-habit-clear-face . org-habit-clear-future-face)
		      (org-habit-get-faces
		       habit day (and in-the-past-p
				      (if last-done-date
					  (time-add last-done-date
						    (days-to-time s-repeat))
					scheduled))
		       donep)))
	     markedp face)
	(if donep
	    (progn
	      (aset graph index ?*)
	      (setq last-done-date (car done-dates)
		    done-dates (cdr done-dates)
		    markedp t))
	  (if todayp
	      (aset graph index ?!)))
	(setq face (if (or in-the-past-p
			   todayp)
		       (car faces)
		     (cdr faces)))
	(if (and in-the-past-p
		 (not (eq face 'org-habit-overdue-face))
		 (not markedp))
	    (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph))
      (setq day (time-add day (days-to-time 1))
	    index (1+ index)))
    graph))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t) l c
	(moment (time-subtract (current-time)
			       (list 0 (* 3600 org-extend-today-until) 0))))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			      (- (line-end-position) (point))))
	    (insert (org-habit-build-graph
		     habit
		     (time-subtract moment
				    (days-to-time org-habit-preceding-days))
		     moment
		     (time-add moment
			       (days-to-time org-habit-following-days))))))
	(forward-line)))))

(defun org-habit-toggle-habits ()
  "Toggle display of habits in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Habits turned %s"
	   (if org-habit-show-habits "on" "off")))

(org-defkey org-agenda-mode-map "K" 'org-habit-toggle-habits)

(provide 'org-habit)

;; arch-tag: 

;;; org-habit.el ends here
