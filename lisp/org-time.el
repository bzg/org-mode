;;; org-time.el --- Org helpers to work with time values                      -*- lexical-binding: t; -*-

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

;; This library provides additional tooling to work with Emacs time
;; objects and their string representation.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'calendar)

(defvar org-time-stamp-rounding-minutes)
(declare-function org-clock-get-last-clock-out-time "org-clock" ())

(defcustom org-extend-today-until 0
  "The hour when your day really ends.  Must be an integer.
This has influence for the following applications:
- When switching the agenda to \"today\".  If it is still earlier than
  the time given here, the day recognized as TODAY is actually yesterday.
- When a date is read from the user and it is still before the time given
  here, the current date and time will be assumed to be yesterday, 23:59.
  Also, timestamps inserted in capture templates follow this rule.

IMPORTANT:  This is a feature whose implementation is and likely will
remain incomplete.  Really, it is only here because past midnight seems to
be the favorite working time of John Wiegley :-)"
  :group 'org-time
  :type 'integer)

(defcustom org-use-effective-time nil
  "If non-nil, consider `org-extend-today-until' when creating timestamps.
For example, if `org-extend-today-until' is 8, and it's 4am, then the
\"effective time\" of any timestamps between midnight and 8am will be
23:59 of the previous day."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-use-last-clock-out-time-as-effective-time nil
  "When non-nil, use the last clock out time for `org-todo'.
Note that this option has precedence over the combined use of
`org-use-effective-time' and `org-extend-today-until'."
  :group 'org-time
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defun org-make-tdiff-string (y d h m)
  (let ((fmt "")
	(l nil))
    (when (> y 0)
      (setq fmt (concat fmt "%d year" (if (> y 1) "s" "") " "))
      (push y l))
    (when (> d 0)
      (setq fmt (concat fmt "%d day"  (if (> d 1) "s" "") " "))
      (push d l))
    (when (> h 0)
      (setq fmt (concat fmt "%d hour" (if (> h 1) "s" "") " "))
      (push h l))
    (when (> m 0)
      (setq fmt (concat fmt "%d minute" (if (> m 1) "s" "") " "))
      (push m l))
    (apply #'format fmt (nreverse l))))

(defun org-time-string-to-time (s)
  "Convert timestamp string S into internal time."
  (org-encode-time (org-parse-time-string s)))

(defun org-time-string-to-seconds (s)
  "Convert a timestamp string S into a number of seconds."
  (float-time (org-time-string-to-time s)))

(defun org-current-time (&optional rounding-minutes past)
  "Current time, possibly rounded to ROUNDING-MINUTES.
When ROUNDING-MINUTES is not an integer, fall back on the car of
`org-time-stamp-rounding-minutes'.  When PAST is non-nil, ensure
the rounding returns a past time."
  (let ((r (or (and (integerp rounding-minutes) rounding-minutes)
	       (car org-time-stamp-rounding-minutes)))
	(now (current-time)))
    (if (< r 1)
	now
      (let* ((time (decode-time now))
	     (res (org-encode-time
                   (apply #'list
                          0 (* r (round (decoded-time-minute time) r))
                          (nthcdr 2 time)))))
	(if (or (not past) (time-less-p res now))
	    res
	  (time-subtract res (* r 60)))))))

(defun org-current-effective-time ()
  "Return current time adjusted for `org-extend-today-until' variable."
  (let* ((ct (org-current-time))
	 (dct (decode-time ct))
	 (ct1
	  (cond
	   (org-use-last-clock-out-time-as-effective-time
	    (or (org-clock-get-last-clock-out-time) ct))
	   ((and org-use-effective-time (< (decoded-time-hour dct) org-extend-today-until))
	    (org-encode-time 0 59 23 (1- (decoded-time-day dct)) (decoded-time-month dct) (decoded-time-year dct)))
	   (t ct))))
    ct1))

(defun org-today ()
  "Return today date, considering `org-extend-today-until'."
  (time-to-days
   (time-since (* 3600 org-extend-today-until))))

(defvar org--diary-sexp-entry-cache (make-hash-table :test #'equal)
  "Hash table holding return values of `org-diary-sexp-entry'.")
(defun org-diary-sexp-entry (sexp entry d)
  "Process a SEXP diary ENTRY for date D."
  (require 'diary-lib)
  ;; `org-anniversary' and alike expect ENTRY and DATE to be bound
  ;; dynamically.
  (let ((cached (gethash (list sexp entry d) org--diary-sexp-entry-cache 'none)))
    (if (not (eq 'none cached)) cached
      (puthash (list sexp entry d)
               (let* ((sexp `(let ((entry ,entry)
		                   (date ',d))
		               ,(car (read-from-string sexp))))
                      ;; FIXME: Do not use (eval ... t) in the following sexp as
                      ;; diary vars are still using dynamic scope.
	              (result (if calendar-debug-sexp (eval sexp)
		                (condition-case nil
		                    (eval sexp)
		                  (error
		                   (beep)
		                   (message "Bad sexp at line %d in %s: %s"
			                    (org-current-line)
			                    (buffer-file-name) sexp)
		                   (sleep-for 2))))))
                 (cond ((stringp result) (split-string result "; "))
	               ((and (consp result)
		             (not (consp (cdr result)))
		             (stringp (cdr result)))
	                (cdr result))
	               ((and (consp result)
		             (stringp (car result)))
	                result)
	               (result entry)))
               org--diary-sexp-entry-cache))))

(define-error 'org-diary-sexp-no-match "Unable to match diary sexp")

(defun org-time-string-to-absolute (s &optional daynr prefer buffer pos)
  "Convert time stamp S to an absolute day number.

If DAYNR in non-nil, and there is a specifier for a cyclic time
stamp, get the closest date to DAYNR.  If PREFER is
`past' (respectively `future') return a date past (respectively
after) or equal to DAYNR.

POS is the location of time stamp S, as a buffer position in
BUFFER.

Diary sexp timestamps are matched against DAYNR, when non-nil.
If matching fails or DAYNR is nil, `org-diary-sexp-no-match' is
signaled."
  (cond
   ((string-match "\\`%%\\((.*)\\)" s)
    ;; Sexp timestamp: try to match DAYNR, if available, since we're
    ;; only able to match individual dates.  If it fails, raise an
    ;; error.
    (if (and daynr
	     (org-diary-sexp-entry
	      (match-string 1 s) "" (calendar-gregorian-from-absolute daynr)))
	daynr
      (signal 'org-diary-sexp-no-match (list s))))
   (daynr (org-closest-date s daynr prefer))
   (t (time-to-days
       (condition-case errdata
	   (org-time-string-to-time s)
	 (error (error "Bad timestamp `%s'%s\nError was: %s"
		       s
		       (if (not (and buffer pos)) ""
			 (format-message " at %d in buffer `%s'" pos buffer))
		       (cdr errdata))))))))

(defun org-days-to-iso-week (days)
  "Return the ISO week number."
  (require 'cal-iso)
  (car (calendar-iso-from-absolute days)))

(defun org-small-year-to-year (year)
  "Convert 2-digit years into 4-digit years.
YEAR is expanded into one of the 30 next years, if possible, or
into a past one.  Any year larger than 99 is returned unchanged."
  (if (>= year 100) year
    (let* ((current (string-to-number (format-time-string "%Y")))
	   (century (/ current 100))
	   (offset (- year (% current 100))))
      (cond ((> offset 30) (+ (* (1- century) 100) year))
	    ((> offset -70) (+ (* century 100) year))
	    (t (+ (* (1+ century) 100) year))))))

(defun org-time-from-absolute (d)
  "Return the time corresponding to date D.
D may be an absolute day number, or a calendar-type list (month day year)."
  (when (numberp d) (setq d (calendar-gregorian-from-absolute d)))
  (org-encode-time
   0 0 0
   (calendar-extract-day d)
   (calendar-extract-month d)
   (calendar-extract-year d)))

(defun org-closest-date (start current prefer)
  "Return closest date to CURRENT starting from START.

CURRENT and START are both time stamps.

When PREFER is `past', return a date that is either CURRENT or
past.  When PREFER is `future', return a date that is either
CURRENT or future.

Only time stamps with a repeater are modified.  Any other time
stamp stay unchanged.  In any case, return value is an absolute
day number."
  (if (not (string-match "\\+\\([0-9]+\\)\\([hdwmy]\\)" start))
      ;; No repeater.  Do not shift time stamp.
      (time-to-days (org-time-string-to-time start))
    (let ((value (string-to-number (match-string 1 start)))
	  (type (match-string 2 start)))
      (if (= 0 value)
	  ;; Repeater with a 0-value is considered as void.
	  (time-to-days (org-time-string-to-time start))
	(let* ((base (org-date-to-gregorian start))
	       (target (org-date-to-gregorian current))
	       (sday (calendar-absolute-from-gregorian base))
	       (cday (calendar-absolute-from-gregorian target))
	       n1 n2)
	  ;; If START is already past CURRENT, just return START.
	  (if (<= cday sday) sday
	    ;; Compute closest date before (N1) and closest date past
	    ;; (N2) CURRENT.
	    (pcase type
	      ("h"
	       (let ((missing-hours
		      (mod (+ (- (* 24 (- cday sday))
				 (decoded-time-hour (org-parse-time-string start)))
			      org-extend-today-until)
			   value)))
		 (setf n1 (if (= missing-hours 0) cday
			    (- cday (1+ (/ missing-hours 24)))))
		 (setf n2 (+ cday (/ (- value missing-hours) 24)))))
	      ((or "d" "w")
	       (let ((value (if (equal type "w") (* 7 value) value)))
		 (setf n1 (+ sday (* value (/ (- cday sday) value))))
		 (setf n2 (+ n1 value))))
	      ("m"
	       (let* ((add-months
		       (lambda (d n)
			 ;; Add N months to gregorian date D, i.e.,
			 ;; a list (MONTH DAY YEAR).  Return a valid
			 ;; gregorian date.
			 (let ((m (+ (calendar-extract-month d) n)))
			   (list (mod m 12)
				 (calendar-extract-day d)
				 (+ (/ m 12) (calendar-extract-year d))))))
		      (months		; Complete months to TARGET.
		       (* (/ (+ (* 12 (- (calendar-extract-year target) (calendar-extract-year base)))
				(- (calendar-extract-month target) (calendar-extract-month base))
				;; If START's day is greater than
				;; TARGET's, remove incomplete month.
				(if (> (calendar-extract-day target) (calendar-extract-day base)) 0 -1))
			     value)
			  value))
		      (before (funcall add-months base months)))
		 (setf n1 (calendar-absolute-from-gregorian before))
		 (setf n2
		       (calendar-absolute-from-gregorian
			(funcall add-months before value)))))
	      (_
	       (let* ((d (calendar-extract-day   base))
		      (m (calendar-extract-month base))
		      (y (calendar-extract-year  base))
		      (years		; Complete years to TARGET.
		       (* (/ (- (calendar-extract-year target)
				y
				;; If START's month and day are
				;; greater than TARGET's, remove
				;; incomplete year.
				(if (or (> (calendar-extract-month target) m)
					(and (= (calendar-extract-month target) m)
					     (> (calendar-extract-day target) d)))
				    0
				  1))
			     value)
			  value))
		      (before (list m d (+ y years))))
		 (setf n1 (calendar-absolute-from-gregorian before))
		 (setf n2 (calendar-absolute-from-gregorian
			   (list m d (+ (calendar-extract-year before) value)))))))
	    ;; Handle PREFER parameter, if any.
	    (cond
	     ((eq prefer 'past)   (if (= cday n2) n2 n1))
	     ((eq prefer 'future) (if (= cday n1) n1 n2))
	     (t (if (> (abs (- cday n1)) (abs (- cday n2))) n2 n1)))))))))

(defun org-date-to-gregorian (d)
  "Turn any specification of date D into a Gregorian date for the calendar."
  (cond ((integerp d) (calendar-gregorian-from-absolute d))
	((and (listp d) (= (length d) 3)) d)
	((stringp d)
	 (let ((d (org-parse-time-string d)))
	   (list (decoded-time-month d) (decoded-time-day d) (decoded-time-year d))))
	((listp d) (list (decoded-time-month d) (decoded-time-day d) (decoded-time-year d)))))

(provide 'org-time)

;;; org-time.el ends here
