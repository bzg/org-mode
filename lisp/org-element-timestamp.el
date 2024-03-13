;;; org-element-timestamp.el --- Tools to work with timestamp objects         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
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
;;
;; This library contains tools to operate on, or create, timestamp
;; objects, as returned by, e.g. `org-element-context'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)

(defcustom org-display-custom-times nil
  "Non-nil means overlay custom formats over all time stamps.
The formats are defined through the variable `org-timestamp-custom-formats'.
To turn this on on a per-file basis, insert anywhere in the file:
   #+STARTUP: customtime"
  :group 'org-time
  :type 'sexp)
(make-variable-buffer-local 'org-display-custom-times)

(defvaralias 'org-time-stamp-custom-formats 'org-timestamp-custom-formats)
(defcustom org-timestamp-custom-formats
  '("%m/%d/%y %a" . "%m/%d/%y %a %H:%M") ; american
  "Custom formats for time stamps.

See `format-time-string' for the syntax.

These are overlaid over the default ISO format if the variable
`org-display-custom-times' is set.  Time like %H:%M should be at the
end of the second format.  The custom formats are also honored by export
commands, if custom time display is turned on at the time of export.

This variable also affects how timestamps are exported when
`org-display-custom-times' is set.

Leading \"<\" or \"[\" and trailing \">\" or \"]\" pair will be
stripped from the format strings in Emacs buffers.  The brackets
will be preserved on export."
  :group 'org-time
  :package-version '(Org . "9.8")
  :type '(cons string string)
  :safe #'consp)

;; The weekday name "%a" is considered semi-optional in these formats,
;; see https://list.orgmode.org/87fricxatw.fsf@localhost/.  It is
;; "optional" because the `org-timestamp-*' functions work alright on
;; weekday-less timestamps in paragraphs when one omits the "%a".  But
;; it is only "semi"-optional since Org cannot process properly
;; timestamps in CLOCK, DEADLINE, and SCHEDULED lines when one omits
;; the "%a".
(defvaralias 'org-time-stamp-formats 'org-timestamp-formats)
(defconst org-timestamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M")
  "Formats for `format-time-string' which are used for time stamps.

The value is a cons cell containing two strings.  The `car' and `cdr'
of the cons cell are used to format time stamps that do not and do
contain time, respectively.

Leading \"<\"/\"[\" and trailing \">\"/\"]\" pair will be stripped
from the format strings.

Also, see `org-time-stamp-format'.")

(defun org-timestamp-from-string (s)
  "Convert Org timestamp S, as a string, into a timestamp object.
Return nil if S is not a valid timestamp string."
  (when (org-string-nw-p s)
    (with-temp-buffer
      (save-excursion (insert s))
      (org-element-timestamp-parser))))

(defun org-timestamp-from-time (time &optional with-time inactive)
  "Convert a time value into a timestamp object.

TIME is an Emacs internal time representation, as returned, e.g.,
by `current-time'.

When optional argument WITH-TIME is non-nil, return a timestamp
object with a time part, i.e., with hours and minutes.

Return an inactive timestamp if INACTIVE is non-nil.  Otherwise,
return an active timestamp."
  (pcase-let ((`(,_ ,minute ,hour ,day ,month ,year . ,_) (decode-time time)))
    (org-element-create 'timestamp
			(list :type (if inactive 'inactive 'active)
			      :year-start year
			      :month-start month
			      :day-start day
			      :hour-start (and with-time hour)
			      :minute-start (and with-time minute)))))

(defun org-timestamp-to-time (timestamp &optional end)
  "Convert TIMESTAMP object into an Emacs internal time value.
Use end of date range or time range when END is non-nil.
Otherwise, use its start."
  (org-encode-time
   (append '(0)
           (mapcar
            (lambda (prop) (or (org-element-property prop timestamp) 0))
            (if end '(:minute-end :hour-end :day-end :month-end :year-end)
              '(:minute-start :hour-start :day-start :month-start
                              :year-start)))
           '(nil -1 nil))))

(defun org-timestamp-has-time-p (timestamp)
  "Non-nil when TIMESTAMP has a time specified."
  (org-element-property :hour-start timestamp))

(defun org-format-timestamp (timestamp format &optional end utc)
  "Format a TIMESTAMP object into a string.

FORMAT is a format specifier to be passed to
`format-time-string'.

When optional argument END is non-nil, use end of date-range or
time-range, if possible.

When optional argument UTC is non-nil, time is expressed as
Universal Time."
  (format-time-string format (org-timestamp-to-time timestamp end)
		      (and utc t)))

(defun org-timestamp-split-range (timestamp &optional end)
  "Extract a TIMESTAMP object from a date or time range.

END, when non-nil, means extract the end of the range.
Otherwise, extract its start.

Return a new timestamp object."
  (let ((type (org-element-property :type timestamp)))
    (if (memq type '(active inactive diary)) timestamp
      (let ((split-ts (org-element-copy timestamp)))
	;; Set new type.
	(org-element-put-property
	 split-ts :type (if (eq type 'active-range) 'active 'inactive))
        (org-element-put-property split-ts :range-type nil)
	;; Copy start properties over end properties if END is
	;; non-nil.  Otherwise, copy end properties over `start' ones.
	(let ((p-alist '((:minute-start . :minute-end)
			 (:hour-start . :hour-end)
			 (:day-start . :day-end)
			 (:month-start . :month-end)
			 (:year-start . :year-end))))
	  (dolist (p-cell p-alist)
	    (org-element-put-property
	     split-ts
	     (funcall (if end #'car #'cdr) p-cell)
	     (org-element-property
	      (funcall (if end #'cdr #'car) p-cell) split-ts)))
	  ;; Eventually refresh `:raw-value'.
	  (org-element-put-property split-ts :raw-value nil)
	  (org-element-put-property
	   split-ts :raw-value (org-element-interpret-data split-ts)))))))

(defun org-time-stamp-format (&optional with-time inactive custom)
  "Get timestamp format for a time string.

The format is based on `org-timestamp-formats' (if CUSTOM is nil) or
`org-timestamp-custom-formats' (if CUSTOM is non-nil).

When optional argument WITH-TIME is non-nil, the timestamp will contain
time.

When optional argument INACTIVE is nil, format active timestamp.
When `no-brackets', strip timestamp brackets.
When `keep-format', keep as is.
Otherwise, format inactive timestamp."
  (let ((format (funcall
                 (if with-time #'cdr #'car)
                 (if custom
                     org-timestamp-custom-formats
                   org-timestamp-formats))))
    (unless (eq inactive 'keep-format)
      ;; Strip brackets, if any.
      (when (or (and (string-prefix-p "<" format)
                     (string-suffix-p ">" format))
                (and (string-prefix-p "[" format)
                     (string-suffix-p "]" format)))
        (setq format (substring format 1 -1))))
    (pcase inactive
      (`no-brackets format)
      (`keep-format format)
      (`nil (concat "<" format ">"))
      (_ (concat "[" format "]")))))

(defun org-timestamp-translate (timestamp &optional boundary)
  "Translate TIMESTAMP object to custom format.

Format string is defined in `org-timestamp-custom-formats',
which see.

When optional argument BOUNDARY is non-nil, it is either the
symbol `start' or `end'.  In this case, only translate the
starting or ending part of TIMESTAMP if it is a date or time
range.  Otherwise, translate both parts.

Return timestamp as-is if `org-display-custom-times' is nil or if
it has a `diary' type."
  (let ((type (org-element-property :type timestamp)))
    (if (or (not org-display-custom-times) (eq type 'diary))
	(org-element-interpret-data timestamp)
      (let ((fmt (org-time-stamp-format
                  (org-timestamp-has-time-p timestamp) 'keep-format 'custom)))
	(if (and (not boundary) (memq type '(active-range inactive-range)))
	    (concat (org-format-timestamp timestamp fmt)
		    "--"
		    (org-format-timestamp timestamp fmt t))
	  (org-format-timestamp timestamp fmt (eq boundary 'end)))))))


(provide 'org-element-timestamp)

;;; org-element-timestamp.el ends here


