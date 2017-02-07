;;; test-org-clock.el --- Tests for org-clock.el

;; Copyright (C) 2012, 2014, 2015  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments



;;; Code:

(defun org-test-clock-create-timestamp (input &optional inactive with-time)
  "Create a timestamp out of a date/time prompt string.

INPUT is a string as expected in a date/time prompt, i.e \"+2d\"
or \"2/5\".

When optional argument INACTIVE is non-nil, return an inactive
timestamp.  When optional argument WITH-TIME is non-nil, also
insert hours and minutes.

Return the timestamp as a string."
  (org-element-interpret-data
   (let ((time (decode-time
                (apply #'encode-time
                       (mapcar (lambda (el) (or el 0))
                               (org-read-date-analyze
                                input nil (decode-time (current-time))))))))
     (list 'timestamp
           (list :type (if inactive 'inactive 'active)
                 :minute-start (and with-time (nth 1 time))
                 :hour-start (and with-time (nth 2 time))
                 :day-start (nth 3 time)
                 :month-start (nth 4 time)
                 :year-start (nth 5 time))))))

(defun org-test-clock-create-clock (input1 &optional input2)
  "Create a clock line out of two date/time prompts.

INPUT1 and INPUT2 are strings as expected in a date/time prompt,
i.e \"+2d\" or \"2/5\".  They respectively refer to start and end
range.  INPUT2 can be omitted if clock hasn't finished yet.

Return the clock line as a string."
  (let* ((beg (org-test-clock-create-timestamp input1 t t))
	 (end (and input2 (org-test-clock-create-timestamp input2 t t)))
	 (sec-diff (and input2 (floor (- (org-time-string-to-seconds end)
					 (org-time-string-to-seconds beg))))))
    (concat org-clock-string " " beg
	    (when end
	      (concat "--" end " => "
		      (format "%2d:%02d"
			      (/ sec-diff 3600)
			      (/ (mod sec-diff 3600) 60))))
	    "\n")))

(defun test-org-clock-clocktable-contents-at-point (options)
  "Return contents of a clocktable at point.
OPTIONS is a string of clocktable options.  Caption is ignored in
contents.  The clocktable doesn't appear in the buffer."
  (save-excursion
    (insert "#+BEGIN: clocktable " options "\n")
    (insert "#+END:\n"))
  (unwind-protect
      (save-excursion
	(let ((org-time-clocksum-format
	       '(:hours "%d" :require-hours t :minutes ":%02d"
			:require-minutes t)))
	  (org-update-dblock))
	(forward-line)
	;; Skip caption.
	(when (looking-at "#\\+CAPTION:") (forward-line))
	(buffer-substring (point)
			  (progn (search-forward "#+END:")
				 (match-beginning 0))))
    ;; Remove clocktable.
    (delete-region (point) (search-forward "#+END:\n"))))


;;; Clock drawer

(ert-deftest test-org-clock/into-drawer ()
  "Test `org-clock-into-drawer' specifications."
  ;; When `org-clock-into-drawer' is nil, do not use a clock drawer.
  (should-not
   (org-test-with-temp-text "* H"
     (let ((org-clock-into-drawer nil)
	   (org-log-into-drawer nil))
       (org-clock-into-drawer))))
  (should-not
   (org-test-with-temp-text "* H"
     (let ((org-clock-into-drawer nil)
	   (org-log-into-drawer t))
       (org-clock-into-drawer))))
  (should-not
   (org-test-with-temp-text "* H"
     (let ((org-clock-into-drawer nil)
	   (org-log-into-drawer "BAR"))
       (org-clock-into-drawer))))
  ;; When `org-clock-into-drawer' is a string, use it
  ;; unconditionally.
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer "FOO")
		  (org-log-into-drawer nil))
	      (org-clock-into-drawer)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer "FOO")
		  (org-log-into-drawer t))
	      (org-clock-into-drawer)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer "FOO")
		  (org-log-into-drawer "BAR"))
	      (org-clock-into-drawer)))))
  ;; When `org-clock-into-drawer' is an integer, return it.
  (should
   (= 1
      (org-test-with-temp-text "* H"
	(let ((org-clock-into-drawer 1)
	      (org-log-into-drawer nil))
	  (org-clock-into-drawer)))))
  (should
   (= 1
      (org-test-with-temp-text "* H"
	(let ((org-clock-into-drawer 1)
	      (org-log-into-drawer t))
	  (org-clock-into-drawer)))))
  (should
   (= 1
      (org-test-with-temp-text "* H"
	(let ((org-clock-into-drawer 1)
	      (org-log-into-drawer "BAR"))
	  (org-clock-into-drawer)))))
  ;; Otherwise, any non-nil value defaults to `org-log-into-drawer' or
  ;; "LOGBOOK" if it is nil.
  (should
   (equal "LOGBOOK"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer t)
		  (org-log-into-drawer nil))
	      (org-clock-into-drawer)))))
  (should
   (equal "LOGBOOK"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer t)
		  (org-log-into-drawer t))
	      (org-clock-into-drawer)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer t)
		  (org-log-into-drawer "FOO"))
	      (org-clock-into-drawer)))))
  ;; A non-nil "CLOCK_INTO_DRAWER" property overrides
  ;; `org-clock-into-drawer' value.
  (should
   (equal "LOGBOOK"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: t\n:END:"
	    (let ((org-clock-into-drawer nil)
		  (org-log-into-drawer nil))
	      (org-clock-into-drawer)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: FOO\n:END:"
	    (let ((org-clock-into-drawer nil)
		  (org-log-into-drawer nil))
	      (org-clock-into-drawer)))))
  (should-not
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: nil\n:END:"
     (let ((org-clock-into-drawer t)
	   (org-log-into-drawer nil))
       (org-clock-into-drawer))))
  ;; "CLOCK_INTO_DRAWER" can be inherited.
  (should
   (equal "LOGBOOK"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: t\n:END:\n** H2<point>"
	    (let ((org-clock-into-drawer nil)
		  (org-log-into-drawer nil))
	      (org-clock-into-drawer)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text
	      "* H\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: FOO\n:END:\n** H2<point>"
	    (let ((org-clock-into-drawer nil)
		  (org-log-into-drawer nil))
	      (org-clock-into-drawer)))))
  (should-not
   (org-test-with-temp-text
       "* H\n:PROPERTIES:\n:CLOCK_INTO_DRAWER: nil\n:END:\n** H2<point>"
     (let ((org-clock-into-drawer t)
	   (org-log-into-drawer nil))
       (org-clock-into-drawer)))))

(ert-deftest test-org-clock/drawer-name ()
  "Test `org-clock-drawer-name' specifications."
  ;; A nil value for `org-clock-into-drawer' means no drawer is
  ;; expected whatsoever.
  (should-not
   (org-test-with-temp-text "* H"
     (let ((org-clock-into-drawer nil)
	   (org-log-into-drawer nil))
       (org-clock-drawer-name))))
  (should-not
   (org-test-with-temp-text "* H"
     (let ((org-clock-into-drawer nil)
	   (org-log-into-drawer t))
       (org-clock-drawer-name))))
  (should-not
   (org-test-with-temp-text "* H"
     (let ((org-clock-into-drawer nil)
	   (org-log-into-drawer "FOO"))
       (org-clock-drawer-name))))
  ;; A string value for `org-clock-into-drawer' means to use it
  ;; unconditionally.
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer "FOO")
		  (org-log-into-drawer nil))
	      (org-clock-drawer-name)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer "FOO")
		  (org-log-into-drawer t))
	      (org-clock-drawer-name)))))
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer "FOO")
		  (org-log-into-drawer "BAR"))
	      (org-clock-drawer-name)))))
  ;; When the value in `org-clock-into-drawer' is a number, re-use
  ;; `org-log-into-drawer' or use default "LOGBOOK" value.
  (should
   (equal "FOO"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer 1)
		  (org-log-into-drawer "FOO"))
	      (org-clock-drawer-name)))))
  (should
   (equal "LOGBOOK"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer 1)
		  (org-log-into-drawer t))
	      (org-clock-drawer-name)))))
  (should
   (equal "LOGBOOK"
	  (org-test-with-temp-text "* H"
	    (let ((org-clock-into-drawer 1)
		  (org-log-into-drawer nil))
	      (org-clock-drawer-name))))))


;;; Clocktable

(ert-deftest test-org-clock/clocktable ()
  "Test clocktable specifications."
  ;; Relative time: Previous two days.
  (should
   (equal
    "| Headline                     | Time   |      |
|------------------------------+--------+------|
| *Total time*                 | *8:00* |      |
|------------------------------+--------+------|
| Relative times in clocktable | 8:00   |      |
| Foo                          |        | 8:00 |
"
    (org-test-with-temp-text
	"* Relative times in clocktable\n** Foo\n<point>"
      (insert (org-test-clock-create-clock "-3d 8:00" "-3d 12:00"))
      (insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
      (insert (org-test-clock-create-clock "-1d 8:00" "-1d 13:00"))
      (test-org-clock-clocktable-contents-at-point
       ":tstart \"<-2d>\" :tend \"<today>\" :indent nil"))))
  ;; Relative time: Yesterday until now.
  (should
   (equal
    "| Headline                     | Time   |      |
|------------------------------+--------+------|
| *Total time*                 | *6:00* |      |
|------------------------------+--------+------|
| Relative times in clocktable | 6:00   |      |
| Foo                          |        | 6:00 |
"
    (org-test-with-temp-text
	"* Relative times in clocktable\n** Foo\n<point>"
      (insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
      (insert (org-test-clock-create-clock "-1d 8:00" "-1d 13:00"))
      (insert (org-test-clock-create-clock ". 1:00" ". 2:00"))
      (test-org-clock-clocktable-contents-at-point
       ":tstart \"<yesterday>\" :tend \"<tomorrow>\" :indent nil"))))
  ;; Test `untilnow' block.
  (should
   (equal
    "| Headline                     | Time   |      |
|------------------------------+--------+------|
| *Total time*                 | *6:00* |      |
|------------------------------+--------+------|
| Relative times in clocktable | 6:00   |      |
| Foo                          |        | 6:00 |
"
    (org-test-with-temp-text
	"* Relative times in clocktable\n** Foo\n<point>"
      (insert (org-test-clock-create-clock "-10y 15:00" "-10y 18:00"))
      (insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
      (test-org-clock-clocktable-contents-at-point
       ":block untilnow :indent nil"))))
  ;; Test tag filtering.
  (should
   (equal
    "| Headline     | Time   |      |
|--------------+--------+------|
| *Total time* | *2:00* |      |
|--------------+--------+------|
| H1           |        | 2:00 |
"
    (org-test-with-temp-text "** H1\n\n*** H2 :tag:\n\n*** H3\n<point>"
      (insert (org-test-clock-create-clock ". 1:00" ". 2:00"))
      (goto-line 4)
      (insert (org-test-clock-create-clock ". 2:00" ". 4:00"))
      (goto-line 2)
      (test-org-clock-clocktable-contents-at-point
       ":tags \"tag\" :indent nil"))))
  ;; Test `file-with-archives' scope.  In particular, preserve "TBLFM"
  ;; line, and ignore "file" column.
  (should
   (equal
    "| Headline     | Time        |     |
|--------------+-------------+-----|
| *Total time* | *704d 9:01* | foo |
|--------------+-------------+-----|
| Test         | 704d 9:01   | foo |
"
    (org-test-with-temp-text-in-file
	"* Test
CLOCK: [2012-03-29 Thu 16:40]--[2014-03-04 Thu 00:41] => 16905:01

#+BEGIN: clocktable :scope file-with-archives
#+TBLFM: $3=string(\"foo\")
#+END:
"
      (search-forward "#+begin:")
      (beginning-of-line)
      (org-update-dblock)
      (forward-line 2)
      (buffer-substring-no-properties
       (point) (progn (goto-char (point-max))
		      (line-beginning-position -1))))))
  ;; Test ":formula %".  Handle various duration formats.
  (should
   (equal
    "| Headline     |   Time |     % |
|--------------+--------+-------|
| *Total time* | *6:00* | 100.0 |
|--------------+--------+-------|
| Foo          |   4:00 |  66.7 |
| Bar          |   2:00 |  33.3 |
"
    (org-test-with-temp-text
	"* Foo
  CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 15:09] =>  4:00
* Bar
  CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00

* Report
<point>#+BEGIN: clocktable :maxlevel 1 :formula %
#+END:
"
      (org-update-dblock)
      (buffer-substring-no-properties (line-beginning-position 3)
				      (line-beginning-position 9)))))
  (should
   (equal
    "| Headline     | Time      |     % |
|--------------+-----------+-------|
| *Total time* | *1d 4:00* | 100.0 |
|--------------+-----------+-------|
| Foo          | 1d 2:00   |  92.9 |
| Bar          | 2:00      |   7.1 |
"
    (org-test-with-temp-text
	"
* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
* Bar
  CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00
* Report
<point>#+BEGIN: clocktable :maxlevel 1 :formula %
#+END:"
      (org-update-dblock)
      (buffer-substring-no-properties (line-beginning-position 3)
				      (line-beginning-position 9))))))

(provide 'test-org-clock)
;;; test-org-clock.el end here
