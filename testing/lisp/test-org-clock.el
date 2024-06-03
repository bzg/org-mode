;;; test-org-clock.el --- Tests for org-clock.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2014, 2015, 2019  Nicolas Goaziou

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>

;; Released under the GNU General Public License version 3
;; see: https://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments



;;; Code:

(require 'org-duration)
(require 'org-clock)

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
                (org-encode-time
                 (org-fix-decoded-time
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
         (sec-diff (and input2
                        (floor (- (org-time-string-to-seconds end)
                                  (org-time-string-to-seconds beg))))))
    (concat org-clock-string " " beg
            (when end
              (concat "--" end " => "
                      (format "%2d:%02d"
                              (/ sec-diff 3600)
                              (/ (mod sec-diff 3600) 60))))
            "\n")))

(defun test-org-clock-clocktable-contents (options &optional initial)
  "Return contents of a Clock table for current buffer

OPTIONS is a string of Clock table options.  Optional argument
INITIAL is a string specifying initial contents within the Clock
table.

Caption is ignored in contents.  The clocktable doesn't appear in
the buffer."
  (declare (indent 2))
  (goto-char (point-min))
  (save-excursion
    (insert "#+BEGIN: clocktable " options "\n")
    (when initial (insert initial))
    (unless (string-suffix-p "\n" initial) (insert "\n"))
    (insert "#+END:\n"))
  (unwind-protect
      (save-excursion
        (let ((org-duration-format 'h:mm)) (org-update-dblock))
        (forward-line)
        ;; Skip caption.
        (when (looking-at "#\\+CAPTION:") (forward-line))
        (buffer-substring-no-properties
         (point) (progn (search-forward "#+END:") (line-end-position 0))))
    ;; Remove clocktable.
    (delete-region (point) (search-forward "#+END:\n"))))

(ert-deftest test-org-clock/org-clock-timestamps-change ()
  "Test `org-clock-timestamps-change' specifications."
  (let ((sun (org-test-get-day-name "Sun"))
        (mon (org-test-get-day-name "Mon")))
    (should
     (equal
      (format "CLOCK: [2023-02-19 %s 21:30]--[2023-02-19 %s 23:35] =>  2:05"
              sun sun)
      (org-test-with-temp-text
          "CLOCK: [2023-02-19 Sun 2<point>2:30]--[2023-02-20 Mon 00:35] =>  2:05"
        (org-clock-timestamps-change 'down 1)
        (buffer-string))))
    (should
     (equal
      (format "CLOCK: [2023-02-20 %s 00:00]--[2023-02-20 %s 00:40] =>  0:40"
              mon mon)
      (org-test-with-temp-text
          "CLOCK: [2023-02-19 Sun 23:<point>55]--[2023-02-20 Mon 00:35] =>  0:40"
        (org-clock-timestamps-change 'up 1)
        (buffer-string))))
    (should
     (equal
      (format "CLOCK: [2023-02-20 %s 00:30]--[2023-02-20 %s 01:35] =>  1:05"
              mon mon)
      (org-test-with-temp-text
          "CLOCK: [2023-02-19 Sun 2<point>3:30]--[2023-02-20 Mon 00:35] =>  1:05"
        (org-clock-timestamps-change 'up 1)
        (buffer-string))))))

(ert-deftest test-org-clock/org-clock-update-time-maybe ()
  "Test `org-clock-update-time-maybe' specifications."
  (should
   (equal
    (format
     "CLOCK: [2023-04-29 %s 00:00]--[2023-05-04 %s 01:00] => 121:00"
     (org-test-get-day-name "Sat")
     (org-test-get-day-name "Thu"))
    (org-test-with-temp-text
        "CLOCK: [2023-04-29 Sat 00:00]--[2023-05-04 Thu 01:00]"
      (should (org-clock-update-time-maybe))
      (buffer-string))))
  (should-not
   (org-test-with-temp-text
       "[2023-04-29 Sat 00:00]--[2023-05-04 Thu 01:00]"
     (org-clock-update-time-maybe))))


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

(ert-deftest test-org-clock/clock-drawer-dwim ()
  "Test DWIM update of days for clocks in logbook drawers."
  (let ((thu (org-test-get-day-name "Thu")))
    (should (equal (format "* Foo
:LOGBOOK:
CLOCK: [2022-11-03 %s 06:00]--[2022-11-03 %s 06:01] =>  0:01
:END:
" thu thu)
                   (org-test-with-temp-text
                       "* Foo
:LOGBOOK:
<point>CLOCK: [2022-11-03 ??? 06:00]--[2022-11-03 ??? 06:01] =>  0:01
:END:
"
                     (org-ctrl-c-ctrl-c)
                     (buffer-string))))))


;;; Clocktable

(ert-deftest test-org-clock/clocktable/insert ()
  "Test insert clocktable dynamic block with `org-dynamic-block-insert-dblock'."
  (should
   (equal
    "| Headline     | Time   |
|--------------+--------|
| *Total time* | *1:00* |
|--------------+--------|
| H1           | 1:00   |"
    (org-test-with-temp-text "* H1\n<point>"
      (insert (org-test-clock-create-clock ". 1:00" ". 2:00"))

      (goto-line 2)
      (require 'org-clock)
      (org-dynamic-block-insert-dblock "clocktable")

      (goto-line 1)
      (unwind-protect
	  (save-excursion
	    (when (search-forward "#+CAPTION:") (forward-line))
	    (buffer-substring-no-properties
	     (point) (progn (search-forward "#+END:") (line-end-position 0))))
	(delete-region (point) (search-forward "#+END:\n")))))))

(ert-deftest test-org-clock/clocktable/ranges ()
  "Test ranges in Clock table."
  ;; Relative time: Previous two days.
  (should
   (equal
    "| Headline                     | Time   |      |
|------------------------------+--------+------|
| *Total time*                 | *8:00* |      |
|------------------------------+--------+------|
| Relative times in clocktable | 8:00   |      |
| Foo                          |        | 8:00 |"
    (org-test-with-temp-text
        "* Relative times in clocktable\n** Foo\n<point>"
      (insert (org-test-clock-create-clock "-3d 8:00" "-3d 12:00"))
      (insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
      (insert (org-test-clock-create-clock "-1d 8:00" "-1d 13:00"))
      (test-org-clock-clocktable-contents
       ":tstart \"<-2d>\" :tend \"<today>\" :indent nil"))))
  ;; Relative time: Yesterday until now.
  (should
   (equal
    "| Headline                     | Time   |      |
|------------------------------+--------+------|
| *Total time*                 | *6:00* |      |
|------------------------------+--------+------|
| Relative times in clocktable | 6:00   |      |
| Foo                          |        | 6:00 |"
    (org-test-with-temp-text
        "* Relative times in clocktable\n** Foo\n<point>"
      (insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
      (insert (org-test-clock-create-clock "-1d 8:00" "-1d 13:00"))
      (insert (org-test-clock-create-clock ". 1:00" ". 2:00"))
      (test-org-clock-clocktable-contents
       ":tstart \"<yesterday>\" :tend \"<tomorrow>\" :indent nil"))))
  ;; Test `untilnow' block.
  (should
   (equal
    "| Headline                     | Time   |      |
|------------------------------+--------+------|
| *Total time*                 | *6:00* |      |
|------------------------------+--------+------|
| Relative times in clocktable | 6:00   |      |
| Foo                          |        | 6:00 |"
    (org-test-with-temp-text
        "* Relative times in clocktable\n** Foo\n<point>"
      (insert (org-test-clock-create-clock "-10y 15:00" "-10y 18:00"))
      (insert (org-test-clock-create-clock "-2d 15:00" "-2d 18:00"))
      (test-org-clock-clocktable-contents ":block untilnow :indent nil")))))

(ert-deftest test-org-clock/clocktable/match ()
  "Test \":match\" parameter in Clock table."
  ;; Test match filtering.
  (should
   (equal
    "| Headline     | Time   |      |
|--------------+--------+------|
| *Total time* | *2:00* |      |
|--------------+--------+------|
| H1           |        | 2:00 |"
    (org-test-with-temp-text "** H1\n\n*** H2 :tag:\n\n*** H3\n<point>"
      (insert (org-test-clock-create-clock ". 8:00" ". 9:00"))
      (goto-line 4)
      (insert (org-test-clock-create-clock ". 9:00" ". 11:00"))
      (test-org-clock-clocktable-contents ":match \"tag\" :indent nil")))))

(ert-deftest test-org-clock/clocktable/tags ()
  "Test \":tags\" parameter in Clock table."
  ;; Test tags column.
  (should
   (equal
    "| Tags | Headline     | Time   |      |
|------+--------------+--------+------|
|      | *Total time* | *1:00* |      |
|------+--------------+--------+------|
| tag  | H1           |        | 1:00 |"
    (org-test-with-temp-text "** H1 :tag:\n\n*** H2 \n<point>"
      (insert (org-test-clock-create-clock ". 1:00" ". 2:00"))
      (goto-line 4)
      (test-org-clock-clocktable-contents ":tags t :indent nil")))))

(ert-deftest test-org-clock/clocktable/scope ()
  "Test \":scope\" parameter in Clock table."
  ;; Test `file-with-archives' scope.  In particular, preserve "TBLFM"
  ;; line, and ignore "file" column.
  (should
   (equal
    "| Headline     | Time   |     |
|--------------+--------+-----|
| *Total time* | *8:40* | foo |
|--------------+--------+-----|
| Test         | 8:40   | foo |
#+TBLFM: $3=string(\"foo\")"
    (org-test-with-temp-text-in-file
        "* Test
CLOCK: [2012-03-29 Thu 8:00]--[2012-03-29 Thu 16:40] => 8:40"
      (test-org-clock-clocktable-contents ":scope file-with-archives"
                                          "#+TBLFM: $3=string(\"foo\")"))))
  ;; Test "function" scope.
  (should
   (string-match-p
    (regexp-quote "| ALL *Total time* | *1:00* |")
    (org-test-with-temp-text-in-file
        "* Test
CLOCK: [2012-03-29 Thu 16:00]--[2012-03-29 Thu 17:00] =>  1:00"
      (let ((the-file (buffer-file-name)))
        (org-test-with-temp-text-in-file ""
          (test-org-clock-clocktable-contents
           (format ":scope (lambda () (list %S))" the-file))))))))

(ert-deftest test-org-clock/clocktable/maxlevel ()
  "Test \":maxlevel\" parameter in Clock table."
  (should
   (equal "| Headline     | Time   |      |
|--------------+--------+------|
| *Total time* | *6:00* |      |
|--------------+--------+------|
| Foo          | 6:00   |      |
| \\_  Bar      |        | 2:00 |"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 15:09] =>  4:00
** Bar
CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00"
            (test-org-clock-clocktable-contents ":maxlevel 3"))))
  (should
   (equal "| Headline     | Time   |      |
|--------------+--------+------|
| *Total time* | *6:00* |      |
|--------------+--------+------|
| Foo          | 6:00   |      |
| \\_  Bar      |        | 2:00 |"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 15:09] =>  4:00
** Bar
CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00"
            (test-org-clock-clocktable-contents ":maxlevel 2"))))
  (should
   (equal "| Headline     | Time   |
|--------------+--------|
| *Total time* | *6:00* |
|--------------+--------|
| Foo          | 6:00   |"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 15:09] =>  4:00
** Bar
CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00"
            (test-org-clock-clocktable-contents ":maxlevel 1"))))
  ;; Special ":maxlevel 0" case: only report total file time.
  (should
   (equal "| Headline     | Time   |
|--------------+--------|
| *Total time* | *6:00* |
|--------------+--------|"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 15:09] =>  4:00
** Bar
CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00"
            (test-org-clock-clocktable-contents ":maxlevel 0")))))

(ert-deftest test-org-clock/clocktable/formula ()
  "Test \":formula\" parameter in Clock table."
  ;; Test ":formula %".  Handle various duration formats.
  (should
   (equal
    "| Headline     |   Time |     % |
|--------------+--------+-------|
| *Total time* | *6:00* | 100.0 |
|--------------+--------+-------|
| Foo          |   4:00 |  66.7 |
| Bar          |   2:00 |  33.3 |"
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 15:09] =>  4:00
* Bar
  CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00"
      (test-org-clock-clocktable-contents ":maxlevel 1 :formula %"))))
  (should
   (equal
    "| Headline     |    Time |     % |
|--------------+---------+-------|
| *Total time* | *28:00* | 100.0 |
|--------------+---------+-------|
| Foo          |   26:00 |  92.9 |
| Bar          |    2:00 |   7.1 |"
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
* Bar
  CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 15:09] =>  2:00"
      (test-org-clock-clocktable-contents ":maxlevel 1 :formula %"))))
  ;; Properly align column with different depths.
  (should
   (equal "| Headline      | Time   |      |      |     % |
|---------------+--------+------+------+-------|
| *Total time*  | *1:00* |      |      | 100.0 |
|---------------+--------+------+------+-------|
| foo           | 1:00   |      |      | 100.0 |
| \\_  sub       |        | 0:15 |      |  25.0 |
| \\_  sub2      |        | 0:15 |      |  25.0 |
| \\_  sub3      |        | 0:30 |      |  50.0 |
| \\_    subsub1 |        |      | 0:15 |  25.0 |
| \\_    subsub1 |        |      | 0:15 |  25.0 |"
          (org-test-with-temp-text
              "* foo
** sub
   :LOGBOOK:
   CLOCK: [2017-03-18 Sat 15:00]--[2017-03-18 Sat 15:15] =>  0:15
   :END:
** sub2
   :LOGBOOK:
   CLOCK: [2017-03-18 Sat 15:15]--[2017-03-18 Sat 15:30] =>  0:15
   :END:
** sub3
*** subsub1
    :LOGBOOK:
    CLOCK: [2017-03-18 Sat 13:00]--[2017-03-18 Sat 13:15] =>  0:15
    :END:
*** subsub1
    :LOGBOOK:
    CLOCK: [2017-03-18 Sat 14:00]--[2017-03-18 Sat 14:15] =>  0:15
    :END:"
            (test-org-clock-clocktable-contents ":maxlevel 3 :formula %")))))

(ert-deftest test-org-clock/clocktable/lang ()
  "Test \":lang\" parameter in Clock table."
  ;; Test foreign translation
  (should
   (equal
    "| Headline     | Time    |
|--------------+---------|
| *Total time* | *26:00* |
|--------------+---------|
| Foo          | 26:00   |"
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":maxlevel 1 :lang en"))))
  (should
   (equal
    "| En-tête        | Durée   |
|----------------+---------|
| *Durée totale* | *26:00* |
|----------------+---------|
| Foo            | 26:00   |"
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":maxlevel 1 :lang fr"))))
  ;; No :lang parameter is equivalent to "en".
  (should
   (equal
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":maxlevel 1 :lang en"))
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":maxlevel 1"))))
  ;; Unknown translation fall backs to "en".
  (should
   (equal
    "| Headline     | Time    |
|--------------+---------|
| *Total time* | *26:00* |
|--------------+---------|
| Foo          | 26:00   |"
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":maxlevel 1 :lang foo")))))

(ert-deftest test-org-clock/clocktable/link ()
  "Test \":link\" parameter in Clock table."
  ;; If there is no file attached to the document, link directly to
  ;; the headline.
  (should
   (string-match-p "| +\\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
		   (org-test-with-temp-text
		       "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
		     (test-org-clock-clocktable-contents ":link t"))))
  ;; Otherwise, link to the headline in the current file.
  (should
   (string-match-p
    "| \\[\\[file:filename::\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	(org-test-with-temp-text-in-file
	    "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
	  (let ((file (buffer-file-name)))
	    (replace-regexp-in-string
	     (regexp-quote file) "filename"
	     (test-org-clock-clocktable-contents ":link t :lang en"))))
      (org-table-align)
      (buffer-substring-no-properties (point-min) (point-max)))))
  ;; Ignore TODO keyword, priority cookie, COMMENT and tags in
  ;; headline.
  (should
   (string-match-p
    "| \\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	"* TODO Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en"))))
  (should
   (string-match-p
    "| \\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	"* [#A] Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en"))))
  (should
   (string-match-p
    "| \\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	"* COMMENT Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t"))))
  (should
   (string-match-p
    "| \\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	"* Foo :tag:
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en"))))
  ;; Remove statistics cookie from headline description.
  (should
   (string-match-p
    "| \\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	"* Foo [50%]
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en"))))
  (should
   (string-match-p
    "| \\[\\[\\*Foo]\\[Foo]] +| 26:00 +|"
    (org-test-with-temp-text
	"* Foo [1/2]
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en"))))
  ;; Replace links with their description, or turn them into plain
  ;; links if there is no description.
  (should
   (string-match-p
    "| \\[\\[\\*Foo \\\\\\[\\\\\\[https://orgmode\\.org\\\\]\\\\\\[Org mode\\\\]\\\\]]\\[Foo Org mode]] +| 26:00 +|"
    (org-test-with-temp-text
	"* Foo [[https://orgmode.org][Org mode]]
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en"))))
  (should
   (string-match-p
    "| \\[\\[\\*Foo \\\\\\[\\\\\\[https://orgmode\\.org\\\\]\\\\]]\\[Foo https://orgmode\\.org]] +| 26:00 +|"
    (org-test-with-temp-text
	"* Foo [[https://orgmode.org]]
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":link t :lang en")))))

(ert-deftest test-org-clock/clocktable/compact ()
  "Test \":compact\" parameter in Clock table."
  ;; With :compact, all headlines are in the same column.
  (should
   (equal
    "| Headline     | Time    |
|--------------+---------|
| *Total time* | *26:00* |
|--------------+---------|
| Foo          | 26:00   |"
    (org-test-with-temp-text
        "* Foo
  CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":compact t"))))
  (should
   (equal
    "| Headline     |    Time |
|--------------+---------|
| *Total time* | *52:00* |
|--------------+---------|
| Foo          |   52:00 |
| \\_  Bar      |   26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":compact t"))))
  ;; :maxlevel does not affect :compact parameter.
  (should
   (equal
    "| Headline     |    Time |
|--------------+---------|
| *Total time* | *52:00* |
|--------------+---------|
| Foo          |   52:00 |
| \\_  Bar      |   26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":compact t :maxlevel 2"))))
  ;; :compact implies a non-nil :indent parameter.
  (should
   (equal
    "| Headline     |    Time |
|--------------+---------|
| *Total time* | *52:00* |
|--------------+---------|
| Foo          |   52:00 |
| \\_  Bar      |   26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":compact t :indent nil"))))
  ;; :compact implies a nil :level parameter.
  (should
   (equal
    "| Headline     |    Time |
|--------------+---------|
| *Total time* | *52:00* |
|--------------+---------|
| Foo          |   52:00 |
| \\_  Bar      |   26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":compact t :level t")))))

(ert-deftest test-org-clock/clocktable/properties ()
  "Test \":properties\" parameter in Clock table."
  ;; Include a new column with list properties.
  (should
   (equal
    "| A | Headline     | Time    |
|---+--------------+---------|
|   | *Total time* | *26:00* |
|---+--------------+---------|
| 1 | Foo          | 26:00   |"
    (org-test-with-temp-text
        "* Foo
:PROPERTIES:
:A: 1
:END:
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":properties (\"A\")"))))
  (should
   (equal
    "| A | Headline     | Time    |       |
|---+--------------+---------+-------|
|   | *Total time* | *52:00* |       |
|---+--------------+---------+-------|
|   | Foo          | 52:00   |       |
| 1 | \\_  Bar      |         | 26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
:PROPERTIES:
:A: 1
:END:
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":properties (\"A\")")))))

(ert-deftest test-org-clock/clocktable/tcolumns ()
  "Test \":tcolumns\" parameter in Clock table."
  ;; When :tcolumns is smaller than the deepest headline level, lump
  ;; lower levels in the last column.
  (should
   (equal
    "| Headline     |    Time |
|--------------+---------|
| *Total time* | *52:00* |
|--------------+---------|
| Foo          |   52:00 |
| \\_  Bar      |   26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":tcolumns 1"))))
  ;; :tcolumns cannot create more columns than the deepest headline
  ;; level.
  (should
   (equal
    "| Headline     | Time    |       |
|--------------+---------+-------|
| *Total time* | *52:00* |       |
|--------------+---------+-------|
| Foo          | 52:00   |       |
| \\_  Bar      |         | 26:00 |"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00
** Bar
CLOCK: [2016-12-27 Wed 13:09]--[2016-12-28 Wed 15:09] => 26:00"
      (test-org-clock-clocktable-contents ":tcolumns 3"))))
  ;; Pathological case: when no headline contributes to the total
  ;; time, there is only one time column.
  (should
   (equal "| Headline     | Time   |
|--------------+--------|
| *Total time* | *0:00* |"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2016-12-28 Wed 11:09]--[2016-12-28 Wed 11:09] =>  0:00
** Bar
CLOCK: [2016-12-28 Wed 13:09]--[2016-12-28 Wed 13:09] =>  0:00"
            (test-org-clock-clocktable-contents ":tcolumns 2")))))

(ert-deftest test-org-clock/clocktable/step ()
  "Test \":step\" parameter in Clock table."
  ;; Regression test: week crossing month boundary before :wstart
  ;; day-of-week.
  (should
   (string-match-p
    "
.*?\\[2017-09-25 .*
.*
.*
|.*?| \\*1:00\\* |
.*
| Foo +| 1:00 +|"
    (org-test-with-temp-text
	"* Foo
CLOCK: [2017-09-30 Sat 12:00]--[2017-09-30 Sat 13:00] =>  1:00
CLOCK: [2017-10-01 Sun 11:00]--[2017-10-01 Sun 13:00] =>  2:00
CLOCK: [2017-10-02 Mon 11:00]--[2017-10-02 Mon 14:00] =>  3:00"
      (let ((system-time-locale "en_US"))
	(test-org-clock-clocktable-contents
	    ":step week :block 2017-09 :stepskip0 t")))))
  (should
   (string-match-p
    "
.*?\\[2017-10-01 .*
.*
.*
|.*?| \\*2:00\\* |
.*
| Foo +| 2:00   |

.*?\\[2017-10-02 .*
.*
.*
|.*?| \\*7:00\\* |
.*
| Foo +| 7:00 +|

.*?\\[2017-10-09 .*
.*
.*
|.*?| \\*5:00\\* |
.*
| Foo +| 5:00 +|
"
    (org-test-with-temp-text
	"* Foo
CLOCK: [2017-09-30 Sat 12:00]--[2017-09-30 Sat 13:00] =>  1:00
CLOCK: [2017-10-01 Sun 11:00]--[2017-10-01 Sun 13:00] =>  2:00
CLOCK: [2017-10-02 Mon 11:00]--[2017-10-02 Mon 14:00] =>  3:00
CLOCK: [2017-10-08 Sun 09:00]--[2017-10-08 Sun 13:00] =>  4:00
CLOCK: [2017-10-09 Mon 09:00]--[2017-10-09 Mon 14:00] =>  5:00"
      (let ((system-time-locale "en_US"))
	(test-org-clock-clocktable-contents
	    ":step week :block 2017-10 :stepskip0 t")))))
  ;; :step day
  (should
   (string-match-p
    "
.*?\\[2017-10-02 .*
.*
.*
|.*?| \\*3:00\\* |
.*
| Foo +| 3:00 +|

.*?\\[2017-10-03 .*
.*
.*
|.*?| \\*0:00\\* |

.*?\\[2017-10-04 .*
.*
.*
|.*?| \\*0:00\\* |

.*?\\[2017-10-05 .*
.*
.*
|.*?| \\*0:00\\* |

.*?\\[2017-10-06 .*
.*
.*
|.*?| \\*0:00\\* |

.*?\\[2017-10-07 .*
.*
.*
|.*?| \\*0:00\\* |

.*?\\[2017-10-08 .*
.*
.*
|.*?| \\*4:00\\* |
.*
| Foo +| 4:00 +|"
    (org-test-with-temp-text
	"* Foo
CLOCK: [2017-09-30 Sat 12:00]--[2017-09-30 Sat 13:00] =>  1:00
CLOCK: [2017-10-01 Sun 11:00]--[2017-10-01 Sun 13:00] =>  2:00
CLOCK: [2017-10-02 Mon 11:00]--[2017-10-02 Mon 14:00] =>  3:00
CLOCK: [2017-10-08 Sun 09:00]--[2017-10-08 Sun 13:00] =>  4:00
CLOCK: [2017-10-09 Mon 09:00]--[2017-10-09 Mon 14:00] =>  5:00"
      (let ((system-time-locale "en_US"))
	(test-org-clock-clocktable-contents
	    ":step day :block 2017-W40")))))
  ;; Regression test: take :tstart and :tend hours into consideration.
  (should
   (string-match-p
    "
.*?\\[2017-12-25 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|"
    (org-test-with-temp-text
	"* Foo
CLOCK: [2017-12-27 Wed 08:00]--[2017-12-27 Wed 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
	(test-org-clock-clocktable-contents
	    (concat ":step week :tstart \"<2017-12-25 Mon>\" "
		    ":tend \"<2017-12-27 Wed 23:59>\""))))))
  (should
   (string-match-p
    "
.*?\\[2017-12-27 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|"
    (org-test-with-temp-text
	"* Foo
CLOCK: [2017-12-27 Wed 08:00]--[2017-12-27 Wed 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
	(test-org-clock-clocktable-contents
	    (concat ":step day :tstart \"<2017-12-25 Mon>\" "
		    ":tend \"<2017-12-27 Wed 23:59>\" :stepskip0 t"))))))
  ;; Test :step week", without or with ":wstart" parameter.
  (should
   (string-match-p
    "
.*?\\[2012-03-26 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|

.*?\\[2012-04-02 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2012-03-29 Thu 08:00]--[2012-03-29 Thu 16:00] =>  8:00
CLOCK: [2012-04-03 Thu 08:00]--[2012-04-03 Thu 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    ":step week :block 2012 :stepskip0 t")))))
  (should
   (string-match-p
    "
.*?\\[2012-03-29 .*
.*
.*
|.*?| \\*16:00\\* |
.*
| Foo +| 16:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2012-03-29 Thu 08:00]--[2012-03-29 Thu 16:00] =>  8:00
CLOCK: [2012-04-03 Thu 08:00]--[2012-04-03 Thu 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    ":step week :wstart 4 :block 2012 :stepskip0 t")))))
  ;; Test ":step month" without and with ":mstart".
  (should
   (string-match-p
    "
.*?\\[2014-03-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|

.*?\\[2014-04-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2014-03-04 Tue 08:00]--[2014-03-04 Tue 16:00] =>  8:00
CLOCK: [2014-04-03 Thu 08:00]--[2014-04-03 Thu 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    ":step month :block 2014 :stepskip0 t")))))
  (should
   (string-match-p
    "
.*?\\[2014-03-04 .*
.*
.*
|.*?| \\*16:00\\* |
.*
| Foo +| 16:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2014-03-04 Tue 08:00]--[2014-03-04 Tue 16:00] =>  8:00
CLOCK: [2014-04-03 Thu 08:00]--[2014-04-03 Thu 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    ":step month :mstart 4 :block 2014 :stepskip0 t")))))
  ;; Test ":step quarter".
  (should
   (string-match-p
    "
Quarterly report starting on:.*?\\[2014-01-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|

.*?\\[2014-04-01 .*
.*
.*
|.*?| \\*16:00\\* |
.*
| Foo +| 16:00 +|

.*?\\[2014-07-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2014-03-04 Tue 08:00]--[2014-03-04 Tue 16:00] =>  8:00
CLOCK: [2014-04-03 Thu 08:00]--[2014-04-03 Thu 16:00] =>  8:00
CLOCK: [2014-06-04 Wed 08:00]--[2014-06-04 Wed 16:00] =>  8:00
CLOCK: [2014-07-03 Thu 08:00]--[2014-07-03 Thu 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    ":step quarter :block 2014 :stepskip0 t")))))
  ;; Test ":step semimonth".
  (should
   (string-match-p
    "
.*?\\[2014-03-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|

.*?\\[2014-03-16 .*
.*
.*
|.*?| \\*2:00\\* |
.*
| Foo +| 2:00 +|

.*?\\[2014-04-01 .*
.*
.*
|.*?| \\*7:00\\* |
.*
| Foo +| 7:00 +|
"
    (org-test-with-temp-text
     "* Foo
CLOCK: [2014-03-04 Tue 08:00]--[2014-03-04 Tue 16:00] =>  8:00
CLOCK: [2014-03-24 Mon 08:00]--[2014-03-24 Mon 10:00] =>  2:00
CLOCK: [2014-04-03 Thu 08:00]--[2014-04-03 Thu 15:00] =>  7:00"
     (let ((system-time-locale "en_US"))
       (test-org-clock-clocktable-contents
	":step semimonth :block 2014 :stepskip0 t")))))
  ;; Test ":step year".
  (should
   (string-match-p
    "
.*?\\[2012-01-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|

.*?\\[2014-01-01 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2012-03-29 Thu 08:00]--[2012-03-29 Thu 16:00] =>  8:00
CLOCK: [2014-03-04 Tue 08:00]--[2014-03-04 Tue 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    ":step year :block untilnow :stepskip0 t")))))
  ;; Regression test: Respect DST
  (should
   (string-match-p
    "
.*?\\[2018-10-29 .*
.*
.*
|.*?| \\*8:00\\* |
.*
| Foo +| 8:00 +|
"
    (org-test-with-temp-text
        "* Foo
CLOCK: [2018-10-29 Mon 08:00]--[2018-10-29 Mon 16:00] =>  8:00"
      (let ((system-time-locale "en_US"))
        (test-org-clock-clocktable-contents
	    (concat ":step day "
		    ":stepskip0 t "
		    ":tstart \"2018-10-01\" "
		    ":tend \"2018-11-01\"")))))))

(ert-deftest test-org-clock/clocktable/extend-today-until ()
  "Test assignment of clock time to days in presence of \"org-extend-today-until\"."
  ;; Basic test of :block with org-extend-today-until - the report for
  ;; 2017-09-30 should include the time clocked on 2017-10-01 before
  ;; 04:00.
  (should
   (equal "| Headline     | Time   |
|--------------+--------|
| *Total time* | *2:00* |
|--------------+--------|
| Foo          | 2:00   |"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2017-09-30 Sat 12:00]--[2017-09-30 Sat 13:00] =>  1:00
CLOCK: [2017-10-01 Sun 02:00]--[2017-10-01 Sun 03:00] =>  1:00
CLOCK: [2017-10-01 Sun 11:00]--[2017-10-01 Sun 13:00] =>  2:00"
            (setq-local org-extend-today-until 4)
            (let ((system-time-locale "en_US"))
              (test-org-clock-clocktable-contents
               ":block 2017-09-30")))))

  ;; Week-length block - time on Monday before 04:00 should be
  ;; assigned to previous week.
  (should
   (string-match-p "
.*? \\[2017-10-01 .*
.*
.*
|.*?| \\*2:00\\* |
.*
| Foo +| 2:00   |

.*? \\[2017-10-02 .*
.*
.*
|.*?| \\*2:00\\* |
.*
| Foo +| 2:00   |
"
          (org-test-with-temp-text
              "* Foo
CLOCK: [2017-10-01 Sun 12:00]--[2017-10-01 Sun 13:00] =>  1:00
CLOCK: [2017-10-02 Mon 02:00]--[2017-10-02 Mon 03:00] =>  1:00
CLOCK: [2017-10-02 Mon 11:00]--[2017-10-02 Mon 13:00] =>  2:00"
            (setq-local org-extend-today-until 4)
            (let ((system-time-locale "en_US"))
              (test-org-clock-clocktable-contents
               ":step week :block 2017-10 :stepskip0 t"))))))

(ert-deftest test-org-clock/clocktable/hidefiles ()
  "Test \":hidefiles\" parameter in Clock table."
  ;; Test that hidefiles removes the file column.
  (should
   (equal
    "| Headline     | Time   |
|--------------+--------|
| *Total time* | *1:00* |
|--------------+--------|
| Test         | 1:00   |"
    (org-test-with-temp-text-in-file
        "* Test
CLOCK: [2012-03-29 Thu 16:00]--[2012-03-29 Thu 17:00] =>  1:00"
      (let ((the-file (buffer-file-name)))
        (org-test-with-temp-text-in-file ""
          (test-org-clock-clocktable-contents
           (format ":hidefiles t :scope (lambda () (list %S))" the-file))))))))

;;; Mode line

(ert-deftest test-org-clock/mode-line ()
  "Test mode line string ends in a space.

\"Elements that are added to [the mode line] should normally end
in a space (to ensure that consecutive 'global-mode-string'
elements display properly)\" per the Info node `(elisp)Mode Line
Variables'."
  ;; Test the variant without effort.
  (should
   (equal
    "<before> [0:00] (Heading) <after> "
    (org-test-with-temp-text
        "* Heading"
      (org-clock-in)
      (prog1 (concat "<before> "
                     (org-clock-get-clock-string)
                     "<after> ")
        (org-clock-out)))))
  ;; Test the variant with effort.
  (should
   (equal
    "<before> [0:00/1:00] (Heading) <after> "
    (org-test-with-temp-text
        "* Heading
:PROPERTIES:
:EFFORT: 1h
:END:"
      (org-clock-in)
      (prog1 (concat "<before> "
                     (org-clock-get-clock-string)
                     "<after> ")
        (org-clock-out))))))

;;; Helpers

(ert-deftest test-org-clock/special-range ()
  "Test `org-clock-special-range'."
  (let* ((cases
          '((("2023-04-23 Sun" "2023-04-24 Mon" "2023-04-25 Tue" "2023-04-26 Wed"
              "2023-04-27 Thu" "2023-04-28 Fri" "2023-04-29 Sat")
             thisweek 0
             "2023-04-23 Sun" "2023-04-30 Sun")
            (("2023-04-24 Mon" "2023-04-25 Tue" "2023-04-26 Wed"
              "2023-04-27 Thu" "2023-04-28 Fri" "2023-04-29 Sat" "2023-04-30 Sun")
             thisweek 1
             "2023-04-24 Mon" "2023-05-01 Mon")
            (("2023-04-24 Mon" "2023-04-25 Tue" "2023-04-26 Wed"
              "2023-04-27 Thu" "2023-04-28 Fri" "2023-04-29 Sat" "2023-04-30 Sun")
             thisweek nil ; Copy of 1.
             "2023-04-24 Mon" "2023-05-01 Mon")
            (("2023-04-22 Sat"
              "2023-04-23 Sun" "2023-04-24 Mon" "2023-04-25 Tue" "2023-04-26 Wed"
              "2023-04-27 Thu" "2023-04-28 Fri")
             thisweek 6
             "2023-04-22 Sat" "2023-04-29 Sat")
            (("2023-04-23 Sun" "2023-04-24 Mon" "2023-04-25 Tue" "2023-04-26 Wed"
              "2023-04-27 Thu" "2023-04-28 Fri" "2023-04-29 Sat")
             thisweek 7 ; Copy of 0.
             "2023-04-23 Sun" "2023-04-30 Sun")))
         (failed
          (delq
           nil
           (mapcar (lambda (params)
                     (pcase-let ((`(,days ,key ,wstart ,begin ,end) params))
                       (delq
                        nil
                        (mapcar (lambda (today)
                                  (let* ((ts-today (org-time-string-to-time today))
                                         (range (org-clock-special-range
                                                 key ts-today nil wstart nil))
                                         (ts-begin (nth 0 range))
                                         (ts-end (nth 1 range))
                                         (expected-begin (org-time-string-to-time begin))
                                         (expected-end (org-time-string-to-time end)))
                                    (unless (and (equal ts-begin expected-begin)
                                                 (equal ts-end expected-end))
                                      (format "%s..%s != %s..%s %s %s :wstart %s"
                                              begin end
                                              (format-time-string "%F" ts-begin)
                                              (format-time-string "%F" ts-end)
                                              today key wstart))))
                                days))))
                   cases))))
    (should-not failed)))

(provide 'test-org-clock)
;;; test-org-clock.el end here
