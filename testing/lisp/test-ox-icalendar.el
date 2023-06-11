;;; test-ox-icalendar.el --- tests for ox-icalendar.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jack Kamm

;; Author: Jack Kamm <jackkamm@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests checking validity of Org iCalendar export output.

;;; Code:

(require 'ox-icalendar)

(ert-deftest test-ox-icalendar/crlf-endings ()
  "Test every line of iCalendar export has CRLF ending."
  (let ((tmp-ics (org-test-with-temp-text-in-file
                  "* Test event
:PROPERTIES:
:ID:       b17d8f92-1beb-442e-be4d-d2060fa3c7ff
:END:
<2023-03-30 Thu>"
                  (expand-file-name (org-icalendar-export-to-ics)))))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents tmp-ics)
          (should (eql 1 (coding-system-eol-type last-coding-system-used))))
      (when (file-exists-p tmp-ics) (delete-file tmp-ics)))))

(ert-deftest test-ox-icalendar/todo-repeater-shared ()
  "Test shared repeater on todo scheduled and deadline."
  (let* ((org-icalendar-include-todo 'all)
         (tmp-ics (org-test-with-temp-text-in-file
                   "* TODO Both repeating
DEADLINE: <2023-04-02 Sun +1m> SCHEDULED: <2023-03-26 Sun +1m>"
                   (expand-file-name (org-icalendar-export-to-ics)))))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents tmp-ics)
          (save-excursion
            (should (search-forward "DTSTART;VALUE=DATE:20230326")))
          (save-excursion
            (should (search-forward "DUE;VALUE=DATE:20230402")))
          (save-excursion
            (should (search-forward "RRULE:FREQ=MONTHLY;INTERVAL=1"))))
      (when (file-exists-p tmp-ics) (delete-file tmp-ics)))))

(ert-deftest test-ox-icalendar/todo-repeating-deadline-warndays ()
  "Test repeating deadline with DTSTART as warning days."
  (let* ((org-icalendar-include-todo 'all)
         (org-icalendar-todo-unscheduled-start 'recurring-deadline-warning)
         (tmp-ics (org-test-with-temp-text-in-file
                   "* TODO Repeating deadline
DEADLINE: <2023-04-02 Sun +2w -3d>"
                   (expand-file-name (org-icalendar-export-to-ics)))))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents tmp-ics)
          (save-excursion
            (should (search-forward "DTSTART;VALUE=DATE:20230330")))
          (save-excursion
            (should (search-forward "DUE;VALUE=DATE:20230402")))
          (save-excursion
            (should (search-forward "RRULE:FREQ=WEEKLY;INTERVAL=2"))))
      (when (file-exists-p tmp-ics) (delete-file tmp-ics)))))

(ert-deftest test-ox-icalendar/todo-repeater-until ()
  "Test repeater on todo scheduled until deadline."
  (let* ((org-icalendar-include-todo 'all)
         (tmp-ics (org-test-with-temp-text-in-file
                   "* TODO Repeating scheduled with nonrepeating deadline
DEADLINE: <2023-05-01 Mon> SCHEDULED: <2023-03-26 Sun +3d>"
                   (expand-file-name (org-icalendar-export-to-ics)))))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents tmp-ics)
          (save-excursion
            (should (search-forward "DTSTART;VALUE=DATE:20230326")))
          (save-excursion
            (should (not (re-search-forward "^DUE" nil t))))
          (save-excursion
            (should (search-forward "RRULE:FREQ=DAILY;INTERVAL=3;UNTIL=20230501"))))
      (when (file-exists-p tmp-ics) (delete-file tmp-ics)))))

(ert-deftest test-ox-icalendar/todo-repeater-until-utc ()
  "Test that UNTIL is in UTC when DTSTART is not in local time format."
  (let* ((org-icalendar-include-todo 'all)
         (org-icalendar-date-time-format ":%Y%m%dT%H%M%SZ")
         (tmp-ics (org-test-with-temp-text-in-file
                   "* TODO Repeating scheduled with nonrepeating deadline
DEADLINE: <2023-05-02 Tue> SCHEDULED: <2023-03-26 Sun 15:00 +3d>"
                   (expand-file-name (org-icalendar-export-to-ics)))))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents tmp-ics)
          (save-excursion
            (should (re-search-forward "DTSTART:2023032.T..0000")))
          (save-excursion
            (should (not (re-search-forward "^DUE" nil t))))
          (save-excursion
            (should (re-search-forward "RRULE:FREQ=DAILY;INTERVAL=3;UNTIL=2023050.T..0000Z"))))
      (when (file-exists-p tmp-ics) (delete-file tmp-ics)))))

(ert-deftest test-ox-icalendar/warn-unsupported-repeater ()
  "Test warning is emitted for unsupported repeater type."
  (let ((org-icalendar-include-todo 'all))
    (should
     (member
      "Repeater-type restart not currently supported by iCalendar export"
      (org-test-capture-warnings
       (let ((tmp-ics (org-test-with-temp-text-in-file
                       "* TODO Unsupported restart repeater
SCHEDULED: <2023-03-26 Sun .+1m>"
                       (expand-file-name (org-icalendar-export-to-ics)))))
         (when (file-exists-p tmp-ics)
           (delete-file tmp-ics))))))))

(provide 'test-ox-icalendar)
;;; test-ox-icalendar.el ends here
