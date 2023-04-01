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

(provide 'test-ox-icalendar)
;;; test-ox-icalendar.el ends here
