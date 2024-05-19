;;; test-ox-ascii.el --- tests for ox-ascii.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>

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

;; Tests checking validity of Org ASCII export output.

;;; Code:

(require 'ox-ascii nil t)
(unless (featurep 'ox-ascii)
  (signal 'missing-test-dependency '("org-export-ascii")))



(ert-deftest test-ox-ascii/list ()
  "Test lists."
  ;; Number counter.
  (org-test-with-exported-text
      'ascii
      "1. [@3] foo"
    (goto-char (point-min))
    (should
     (search-forward
      "3. foo")))
  ;; Number counter.  Start from 1.
  (org-test-with-exported-text
      'ascii
      "3. foo"
    (goto-char (point-min))
    (should
     (search-forward
      "1. foo")))
  ;; Alphanumeric counter.
  (let ((org-list-allow-alphabetical t))
    (org-test-with-exported-text
        'ascii
        "m. [@k] baz"
      (goto-char (point-min))
      (should
       (search-forward
        "11. baz"))))
  ;; Start from 1.
  (let ((org-list-allow-alphabetical t))
    (org-test-with-exported-text
        'ascii
        "m. bar"
      (goto-char (point-min))
      (should
       (search-forward
        "1. bar")))))

(ert-deftest test-ox-ascii/justify ()
  "Test justification."
  ;; Right justify.
  (org-test-with-exported-text
      'ascii
      "#+OPTIONS: author:nil *:t
#+BEGIN_JUSTIFYRIGHT
left or right
#+END_JUSTIFYRIGHT
"
    (goto-char (point-min))
    (search-forward
     "left or right")
    (should
     (equal org-ascii-text-width (org-current-text-column)))))

(provide 'test-ox-ascii)
;;; test-ox-ascii.el ends here
