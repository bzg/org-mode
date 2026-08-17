;;; test-ox-md.el --- Tests for ox-org.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ihor Radchenko

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
;;

;;; Code:

(require 'org-test "../testing/org-test")

(require 'ox-org)

(ert-deftest ox-org/honor-export-snippets ()
  "Test exporting export snippets."
  (org-test-with-temp-text "
** @@comment:nope@@ title
#+caption: @@comment:nope@@ Caption
- @@comment:nope@@ tag ::
- @@org:yes@@ tag ::
"
    (let ((export-buffer "*Test Org Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'org export-buffer)
      (with-current-buffer export-buffer
        (goto-char (point-min))
        (should (search-forward "* title"))
        (should (search-forward "#+caption: Caption"))
        (should (search-forward "- tag :: "))
        (should (search-forward "- yes tag :: "))))))

(provide 'test-ox-org)

;;; test-ox-org.el ends here
