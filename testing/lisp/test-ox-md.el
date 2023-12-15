;;; test-ox-md.el --- Tests from ox-md.el            -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ox-md)

(ert-deftest ox-md/footnotes-level ()
  "Test `org-md-toplevel-hlevel' being honored by footnote section."
  (org-test-with-temp-text "
** level 1
   Post starts here. [fn:1]
*** level2
    lorem ipsum
** Footnotes
[fn:1] a footnote 
"
    (let ((org-md-toplevel-hlevel 4)
          (export-buffer "*Test MD Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'md export-buffer)
      (with-current-buffer export-buffer
        (goto-char (point-min))
        (should (search-forward "#### Footnotes"))))))

(provide 'test-ox-md)
;;; test-ox-md.el ends here
