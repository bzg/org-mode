;;; test-ox-man.el --- Tests from ox-man.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>

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

(require 'ox-man)

(ert-deftest ox-man/bold ()
  "Test bold text."
  (org-test-with-exported-text 'man "*bold* text"
    (should (search-forward "\\fBbold\\fP text"))))

(ert-deftest ox-man/code ()
  "Test text formatted as code."
  (org-test-with-exported-text 'man "~code~"
    (should (search-forward "\\fCcode\\fP"))))

(ert-deftest ox-man/italic-underlined-verbatim ()
  "Test italic, underlined and verbatim text."
  (org-test-with-exported-text 'man "/italic/, _underlined_, =verbatim="
    (should (search-forward "\\fIitalic\\fP"))
    (should (search-forward "\\fIunderlined\\fP"))
    (should (search-forward "\\fIverbatim\\fP"))))

(provide 'test-ox-man)
;;; test-ox-man.el ends here
