;;; test-ob-table.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2011-2014, 2019 Eric Schulte
;; Authors: Eric Schulte

;; This file is not part of GNU Emacs.

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

;;; Comments:

;; Template test file for Org tests

;;; Code:

(ert-deftest test-ob-table/sbe ()
  "Test that `sbe' can be used to call code blocks from inside tables."
  (org-test-with-temp-text
      "#+name: take-sqrt
#+begin_src emacs-lisp :var n=9
  (sqrt n)
#+end_src"
    ;; Symbol src block name.
    (should (equal "2.0" (org-sbe take-sqrt (n "4"))))
    ;; String src block name.
    (should (equal "2.0" (org-sbe "take-sqrt" (n "4")))))
  (org-test-with-temp-text
      "#+name: identity
#+begin_src emacs-lisp :var x=1
  x
#+end_src"
    ;; Escape quotes.
    (should
     (equal "123°34'23.34\"otherthing"
            (org-sbe identity (x $"123°34'23.34\"otherthing"))))))

(provide 'test-ob-table)

;;; test-ob-table.el ends here
