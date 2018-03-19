;;; test-ob-table.el

;; Copyright (c) 2011-2014 Eric Schulte
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comments:

;; Template test file for Org tests

;;; Code:

;; TODO Test Broken (wrong-type-argument number-or-marker-p "2.0")
;; (ert-deftest test-ob-table/sbe ()
;;   "Test that `sbe' can be used to call code blocks from inside tables."
;;   (org-test-at-id "6d2ff4ce-4489-4e2a-9c65-e3f71f77d975"
;;     (should (= 2 (sbe take-sqrt (n "4"))))))

(ert-deftest test-ob-table/sbe-quote ()
  "Test that `org-sbe' can correctly handle cell values containing quotes."
  (org-test-table-target-expect
   "
#+name: identity
#+begin_src emacs-lisp :eval yes
  x
#+end_src

| a\"b\"c | replace |
"
   "
#+name: identity
#+begin_src emacs-lisp :eval yes
  x
#+end_src

| a\"b\"c | a\"b\"c |
"
   1
   "#+TBLFM: $2 = '(org-sbe identity (x $$1))"))

(ert-deftest test-ob-table/sbe-list ()
  "Test that `org-sbe' can correctly handle ranges as lists."
  (org-test-table-target-expect
   "
#+name: concat
#+begin_src emacs-lisp :eval yes
  (mapconcat #'identity x \"\")
#+end_src

| foo | bar | replace |
"
   "
#+name: concat
#+begin_src emacs-lisp :eval yes
  (mapconcat #'identity x \"\")
#+end_src

| foo | bar | foobar |
"
   1
   "#+TBLFM: $3 = '(org-sbe concat (x   (list $1..$2)))"
   "#+TBLFM: $3 = '(org-sbe concat (x $ (list $1..$2)))"))

(provide 'test-ob-table)

;;; test-ob-table.el ends here
