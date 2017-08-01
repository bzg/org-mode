;;; test-org-macs.el --- Tests for Org Macs library  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;;; Code:


(ert-deftest test-org/split-string ()
  "Test `org-split-string' specifications."
  ;; Regular test.
  (should (equal '("a" "b") (org-split-string "a b" " ")))
  ;; Empty parts are not removed.
  (should (equal '("a" "" "b") (org-split-string "a||b" "|")))
  ;; However, empty parts at beginning or end of string are removed.
  (should (equal '("a" "b") (org-split-string "|a|b|" "|")))
  ;; Pathological case: call on an empty string.  Since empty parts
  ;; are not removed, it shouldn't return nil.
  (should (equal '("") (org-split-string "")))
  ;; SEPARATORS, when non-nil, is a regexp.  In particular, do not
  ;; match more than specified.
  (should-not (equal '("a" "b") (org-split-string "a    b" " ")))
  ;; When nil, SEPARATORS matches any number of blank characters.
  (should (equal '("a" "b") (org-split-string "a \t\nb"))))

(ert-deftest test-org/string-display ()
  "Test `org-string-display' specifications."
  (should (equal "a" (org-string-display "a")))
  (should (equal "" (org-string-display "")))
  ;; Ignore invisible characters.
  (should (equal "" (org-string-display #("a" 0 1 (invisible t)))))
  (should (equal "b" (org-string-display #("ab" 0 1 (invisible t)))))
  (should (equal "a" (org-string-display #("ab" 1 2 (invisible t)))))
  (should (equal "ace" (org-string-display
                        #("abcde" 1 2 (invisible t) 3 4 (invisible t)))))
  ;; Check if `invisible' value really means invisibility.
  (should (equal "" (let ((buffer-invisibility-spec t))
                      (org-string-display #("a" 0 1 (invisible foo))))))
  (should (equal "" (let ((buffer-invisibility-spec '(foo)))
                      (org-string-display #("a" 0 1 (invisible foo))))))
  (should (equal "" (let ((buffer-invisibility-spec '((foo . t))))
                      (org-string-display #("a" 0 1 (invisible foo))))))
  (should (equal "a" (let ((buffer-invisibility-spec '(bar)))
                       (org-string-display #("a" 0 1 (invisible foo))))))
  ;; Check `display' property.
  (should (equal "abc" (org-string-display #("a" 0 1 (display "abc")))))
  (should (equal "1abc3" (org-string-display #("1a3" 1 2 (display "abc")))))
  ;; `display' string can also contain invisible characters.
  (should (equal "1ac3" (org-string-display
			 #("123" 1 2 (display #("abc" 1 2 (invisible t)))))))
  ;; Preserve other text properties when replacing with a display
  ;; string.
  (should
   (eq 'foo
       (get-text-property 1 'face
			  (org-string-display
			   #("123" 1 2 (display "abc" face foo)))))))


(provide 'test-org-macs)
;;; test-org-macs.el ends here
