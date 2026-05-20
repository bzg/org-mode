;;; test-ob-clojure.el --- tests for test-ob-clojure.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2026 Free Software Foundation, Inc.
;; Authors: stardiviner

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

;;; Commentary:

;; Org tests for ob-clojure.el live here

;;; Code:
(require 'org-test "../testing/org-test")

(unless (featurep 'ob-clojure)
  (signal 'missing-test-dependency '("Support for Clojure code blocks")))

;; tangle
(ert-deftest ob-clojure/org-babel-tangle ()
  "org-babel-tangle returns the exact body of the source block."
  (org-test-with-temp-text-in-file
   "#+begin_src clojure :tangle \"tangle.clj\" :results value\n
(+ 1 2)\n#+end_src"
   (unwind-protect
       (progn (org-babel-tangle)
              (with-temp-buffer
                (insert-file-contents "tangle.clj")
                (let ((tangled (buffer-string)))
                  (should
                   (string-match-p "^(\\+ 1 2)\\\n$" tangled)))))
     (delete-file "tangle.clj"))))

;; execute
(ert-deftest ob-clojure/org-babel-execute ()
  "org-babel-execute:clojure correctly handles :result-params."
  (should (equal 3
                 (org-babel-execute:clojure
                  "(+ 1 2)"
                  '((:result-params "value")))))
  (should (equal ""
                 (org-babel-execute:clojure
                  "(+ 1 2)"
                  '((:result-params "output"))))))

(provide 'test-ob-clojure)

;;; test-ob-clojure.el ends here
