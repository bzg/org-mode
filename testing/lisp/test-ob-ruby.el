;;; test-ob-ruby.el --- tests for ob-ruby.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2015, 2019 Oleh Krehel
;; Authors: Oleh Krehel

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

(org-test-for-executable "ruby")
(unless (featurep 'ob-ruby)
  (signal 'missing-test-dependency '("Support for Ruby code blocks")))
(unless (featurep 'inf-ruby)
  (signal 'missing-test-dependency '("inf-ruby")))

(ert-deftest test-ob-ruby/session-output-1 ()
    (should (equal (org-test-with-temp-text "#+begin_src ruby :session org-test-ruby :results output
s = \"1\"
s = \"2\"
s = \"3\"
puts s
s = \"4\"
#+end_src"
  (org-babel-execute-maybe)
  (substring-no-properties
   (buffer-string)))
		   "#+begin_src ruby :session org-test-ruby :results output
s = \"1\"
s = \"2\"
s = \"3\"
puts s
s = \"4\"
#+end_src

#+RESULTS:
: 3
")))
(ert-deftest test-ob-ruby/session-output-2 ()
    (should (equal (org-test-with-temp-text "#+begin_src ruby :session org-test-ruby :results output
puts s
s = \"5\"
#+end_src"
  (org-babel-execute-maybe)
  (substring-no-properties
   (buffer-string)))
		   "#+begin_src ruby :session org-test-ruby :results output
puts s
s = \"5\"
#+end_src

#+RESULTS:
: 4
")))
(ert-deftest test-ob-ruby/session-output-3 ()
    (should (equal (org-test-with-temp-text "#+begin_src ruby :session org-test-ruby :results output
puts s
s = \"6\"
#+end_src"
  (org-babel-execute-maybe)
  (substring-no-properties
   (buffer-string)))
		   "#+begin_src ruby :session org-test-ruby :results output
puts s
s = \"6\"
#+end_src

#+RESULTS:
: 5
")))

(ert-deftest test-ob-ruby/value ()
  (should
   (equal 3
          (org-test-with-temp-text "#+begin_src ruby :results value
1 + 2
#+end_src"
            (org-babel-execute-src-block)))))

(ert-deftest test-ob-ruby/value-with-class-definition ()
  (should
   (equal 3
          (org-test-with-temp-text "#+begin_src ruby :results value
class Three
  def result
    1 + 2
  end
end
Three.new.result
#+end_src"
            (org-babel-execute-src-block)))))

(ert-deftest test-ob-ruby/value-with-return ()
  (should
   (equal 3
          (org-test-with-temp-text "#+begin_src ruby :results value
return 3
2
#+end_src"
            (org-babel-execute-src-block)))))

(ert-deftest test-ob-ruby/pp ()
  (should
   (equal "3\n"
          (org-test-with-temp-text "#+begin_src ruby :results pp
1 + 2
#+end_src"
                   (org-babel-execute-src-block)))))

(ert-deftest test-ob-ruby/pp-with-class-definition ()
  (should
   (equal "3\n"
          (org-test-with-temp-text "#+begin_src ruby :results pp
class Three
  def result
    1 + 2
  end
end
Three.new.result
#+end_src"
                   (org-babel-execute-src-block)))))

(ert-deftest test-ob-ruby/pp-with-return ()
  (should
   (equal "3\n"
          (org-test-with-temp-text "#+begin_src ruby :results pp
return 3
2
#+end_src"
                   (org-babel-execute-src-block)))))

(provide 'test-ob-ruby)

;;; test-ob-ruby.el ends here
