;;; test-ob-calc.el --- tests for ob-calc.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Visuwesh

;; Author: Visuwesh <visuweshm@gmail.com>

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
(require 'ob-calc)

(unless (featurep 'ob-calc)
  (signal 'missing-test-dependency '("Support for Calc code blocks")))

(ert-deftest ob-calc/simple-program-mult ()
  "Test of simple multiplication."
  (org-test-with-temp-text "\
#+BEGIN_SRC calc :results silent
	1 * 2
#+END_SRC"
    (should (equal "2" (org-babel-execute-src-block)))))

(ert-deftest ob-calc/simple-program-arith ()
  "Test of simple arithmetic."
  (org-test-with-temp-text "\
#+BEGIN_SRC calc :results silent
	12 + 16 - 1
#+END_SRC"
    (should (equal "27" (org-babel-execute-src-block)))))

(ert-deftest ob-calc/float-var ()
  "Test of floating variable."
  (org-test-with-temp-text "\
#+BEGIN_SRC calc :results silent :var x=2.0
	1/x
#+END_SRC"
    (should (equal "0.5" (org-babel-execute-src-block)))))

(ert-deftest ob-calc/simple-program-symbolic ()
  "Test of simple symbolic algebra."
  (org-test-with-temp-text "\
#+BEGIN_SRC calc :results silent
	inv(a)
#+END_SRC"
    (should (equal "1 / a" (org-babel-execute-src-block)))))

(ert-deftest ob-calc/matrix-inversion ()
  "Test of a matrix inversion."
  (org-test-with-temp-text "\
#+NAME: ob-calc-table-1
| 1 |  2 |  3 |
| 5 |  6 |  7 |
| 9 | 14 | 11 |

<point>#+BEGIN_SRC calc :results silent :var a=ob-calc-table-1
	inv(a)
#+END_SRC "
    (should (equal "[[-1, 0.625, -0.125], [0.25, -0.5, 0.25], [0.5, 0.125, -0.125]]"
                   (let ((calc-float-format '(float 0)))
                     ;; ;; Make sure that older Calc buffers are not present.
                     (save-current-buffer
                       (when (ignore-errors (calc-select-buffer))
                         (kill-buffer)))
                     ;; Now, let-bound `calc-float-format' will take
                     ;; effect.
                     (org-babel-execute-src-block))))))

(ert-deftest ob-calc/matrix-algebra ()
  "Test of simple matrix algebra."
  (org-test-with-temp-text "\
#+NAME: ob-calc-table-2
| 1 | 2 | 3 | 4 | 5 |

<point>#+BEGIN_SRC calc :results silent :var a=ob-calc-table-2
	a*2 - 2
#+END_SRC"
    (should (equal "[0, 2, 4, 6, 8]"
                   (org-babel-execute-src-block)))))

(ert-deftest ob-calc/matrix-mean ()
  "Test of simple mean of a vector."
  (org-test-with-temp-text "\
#+NAME: ob-calc-table-2
| 1 | 2 | 3 | 4 | 5 |

<point>#+BEGIN_SRC calc :results silent :var a=ob-calc-table-2
	vmean(a)
#+END_SRC"
    (should (equal "3"
                   (org-babel-execute-src-block)))))

(ert-deftest ob-calc/matrix-correct-conv-column ()
  "Test of conversion of column table to Calc format."
  (org-test-with-temp-text "\
#+NAME: ob-calc-table-3
| 1 |
| 2 |
| 3 |

<point>#+BEGIN_SRC calc :results silent :var a=ob-calc-table-3
	a
#+END_SRC"
    (should (equal "[[1], [2], [3]]"
                   (org-babel-execute-src-block)))))

(ert-deftest ob-calc/matrix-correct-conv-row ()
  "Test of conversion of row table to Calc format."
  (org-test-with-temp-text "\
#+NAME: ob-calc-table-2
| 1 | 2 | 3 | 4 | 5 |

<point>#+BEGIN_SRC calc :results silent :var a=ob-calc-table-2
	a
#+END_SRC"
    (should (equal "[1, 2, 3, 4, 5]"
                   (org-babel-execute-src-block)))))

(provide 'test-ob-calc)
;;; test-ob-calc.el ends here
