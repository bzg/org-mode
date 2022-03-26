;;; test-ox-texinfo.el --- Tests for ox-texinfo.el

;; Copyright (C) 2022  Rudolf Adamkovič

;; Author: Rudolf Adamkovič <salutis@me.com>

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

;;; Code:

(require 'cl-lib)
(require 'ox-texinfo)

(unless (featurep 'ox-texinfo)
  (signal 'missing-test-dependency "org-export-texinfo"))


;;; TeX fragments

(ert-deftest test-ox-texinfo/tex-fragment-inline ()
  "Test inline TeX fragment."
  (should
   (equal "@math{a^2 = b}"
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 '(:value "$a^2 = b$"))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/tex-fragment-inline-padded ()
  "Test inline TeX fragment padded with whitespace."
  (should
   (equal "@math{a^2 = b}"
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 '(:value "$ a^2 = b $"))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/tex-fragment-displayed ()
  "Test displayed TeX fragment."
   (should
    (equal (string-join
            (list ""
                  "@displaymath"
                  "a ^ 2 = b"
                  "@end displaymath"
                  "")
            "\n")
           (let ((org-texinfo-with-latex t))
             (org-texinfo-latex-fragment
              (org-element-create 'latex-fragment
                                  (list :value "$$a ^ 2 = b$$"))
              nil
              '(:with-latex t))))))

(ert-deftest test-ox-texinfo/tex-fragment-displayed-padded ()
  "Test displayed TeX fragment padded with whitespace."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value "$$ a ^ 2 = b $$"))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/tex-fragment-displayed-multi-line ()
  "Test displayed TeX fragment with multiple lines."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "b ^ 2 = c"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value
                                       (string-join
                                        (list "$$"
                                              "a ^ 2 = b"
                                              "b ^ 2 = c"
                                              "$$")
                                        "\n")))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/tex-fragment-displayed-indented ()
  "Test displayed TeX fragment with indentation."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "b ^ 2 = c"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value
                                       (string-join
                                        (list "  $$"
                                              "  a ^ 2 = b"
                                              "  b ^ 2 = c"
                                              "  $$")
                                        "\n")))
             nil
             '(:with-latex t))))))


;;; LaTeX fragments

(ert-deftest test-ox-texinfo/latex-fragment-inline ()
  "Test inline LaTeX fragment."
  (should
   (equal "@math{a^2 = b}"
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 '(:value "\\(a^2 = b\\)"))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/latex-fragment-inline-padded ()
  "Test inline LaTeX fragment padded with whitespace."
   (should
    (equal "@math{a^2 = b}"
           (let ((org-texinfo-with-latex t))
             (org-texinfo-latex-fragment
              (org-element-create 'latex-fragment
                                  '(:value "\\( a^2 = b \\)"))
              nil
              '(:with-latex t))))))

(ert-deftest test-ox-texinfo/latex-fragment-displayed ()
  "Test displayed LaTeX fragment."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value "\\[a ^ 2 = b\\]"))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/latex-fragment-displayed-padded ()
  "Test displayed LaTeX fragment with multiple lines."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value "\\[ a ^ 2 = b \\]"))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/latex-fragment-displayed-multi-line ()
  "Test displayed LaTeX fragment with multiple lines."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "b ^ 2 = c"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value
                                       (string-join
                                        (list "\\["
                                              "a ^ 2 = b"
                                              "b ^ 2 = c"
                                              "\\]")
                                        "\n")))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/latex-fragment-displayed-indented ()
  "Test displayed LaTeX fragment with indentation."
  (should
   (equal (string-join
           (list ""
                 "@displaymath"
                 "a ^ 2 = b"
                 "b ^ 2 = c"
                 "@end displaymath"
                 "")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-fragment
             (org-element-create 'latex-fragment
                                 (list :value
                                       (string-join
                                        (list "  \\["
                                              "  a ^ 2 = b"
                                              "  b ^ 2 = c"
                                              "  \\]")
                                        "\n")))
             nil
             '(:with-latex t))))))


;;; LaTeX environments

(ert-deftest test-ox-texinfo/latex-environment ()
  "Test LaTeX environment."
  (should
   (equal (string-join
           (list "@displaymath"
                 "\\begin{equation}"
                 "a ^ 2 = b"
                 "b ^ 2 = c"
                 "\\end{equation}"
                 "@end displaymath")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-environment
             (org-element-create 'latex-environment
                                 (list :value
                                       (string-join
                                        (list "\\begin{equation}"
                                              "a ^ 2 = b"
                                              "b ^ 2 = c"
                                              "\\end{equation}")
                                        "\n")))
             nil
             '(:with-latex t))))))

(ert-deftest test-ox-texinfo/latex-environment-indented ()
  "Test LaTeX environment with indentation."
  (should
   (equal (string-join
           (list "@displaymath"
                 "\\begin{equation}"
                 "a ^ 2 = b"
                 "b ^ 2 = c"
                 "\\end{equation}"
                 "@end displaymath")
           "\n")
          (let ((org-texinfo-with-latex t))
            (org-texinfo-latex-environment
             (org-element-create 'latex-environment
                                 (list :value
                                       (string-join
                                        (list "  \\begin{equation}"
                                              "  a ^ 2 = b"
                                              "  b ^ 2 = c"
                                              "  \\end{equation}")
                                        "\n")))
             nil
             '(:with-latex t))))))

(provide 'test-ox-texinfo)
;;; test-ox-texinfo.el end here
