;;; test-ox-texinfo.el --- Tests for ox-texinfo.el  -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'subr-x))

(unless (featurep 'ox-texinfo)
  (signal 'missing-test-dependency '("org-export-texinfo")))


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


;;; End-to-end

(ert-deftest test-ox-texinfo/end-to-end-inline ()
  "Test end-to-end with inline TeX fragment."
  (should
   (org-test-with-temp-text
    "$a^2 = b$"
    (let ((export-buffer "*Test Texinfo Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'texinfo export-buffer
        nil nil nil nil nil
        #'texinfo-mode)))))

(ert-deftest test-ox-texinfo/end-to-end-sanity-check-displayed ()
  "Test end-to-end with LaTeX environment."
  (should
   (org-test-with-temp-text
    (string-join
     (list "\\begin{equation}"
           "a ^ 2 = b"
           "b ^ 2 = c"
           "\\end{equation}")
     "\n")
    (let ((export-buffer "*Test Texinfo Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'texinfo export-buffer
        nil nil nil nil nil
        #'texinfo-mode)))))


;;; Filters

(ert-deftest test-ox-texinfo/normalize-headlines ()
  "Test adding empty sections to headlines without one."
  (org-test-with-temp-text
   "* only subsections, no direct content
** sub 1
body
** sub 2
body
"
   (let ((tree (org-element-parse-buffer)))
     (setq tree (org-texinfo--normalize-headlines tree nil nil))
     (let* ((first-heading (car (org-element-contents tree)))
            (section (car (org-element-contents first-heading))))
       (should (org-element-type-p first-heading 'headline))
       (should (org-element-type-p section 'section))
       (should-not (org-element-contents section))
       (should (eq first-heading (org-element-parent section)))))))


;;; References

(ert-deftest test-ox-texinfo/references ()
  "Test references with manual and automatic descriptions."
  (should
   (org-test-with-temp-text
       (string-join
        (list "* A"
              ":PROPERTIES:"
              ":ALT_TITLE: B"
              ":END:"
              "[[A]]"
              "[[A][B]]"
              "[[A][C]]"
              "  ....")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "@ref{B}")
          (re-search-forward "@ref{B, , B}")
          (re-search-forward "@ref{B, , C}")))))))

(ert-deftest test-ox-texinfo/anchors ()
  "Test anchors and references."
  (should
   (org-test-with-temp-text
       (string-join
        (list "* The document"
              "** The model"
              "This is some text describing the model."
              "#+name: model"
              "#+begin_src julia"
              "  struct Model"
              "  end"
              "#+end_src"
              "** Solution"
              "Solving the model ([[model]]) leads to some interesting results."
              )
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "@anchor{model}")
          (re-search-forward "@ref{model}"))))))
  (should
   (org-test-with-temp-text
       (string-join
        (list "* The document"
              "** The model"
              "This is some text describing the model."
              "#+name: model"
              "| foo | bar |"
              "** Solution"
              "Solving the model ([[model]]) leads to some interesting results."
              )
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "@anchor{model}")
          (re-search-forward "@ref{model}")))))))


;;; Headings with links

(ert-deftest test-ox-texinfo/headings-with-links ()
  "Test links are removed from headings conditionally.

Headings exported as chapters, sections, and subsections must not
contain links in their titles, for such links break Texinfo menus.
Headings exported as list items have no such problem."
  (should
   (org-test-with-temp-text
       (string-join
        (list "* Chapter [[https://example.com][Example]]"
              "** Section [[https://example.com][Example]]"
              "*** Subsection [[https://example.com][Example]]"
              "**** Item [[https://example.com][Example]]")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "^@menu$")
          (re-search-forward "^\\* Chapter Example::$")
          (re-search-forward "^Chapter Example$")
          (re-search-forward "^\\* Section Example::$")
          (re-search-forward "^Section Example$")
          (re-search-forward "^\\* Subsection Example::$")
          (re-search-forward "^@node Chapter Example$")
          (re-search-forward "^@chapter Chapter Example$")
          (re-search-forward "^@menu$")
          (re-search-forward "^\\* Section Example::$")
          (re-search-forward "^@node Section Example$")
          (re-search-forward "^@section Section Example$")
          (re-search-forward "^@menu$")
          (re-search-forward "^\\* Subsection Example::$")
          (re-search-forward "^@node Subsection Example$")
          (re-search-forward "^@subsection Subsection Example$")
          (re-search-forward "^@item$")
          (re-search-forward "^@anchor{Item Example}Item @uref{https://example.com, Example}$")))))))


;;; Definitions

(ert-deftest test-ox-texinfo/definition ()
  "Test definitions."
  (should
   (org-test-with-temp-text
       (string-join
        (list "- Variable: name ::"
              "  Description")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "@defvar name")
          (re-search-forward "Description")
          (re-search-forward "@end defvar"))))))
  ;; Edge case: Variable name = nil
  (should
   (org-test-with-temp-text
       (string-join
        (list "- Variable: nil ::"
              "  Description")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "@defvar nil")
          (re-search-forward "Description")
          (re-search-forward "@end defvar"))))))
  ;; Function name containing markup
  (should
   (org-test-with-temp-text
       (string-join
        (list "- Function: ~foo_bar~ arg ::"
              "  Description")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "@defun @code{foo_bar} arg")
          (re-search-forward "Description")
          (re-search-forward "@end defun")))))))


;;; Escaping

(ert-deftest test-ox-texinfo/escape-special-characters ()
  "Test escaping special characters."
  (should
   (org-test-with-temp-text
       (string-join
        (list "[[https://example.com][Foo, Bar]]"
              "[[https://example.com][Foo, Bar}]]")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (should (search-forward "@uref{https://example.com, Foo@comma{} Bar}"))
         (should (search-forward "@uref{https://example.com, Foo@comma{} Bar@}}")))))))


;;; Structure

(ert-deftest test-ox-texinfo/menus-nodes-headings ()
  "Test menus, nodes, and headings."
  (should
   (org-test-with-temp-text
       (string-join
        (list "#+OPTIONS: H:3 toc:2 num:nil"
              "* Heading 1"
              "** Heading 1-1"
              "*** Heading 1-1-1"
              "*** Heading 1-1-2"
              "** Heading 1-2"
              "*** Heading 1-2-1"
              "*** Heading 1-2-2"
              "* Heading 2"
              "** Heading 2-1"
              "*** Heading 2-1-1"
              "*** Heading 2-1-2"
              "** Heading 2-2"
              "*** Heading 2-2-1"
              "*** Heading 2-2-2")
        "\n")
     (let ((export-buffer "*Test Texinfo Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'texinfo export-buffer
         nil nil nil nil nil
         #'texinfo-mode)
       (with-current-buffer export-buffer
         (goto-char (point-min))
         (and
          (re-search-forward "^@menu$")
          (re-search-forward "^* Heading 1::$")
          (re-search-forward "^* Heading 2::$")
          (re-search-forward "^@detailmenu$")
          (re-search-forward "^Heading 1$")
          (re-search-forward "^* Heading 1-1::$")
          (re-search-forward "^* Heading 1-2::$")
          (re-search-forward "^Heading 2$")
          (re-search-forward "^* Heading 2-1::$")
          (re-search-forward "^* Heading 2-2::$")
          (re-search-forward "^@end detailmenu$")
          (re-search-forward "^@end menu$")
          (re-search-forward "^@node Heading 1$")
          (re-search-forward "^@unnumbered Heading 1$")
          (re-search-forward "^@menu$")
          (re-search-forward "^* Heading 1-1::$")
          (re-search-forward "^* Heading 1-2::$")
          (re-search-forward "^@end menu$")
          (re-search-forward "^@node Heading 1-1$")
          (re-search-forward "^@unnumberedsec Heading 1-1$")
          (re-search-forward "^@anchor{Heading 1-1-1}$")
          (re-search-forward "^@subheading Heading 1-1-1$")
          (re-search-forward "^@anchor{Heading 1-1-2}$")
          (re-search-forward "^@subheading Heading 1-1-2$")
          (re-search-forward "^@node Heading 1-2$")
          (re-search-forward "^@unnumberedsec Heading 1-2$")
          (re-search-forward "^@anchor{Heading 1-2-1}$")
          (re-search-forward "^@subheading Heading 1-2-1$")
          (re-search-forward "^@anchor{Heading 1-2-2}$")
          (re-search-forward "^@subheading Heading 1-2-2$")
          (re-search-forward "^@node Heading 2$")
          (re-search-forward "^@unnumbered Heading 2$")
          (re-search-forward "^@menu$")
          (re-search-forward "^* Heading 2-1::$")
          (re-search-forward "^* Heading 2-2::$")
          (re-search-forward "^@end menu$")
          (re-search-forward "^@node Heading 2-1$")
          (re-search-forward "^@unnumberedsec Heading 2-1$")
          (re-search-forward "^@anchor{Heading 2-1-1}$")
          (re-search-forward "^@subheading Heading 2-1-1$")
          (re-search-forward "^@anchor{Heading 2-1-2}$")
          (re-search-forward "^@subheading Heading 2-1-2$")
          (re-search-forward "^@node Heading 2-2$")
          (re-search-forward "^@unnumberedsec Heading 2-2$")
          (re-search-forward "^@anchor{Heading 2-2-1}$")
          (re-search-forward "^@subheading Heading 2-2-1$")
          (re-search-forward "^@anchor{Heading 2-2-2}$")
          (re-search-forward "^@subheading Heading 2-2-2$")))))))

(provide 'test-ox-texinfo)
;;; test-ox-texinfo.el end here
