;;; test-ox-latex.el --- tests for ox-latex.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ihor Radchenko

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

;;; Commentary:

;; Tests checking validity of Org LaTeX export output.

;;; Code:

(require 'ox-latex nil t)
(unless (featurep 'ox-latex)
  (signal 'missing-test-dependency '("org-export-latex")))



(ert-deftest test-ox-latex/protect-square-brackets ()
  "Test [foo] being interpreted as plain text even after LaTeX commands."
  (org-test-with-exported-text
      'latex
      "* This is test
lorem @@latex:\\pagebreak@@ [ipsum]

#+begin_figure
[lorem] figure
#+end_figure

| [foo] | 2 |
| [bar] | 3 |

- [bax]
- [aur]
"
    (goto-char (point-min))
    (should (search-forward "lorem \\pagebreak {[}ipsum]"))
    (should (search-forward "{[}lorem] figure"))
    (should (search-forward "{[}foo]"))
    (should (search-forward "\\item {[}bax]"))))

(ert-deftest test-ox-latex/verse ()
  "Test verse blocks."
  (org-test-with-exported-text
      'latex
      "#+begin_verse
lorem ipsum dolor
lorem ipsum dolor

lorem ipsum dolor
lorem ipsum dolor

lorem ipsum dolor
lorem ipsum dolor
#+end_verse
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{verse}
lorem ipsum dolor\\\\
lorem ipsum dolor

lorem ipsum dolor\\\\
lorem ipsum dolor

lorem ipsum dolor\\\\
lorem ipsum dolor\\\\
\\end{verse}")))
  ;; Footnotes inside verse blocks
  (org-test-with-exported-text
      'latex
      "#+begin_verse
lorem
ipsum[fn::Foo

bar]
dolor
#+end_verse

[fn:1] Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Donec hendrerit tempor.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec
hendrerit tempor tellus.
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{verse}
lorem\\\\
ipsum\\footnote{Foo

bar}\\\\
dolor\\\\
\\end{verse}"))))

(ert-deftest test-ox-latex/longtable ()
  "Test table export with longtable environment."
  (org-test-with-exported-text
      'latex
      "#+attr_latex: :environment longtable
| First        | Second |
| Column       | Column |
|--------------+--------|
| a            |      1 |
| b            |      2 |
| \\pagebreak c |      3 |
| d            |      4 |
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{longtable}{lr}
First & Second\\\\
Column & Column\\\\
\\hline
\\endfirsthead"))
    (goto-char (point-min))
    (should
     (search-forward
      "First & Second\\\\
Column & Column \\\\

\\hline
\\endhead"))
    (goto-char (point-min))
    (should
     (search-forward
      "\\hline\\multicolumn{2}{r}{Continued on next page} \\\\
\\endfoot"))))

(ert-deftest test-ox-latex/inline-image ()
  "Test inline images."
  (org-test-with-exported-text
      'latex
      "#+caption: Schematic
[[https://orgmode.org/worg/images/orgmode/org-mode-unicorn.svg][file:/wallpaper.png]]"
    (goto-char (point-min))
    (should
     (search-forward
      "\\href{https://orgmode.org/worg/images/orgmode/org-mode-unicorn.svg}{\\includegraphics[width=.9\\linewidth]{/wallpaper.png}}"))))

(ert-deftest test-ox-latex/num-t ()
  "Test toc treatment for fixed num:t"
  (org-test-with-exported-text
   'latex
   "#+TITLE: num: fix
#+OPTIONS: toc:t H:3 num:t

* Section

** Subsection 1
:PROPERTIES:
:UNNUMBERED: t
:END:
is suppressed
** Subsection 2
:PROPERTIES:
:UNNUMBERED: toc
:END:

** Subsection 3
:PROPERTIES:
:UNNUMBERED: toc
:ALT_TITLE: Alternative
:END:

* Section 2[fn::Test]
:PROPERTIES:
:ALT_TITLE: SECTION 2
:END:
"
   (goto-char (point-min))
   (should
    (search-forward "\\begin{document}

\\maketitle
\\tableofcontents

\\section{Section}
\\label{"))
   (should (search-forward "}

\\subsection*{Subsection 1}
\\label{"))
   (should (search-forward "}
is suppressed
\\subsection*{Subsection 2}
\\label{"))
  (should (search-forward "}
\\addcontentsline{toc}{subsection}{Subsection 2}
\\subsection*{Subsection 3}
\\label{"))
  (should (search-forward "}
\\addcontentsline{toc}{subsection}{Alternative}
\\section[SECTION 2]{Section 2\\footnote{Test}}
\\label{"))
  (should (search-forward "}
\\end{document}"))))

(ert-deftest test-ox-latex/new-toc-as-org ()
  "test toc treatment with `org-latex-toc-include-unnumbered' set to `t'"
  (let ((org-latex-toc-include-unnumbered t))
    (org-test-with-exported-text 'latex
        "#+TITLE: num: fix
#+OPTIONS: toc:t H:3 num:nil

* Section

** Subsection 1

** Subsection 2
:PROPERTIES:
:UNNUMBERED: notoc
:END:
is suppressed

** Subsection 3
:PROPERTIES:
:ALT_TITLE: Alternative
:END:

* Section 2[fn::Test]
:PROPERTIES:
:ALT_TITLE: SECTION 2
:END:

* Section 3[fn::Test]
"
      (goto-char (point-min))
      (should (search-forward "\\begin{document}

\\maketitle
\\tableofcontents

\\section*{Section}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{section}{Section}

\\subsection*{Subsection 1}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{subsection}{Subsection 1}

\\subsection*{Subsection 2}
\\label{"))
      (should (search-forward "}
is suppressed
\\subsection*{Subsection 3}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{subsection}{Alternative}
\\section*{Section 2\\footnote{Test}}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{section}{SECTION 2}"))
      (should (search-forward "}
\\addcontentsline{toc}{section}{Section 3}")))))

(ert-deftest test-ox-latex/use-sans ()
  "Test `org-latex-use-sans' set to t."
  (let ((org-latex-use-sans t))
    (org-test-with-exported-text 'latex
        "#+TITLE: Test sans fonts
* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (should (search-forward "\\begin{document}" nil t)))))

(ert-deftest test-ox-latex/use-sans-option ()
  "Test latex-use-sans in OPTIONS set to t."
  (org-test-with-exported-text 'latex
"#+TITLE: Test sans fonts
#+OPTIONS: latex-use-sans:t

* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (should (search-forward "\\begin{document}" nil t))))

(ert-deftest test-ox-latex/use-sans-default ()
  "Test `org-latex-use-sans' default setting."
  (org-test-with-exported-text 'latex
                               "#+TITLE: Test no sans fonts
* Test

Fake test document
"
      (goto-char (point-min))
      (should-not (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (goto-char (point-min))
      (should (search-forward "\\begin{document}" nil t))))

(ert-deftest test-ox-latex/use-sans-override ()
  "Test `org-latex-use-sans' overriding variable."
  (let ((org-latex-use-sans t))
    (org-test-with-exported-text 'latex
                                 "#+TITLE: Test no sans fonts
#+OPTIONS: latex-use-sans:nil

* Test

Fake test document
"
      (goto-char (point-min))
      (should-not (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (goto-char (point-min))
      (should (search-forward "\\begin{document}" nil t)))))

(ert-deftest test-ox-latex/latex-class-pre ()
  "Test #+LATEX_CLASS_PRE"
  (org-test-with-exported-text 'latex
                               "#+LATEX_CLASS_PRE: \\PassOptionsToPackage{dvipsnames}{xcolor}
#+TITLE: Test prepending LaTeX before the preamble

* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\PassOptionsToPackage{dvipsnames}{xcolor}" nil t))
      ;; And after this
      (should (search-forward "\\documentclass" nil t))
      ;; And after this
      (should (search-forward "\\begin{document}" nil t))))

(ert-deftest test-ox-latex/math-in-alt-title ()
  "Test math wrapping in ALT_TITLE properties."
  (org-test-with-exported-text
      'latex
      "* \\phi wraps
:PROPERTIES:
:ALT_TITLE: \\psi wraps too
:END:"
    (goto-char (point-min))
    (should (search-forward
             "\\section[\\(\\psi\\) wraps too]{\\(\\phi\\) wraps}"))))

(ert-deftest test-ox-latex/numeric-priority-headline ()
  "Test numeric priorities in headlines."
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#3] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#3}")))
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#42] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#42}")))
  ;; Test inline task (level >= org-inlinetask-min-level, default 15)
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t inline:t
***************** [#42] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#42}"))))

(ert-deftest test-ox-latex/alphabetical-priority-headline ()
  "Test numeric priorities in headlines."
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#C] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#C}")))
  ;; Test inline task (level >= org-inlinetask-min-level, default 15)
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t inline:t
***************** [#C] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#C}"))))

(ert-deftest test-ox-latex/subtree-export-with-language ()
  "Test export of subtrees with language detection."
  ;; We can't use `org-test-with-exported-text' because we need a subtree export
  (let ((export-buffer (generate-new-buffer "Org temporary export")))
    (org-test-with-temp-text
     "* subtree
:PROPERTIES:
:EXPORT_LATEX_HEADER: \\usepackage[utf8]{inputenc}
:EXPORT_LATEX_HEADER+: \\usepackage[french]{babel}
:END:

<point>"
     (org-export-to-buffer 'latex export-buffer nil t)
     (with-current-buffer export-buffer
       (goto-char (point-min))
       ;; This is somewhat redundant since the reported issue triggers an error on export
       (should (search-forward "\\usepackage[utf8]{inputenc} \\usepackage[french, english]{babel}")))
     (kill-buffer export-buffer))))

(provide 'test-ox-latex)
;;; test-ox-latex.el ends here
