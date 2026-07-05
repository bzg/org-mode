;;; test-ox-beamer.el --- tests for ox-beamer.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Leo Butler

;; Author: Leo Butler <leo.butler@umanitoba.ca>

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

;; Tests checking validity of Org Beamer export output.

;;; Code:

(require 'org-test "../testing/org-test")

(require 'ox-beamer nil t)
(unless (featurep 'ox-beamer)
  (signal 'missing-test-dependency '("org-export-beamer")))

(eval-when-compile
  (require 'test-duplicates-detector "../testing/lisp/test-duplicates-detector"))



(ert-deftest test-ox-beamer/orgframe ()
  "Test that `org-beamer-frame-environment' is defined and used."
  (org-test-with-exported-text
      'beamer
      "#+OPTIONS: toc:nil
* A frame
Here is an example:
#+begin_example
\\begin{frame}
...
\\end{frame}
#+end_example
"
    (goto-char (point-min))
    (should (search-forward (concat "\\newenvironment<>{" org-beamer-frame-environment "}") nil t))
    (should (search-forward (concat "\\begin{" org-beamer-frame-environment "}") nil t))
    (should (search-forward (concat "\\end{" org-beamer-frame-environment "}") nil t))))

(ert-deftest test-ox-beamer/orgframe-in-example ()
  "Test that `org-beamer-frame-environment' is not defined."
  (org-test-with-exported-text
      'beamer
      (concat "#+OPTIONS: toc:nil
* A frame
Here is an example:
#+begin_example
\\begin{" org-beamer-frame-environment "}
...
\\end{" org-beamer-frame-environment "}
#+end_example
")
    (goto-char (point-min))
    (should-not (search-forward
                 (concat "\\newenvironment<>{" org-beamer-frame-environment "}") nil t))
    (forward-line)
    (should (search-forward (concat "\\begin{frame}") nil t))
    (should (search-forward (concat "\\begin{" org-beamer-frame-environment "}")))
    (should (search-forward (concat "\\end{" org-beamer-frame-environment "}")))
    (should (search-forward (concat "\\end{frame}") nil t))))

(ert-deftest test-ox-beamer/orgframe-in-one-example ()
  "Test that `org-beamer-frame-environment' is defined.
First frame should use \"frame\" environment, the second uses
`org-beamer-frame-environment'."
  (org-test-with-exported-text
      'beamer
      (concat "#+OPTIONS: toc:nil
* A frame
Here is an example:
#+begin_example
\\begin{" org-beamer-frame-environment "}
...
\\end{" org-beamer-frame-environment "}
#+end_example

* A second frame
Here is a second example:
#+begin_example
\\begin{frame}
...
\\end{frame}
#+end_example
")
    (goto-char (point-min))
    (should (search-forward
             (concat "\\newenvironment<>{" org-beamer-frame-environment "}") nil t))
    (forward-line)
    (org-test-ignore-duplicate
     (should (search-forward (concat "\\begin{frame}") nil t))
     (should (search-forward (concat "\\begin{" org-beamer-frame-environment "}")))
     (should (search-forward (concat "\\end{" org-beamer-frame-environment "}")))
     (should (search-forward (concat "\\end{frame}") nil t))
     (should (search-forward (concat "\\begin{" org-beamer-frame-environment "}")))
     (should (search-forward (concat "\\begin{frame}") nil t))
     (should (search-forward (concat "\\end{frame}") nil t))
     (should (search-forward (concat "\\end{" org-beamer-frame-environment "}"))))))

(ert-deftest test-ox-beamer/ltx-talk-class ()
  "Initial test for a simple ltx-talk example.
1. Default Metadata is inserted
2. Sectioning is supported
3. Labels and Beamer theme stuff are ignored"

  (let ((org-latex-compiler "lualatex")
        (org-latex-hyperref-template nil)
        (org-latex-packages-alist nil)
        (org-latex-default-packages-alist nil))
    (org-test-with-exported-text
     'beamer
     "#+STARTUP: beamer
#+OPTIONS: toc:nil H:2
#+LATEX_CLASS: ltx-talk
#+LATEX_CLASS_OPTIONS:
#+LATEX_CLASS_PRE: \\DocumentMetadata{tagging = on}
#+TITLE: Testing =ltx-talk=
#+BEAMER_THEME: Boadilla

* A section
** A frame
- First
- Second
- Third
"
     ;; (message "--> \n%s" (buffer-string))
     (goto-char (point-min))
     ;; ltx-talk ignores theme info
     (save-excursion (should-not (search-forward "\\usetheme{" nil t)))
     ;; ltx-talk doesn't generate labels
     (save-excursion (should-not (search-forward "[label=]" nil t)))
     (save-excursion
       (should (search-forward "\\DocumentMetadata{tagging = on}\n" nil t))
       (should (search-forward "\\documentclass{ltx-talk}\n" nil t))
       (should (search-forward "\\section{A section}" nil t))
       (should (search-forward "\\frametitle{A frame}" nil t))))))

(ert-deftest test-ox-beamer/ltx-talk-multi-line-metadata ()
  "Initial test for a simple ltx-talk example.
1. Default Metadata is inserted
2. Sectioning is supported
3. Labels are supppressed."
  (let ((org-latex-compiler "lualatex")
        (org-latex-hyperref-template nil)
        (org-latex-packages-alist nil)
        (org-latex-default-packages-alist nil))
    (org-test-with-exported-text
     'beamer
     "#+STARTUP: beamer
#+OPTIONS: toc:nil H:2
#+LATEX_CLASS: ltx-talk
#+LATEX_CLASS_OPTIONS:
#+LATEX_CLASS_PRE: \\DocumentMetadata{lang=en,
#+LATEX_CLASS_PRE:   pdfstandard = ua-2,
#+LATEX_CLASS_PRE:   pdfstandard = a-4f,
#+LATEX_CLASS_PRE:   pdfversion = 2.0,
#+LATEX_CLASS_PRE:   tagging=on}
#+LATEX_CLASS_PRE: \\tagpdfsetup{table/header-rows={1},role / new-tag = frametitle / H2}
#+TITLE: Testing =ltx-talk=
#+BEAMER_THEME:

* A section
** A frame
- First
- Second
- Third
"
     ;; (message "--> \n%s" (buffer-string))
     (goto-char (point-min))
     (save-excursion (should-not (search-forward "\\usetheme{" nil t)))
     (save-excursion (should-not (search-forward "[label=]" nil t)))
     (save-excursion
       (should (search-forward "\\DocumentMetadata{lang=en," nil t))
       (should (search-forward "pdfversion = 2.0," nil t))
       (should (search-forward "tagging=on}" nil t))
       (should (search-forward "\\tagpdfsetup{table" nil t))
       (should (search-forward "\\section{A section}" nil t))
       (should (search-forward "\\frametitle{A frame}" nil t))))))

(ert-deftest test-ox-beamer/ltx-talk-frame-subtitle ()
  "Test that a frame with verbatim elements uses \"frame*\"."
  (let ((org-latex-compiler "lualatex")
        (org-latex-hyperref-template nil)
        (org-latex-packages-alist nil)
        (org-latex-default-packages-alist nil))
    (org-test-with-exported-text
     'beamer
     "#+STARTUP: beamer
#+OPTIONS: toc:nil H:2 title:t
#+LATEX_CLASS: ltx-talk
#+LATEX_DOC_METADATA: tagging=on
#+LATEX_CLASS_OPTIONS: handout
#+TITLE: Testing =ltx-talk= verbatims
#+BEAMER_THEME:

* A section
** A frame
- First
- Second
- Third
** A second frame with a subtitle
   :properties:
   :beamer_subtitle: Testing listings
   :end:
- This is a listing:
  #+BEGIN_SRC python
import sys

print(\"Hello, ltx-talk!\", file=sys.stderr)
  #+END_SRC
"
     ;; (message "--> \n%s" (buffer-string))
     (goto-char (point-min))
     (save-excursion (should-not (search-forward "\\usetheme{" nil t)))
     (save-excursion (should-not (search-forward "[label=]" nil t)))
     (save-excursion
       (should (search-forward "\\DocumentMetadata{tagging=on}" nil t))
       (should (search-forward "\\documentclass[handout]{ltx-talk}" nil t))
       (should (search-forward "\\section{A section}\n" nil t))
       (should (search-forward "\\begin{frame}" nil t))
       (should (search-forward "\\frametitle{A frame}\n" nil t))
       (should (search-forward "\\end{frame}" nil t))
       (should (search-forward "\\begin{frame*}" nil t))
       (should (search-forward "\\frametitle{A second frame with a subtitle}\n" nil t))
       (should (search-forward "\\framesubtitle{Testing listings}\n" nil t))
       (should (search-forward "\\end{frame*}" nil t))))))

(ert-deftest test-ox-beamer/beamer-verb-frame ()
  "Test that a frame with verbatim elements sets fragile in beamer.
Added to check that the ltx-talk adaptation doesn't break anything in Beamer."
  (let ((org-latex-compiler "lualatex")
        (org-latex-hyperref-template nil)
        (org-latex-packages-alist nil)
        (org-latex-default-packages-alist nil))
    (org-test-with-exported-text
     'beamer
     "#+STARTUP: beamer
#+OPTIONS: toc:nil H:2 title:t
#+LATEX_CLASS_OPTIONS: [presentation,11pt,t]
#+TITLE: Testing =beamer= verbatims
#+BEAMER_THEME: Boadilla

* A section
** A frame
- First
- Second
- Third
** A frame with a listing
- This is a listing:
  #+BEGIN_SRC python
import sys

print(\"Hello, beamer!\", file=sys.stderr)
  #+END_SRC
"
     ;; (message "--> \n%s" (buffer-string))
     (goto-char (point-min))
     (save-excursion
       (should-not (search-forward "\\DocumentMetadata{tagging=on}" nil t)))
     (save-excursion
       (should (search-forward "\\documentclass[presentation,11pt,t]{beamer}" nil t))
       (should (search-forward "\\usetheme{" nil t)))
     (should (search-forward "\\section{A section}\n" nil t))
     (should (search-forward-regexp "^\\\\begin{frame}\\[.+?]{A frame}$" nil t))
     (should (search-forward "\\end{frame}\n" nil t))
     (should (search-forward-regexp "^\\\\begin{frame}\\[.+?\\,fragile]{A frame with a listing}$" nil t))
     (org-test-ignore-duplicate
      (should (search-forward "\\end{frame}\n" nil t))))))

(provide 'test-ox-beamer)
;;; test-ox-beamer.el ends here
