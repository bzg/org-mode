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

(require 'ox-beamer nil t)
(unless (featurep 'ox-beamer)
  (signal 'missing-test-dependency '("org-export-beamer")))



(ert-deftest ox-beamer/orgframe ()
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

(ert-deftest ox-beamer/orgframe-in-example ()
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

(ert-deftest ox-beamer/orgframe-in-one-example ()
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

(provide 'test-ox-beamer)
;;; test-ox-beamer.el ends here
