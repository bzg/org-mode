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
  (signal 'missing-test-dependency "org-export-latex"))

(defmacro org-test-with-exported-text (backend source &rest body)
  "Run BODY in export buffer for SOURCE string via BACKEND."
  (declare (indent 2))
  `(org-test-with-temp-text ,source
     (let ((export-buffer (generate-new-buffer "Org temporary export")))
       (unwind-protect
           (progn
             (org-export-to-buffer ,backend export-buffer)
             (with-current-buffer export-buffer
               ,@body))
         (kill-buffer export-buffer)))))



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
lorem ipsum dolor\\\\[0pt]
lorem ipsum dolor\\\\[0pt]
\\vspace*{1em}
lorem ipsum dolor\\\\[0pt]
lorem ipsum dolor\\\\[0pt]
\\vspace*{1em}
lorem ipsum dolor\\\\[0pt]
lorem ipsum dolor\\\\[0pt]
\\end{verse}"))))

(provide 'test-ox-latex)
;;; test-ox-latex.el ends here
