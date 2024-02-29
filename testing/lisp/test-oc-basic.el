;;; test-oc-basic.el --- Tests for Org Cite basic processor        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92 at posteo dot net>

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

;; Unit tests for Org cite basic processor.

;;; Code:

(require 'oc-basic)

(ert-deftest test-org-cite-basic/parse-bibliography ()
  "Test `org-cite-basic--parse-bibliography'."
  ;; Bibtex bibliography.
  (org-test-with-temp-text
      (format "#+bibliography: %s"
              (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((data (org-cite-basic--parse-bibliography)))
      (should (= 1 (length data)))
      (should (equal (expand-file-name "examples/Basic.bib" org-test-dir)
                     (caar data)))
      (dolist (k (hash-table-keys (cdar data)))
        (when (equal k "friends")
          (should (equal (gethash k (cdar data))
                         '((type . "book")
                           (id . "friends")
                           (title . "{{LaTeX}} and Friends")
                           (author . "van Dongen, M.R.C.")
                           (date . "2012")
                           (location . "Berlin")
                           (publisher . "Springer")
                           (doi . "10.1007/978-3-642-23816-1")
                           (isbn . "9783642238161")))))))))

(ert-deftest test-org-cite-basic/export-citation ()
  "Test `org-cite-basic-export-citation'."
  ;; Default "nil" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Default: [cite:Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (search-forward "Default: (Citing van Dongen, M.R.C., 2012, and van Dongen, M.R.C., 2012
also is duplication.)" nil t)))))
  ;; "author" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Author: [cite/a:Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should
           (search-forward "Author: Citing van Dongen, M.R.C., and van Dongen, M.R.C. also is
duplication." nil t))))))
  ;; "note" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Note: [cite/ft:Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should
           (search-forward "[1] Citing van Dongen, M.R.C. (2012), and van Dongen, M.R.C. (2012) also
is duplication." nil t))))))
  ;; "nocite" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Nocite (should be blank): [cite/n:Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should (search-forward "Nocite (should be blank):\n" nil t))
          (goto-char (point-min))
          (should-not (search-forward "2012" nil t))
          (goto-char (point-min))
          (should-not (search-forward "Dongen" nil t))))))
  ;; "noauthor" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Noauthor: [cite/na:Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should
           (search-forward "Noauthor: (Citing 2012, and 2012 also is duplication.)" nil t))))))
  ;; "numeric" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Numeric (should \"use global affixes and ignore local ones\"): [cite/nb:Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should
           (search-forward "Numeric (should \"use global affixes and ignore local ones\"): (Citing 1,
1 is duplication.)" nil t))))))
  ;; "text" citation style.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Text: [cite/t: Citing ; @friends; and @friends also; is duplication.]"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should
           (search-forward "Text: Citing van Dongen, M.R.C. (2012), and van Dongen, M.R.C. (2012)
also is duplication." nil t)))))))

(ert-deftest test-org-cite-basic/export-bibliography ()
  "Test `org-cite-basic-export-bibliography'."
  ;; Drop {...} Bibtex brackets and render entities.
  (org-test-with-temp-text
      (format
       "#+bibliography: %s
#+cite_export: basic
Foo [cite/plain:@Geyer2011]
#+print_bibliography:"
       (expand-file-name "examples/Basic.bib" org-test-dir))
    (let ((export-buffer "*Test ASCII Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'ascii export-buffer)
      (with-current-buffer export-buffer
        (let ((case-fold-search t))
          (should
           ;; Rendered from {Introduction to Markov\plus Chain Monte Carlo}
           (search-forward "Introduction to Markov+ Chain Monte Carlo" nil t)))))))

(provide 'test-oc-basic)
;;; test-oc-basic.el ends here
