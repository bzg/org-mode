;;; test-org-pcomplete.el --- test pcomplete integration

;; Copyright (C) 2015-2016  Alexey Lebedeff
;; Authors: Alexey Lebedeff

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comments:



;;; Code:

(ert-deftest test-org-pcomplete/prop ()
  "Test property completion."
  ;; Drawer where we are currently completing property name is
  ;; malformed in any case, it'll become valid only after successful
  ;; completion.  We expect that this completion process will finish
  ;; successfully, and there will be no interactive drawer repair
  ;; attempts.
  (should
   (equal
    "* a\n:PROPERTIES:\n:pname: \n:END:\n* b\n:PROPERTIES:\n:pname: pvalue\n:END:\n"
    (org-test-with-temp-text "* a\n:PROPERTIES:\n:pna<point>\n:END:\n* b\n:PROPERTIES:\n:pname: pvalue\n:END:\n"
      (cl-letf (((symbol-function 'y-or-n-p)
		 (lambda (_) (error "Should not be called"))))
	(pcomplete))
      (buffer-string)))))

(ert-deftest test-org-pcomplete/keyword ()
  "Test keyword and block completion."
  (should
   (string-prefix-p
    "#+startup: "
    (org-test-with-temp-text "#+start<point>"
      (pcomplete)
      (buffer-string))
    t))
  (should
   (string-prefix-p
    "#+begin_center"
    (org-test-with-temp-text "#+begin_ce<point>"
      (pcomplete)
      (buffer-string))
    t)))

(provide 'test-org-pcomplete)
;;; test-org-pcomplete.el ends here
