;;; test-org-html.el --- tests for org-html.el

;; Copyright (c) David Maus
;; Authors: David Maus

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

;; Template test file for Org-mode tests

;;; Code:
(unless (featurep 'org-html)
  (signal 'missing-test-dependency "Support for Org-html"))

(defmacro org-test-html/export-link (name link expected &optional desc opt-plist)
  `(ert-deftest ,(intern (concat "test-org-html/export-link/" name)) ()
     ,(or desc name)
     (should
      (string=
       (org-test-strip-text-props
	(org-html-handle-links ,link ,opt-plist))
       ,expected))))

(org-test-html/export-link "mailto" "[[mailto:john@example.tld]]"
			   "<a href=\"mailto:john@example.tld\">mailto:john@example.tld</a>"
			   "mailto: link without description")

(provide 'test-org-html)

;;; test-org-html.el ends here
