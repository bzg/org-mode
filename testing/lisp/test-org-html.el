;;; test-org-html.el

;; Copyright (c) ߛ David Maus
;; Authors: David Maus

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

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
