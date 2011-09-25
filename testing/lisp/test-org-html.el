;;; test-org-html.el

;; Copyright (c) ߛ David Maus
;; Authors: David Maus

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests


;;; Code:
(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))


;;; Tests
(require 'org-html)

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
