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
(defvar test-org-html/export-link-alist
  '((:description "mailto: link"
     :link "[[mailto:john@example.tld]]"
     :expected "<a href=\"mailto:john@example.tld\">mailto:john@example.tld</a>"
     :opt-plist nil))
  "List of link definitions to test exporting for.
Each cell is a property list that defines a link export test
using the properties as follows:

:description     A string with a short description of the test.  This
                 is used as the doc-string of the created test.

:link            A string with the normalized Org mode link to test.

:expected        A string with the expected HTML markup.

:opt-plist       A property list with exporting options.")

(defun test-org-html/export-link-factory ()
  "*Create tests for links defined in
  `test-org-html/export-link-alist'."
  (let ((count 0))
    (mapc
     (lambda (link)
       (eval
	`(ert-deftest ,(intern (format "test-org-html/export-link/%d" count)) ()
	   ,(plist-get link :description)
	   (should
	    (string=
	     ,(plist-get link :expected)
	     (org-test-strip-text-props
	      (org-html-handle-links ,(plist-get link :link) ,(plist-get link :opt-plist)))))))
       (setq count (1+ count))) test-org-html/export-link-alist)))

;; Create tests for link export
(test-org-html/export-link-factory)

(provide 'test-org-html)

;;; test-org-html.el ends here
