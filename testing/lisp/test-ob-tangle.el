;;; test-ob-tangle.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte

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
(ert-deftest ob-tangle/noweb-on-tangle ()
  "Noweb header arguments tangle correctly.
- yes      expand on both export and tangle
- no       expand on neither export or tangle
- tangle   expand on only tangle not export"
  (let ((target-file (make-temp-file "ob-tangle-test-")))
    (org-test-at-id "eb1f6498-5bd9-45e0-9c56-50717053e7b7"
      (org-narrow-to-subtree)
      (org-babel-tangle target-file))
    (let ((tang (with-temp-buffer
		  (insert-file-contents target-file)
		  (buffer-string))))
      (flet ((exp-p (arg)
		    (and
		     (string-match
		      (format "noweb-%s-start\\([^\000]*\\)noweb-%s-end" arg arg)
		      tang)
		     (string-match "expanded" (match-string 1 tang)))))
	(should (exp-p "yes"))
	(should-not (exp-p "no"))
	(should (exp-p "tangle"))))))

(ert-deftest ob-tangle/no-excessive-id-insertion-on-tangle ()
  "Don't add IDs to headings without tangling code blocks."
  (org-test-at-id "ae7b55ca-9ef2-4d30-bd48-da30e35fd0f3"
    (org-babel-next-src-block)
    (org-babel-tangle)
    (org-babel-previous-src-block)
    (should (null (org-id-get)))))

(provide 'test-ob-tangle)

;;; test-ob-tangle.el ends here
