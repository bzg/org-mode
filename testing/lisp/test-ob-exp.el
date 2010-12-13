;;; test-ob-exp.el

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
(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the org-mode file."
  (let ((html-file (concat (file-name-sans-extension org-test-no-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-no-heading-file
      ;; export the file to html
      (org-export-as-html nil))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat org-test-no-heading-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-file ()
  "Testing export from buffers which are not visiting any file."
  (when (get-buffer "*Org HTML Export*") (kill-buffer "*Org HTML Export*"))
  (should-not (get-buffer "*Org HTML Export*"))
  ;; export the file to HTML in a temporary buffer
  (org-test-in-example-file nil (org-export-as-html-to-buffer nil))
  ;; should create a .html buffer
  (should (buffer-live-p (get-buffer "*Org HTML Export*")))
  ;; should contain the content of the buffer
  (save-excursion
    (set-buffer (get-buffer "*Org HTML Export*"))
    (should (string-match (regexp-quote org-test-file-ob-anchor)
			  (buffer-string))))
  (when (get-buffer "*Org HTML Export*") (kill-buffer "*Org HTML Export*")))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the org-mode file."
  (let ((html-file (concat (file-name-sans-extension
			    org-test-link-in-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-link-in-heading-file
      ;; export the file to html
      (org-export-as-html nil))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat org-test-link-in-heading-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))

(ert-deftest ob-exp/noweb-on-export ()
  "Noweb header arguments export correctly.
- yes      expand on both export and tangle
- no       expand on neither export or tangle
- tangle   expand on only tangle not export"
  (let (html)
    (org-test-at-id "eb1f6498-5bd9-45e0-9c56-50717053e7b7"
      (org-narrow-to-subtree)
      (setq html (org-export-as-html nil nil nil 'string)))
    (flet ((exp-p (arg)
		  (and
		   (string-match
		    (format "noweb-%s-start\\([^\000]*\\)noweb-%s-end" arg arg)
		    html)
		   (string-match "expanded" (match-string 1 html)))))
      (should (exp-p "yes"))
      (should-not (exp-p "no"))
      (should-not (exp-p "tangle")))))

(ert-deftest ob-exp/exports-both ()
    "Test the :exports both header argument.
The code block should create both <pre></pre> and <table></table>
elements in the final html."
  (let (html)
    (org-test-at-id "92518f2a-a46a-4205-a3ab-bcce1008a4bb"
      (org-narrow-to-subtree)
      (setq html (org-export-as-html nil nil nil 'string))
      (should (string-match "<pre.*>[^\000]*</pre>" html))
      (should (string-match "<table.*>[^\000]*</table>" html)))))

(ert-deftest ob-exp/export-subtree ()
  (org-test-at-id "5daa4d03-e3ea-46b7-b093-62c1b7632df3"
    (org-mark-subtree)
    (org-export-as-latex nil)))

(provide 'test-ob-exp)

;;; test-ob-exp.el ends here
