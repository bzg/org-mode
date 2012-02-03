;;; test-ob-exp.el

;; Copyright (c) 2010-2012 Eric Schulte
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

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers2 ()
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
  (org-test-at-id "eb1f6498-5bd9-45e0-9c56-50717053e7b7"
    (org-narrow-to-subtree)
    (let ((exported-html
	   (org-export-as-html nil nil nil 'string 'body-only))
	  (test-point 0))

      (org-test-with-temp-text-in-file
	  exported-html

	;; check following ouput exists and in order
	(mapcar (lambda (x)
		  (should (< test-point
			     (re-search-forward
			      x
			      nil t)))
		  (setq test-point (point)))
		'("<code>:noweb</code> header argument expansion"
		  "message" "expanded1"
		  "message" "expanded2"
		  "noweb-1-yes-start"
		  "message" "expanded1"
		  "noweb-no-start"
		  "&lt;&lt;noweb-example1&gt;&gt;"
		  "noweb-2-yes-start"
		  "message" "expanded2"
		  "noweb-tangle-start"
		  "&lt;&lt;noweb-example1&gt;&gt;"
		  "&lt;&lt;noweb-example2&gt;&gt;"))))))

(ert-deftest ob-exp/noweb-on-export-with-exports-results ()
  "Noweb header arguments export correctly using :exports results.
- yes      expand on both export and tangle
- no       expand on neither export or tangle
- tangle   expand on only tangle not export"
  (org-test-at-id "8701beb4-13d9-468c-997a-8e63e8b66f8d"
    (org-narrow-to-subtree)
    (let ((exported-html
	   (org-export-as-html nil nil nil 'string 'body-only))
	  (test-point 0))

      (org-test-with-temp-text-in-file
	  exported-html

	;; check following ouput exists and in order
	(mapcar (lambda (x)
		  (should (< test-point
			     (re-search-forward
			      x
			      nil t)))
		  (setq test-point (point)))
		'("<code>:noweb</code> header argument expansion using :exports results"
		  "expanded1"
		  "expanded2"
		  "expanded1"
		  "noweb-no-start"
		  "&lt;&lt;noweb-example1&gt;&gt;"
		  "expanded2"
		  "&lt;&lt;noweb-example1&gt;&gt;"
		  "&lt;&lt;noweb-example2&gt;&gt;"))))))

(ert-deftest ob-exp/exports-both ()
  "Test the :exports both header argument.
The code block should create both <pre></pre> and <table></table>
elements in the final html."
  (org-test-at-id "92518f2a-a46a-4205-a3ab-bcce1008a4bb"
    (org-narrow-to-subtree)
    (let ((exported-html
	   (org-export-as-html nil nil nil 'string 'body-only))
	  (test-point 0))
      (org-test-with-temp-text-in-file
	  exported-html

	;; check following ouput exists and in order
	(mapcar (lambda (x)
		  (should (< test-point
			     (re-search-forward
			      x
			      nil t)))
		  (setq test-point (point)))
		'( "Pascal's Triangle &ndash; exports both test"
		   "<pre"
		   "defun" "pascals-triangle"
		   "if""list""list""let*""prev-triangle"
		   "pascals-triangle""prev-row""car""reverse""prev-triangle"
		   "append""prev-triangle""list""map""list"
		   "append""prev-row""append""prev-row""pascals-triangle"
		   "</pre>"
		   "<table""<tbody>"
		   "<tr>"">1<""</tr>"
		   "<tr>"">1<"">1<""</tr>"
		   "<tr>"">1<"">2<"">1<""</tr>"
		   "<tr>"">1<"">3<"">3<"">1<""</tr>"
		   "<tr>"">1<"">4<"">6<"">4<"">1<""</tr>"
		   "<tr>"">1<"">5<"">10<"">10<"">5<"">1<""</tr>"
		   "</tbody>""</table>"))))))

(ert-deftest ob-exp/mixed-blocks-with-exports-both ()
    (org-test-at-id "5daa4d03-e3ea-46b7-b093-62c1b7632df3"
    (org-narrow-to-subtree)
    (let ((exported-html
	   (org-export-as-html nil nil nil 'string  'body-only))
	  (test-point 0))
      (org-test-with-temp-text-in-file
	  exported-html

	;; check following ouput exists and in order
	(mapcar (lambda (x)
		  (should (< test-point
			     (re-search-forward
			      x
			      nil t)))
		  (setq test-point (point)))
		'("mixed blocks with exports both"
		  "<ul>"
		  "<li>""a""</li>"
		  "<li>""b""</li>"
		  "<li>""c""</li>"
		  "</ul>"
		  "<pre"
		  "\"code block results\""
		  "</pre>"
		  "<pre class=\"example\">"
		  "code block results"
		  "</pre>"))))))

(ert-deftest ob-exp/export-with-name ()
  (let ((org-babel-exp-code-template
	 "=%name=\n#+BEGIN_SRC %lang%flags\nbody\n#+END_SRC"))
    (org-test-at-id "b02ddd8a-eeb8-42ab-8664-8a759e6f43d9"
      (org-narrow-to-subtree)
      (let ((ascii (org-export-as-ascii nil nil nil 'string 'body-only)))
	(should (string-match "qux" ascii))))))

(ert-deftest ob-exp/export-with-header-argument ()
  (let ((org-babel-exp-code-template
	 "
| header  | value    |
|---------+----------|
| foo     | %foo     |
| results | %results |
#+BEGIN_SRC %lang%flags\nbody\n#+END_SRC"))
    (org-test-at-id "b02ddd8a-eeb8-42ab-8664-8a759e6f43d9"
      (org-narrow-to-subtree)
      (let ((ascii (org-export-as-ascii nil nil nil 'string 'body-only)))
	(should (string-match "baz" ascii))
	(should (string-match "replace" ascii))))))

(ert-deftest ob-exp/noweb-no-export-and-exports-both ()
  (org-test-at-id "8a820f6c-7980-43db-8a24-0710d33729c9"
    (org-narrow-to-subtree)
    (let ((html (org-export-as-html nil nil nil 'string 'body-only)))
      (should (string-match (regexp-quote "noweb-no-export-and-exports-both-1")
			    html)))))

(ert-deftest ob-exp/evaluate-all-executables-in-order ()
  (org-test-at-id "96cc7073-97ec-4556-87cf-1f9bffafd317"
    (org-narrow-to-subtree)
    (let (*evaluation-collector*)
      (org-export-as-ascii nil nil nil 'string)
      (should (equal '(5 4 3 2 1) *evaluation-collector*)))))

(ert-deftest ob-exp/export-call-line-information ()
  (org-test-at-id "bec63a04-491e-4caa-97f5-108f3020365c"
    (org-narrow-to-subtree)
    (let* ((org-babel-exp-call-line-template "\n: call: %line special-token")
	   (html (org-export-as-html nil nil nil 'string t)))
      (should (string-match "double" html))
      (should (string-match "16" html))
      (should (string-match "special-token" html)))))

(ert-deftest ob-exp/noweb-strip-export-ensure-strips ()
  (org-test-at-id "8e7bd234-99b2-4b14-8cd6-53945e409775"
    (org-narrow-to-subtree)
    (org-babel-next-src-block 2)
    (should (= 110 (org-babel-execute-src-block)))
    (let ((ascii (org-export-as-ascii nil nil nil 'string t)))
      (should-not (string-match (regexp-quote "<<strip-export-1>>") ascii))
      (should-not (string-match (regexp-quote "i=\"10\"") ascii)))))

(ert-deftest ob-exp/export-from-a-temp-buffer ()
  :expected-result :failed
  (org-test-with-temp-text
      "
#+Title: exporting from a temporary buffer

#+name: foo
#+BEGIN_SRC emacs-lisp
  :foo
#+END_SRC

#+name: bar
#+BEGIN_SRC emacs-lisp
  :bar
#+END_SRC

#+BEGIN_SRC emacs-lisp :var foo=foo :noweb yes :exports results
  (list foo <<bar>>)
#+END_SRC
"
    (let* ((org-current-export-file (current-buffer))
	   (ascii (org-export-as-ascii nil nil nil 'string)))
      (should (string-match (regexp-quote (format nil "%S" '(:foo :bar)))
			    ascii)))))

(provide 'test-ob-exp)

;;; test-ob-exp.el ends here

