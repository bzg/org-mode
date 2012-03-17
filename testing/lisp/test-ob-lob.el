;;; test-ob-lob.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:


;;; Tests
(org-babel-lob-ingest
 (expand-file-name
  "library-of-babel.org"
  (expand-file-name
   "babel"
   (expand-file-name
    "contrib"
    (expand-file-name
     ".."
     (expand-file-name
      ".."
      (file-name-directory
       (or load-file-name buffer-file-name))))))))

(ert-deftest test-ob-lob/ingest ()
  "Test the ingestion of an org-mode file."
  (should (< 0 (org-babel-lob-ingest
		(expand-file-name "babel.org" org-test-example-dir)))))

(ert-deftest test-ob-lob/call-with-header-arguments ()
  "Test the evaluation of a library of babel #+call: line."
  (org-test-at-id "fab7e291-fde6-45fc-bf6e-a485b8bca2f0"
    (move-beginning-of-line 1)
    (forward-line 6)
    (message (buffer-substring (point-at-bol) (point-at-eol)))
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (caar (org-babel-lob-execute
				      (org-babel-lob-get-info)))))
    (forward-line 1)
    (should (string= "testing" (org-babel-lob-execute
    				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (caar (org-babel-lob-execute
    				      (org-babel-lob-get-info)))))
    (forward-line 1)
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (caar (org-babel-lob-execute
    				      (org-babel-lob-get-info)))))
    (forward-line 1) (beginning-of-line) (forward-char 27)
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1) (beginning-of-line) (forward-char 27)
    (should (string= "testing" (caar (org-babel-lob-execute
				      (org-babel-lob-get-info)))))
    (forward-line 1) (beginning-of-line)
    (should (= 4 (org-babel-lob-execute (org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "testing" (org-babel-lob-execute
				(org-babel-lob-get-info))))
    (forward-line 1)
    (should (string= "123" (org-babel-lob-execute (org-babel-lob-get-info))))))

(ert-deftest test-ob-lob/export-lob-lines ()
  "Test the export of a variety of library babel call lines."
  (org-test-at-id "72ddeed3-2d17-4c7f-8192-a575d535d3fc"
    (org-narrow-to-subtree)
    (let ((html (org-export-as-html nil nil nil 'string 'body-only)))
      ;; check the location of each exported number
      (with-temp-buffer
	(insert html) (goto-char (point-min))
	;; 0 should be on a line by itself
	(should (re-search-forward "0" nil t))
	(should (string= "0" (buffer-substring (point-at-bol) (point-at-eol))))
	;; 2 should be in <code> tags
	(should (re-search-forward "2" nil t))
	(should (re-search-forward (regexp-quote "</code>") (point-at-eol) t))
	(should (re-search-backward (regexp-quote "<code>") (point-at-bol) t))
	;; 4 should not be exported
	(should (not (re-search-forward "4" nil t)))
	;; 6 should also be inline
	(should (re-search-forward "6" nil t))
	(should (re-search-forward (regexp-quote "</code>") (point-at-eol) t))
	(should (re-search-backward (regexp-quote "<code>") (point-at-bol) t))
	;; 8 should not be quoted
	(should (re-search-forward "8" nil t))
	(should (not (= ?= (char-after (point)))))
	(should (not (= ?= (char-before (- (point) 1)))))
	;; 10 should export
	(should (re-search-forward "10" nil t))))))

(ert-deftest test-ob-lob/do-not-eval-lob-lines-in-example-blocks-on-export ()
  (org-test-with-temp-text-in-file "
for export
#+begin_example
#+call: rubbish()
#+end_example"
    (org-export-as-html nil)))

(provide 'test-ob-lob)

;;; test-ob-lob.el ends here
