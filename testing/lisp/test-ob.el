;;; test-ob.el --- tests for ob.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte, Martyn Jago

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))
  (require 'org-test)

(ert-deftest test-org-babel/src-name-regexp ()
  (should(equal "^[ \t]*#\\+\\(srcname\\|source\\|function\\):[ \t]*"
		org-babel-src-name-regexp))
  (mapcar (lambda (name) 
	    (should (org-test-string-exact-match
		     org-babel-src-name-regexp
		     (concat
		      "   \t #+"
		      name
		      ":    \t src-name \t blah blah blah ")))
	    (should (string-match
		     org-babel-src-name-regexp
		     (concat 
		      "#+" (upcase name)
		      ": src-name")))
	    ;;TODO This should fail no?
	    (should (org-test-string-exact-match
		     org-babel-src-name-regexp
		     (concat
		      "#+" name ":")))
	    ;;TODO Check - should this pass?
	    (should (not (org-test-string-exact-match
			  org-babel-src-name-regexp
			  (concat
			   "#+" name " : src-name")))))
	  '("srcname" "source" "function"))
  (should (not  (org-test-string-exact-match
		 org-babel-src-name-regexp
		 "#+invalid-name: src-name"))))

(ert-deftest test-org-babel/multi-line-header-regexp ()
  (should(equal "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
		org-babel-multi-line-header-regexp))
  ;;TODO can be optimised - and what about blah4 blah5 blah6?
  (should (string-match
	   org-babel-multi-line-header-regexp
	   "   \t #+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))
  (should
   (equal
    "blah1 blah2 blah3 \t"
    (match-string
     1
     "   \t #+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n")))
  
  ;;TODO Check - should this fail?
  (should (not (org-test-string-exact-match
	   org-babel-multi-line-header-regexp
	   "   \t #+headers : blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))))

(ert-deftest test-org-babel/src-name-w-name-regexp ()
  (should(equal
	  (concat org-babel-src-name-regexp "\\("
		  org-babel-multi-line-header-regexp "\\)*"
		  "\\([^ ()\f\t\n\r\v]+\\)\\(\(\\(.*\\)\)\\|\\)")
	  org-babel-src-name-w-name-regexp))
  (should (org-test-string-exact-match
	   org-babel-src-name-w-name-regexp
	   (concat
	    "#+srcname: src-name "
	    "#+headers: blah1 blah2 blah3 \t\n\t\n blah4 blah5 blah6 \n"))))

(ert-deftest test-org-babel/src-block-regexp ()
  (should(equal
	  (concat "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
		  "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
		  "\\([^\n]*\\)\n"
		  "\\([^\000]*?\n*\\)[ \t]*#\\+end_src")
	  org-babel-src-block-regexp))
  (let ((test-block(concat
   "#+begin_src language -n-r-a-b -c :argument-1 yes :argument-2 no\n"
   "echo this is a test\n"
   "echo Currently in ' $PWD"
   "#+end_src"))
	(language) (flags) (arguments) (body))
    (should (string-match
	     org-babel-src-block-regexp
	     (upcase test-block)))
    (should (string-match
	     org-babel-src-block-regexp
	     test-block))
    (should(equal "language"
		  (setq language 
			(match-string
			 2
			 test-block))))
    ;;TODO Consider refactoring
    (should(equal "-n-r-a-b -c "
		  (setq flags (match-string
			       3
			       test-block))))
    (should(equal ":argument-1 yes :argument-2 no"
		  (setq arguments (match-string
				   4
				   test-block))))
    (should(equal "echo this is a test\necho Currently in ' $PWD" 
		  (setq body (match-string
			      5
			      test-block))))
    ;;no language
    ;;TODO Is this a valid response?
    (should (org-test-string-exact-match
	     org-babel-src-block-regexp
	     (replace-regexp-in-string language "" test-block)))
    ;;no switches
    (should (org-test-string-exact-match
     	     org-babel-src-block-regexp
     	     (replace-regexp-in-string flags "" test-block)))
    ;;no header arguments
    (should (org-test-string-exact-match
     	     org-babel-src-block-regexp
	     (replace-regexp-in-string arguments "" test-block)))
    ;;TODO Check this ...valid with no body?
    (should (org-test-string-exact-match
		 org-babel-src-block-regexp
		 (replace-regexp-in-string body "" test-block)))))

(ert-deftest test-org-babel/inline-src-block-regexp ()
  (should(equal (concat "[^-[:alnum:]]\\(src_\\([^ \f\t\n\r\v]+\\)"
			"\\(\\|\\[\\(.*?\\)\\]\\)"
			"{\\([^\f\n\r\v]+?\\)}\\)")
		org-babel-inline-src-block-regexp))
  ;; (should (org-test-string-exact-match
  ;; 	   org-babel-inline-src-block-regexp
  ;; 	   "src_lang[:testing1 yes :testing2 no]{ echo This is a test }\n"))
  )

(ert-deftest test-org-babel/default-header-args ()
  (should
   (equal '((:session . "none") (:results . "replace") (:exports . "code")
	    (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no"))
	  org-babel-default-header-args)))

(ert-deftest test-org-babel/get-header ()
  (should (not (org-babel-get-header
		org-babel-default-header-args :doesnt-exist)))
  (should(equal '((:session . "none"))
		(org-babel-get-header
		 org-babel-default-header-args :session)))
  (should(equal '((:session . "none"))
		(org-babel-get-header
		 org-babel-default-header-args :session nil)))
  (should (not (org-babel-get-header
		org-babel-default-header-args :SESSION)))
  (should(equal '((:tangle . "no"))
		(org-babel-get-header
		 org-babel-default-header-args :tangle)))
  ;; with OTHERS option
  (should(equal org-babel-default-header-args
		(org-babel-get-header
		 org-babel-default-header-args :doesnt-exist 'others)))
  (should(equal org-babel-default-header-args
		(org-babel-get-header
		 org-babel-default-header-args nil 'others)))
  (should(equal
	  '((:session . "none") (:results . "replace") (:exports . "code")
	    (:cache . "no") (:hlines . "no") (:tangle . "no"))
	  (org-babel-get-header
	   org-babel-default-header-args :noweb 'others))))

(ert-deftest test-org-babel/default-inline-header-args ()
  (should(equal
	  '((:session . "none") (:results . "replace") (:exports . "results"))
	  org-babel-default-inline-header-args)))

;;; ob-get-src-block-info
(ert-deftest test-org-babel/get-src-block-info-language ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "emacs-lisp" (nth 0 info))))))

(ert-deftest test-org-babel/get-src-block-info-body ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string-match (regexp-quote org-test-file-ob-anchor)
			    (nth 1 info))))))

(ert-deftest test-org-babel/get-src-block-info-tangle ()
  (org-test-at-marker nil org-test-file-ob-anchor
    (let ((info (org-babel-get-src-block-info)))
      (should (string= "no" (cdr (assoc :tangle (nth 2 info))))))))

(ert-deftest test-org-babel/elisp-in-header-arguments ()
  "Test execution of elisp forms in header arguments."
  ;; at the babel.org:elisp-forms-in-header-arguments header
  (org-test-at-id "22d67284-bf14-4cdc-8319-f4bd876829d7"
    (org-babel-next-src-block)
    (let ((info (org-babel-get-src-block-info)))
      (should (= 4 (org-babel-execute-src-block))))))

(ert-deftest test-org-babel/simple-named-code-block ()
  "Test that simple named code blocks can be evaluated."
  (org-test-at-id "0d82b52d-1bb9-4916-816b-2c67c8108dbb"
    (org-babel-next-src-block 1)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/simple-variable-resolution ()
  "Test that simple variable resolution is working."
  (org-test-at-id "f68821bc-7f49-4389-85b5-914791ee3718"
    (org-babel-next-src-block 2)
    (should (= 4 (org-babel-execute-src-block)))))

(ert-deftest test-org-babel/multi-line-header-arguments ()
  "Test that multi-line header arguments and can be read."
  (org-test-at-id "b77c8857-6c76-4ea9-8a61-ddc2648d96c4"
    (org-babel-next-src-block)
    (let ((results (org-babel-execute-src-block)))
      (should(equal 'a (cadr (assoc 1 results))))
      (should(equal 'd (cadr (assoc 4 results)))))))

(ert-deftest test-org-babel/sha1-hash ()
  (org-test-at-id "f68821bc-7f49-4389-85b5-914791ee3718"
    (org-babel-next-src-block 2)
    (should(string= "7374bf4f8a18dfcb6f365f93d15f1a0ef42db745"
		    (org-babel-sha1-hash)))))

(ert-deftest test-org-babel/parse-header-args ()
  (org-test-at-id "7eb0dc6e-1c53-4275-88b3-b22f3113b9c3"
    (org-babel-next-src-block)
    (let* ((info (org-babel-get-src-block-info))
	   (params (nth 2 info)))
      (message "%S" params)
      (should(equal "example-lang" (nth 0 info)))
      (should(string= "the body" (org-babel-trim (nth 1 info))))
      (should-not (member '(:session\ \ \ \ ) params))
      (should(equal '(:session) (assoc :session params)))
      (should(equal '(:result-type . output) (assoc :result-type params)))
      (should(equal '(num . 9) (cdr (assoc :var params)))))))

(provide 'test-ob)

;;; test-ob ends here
