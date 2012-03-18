;;; test-ob-emacs-lisp.el

;; Copyright (c) 2012 Free Software Foundation, Inc.
;; Authors: Eric Schulte, Martyn Jago

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Org-mode tests for ob-emacs-lisp.el live here


;;; Code:

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))


;;; Tests

(ert-deftest ob-emacs-lisp/commented-last-block-line-no-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp
;;
#+end_src"
    (progn
      (org-babel-next-src-block)
      (org-ctrl-c-ctrl-c)
      (should (re-search-forward "results:" nil t))
      (forward-line)
      (should
       (string=
	"" 
	(buffer-substring-no-properties (point-at-bol) (point-at-eol))))))
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp
\"some text\";;
#+end_src"

    (progn
      (org-babel-next-src-block)
      (org-ctrl-c-ctrl-c)
      (should (re-search-forward "results:" nil t))
      (forward-line)
      (should
       (string=
	": some text" 
	(buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))

(ert-deftest ob-emacs-lisp/commented-last-block-line-with-var ()
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=1
;;
#+end_src"
    (progn
      (org-babel-next-src-block)
      (org-ctrl-c-ctrl-c)
      (re-search-forward "results" nil t)
      (forward-line)
      (should (string=
	       "" 
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))
  (org-test-with-temp-text-in-file "
#+begin_src emacs-lisp :var a=2
2;;
#+end_src"
    (progn
      (org-babel-next-src-block)
      (org-ctrl-c-ctrl-c)
      (re-search-forward "results" nil t)
      (forward-line)
      (should (string=
	       ": 2" 
	       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))
(provide 'test-ob-emacs-lisp)

 ;;; test-ob-emacs-lisp.el ends here
