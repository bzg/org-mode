;;; test-ob-R.el --- tests for ob-R.el

;; Copyright (c) 2011 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

(org-test-for-executable "R")
(unless (featurep 'ess)
  (signal 'missing-test-dependency "ESS"))

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))

(require 'ob-R)

(ert-deftest test-ob-R/simple-session ()
  (org-test-with-temp-text
      "#+begin_src R :session R\n  paste(\"Yep!\")\n#+end_src\n"
    (should (string= "Yep!" (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-yes-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-nil-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-no-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") ("a") ("b"))
		   (org-babel-execute-src-block)))))

(provide 'test-ob-R)

;;; test-ob-R.el ends here
