;;; test-ob-maxima.el --- tests for ob-maxima.el

;; Copyright (c) 2010 Sergey Litvinov
;; Authors: Sergey Litvinov

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

(org-test-for-executable "maxima")

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))

(let ((load-path (cons (expand-file-name
			"langs"
			(expand-file-name
			 "babel"
			 (expand-file-name
			  "contrib"
			  (expand-file-name
			   ".."
			   (expand-file-name
			    ".."
			    (file-name-directory
			     (or load-file-name buffer-file-name)))))))
		       load-path)))

  (require 'ob-maxima))

(ert-deftest ob-maxima/assert ()
  (should t))

(ert-deftest ob-maxima/integer-input ()
  "Test of integer input"
  (org-test-at-id "b5842ed4-8e8b-4b18-a1c9-cef006b6a6c8"
    (org-babel-next-src-block)
    (should (equal 4 (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/string-input ()
  "Test of string input"
  (org-test-at-id "b5842ed4-8e8b-4b18-a1c9-cef006b6a6c8"
    (org-babel-next-src-block 2)
    (should (equal "- sin(x)" (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/simple-list-input ()
  "Test of flat list input"
  (org-test-at-id "b5561c6a-73cd-453a-ba5e-62ad84844de6"
    (org-babel-next-src-block)
    (should (equal "[1, 2, 3] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/list-input ()
  "Test of list input"
  (org-test-at-id "b5561c6a-73cd-453a-ba5e-62ad84844de6"
    (org-babel-next-src-block 2)
    (should (equal "[2, [2, 3], 4] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/table-input1 ()
  "Test of table input"
  (org-test-at-id "400ee228-6b12-44fd-8097-7986f0f0db43"
    (org-babel-next-src-block)
    (should (equal "[[2.0], [3.0]] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/table-input2 ()
  "Test of table input"
  (org-test-at-id "400ee228-6b12-44fd-8097-7986f0f0db43"
    (org-babel-next-src-block 2)
    (should (equal "[[2.0, 3.0]] " (org-babel-execute-src-block)))))

(ert-deftest ob-maxima/matrix-output ()
  "Test of table output"
  (org-test-at-id "cc158527-b867-4b1d-8ae0-b8c713a90fd7"
    (org-babel-next-src-block)
    (should (equal '((1 2 3) (2 3 4) (3 4 5)) (org-babel-execute-src-block)))))

(provide 'test-ob-maxima)

;;; test-ob-maxima.el ends here
