;;; test-ob-octave.el --- tests for ob-octave.el

;; Copyright (c) 2010 Sergey Litvinov
;; Authors: Sergey Litvinov

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

(org-test-for-executable "octave")

(let ((load-path (cons (expand-file-name
			".." (file-name-directory
			      (or load-file-name buffer-file-name)))
		       load-path)))
  (require 'org-test)
  (require 'org-test-ob-consts))

(require 'ob-octave)

(ert-deftest ob-octave/input-none ()
  "Number output"
  (org-test-at-id "54dcd61d-cf6c-4d7a-b9e5-854953c8a753"
    (org-babel-next-src-block)
    (should (= 10 (org-babel-execute-src-block)))))

(ert-deftest ob-octave/output-vector ()
  "Vector output"
  (org-test-at-id "54dcd61d-cf6c-4d7a-b9e5-854953c8a753"
    (org-babel-next-src-block 2)
    (should (equal '((1 2 3 4)) (org-babel-execute-src-block)))))

(ert-deftest ob-octave/input-variable ()
  "Input variable"
  (org-test-at-id "cc2d82bb-2ac0-45be-a0c8-d1463b86a3ba"
    (org-babel-next-src-block)
    (should (= 42 (org-babel-execute-src-block)))))

(ert-deftest ob-octave/input-array ()
  "Input an array"
  (org-test-at-id "cc2d82bb-2ac0-45be-a0c8-d1463b86a3ba"
    (org-babel-next-src-block 2)
    (should (equal '((1 2 3)) (org-babel-execute-src-block)))))

(ert-deftest ob-octave/input-matrix ()
  "Input a matrix"
  (org-test-at-id "cc2d82bb-2ac0-45be-a0c8-d1463b86a3ba"
    (org-babel-next-src-block 3)
    (should (equal '((1 2) (3 4)) (org-babel-execute-src-block)))))

(ert-deftest ob-octave/input-string ()
  "Input a string"
  (org-test-at-id "cc2d82bb-2ac0-45be-a0c8-d1463b86a3ba"
    (org-babel-next-src-block 4)
    (should (equal "te" (org-babel-execute-src-block)))))

(ert-deftest ob-octave/input-nil ()
  "Input elisp nil"
  (org-test-at-id "cc2d82bb-2ac0-45be-a0c8-d1463b86a3ba"
    (org-babel-next-src-block 5)
    (should (equal nil (org-babel-execute-src-block)))))
