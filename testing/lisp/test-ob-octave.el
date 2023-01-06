;;; test-ob-octave.el --- tests for ob-octave.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2010-2014, 2019 Sergey Litvinov
;; Authors: Sergey Litvinov

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(org-test-for-executable "octave")
(unless (featurep 'ob-octave)
  (signal 'missing-test-dependency "Support for Octave code blocks"))

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

(ert-deftest ob-octave/graphics-file ()
  "Graphics file.  Test that link is correctly inserted and graphics file is created (and not empty).  Clean-up side-effects."
  ;; In case a prior test left the Error Output buffer hanging around.
  (when (get-buffer "*Org-Babel Error Output*")
    (kill-buffer "*Org-Babel Error Output*"))
  (let ((file (make-temp-file "test-ob-octave-" nil ".png")))
    (unwind-protect
        (org-test-with-temp-text
	    (format "#+begin_src octave :results file graphics :file %s
sombrero;
#+end_src"
		    file)
          (org-babel-execute-src-block)
          (should (search-forward (format "[[file:%s]]" file) nil nil))
          (should (file-readable-p file))
          (should-not (buffer-live-p (get-buffer "*Org-Babel Error Output*"))))
      ;; clean-up
      (delete-file file)
      (when (get-buffer "*Org-Babel Error Output*")
        (kill-buffer "*Org-Babel Error Output*")))))

(ert-deftest ob-octave/graphics-file-session ()
  "Graphics file in a session.  Test that session is started in *Inferior Octave* buffer, link is correctly inserted and graphics file is created (and not empty).  Clean-up side-effects."
  (let ((file (make-temp-file "test-ob-octave-" nil ".png")))
    (unwind-protect
        (org-test-with-temp-text
	    (format "#+begin_src octave :session :results file graphics :file %s
crash_dumps_octave_core(0);
sombrero;
#+end_src"
		    file)
          (org-babel-execute-src-block)
          (should (get-buffer "*Inferior Octave*"))
          (should (search-forward (format "[[file:%s]]" file) nil nil))
          (should (file-readable-p file))
          (should-not (buffer-live-p (get-buffer "*Org-Babel Error Output*"))))
      ;; clean-up
      (delete-file file)
      (let (kill-buffer-query-functions kill-buffer-hook)
        (kill-buffer "*Inferior Octave*"))
      (when (get-buffer "*Org-Babel Error Output*")
        (kill-buffer "*Org-Babel Error Output*")))))

(ert-deftest ob-octave/graphics-file-space ()
  "Graphics file with a space in filename.  Test that session is started in *Inferior Octave* buffer, link is correctly inserted and graphics file is created (and not empty).  Clean-up side-effects."
  (let ((file (make-temp-file "test ob octave-" nil ".png")))
    (unwind-protect
        (org-test-with-temp-text
	    (format "#+begin_src octave :results file graphics :file %s
sombrero;
#+end_src"
		    file)
          (org-babel-execute-src-block)
          (should (search-forward (format "[[file:%s]]" file) nil nil))
          (should (file-readable-p file))
          (should-not (buffer-live-p (get-buffer "*Org-Babel Error Output*"))))
      ;; clean-up
      (delete-file file)
      (when (get-buffer "*Org-Babel Error Output*")
        (kill-buffer "*Org-Babel Error Output*")))))


(provide 'test-ob-octave)
