;;; test-org-attach.el --- tests for org-attach.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author: Marco Wahl
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org-attach)

(defun touch (filename)
  "Make sure FILENAME exists."
  (find-file filename)
  (save-buffer)
  (kill-buffer))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree/1 ()
  "Attach file at point in dired to subtree."

  ;; prepare
  (let* ((tmpdir (make-temp-file "test-org-attach_" t "/"))
	 (orgfilename (concat tmpdir "attach.org"))
	 (a-filename (concat tmpdir "a")))
    (touch a-filename)
    (dired tmpdir)
    (delete-other-windows)
    (find-file-other-window orgfilename)
    (erase-buffer)
    (org-mode)
    (insert "* foo   :foo:")
    (other-window 1)
    (assert (eq 'dired-mode major-mode))
    (dired-goto-file a-filename)

    ;;action
    (call-interactively #'org-attach-dired-attach-to-next-best-subtree)
    (find-file-other-window orgfilename)
    (beginning-of-buffer)
    (search-forward "* foo")

    ;; expectation.  tag ATTACH has been appended.
    (should
     (reduce (lambda (x y) (or x y))
             (mapcar (lambda (x) (string-equal "ATTACH" x))
                     (plist-get
                      (plist-get
                       (org-element-at-point) 'headline) :tags))))

    ;; cleanup
    (delete-directory tmpdir 'recursive)))


;; Use a test core several times.
(defmacro standard-core-test-org-attach/dired-attach-function-for-method (fun)
  "Create test core for FUN.  Attach two marked files."
  `(let* ((tmpdir (make-temp-file "test-org-attach_" t "/"))
	 (orgfilename (concat tmpdir "attach.org"))
	 (a-filename (concat tmpdir "a"))
	 (b-filename (concat tmpdir "b")))
    (touch a-filename)
    (touch b-filename)
    (dired tmpdir)
    (delete-other-windows)
    (find-file-other-window orgfilename)
    (org-mode)
    (insert "* foo   :foo:")
    (other-window 1)
    (assert (eq 'dired-mode major-mode))
    (dired-goto-file a-filename)
    (dired-mark 1)
    (dired-goto-file b-filename)
    (dired-mark 1)

    ;; action
    (call-interactively #',fun)
    (find-file-other-window orgfilename)
    (beginning-of-buffer)
    (search-forward "* foo")

    ;; check
    (should
     (and (file-exists-p (concat (org-attach-dir) "/" "a"))
          (file-exists-p (concat (org-attach-dir) "/" "b"))))

    ;; cleanup
    (delete-directory tmpdir 'recursive)))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree/2 ()
  "Attach two marked."
  (standard-core-test-org-attach/dired-attach-function-for-method
   org-attach-dired-attach-to-next-best-subtree))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree-cp ()
  (standard-core-test-org-attach/dired-attach-function-for-method
   org-attach-dired-attach-to-next-best-subtree-cp))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree-mv ()
  (standard-core-test-org-attach/dired-attach-function-for-method
   org-attach-dired-attach-to-next-best-subtree-mv))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree-ln ()
  (standard-core-test-org-attach/dired-attach-function-for-method
   org-attach-dired-attach-to-next-best-subtree-mv))

(ert-deftest test-org-attach/dired-attach-to-next-best-subtree-lns ()
  (standard-core-test-org-attach/dired-attach-function-for-method
   org-attach-dired-attach-to-next-best-subtree-lns))


(provide 'test-org-attach)
;;; test-org-attach.el ends here
