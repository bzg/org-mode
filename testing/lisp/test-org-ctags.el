;;; test-org-ctags.el --- tests for org-ctags.el  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Max Nikulin
;; Authors: Max Nikulin

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

;;; Code:

;; Alternative implementation for `test-org-ctags/mock-command'
;; is required for cmd.exe.
(unless (string-equal "-c" shell-command-switch)
  (signal 'missing-test-dependency '("POSIX shell")))

(require 'org-ctags)

;;;; Helpers:

(defun test-org-ctags/mock-command (temp-file command-name)
  "Define shell function COMMAND-NAME wrining arguments to TEMP-FILE."
  ;; Failure exit code is used to prevent further `org-ctags' actions.
  (format "%s() { printf '%%s\\n' %s \"$@\" >%s 2>&1 ; false ; } ; %s"
          command-name command-name
          (shell-quote-argument temp-file)
          command-name))

(defun test-org-ctags/get-args (temp-file base magic)
  "Read list of strings from TEMP-FILE.

If TEMP-FILE does not start from MAGIC then return
its content as a string.  Otherwise strip first line
and trailing newline, replace BASE with \"TMPDIR\" string,
return list of lines."
  (let* ((case-fold-search nil)
         (content
          (and
           (file-exists-p temp-file)
           (with-temp-buffer
             (insert-file-contents temp-file)
             (goto-char (point-min))
             (when (looking-at magic)
               (while (search-forward base nil 'noerror)
                 (replace-match "TMPDIR" 'fixedcase 'literal)))
            (goto-char (point-max))
            (when (and (bolp) (> (point) 1))
              (delete-char -1))
            (buffer-string)))))
    (if (and content (string-prefix-p magic content))
        (cdr (split-string content "\n"))
      content)))

(defmacro test-org-ctags/with-fake-ctags
    (temp-dir subdir &rest body)
  "Run BODY with `org-ctags-path-to-ctags' set to a test function.

Create a buffer backed by a file in the TEMP-DIR/SUBDIR directory."
  (declare (indent 2))
  (let ((buffer (gensym "buffer"))
        (base (gensym "base"))
        (dir (gensym "dir"))
        (temp-file (gensym "temp-file")))
    `(let* ((,base ,temp-dir)
            (,dir (concat ,base "/" ,subdir))
            (,temp-file (concat ,dir "/ctags.txt"))
            (org-ctags-path-to-ctags
             (test-org-ctags/mock-command ,temp-file "ctags_mock"))
            ,buffer)
       (make-directory ,dir)
       (unwind-protect
           ;; `org-ctags' commands call `buffer-file-name'.
           (with-current-buffer
               (setq ,buffer (find-file-noselect ,temp-file))
             (insert "Should be overwritten by org-ctags mock script")
             (save-buffer)
             ,@body
             (test-org-ctags/get-args ,temp-file ,base "ctags_mock\n"))
         (kill-buffer ,buffer)
         (delete-file ,temp-file)
         (delete-directory ,dir)))))

;;;; Comparator to have informative failures:

(defun test-org-ctags/list-elements (lst &optional indicies)
  "Select INDICIES elements from LST list.

INDICIES should be sorted in growing order."
  (if (not (and indicies (listp lst)))
      lst
    (let (selected
          (prev 0))
      (dolist (i indicies (nreverse selected))
        (setq lst (nthcdr (- i prev) lst))
        (setq prev i)
        (push (car lst) selected)))))

(defun test-org-ctags/list-elements-equal-p
    (expect actual indicies &rest _comments)
  "Call `equal' for lists EXPECT and INDICIES elements from ACTUAL.

_COMMENTS should appear in failure message."
  (equal expect
         (test-org-ctags/list-elements actual indicies)))

(defun test-org-ctags/list-elements-equal-explain
    (expect actual indicies &rest _comments)
  "`ert-eplainer' for `test-org-ctags/list-elements-equal-p'."
  (if (listp actual)
      (list
       'selected-elements
       (test-org-ctags/list-elements actual indicies))
    "Shell command failed"))

(put 'test-org-ctags/list-elements-equal-p
     'ert-explainer
     'test-org-ctags/list-elements-equal-explain)

;;;; Tests:

(ert-deftest test-org-ctags/create-tags-escape ()
  "Test that `org-ctags-create-tags' escapes shell arguments."
  (let ((temp-dir (make-temp-file "test-org-ctags-" 'dir)))
    (unwind-protect
        (progn
          (should
           (test-org-ctags/list-elements-equal-p
            (list (format "--regex-orgmode=%s" org-ctags-tag-regexp))
            (test-org-ctags/with-fake-ctags temp-dir "regexp"
              (org-ctags-create-tags))
            '(2)
            "Regexp should be escaped."))

          (should
           (test-org-ctags/list-elements-equal-p
            '("TMPDIR/regular/ctags.txt")
            (test-org-ctags/with-fake-ctags temp-dir "regular"
              (org-ctags-create-tags (concat temp-dir "/regular")))
            '(7)
            "Wildcard should be expanded."
            "Directory passed as an argument."))

          (should
           (test-org-ctags/list-elements-equal-p
            '("TMPDIR/space char/TAGS" "TMPDIR/space char/ctags.txt")
            (test-org-ctags/with-fake-ctags temp-dir "space char"
              (org-ctags-create-tags (concat temp-dir "/space char")))
            '(4 7)
            "Space characters should not split arguments."
            "Directory passed as an argument."))

          (should
           (test-org-ctags/list-elements-equal-p
            '("TMPDIR/apostrophe' sep '/TAGS" "TMPDIR/apostrophe' sep '/ctags.txt")
            (test-org-ctags/with-fake-ctags temp-dir "apostrophe' sep '"
              (org-ctags-create-tags))
            '(4 7)
            "Apostrophes should be regular characters."
            "Path is derived from `default-directory'."))

          (should
           (test-org-ctags/list-elements-equal-p
            '("TMPDIR/def-dir.$HOME/TAGS" "TMPDIR/def-dir.$HOME/ctags.txt")
            (test-org-ctags/with-fake-ctags temp-dir "def-dir.$HOME"
              (org-ctags-create-tags))
            '(4 7)
            "$VARIABLES should not be expanded in directory names."
            "Path is derived from `default-directory'."))

          (should
           (test-org-ctags/list-elements-equal-p
            '("TMPDIR/arg.$HOME/TAGS" "TMPDIR/arg.$HOME/ctags.txt")
            (test-org-ctags/with-fake-ctags temp-dir "arg.$HOME"
              (org-ctags-create-tags (concat temp-dir "/arg.$HOME")))
            '(4 7)
            "$VARIABLES should not be expanded in directory names."
            "Directory passed as an argument")))
      (delete-directory temp-dir))))

(provide 'test-org-ctags)
;;; test-org.el ends here
