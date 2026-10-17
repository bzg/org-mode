;;; test-org-persist.el --- Tests for org-persist.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Derek Chen-Becker

;; Author: Derek Chen-Becker <oss at chen-becker dot org>

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


;;; Commentary:
;;

;;; Code:

(require 'org-test "../testing/org-test")

(ert-deftest test-org-persist/refresh-gc-lock ()
  "Test that the GC lock refresh properly handles valid and expired sessions."
  (let* ((org-persist-gc-lock-expiry 60) ;; Set expiry to keep test data simple
         (org-persist--wrote-to-disk t) ;; Make sure we actually trigger GC
         (test-directory (make-temp-file "org-persist-test" t))
         (org-persist-directory test-directory)
         (lockfile (concat test-directory "/" org-persist-gc-lock-file))
         ;; Started 42 seconds ago, should stay
         (current-record `(,(time-subtract nil 42) . ,(current-time)))
         ;; Ended 95 seconds ago, should be expired
         (expired-record `(,(time-subtract nil 120) . ,(time-subtract nil 95))))
    (unwind-protect
        (progn
          (org-persist--write-elisp-file lockfile (list current-record expired-record))
          (org-persist--refresh-gc-lock)
          ;; Now re-read the lock file and ensure that all entries are current now
          (dolist (record (org-persist--read-elisp-file lockfile))
                  (should-not
                   (> (float-time (time-subtract nil (cdr record)))
                      org-persist-gc-lock-expiry))))
        (delete-directory test-directory t))))

(provide 'test-org-persist)
;;; test-org-persist.el ends here
