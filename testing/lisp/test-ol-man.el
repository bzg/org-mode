;;; test-ol-man.el --- tests for ol-man.el -*- lexical-binding: t; -*-

;; Author:  <Catsup4@proton.me>
;; Keywords: outlines, hypermedia, text

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

;; Links to man pages rely on a regexp that uses capture groups to
;; extract the man command and a search string to be run within the
;; given man page.  These tests exersise that logic.

;;; Code:
(require 'ol-man)

(defun test-org-man/open-fixture (body &optional search)
  "Follow [[man:fake(1)::SEARCH]] link and run BODY function in fake man buffer.
When SEARCH is nil, follow [[man:fake(1)]] link.
Avoid running system man command.
The mock man page buffer is killed after the test BODY is run."
  (let* ((fake-man-buffer-name "*Man 1 fake*")
         (org-man-command
          (lambda (_)
            (with-current-buffer (get-buffer-create fake-man-buffer-name)
              (insert "(1)\t\tGNU\t\t fake (1)
line 2 of this fake man page
line 3 of fake man page for the command fake(1)
")
              (goto-char (point-min))
              ;; Return the buffer
              (current-buffer))))
         (org-link (concat "fake(1)" (when search (concat "::" search)))))
    (unwind-protect
        (progn
          ;; the second argument is unused in org-man-open
          (org-man-open org-link nil)
          (with-current-buffer fake-man-buffer-name
            (funcall body)))
      (kill-buffer fake-man-buffer-name))))

(ert-deftest test-org-man/open-no-search ()
  (test-org-man/open-fixture (lambda () (should (bobp)))))

(ert-deftest test-org-man/open-with-found-search ()
  ;; man: links jump to one line before the match
  (test-org-man/open-fixture
   (lambda ()
     (should
      (save-excursion (forward-line 1) (looking-at-p "^.*Line 3"))))
   "Line 3"))

(ert-deftest test-org-man/open-with-unfound-search ()
  (should-error
   (test-org-man/open-fixture #'ignore "not in the man page")))


(provide 'test-ol-man)

;;; test-ol-man.el ends here
