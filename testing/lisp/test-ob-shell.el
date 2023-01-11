;;; test-ob-shell.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2010-2014, 2019 Eric Schulte
;; Authors: Eric Schulte

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

;;; Comment:

;; Template test file for Org tests

;;; Code:
(org-test-for-executable "sh")
(require 'ob-core)
(unless (featurep 'ob-shell)
  (signal 'missing-test-dependency "Support for Shell code blocks"))

(ert-deftest test-ob-shell/dont-insert-spaces-on-expanded-bodies ()
  "Expanded shell bodies should not start with a blank line
unless the body of the tangled block does."
  (should-not (string-match "^[\n\r][\t ]*[\n\r]"
			    (org-babel-expand-body:generic "echo 2" '())))
  (should (string-match "^[\n\r][\t ]*[\n\r]"
			(org-babel-expand-body:generic "\n\necho 2" '()))))

(ert-deftest test-ob-shell/dont-error-on-empty-results ()
  "Was throwing an elisp error when shell blocks threw errors and
returned empty results."
  (should (null (org-babel-execute:sh "ls NoSuchFileOrDirectory.txt" nil))))

(ert-deftest test-ob-shell/session ()
  "This also tests `org-babel-comint-with-output' in
ob-comint.el, which was not previously tested."
  (let ((res (org-babel-execute:sh "echo 1; echo 2" '((:session . "yes")))))
    (should res)
    (should (listp res)))
  ;; Test multi-line input.
  (let ((result (org-babel-execute:sh
		 "if true \n then \n echo yes \n fi"
		 '((:session . "yes")))))
    (should result)
    (should (string= "yes" result))))

; A list of tests using the samples in ob-shell-test.org
(ert-deftest ob-shell/generic-uses-no-arrays ()
  "No arrays for generic"
  (org-test-at-id "0ba56632-8dc1-405c-a083-c204bae477cf"
    (org-babel-next-src-block)
    (should (equal "one two three" (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/bash-uses-arrays ()
  "Bash arrays"
  (org-test-at-id "0ba56632-8dc1-405c-a083-c204bae477cf"
    (org-babel-next-src-block 2)
    (should (equal "one" (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/generic-uses-no-assoc-arrays ()
  "No associative arrays for generic"
  (should
   (equal "first one second two third three"
	  (org-test-at-id
	   "bec1a5b0-4619-4450-a8c0-2a746b44bf8d"
	   (org-babel-next-src-block)
	   (org-trim (org-babel-execute-src-block)))))
  (should
   (equal "bread 2 kg spaghetti 20 cm milk 50 dl"
	  (org-test-at-id
	   "82320a48-3409-49d7-85c9-5de1c6d3ff87"
	   (org-babel-next-src-block)
	   (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/bash-uses-assoc-arrays ()
  "Bash associative arrays"
  (should
   (equal "two"
	  (org-test-at-id
	   "bec1a5b0-4619-4450-a8c0-2a746b44bf8d"
	   (org-babel-next-src-block 2)
	   (org-trim (org-babel-execute-src-block)))))
  ;; Bash associative arrays as strings for the row.
  (should
   (equal "20 cm"
	  (org-test-at-id
	   "82320a48-3409-49d7-85c9-5de1c6d3ff87"
	   (org-babel-next-src-block 2)
	   (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/simple-list ()
  "Test list variables in shell."
  ;; With bash, a list is turned into an array.
  (should
   (equal "2"
	  (org-test-with-temp-text
	   "#+BEGIN_SRC bash :results output :var l='(1 2)\necho ${l[1]}\n#+END_SRC"
	   (org-trim (org-babel-execute-src-block)))))
  ;; On sh, it is a string containing all values.
  (should
   (equal "1 2"
	  (org-test-with-temp-text
	   "#+BEGIN_SRC sh :results output :var l='(1 2)\necho ${l}\n#+END_SRC"
	   (org-trim (org-babel-execute-src-block))))))

(ert-deftest ob-shell/remote-with-stdin-or-cmdline ()
  "Test :stdin and :cmdline with a remote directory."
  ;; We assume `default-directory' is a local directory.
  (skip-unless (not (memq system-type '(ms-dos windows-nt))))
  (org-test-with-tramp-remote-dir remote-dir
      (dolist (spec `( ()
                       (:dir ,remote-dir)
                       (:dir ,remote-dir :cmdline t)
                       (:dir ,remote-dir :stdin   t)
                       (:dir ,remote-dir :cmdline t :shebang t)
                       (:dir ,remote-dir :stdin   t :shebang t)
                       (:dir ,remote-dir :cmdline t :stdin t :shebang t)
                       (:cmdline t)
                       (:stdin   t)
                       (:cmdline t :shebang t)
                       (:stdin   t :shebang t)
                       (:cmdline t :stdin t :shebang t)))
        (let ((default-directory (or (plist-get spec :dir) default-directory))
              (org-confirm-babel-evaluate nil)
              (params-line "")
              (who-line "  export who=tramp")
              (args-line "  echo ARGS: --verbose 23 71"))
          (when-let ((dir (plist-get spec :dir)))
            (setq params-line (concat params-line " " ":dir " dir)))
          (when (plist-get spec :stdin)
            (setq who-line "  read -r who")
            (setq params-line (concat params-line " :stdin input")))
          (when (plist-get spec :cmdline)
            (setq args-line "  echo \"ARGS: $*\"")
            (setq params-line (concat params-line " :cmdline \"--verbose 23 71\"")))
          (when (plist-get spec :shebang)
            (setq params-line (concat params-line " :shebang \"#!/bin/sh\"")))
          (let* ((result (org-test-with-temp-text
                             (mapconcat #'identity
                                        (list "#+name: input"
                                              "tramp"
                                              ""
                                              (concat "<point>"
                                                      "#+begin_src sh :results output " params-line)
                                              args-line
                                              who-line
                                              "  echo \"hello $who from $(pwd)/\""
                                              "#+end_src")
                                        "\n")
                           (org-trim (org-babel-execute-src-block))))
                 (expected (concat "ARGS: --verbose 23 71"
                                   "\nhello tramp from " (file-local-name default-directory))))
            (should (equal result expected)))))))

(ert-deftest ob-shell/results-table ()
  "Test :results table."
  (should
   (equal '(("I \"want\" it all"))
	  (org-test-with-temp-text
	      "#+BEGIN_SRC sh :results table\necho 'I \"want\" it all'\n#+END_SRC"
	    (org-babel-execute-src-block)))))

(ert-deftest ob-shell/results-list ()
  "Test :results list."
  (org-test-with-temp-text
      "#+BEGIN_SRC sh :results list\necho 1\necho 2\necho 3\n#+END_SRC"
    (should
     (equal '((1) (2) (3))
            (org-babel-execute-src-block)))
    (search-forward "#+results")
    (beginning-of-line 2)
    (should
     (equal
      "- 1\n- 2\n- 3\n"
      (buffer-substring-no-properties (point) (point-max))))))

;;; Standard output

(ert-deftest ob-shell/standard-output-after-success ()
  "Test standard output after exiting with a zero code."
  (should (= 1
             (org-babel-execute:sh
              "echo 1" nil))))

(ert-deftest ob-shell/standard-output-after-failure ()
  "Test standard output after exiting with a non-zero code."
  (should (= 1
             (org-babel-execute:sh
              "echo 1; exit 2" nil))))

;;; Standard error

(ert-deftest ob-shell/error-output-after-success ()
  "Test that standard error shows in the error buffer, alongside the
exit code, after exiting with a zero code."
  (should
   (string= "1
[ Babel evaluation exited with code 0 ]"
            (progn (org-babel-eval-wipe-error-buffer)
                   (org-babel-execute:sh
                    "echo 1 >&2" nil)
                   (with-current-buffer org-babel-error-buffer-name
                     (buffer-string))))))

(ert-deftest ob-shell/error-output-after-failure ()
  "Test that standard error shows in the error buffer, alongside the
exit code, after exiting with a non-zero code."
  (should
   (string= "1
[ Babel evaluation exited with code 2 ]"
            (progn (org-babel-eval-wipe-error-buffer)
                   (org-babel-execute:sh
                    "echo 1 >&2; exit 2" nil)
                   (with-current-buffer org-babel-error-buffer-name
                     (buffer-string))))))

(ert-deftest ob-shell/error-output-after-failure-multiple ()
  "Test that multiple standard error strings show in the error
buffer, alongside multiple exit codes."
  (should
   (string= "1
[ Babel evaluation exited with code 2 ]
3
[ Babel evaluation exited with code 4 ]"
            (progn (org-babel-eval-wipe-error-buffer)
                   (org-babel-execute:sh
                    "echo 1 >&2; exit 2" nil)
                   (org-babel-execute:sh
                    "echo 3 >&2; exit 4" nil)
                   (with-current-buffer org-babel-error-buffer-name
                     (buffer-string))))))

;;; Exit codes

(ert-deftest ob-shell/exit-code ()
  "Test that the exit code shows in the error buffer after exiting
with a non-zero return code."
  (should
   (string= "[ Babel evaluation exited with code 1 ]"
            (progn (org-babel-eval-wipe-error-buffer)
                   (org-babel-execute:sh
                    "exit 1" nil)
                   (with-current-buffer org-babel-error-buffer-name
                     (buffer-string))))))

(ert-deftest ob-shell/exit-code-multiple ()
  "Test that multiple exit codes show in the error buffer after
exiting with a non-zero return code multiple times."
  (should
   (string= "[ Babel evaluation exited with code 1 ]
[ Babel evaluation exited with code 2 ]"
            (progn (org-babel-eval-wipe-error-buffer)
                   (org-babel-execute:sh
                    "exit 1" nil)
                   (org-babel-execute:sh
                    "exit 2" nil)
                   (with-current-buffer org-babel-error-buffer-name
                     (buffer-string))))))

(provide 'test-ob-shell)

;;; test-ob-shell.el ends here
