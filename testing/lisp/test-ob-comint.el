;;; test-ob-comint.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Matthew Trzcinski
;; Authors: Matthew Trzcinski

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

;; See testing/README for how to run tests.


;;; Requirements:


;;; Code:
(ert-deftest test-org-babel-comint/prompt-filter-removes-prompt ()
  "Test that prompt is actually removed."
  (let* ((prompt "org_babel_sh_prompt> ")
         (results "org_babel_sh_prompt> echo 'ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59'
# print message
echo \"hello world\"
echo 'ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59'
ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59
org_babel_sh_prompt> org_babel_sh_prompt> \"hello world\"
org_babel_sh_prompt> ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59
org_babel_sh_prompt> "))
    (should (string=
             (org-trim (string-join (mapcar #'org-trim (org-babel-comint--prompt-filter results prompt)) "\n") "\n")
             "echo 'ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59'
# print message
echo \"hello world\"
echo 'ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59'
ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59
\"hello world\"
ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59"))))

(ert-deftest test-org-babel-comint/echo-filter-removes-echo ()
  "Test that echo is actually removed."
  (let* ((echo "echo 'ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59'
# print message
echo \"hello world\"
echo 'ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59'")
         (result "org_babel_sh_prompt> echo 'ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59'
# print message
echo \"hello world\"
echo 'ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59'
ob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59
org_babel_sh_prompt> org_babel_sh_prompt> \"hello world\"
org_babel_sh_prompt> ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59
org_babel_sh_prompt> "))
    (should (string=
             (org-babel-comint--echo-filter result echo)
             "\nob_comint_async_shell_start_d78ac49f-dc8a-4c39-827c-c93225484d59
org_babel_sh_prompt> org_babel_sh_prompt> \"hello world\"
org_babel_sh_prompt> ob_comint_async_shell_end_d78ac49f-dc8a-4c39-827c-c93225484d59
org_babel_sh_prompt> "))))

(provide 'test-ob-comint)

;;; test-ob-comint.el ends here
