;;; test-ox-man.el --- Tests from ox-man.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>

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

(require 'ox-man)

(defvar ox-man/groff-executable (executable-find "groff")
  "Contain path to groff executable of nil when there is no such path.")

(defmacro ox-man/test-with-exported-test (source &rest body)
  "Run BODY in export buffer for SOURCE string exported to man.
Throw an error when exported text does not pass groff linter (if groff
executable is available)."
  (declare (indent 1))
  `(org-test-with-exported-text 'man ,source
     (when ox-man/groff-executable
       (let ((output (generate-new-buffer " *groff-linter-output*")))
         (unwind-protect
             (progn
               (call-process-region
                (point-min) (point-max) ox-man/groff-executable
                nil (list output t) nil
                "-zww" "-rCHECKSTYLE=3" "-man" "-")
               (with-current-buffer output
                 (should
                  (string-empty-p
                   (thread-last
                     (buffer-string)
                     ;; FIXME: ox-man does not yet support passing a
                     ;; project name.
                     (replace-regexp-in-string
                      (rx bol (0+ any)
                          "style: .TH missing fourth argument; "
                          "suggest package/project name and version"
                          (0+ any) eol)
                      "")
                     ;; We do support date via #+DATE keyword, but it
                     ;; is not enabled by default for historical
                     ;; reasons.
                     (replace-regexp-in-string
                      (rx bol (0+ any)
                          "style: .TH missing third argument;"
                          " suggest document modification"
                          " date in ISO 8601 format (YYYY-MM-DD)"
                          (0+ any) eol)
                      "")
                     (replace-regexp-in-string "\n+" ""))))))
           (kill-buffer output))))
     ,@body))

(ert-deftest ox-man/bold ()
  "Test bold text."
  (ox-man/test-with-exported-test "*bold* text"
    (should (search-forward "\\fBbold\\fP text"))))

(ert-deftest ox-man/code ()
  "Test text formatted as code."
  (ox-man/test-with-exported-test "~code~"
    (should (search-forward "\\fCcode\\fP"))))

(ert-deftest ox-man/italic-underlined-verbatim ()
  "Test italic, underlined and verbatim text."
  (ox-man/test-with-exported-test "/italic/, _underlined_, =verbatim="
    (should (search-forward "\\fIitalic\\fP"))
    (should (search-forward "\\fIunderlined\\fP"))
    (should (search-forward "\\fIverbatim\\fP"))))

(provide 'test-ox-man)
;;; test-ox-man.el ends here
