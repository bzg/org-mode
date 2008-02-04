;;; org-mac-message.el - Support for links to Apple Mail messages by Message-ID
;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2008 John Wiegley
;;
;; Author: John Wiegey <johnw@gnu.org>
;; Version: 1.1
;; Keywords: outlines, hypermedia, calendar, wp
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(org-add-link-type "message" 'org-mac-message-open)

(unless (fboundp 'do-applescript)
  ;; Need to fake this using shell-command-to-string
  (defun do-applescript (script)
    (let (start return)
      (while (string-match "\n" script)
	(setq script (replace-match "\r" t t script)))
      (while (string-match "'" script start)
	(setq start (+ 2 match-beginning 0)
	      script (replace-match "\\'" t t script)))
      (setq cmd (concat "osascript -e '" script "'"))
      (setq return (shell-command-to-string cmd))
      (concat "\"" (org-trim return) "\""))))

(defun org-mac-message-open (message-id)
  "Visit the nnml message with the given Message-ID."
  (start-process (concat "open message:" message-id) nil
		 "open" (concat "message:" message-id)))

(defun org-mac-insert-message-link ()
  (interactive)
  (let ((subject (do-applescript "tell application \"Mail\"
	set theMessages to selection
	subject of beginning of theMessages
end tell"))
	(message-id (do-applescript "tell application \"Mail\"
	set theMessages to selection
	message id of beginning of theMessages
end tell")))
    (insert (org-make-link-string
	     (concat "message://"
		     (substring message-id 1 (1- (length message-id))))
	     (substring subject 1 (1- (length subject)))))))

(provide 'org-mac-message)

;;; org-mac-message.el ends here
