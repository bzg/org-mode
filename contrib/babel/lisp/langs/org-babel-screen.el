;;; org-babel-screen.el --- org-babel support for interactive terminal

;; Copyright (C) 2009 Benjamin Andresen

;; Author: Benjamin Andresen
;; Keywords: literate programming, interactive shell
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for interactive terminals. Mostly shell scripts.
;; Heavily inspired by 'eev' from Eduardo Ochs
;;
;; Adding :cmd and :terminal as header arguments
;; :terminal must support the -T (title) and -e (command) parameter
;;
;; You can test the default setup. (xterm + sh) with
;; M-x org-babel-screen-test RET

;;; Code:
(require 'org-babel)

(org-babel-add-interpreter "screen")

(add-to-list 'org-src-lang-modes '("screen" . sh))

(defvar org-babel-screen-location "screen"
  "The command location for screen. 
In case you want to use a different screen than one selected by your $PATH")

(defvar org-babel-default-header-args:screen
  '((:results . "silent") (:session . "default") (:cmd . "sh") (:terminal . "xterm"))
  "Default arguments to use when running screen source blocks.")

(defun org-babel-execute:screen (body params)
  "Send a block of code via screen to a terminal using org-babel.
\"default\" session is be used when none is specified."
  (message "Sending source code block to interactive terminal session...")
  (save-window-excursion
    (let* ((processed-params (org-babel-process-params params))
           (session (first processed-params))
           (socket (org-babel-screen-session-socketname session)))
      (unless socket (org-babel-prep-session:screen session params))
      (org-babel-screen-session-execute-string session body))))

(defun org-babel-prep-session:screen (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((processed-params (org-babel-process-params params))
         (session (first processed-params))
         (vars (second processed-params))
         (socket (org-babel-screen-session-socketname session))
         (vars (org-babel-ref-variables params))
         (cmd (cdr (assoc :cmd params)))
         (terminal (cdr (assoc :terminal params)))
         (process-name (concat "org-babel: terminal (" session ")")))
    (apply 'start-process process-name "*Messages*"
           terminal `("-T" ,(concat "org-babel: " session) "-e" ,org-babel-screen-location
                           "-c" "/dev/null" "-mS" ,(concat "org-babel-session-" session)
                           ,cmd))
    ;; XXX: Is there a better way than the following?
    (while (not (org-babel-screen-session-socketname session))
      ;; wait until screen session is available before returning
      )))

;; helper functions

(defun org-babel-screen-session-execute-string (session body)
  "If SESSION exist, send BODY to it."
  (let ((socket (org-babel-screen-session-socketname session)))
    (when socket
      (let ((tmpfile (org-babel-screen-session-write-temp-file session body)))
        (apply 'start-process (concat "org-babel: screen (" session ")") "*Messages*"
               org-babel-screen-location
               `("-S" ,socket "-X" "eval" "msgwait 0"
                      ,(concat "readreg z " tmpfile)
                      "paste z"))))))

(defun org-babel-screen-session-socketname (session)
  "Check if SESSION exist by parsing output of \"screen -ls\"."
  (let* ((screen-ls (shell-command-to-string "screen -ls"))
         (sockets (remove-if-not
                   '(lambda (x)
                     (string-match (rx (or "(Attached)" "(Detached)")) x))
                   (split-string screen-ls "\n")))
         (match-socket (find-if
                        '(lambda (x)
                          (string-match (concat "org-babel-session-" session) x))
                        sockets)))
    (when match-socket (car (split-string match-socket)))))

(defun org-babel-screen-session-write-temp-file (session body)
  "Save BODY in a temp file that is named after SESSION."
  (let ((tmpfile (concat "/tmp/screen.org-babel-session-" session)))
    (with-temp-file tmpfile
      (insert body)

      ;; org-babel has superflous spaces
      (goto-char (point-min))
      (delete-matching-lines "^ +$"))
    tmpfile))

(defun org-babel-screen-test ()
  "Command that tests if the default setup works. 
The terminal should shortly flicker."
  (interactive)
  (let* ((session "org-babel-testing")
         (random-string (format "%s" (random 99999)))
         (tmpfile "/tmp/org-babel-screen.test")
         (body (concat "echo '" random-string "' > " tmpfile "\nexit\n"))
         process tmp-string)
    (org-babel-execute:screen body org-babel-default-header-args:screen)
    ;; XXX: need to find a better way to do the following
    (while (not (file-readable-p tmpfile))
      ;; do something, otherwise this will be optimized away
      (format "org-babel-screen: File not readable yet."))
    (setq tmp-string (with-temp-buffer
                       (insert-file-contents-literally tmpfile)
                       (buffer-substring (point-min) (point-max))))
    (delete-file tmpfile)
    (message (concat "org-babel-screen: Setup "
                     (if (string-match random-string tmp-string)
                         "WORKS."
                         "DOESN'T work.")))))

(provide 'org-babel-screen)
;;; org-babel-screen.el ends here
