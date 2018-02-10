;;; ob-lua.el --- Execute Lua code within org-mode blocks.
;; Copyright 2016 stardiviner

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: org babel lua
;; URL: https://github.com/stardiviner/ob-lua
;; Created: 12th April 2016
;; Version: 0.0.1
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; Execute Lua code within org-mode blocks.

;;; Code:
(require 'org)
(require 'ob)

(defgroup ob-lua nil
  "org-mode blocks for Lua."
  :group 'org)

(defcustom ob-lua:default-session "*lua*"
  "Default Lua session.

It is lua inferior process from `run-lua'."
  :group 'ob-lua
  :type 'string)

;;;###autoload
(defun org-babel-execute:lua (body params)
  "org-babel lua hook."
  (let* ((session (or (cdr (assoc :session params))
                      ob-lua:default-session))
         (cmd (mapconcat 'identity (list "lua -") " ")))
    (org-babel-eval cmd body)))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("lua" . lua)))

(provide 'ob-lua)

;;; ob-lua.el ends here
