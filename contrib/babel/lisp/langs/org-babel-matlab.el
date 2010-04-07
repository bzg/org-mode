;;; org-babel-matlab.el --- org-babel support for matlab evaluation

;; Copyright (C) Dan Davison

;; Author: Dan Davison
;; Keywords: literate programming, reproducible research
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

;; Functions that are common to org-babel support for matlab and
;; octave are in org-babel-octave.el

;;; Requirements:

;; Matlab

;; matlab.el required for interactive emacs sessions and matlab-mode
;; major mode for source code editing buffer
;; http://matlab-emacs.sourceforge.net/

;;; Code:
(require 'matlab)
(require 'org-babel-octave)

(org-babel-add-interpreter "matlab")
(add-to-list 'org-babel-tangle-langs '("matlab" "m" "#!/usr/bin/env matlab"))


(defvar org-babel-matlab-shell-command "matlab -nosplash"
  "Shell command to use to run matlab as an external process.")

(defun org-babel-execute:matlab (body params)
  "Execute a block of matlab code with org-babel."
  (org-babel-execute:octave body params 'matlab))

(defun org-babel-prep-session:matlab (session params)
  (org-babel-prep-session:octave session params 'matlab))

(defun org-babel-matlab-initiate-session (&optional session params)
  "Create matlab inferior process buffer.
If there is not a current inferior-process-buffer in SESSION
then create. Return the initialized session."
  (org-babel-octave-initiate-session session params 'matlab))

(provide 'org-babel-matlab)
;;; org-babel-matlab.el ends here
