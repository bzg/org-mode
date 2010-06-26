;;; ob-matlab.el --- org-babel support for matlab evaluation

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
(require 'ob-octave)

(defvar org-babel-default-header-args:matlab '())

(defvar org-babel-matlab-shell-command "matlab -nosplash"
  "Shell command to use to run matlab as an external process.")

(defun org-babel-expand-body:matlab (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defvar org-babel-matlab-with-emacs-link nil
  "If non-nil use matlab-shell-run-region for session
  evaluation. This will use EmacsLink if (matlab-with-emacs-link)
  evaluates to a non-nil value.")

(defvar org-babel-matlab-emacs-link-wrapper-method
   "%s
if ischar(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
else, save -ascii %s ans
end
delete('%s')
")

(defun org-babel-execute:matlab (body params)
  "Execute a block of matlab code with org-babel."
  (org-babel-execute:octave body params 'matlab))

(defun org-babel-prep-session:matlab (session params)
  "Prepare SESSION according to PARAMS."
  (org-babel-prep-session:octave session params 'matlab))

(defun org-babel-matlab-initiate-session (&optional session params)
  "Create a matlab inferior process buffer.  If there is not a
current inferior-process-buffer in SESSION then create. Return
the initialized session."
  (org-babel-octave-initiate-session session params 'matlab))

(provide 'ob-matlab)

;; arch-tag: 6b234299-c1f7-4eb1-ace8-7b93344065ac

;;; ob-matlab.el ends here
