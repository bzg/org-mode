;;; org-dependencies.el --- Ensure dependencies are met in the development repository  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Morgan Smith

;; Author: Morgan Smith <Morgan.J.Smith@outlook.com>
;; Keywords:

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

;;; Code:

(require 'package)
(require 'lisp-mnt)

;; Emacs < 30
(unless (fboundp 'lm-package-requires)
  (defun lm--prepare-package-dependencies (deps)
    "Turn DEPS into an acceptable list of dependencies.

Any parts missing a version string get a default version string
of \"0\" (meaning any version) and an appropriate level of lists
is wrapped around any parts requiring it."
    (cond
     ((not (listp deps))
      (error "Invalid requirement specifier: %S" deps))
     (t (mapcar (lambda (dep)
                  (cond
                   ((symbolp dep) `(,dep "0"))
                   ((stringp dep)
                    (error "Invalid requirement specifier: %S" dep))
                   ((and (listp dep) (null (cdr dep)))
                    (list (car dep) "0"))
                   (t dep)))
                deps))))

  (declare-function package-read-from-string "package" (str))

  (defun lm-package-requires (&optional file)
    "Return dependencies listed in file FILE, or current buffer if FILE is nil.
The return value is a list of elements of the form (PACKAGE VERSION)
where PACKAGE is the package name (a symbol) and VERSION is the
package version (a string)."
    (require 'package)
    (lm-with-file file
      (and-let* ((require-lines (lm-header-multiline "package-requires")))
        (lm--prepare-package-dependencies
         (package-read-from-string (mapconcat #'identity require-lines " ")))))))

(defun org-dependencies-pretty-print-instructions (instructions)
  "Print the INSTRUCTIONS to the terminal for the user.
Printing is done with `error' so Emacs will terminate and allow the user
to perform what is asked of them."
  (with-temp-buffer
    (insert "================================================================\n")
    (dolist (line (split-string instructions "\n"))
      (insert (format "= %-60s =\n" line)))
    (insert "================================================================\n")
    (let ((backtrace-on-error-noninteractive nil))
      (error "%s" (buffer-string)))))

(defun org-dependencies ()
  "Get a list of dependencies from org.el.
Each item is a list of (PACKAGE MINIMUM-VERSION)."
  (lm-package-requires (locate-file "org.el" load-path)))

(defun org-install-dependencies ()
  "Install missing dependencies from `org-dependencies'."
  (let ((backtrace-on-error-noninteractive nil))
    (dolist (dep (org-dependencies))
      (let ((package (car dep))
            (version (nth 1 dep)))
        (unless (package-installed-p package (version-to-list version))
          (let ((package-source-dir (concat "./deps/" (symbol-name package) "/")))
            (unless (file-exists-p (concat package-source-dir (symbol-name package) ".el"))
              (org-dependencies-pretty-print-instructions
               (format
                "ERROR: Cannot find %S locally.
Try updating the git submodule.
You can do so with this command:
  git submodule update --init --depth=1 -- %S"
                package package-source-dir)))
            (let ((package-install-upgrade-built-in t)
                  (package-archives nil))
              (package-install-file (expand-file-name package-source-dir)))
            (unless (package-installed-p package (version-to-list version))
              (org-dependencies-pretty-print-instructions
               (format
                "ERROR: Dependency %S is still not satisfied after building.
Try updating the git submodule.
You can do so with this command:
  git submodule update --init --depth=1 -- %S"
                package package-source-dir)))))))))

(provide 'org-dependencies)
;;; org-dependencies.el ends here
