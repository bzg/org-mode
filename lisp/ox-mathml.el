;;; ox-mathml.el --- Support for MathML exports -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2026 Free Software Foundation, Inc.
;;
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: February 27, 2023
;;
;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  This library implements support for MathML exports.
;;
;;; Code:

(defgroup org-mathml nil
  "Options for generation of MathML representations of LaTeX math."
  :tag "Org MathML export"
  :group 'org-export)

(defcustom org-mathml-converter-jar-file nil
  "Value of\"%j\" in `org-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-mathml
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (file :tag "JAR file" :must-match t)))

(defcustom org-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-mathml-converter-jar-file'.
%I:     Input LaTeX file in fully expanded form.
%i:     Shell-escaped LaTeX fragment to be converted.
        It must not be used inside a quoted argument, the result of %i
        expansion inside a quoted argument is undefined.
%o:     Output MathML file.

This command is used by `org-mathml-convert-latex'.

When using MathToWeb as the converter, set this option to
\"java -jar %j -unicode -force -df %o %I\".

When using LaTeXML set this option to
\"latexmlmath %i --preload=amsmath.sty --preload=amssymb.sty\\
  --presentationmathml=%o\"."
  :group 'org-mathml
  :package-version '(Org . "10.0")
  :type '(choice
          (const :tag "None" nil)
          (string :tag "\nShell command")))

(defcustom org-mathml-export-directory "ltxmathml/"
  "Path to store MathML files converted from LaTeX fragments.
A relative path here creates many directories relative to the
processed Org files paths.  An absolute path puts all files
in the same place."
  :group 'org-mathml
  :package-version '(Org . "9.8")
  :type 'string
  :risky t)

(defun org-mathml-converter-available-p ()
  "Return t if `org-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-mathml-convert-command)
               org-mathml-convert-command)
      (let ((executable (car (split-string
                              org-mathml-convert-command))))
        (when (executable-find executable)
          (if (string-match
               "%j" org-mathml-convert-command)
              (file-readable-p org-mathml-converter-jar-file)
            t))))))

(defun org-mathml-convert-latex (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-mathml-convert-command'.  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (use-region-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))))
                       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (user-error "Invalid LaTeX fragment"))
  (let* ((tmp-in-file
          (let ((file (file-relative-name
                       (make-temp-name (expand-file-name "ltxmathml-in")))))
            (write-region latex-frag nil file)
            file))
         (tmp-out-file (file-relative-name
                        (make-temp-name (expand-file-name  "ltxmathml-out"))))
         (cmd (format-spec
               org-mathml-convert-command
               `((?j . ,(and org-mathml-converter-jar-file
                             (shell-quote-argument
                              (expand-file-name
                               org-mathml-converter-jar-file))))
                 (?I . ,(shell-quote-argument tmp-in-file))
                 (?i . ,(shell-quote-argument latex-frag))
                 (?o . ,(shell-quote-argument tmp-out-file)))))
         mathml shell-command-output)
    (when (called-interactively-p 'any)
      (unless (org-mathml-converter-available-p)
        (user-error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
          (when (file-readable-p tmp-out-file)
            (with-temp-buffer
              (insert-file-contents tmp-out-file)
              (goto-char (point-min))
              (when (re-search-forward
                     (format "<math[^>]*?%s[^>]*?>"
                             (regexp-quote
                              "xmlns=\"http://www.w3.org/1998/Math/MathML\""))
                     nil t)
                (let ((from (match-beginning 0)))
                  (when (re-search-forward "</math>" nil t)
                    (buffer-substring from (match-end 0))))))))
    (cond
     (mathml
      (setq mathml
            (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
        (write-region mathml nil mathml-file))
      (when (called-interactively-p 'any)
        (message mathml)))
     ((warn "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-mathml-convert-latex-cached (latex-frag)
  "Use `org-mathml-convert-latex' for LATEX-FRAG.

Check local cache first."
  (let ((latex-hash-path
         (expand-file-name
          (concat
           "org-mathml-formula-"
           (sha1
            (prin1-to-string
             (list latex-frag
                   org-mathml-convert-command)))
           ".mathml")
          org-mathml-export-directory))
        print-length print-level)
    (unless (file-exists-p latex-hash-path)
      (org-mathml-convert-latex latex-frag latex-hash-path))
    (and (file-exists-p latex-hash-path)
         latex-hash-path)))

(provide 'ox-mathml)
;;; ox-mathml.el ends here
