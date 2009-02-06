(require 'org)

(add-to-list 'auto-mode-alist '("\\.[SRsr][Oo]rg\\'" . org-mode))
(setq org-edit-src-region-extra
      '(
        ;; Use muse-style tags
        ("<[sr]>[ \t]*\n?" "\n?[ \t]*</[sr]>" "r") ; not
                                        ; understood by
                                        ; SweaveSyntaxOrg
                                        ; at this point.
                                        ; Maybe make
                                        ; SweaveSyntaxMuse?
        

        ;; R-environments when using SweaveSyntaxLatex
        ("^[ \t]*\\\\begin{[sr]code}\\s-*"
         "\n[ \t]*\\\\end{[sr]code}\\s-*"
         "r")                    ;Scode environment in raw LaTeX
        ("^#\\+latex:[ \t]*\\\\begin{[sr]code}\\s-*"
         "\n#\\+latex:[ \t]*\\\\end{[sr]code}\\s-*"
         "r")                    ;Scode environment in a #+latex block
        ("\\\\Sexpr{"
         "}"
         "r")        ; embed [SR] expressions in text

        ;; R environments when using SweaveSyntaxOrg
        ("^#\\+begin_[sr]\\s-*"
         "\n#\\+end_[sr]\\s-*"
         "r")                    ; an S code block in SweaveSyntaxOrg
        ("^#\\+[sr]_file:?[ \t]*"
         "\n"
         "r")
        ("\\\\[sr]{"
         "}"
         "r")
        ))

(setq org-export-latex-append-header "\\usepackage{Sweave}")

(provide 'org-sweave)
