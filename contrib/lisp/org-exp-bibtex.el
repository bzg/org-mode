;;; org-exp-bibtex.el --- Export bibtex fragments

;; Copyright (C) 2009 Taru Karttunen

;; Author: Taru Karttunen <taruti@taruti.net >

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is an utility to handle BibTeX export to both LaTeX and html
;; exports. It uses the bibtex2html software from
;; http://www.lri.fr/~filliatr/bibtex2html/
;;
;; The usage is as follows:
;; #+BIBLIOGRAPHY: bibfilebasename stylename
;; e.g. given foo.bib and using style plain:
;; #+BIBLIOGRAPHY: foo plain

;; For LaTeX export this simply inserts the lines
;; \bibliographystyle{plain}
;; \bibliography{foo}
;; into the tex-file when exporting.

;; For Html export it:
;; 1) converts all \cite{foo} to links to the bibliography
;; 2) creates a foo.html and foo_bib.html
;; 3) includes the contents of foo.html in the exported html file


(defun org-export-bibtex-preprocess ()
  "Export all BibTeX."
  (interactive)
  (save-window-excursion
    (setq oebp-cite-plist '())
    ;; Convert #+BIBLIOGRAPHY: name style
    (goto-char (point-min))
    (while (re-search-forward "^#\\+BIBLIOGRAPHY:\\s-+\\(\\w+\\)\\s-+\\(\\w+\\)" nil t)
      (let ((file  (match-string 1))
	    (style (match-string 2)))
	(replace-match
	(cond
	 (htmlp ;; We are exporting to HTML
	  (call-process "bibtex2html" nil nil nil "--nodoc"  "--style" style "--no-header" (concat file ".bib"))
	  (with-temp-buffer
	    (save-match-data
	      (insert-file-contents (concat file ".html"))
	      (goto-char (point-min))
	      (while (re-search-forward "a name=\"\\(\\w+\\)\">\\(\\w+\\)" nil t)
		(setq oebp-cite-plist (cons (cons (match-string 1) (match-string 2)) oebp-cite-plist)))
	      (goto-char (point-min))
	      (while (re-search-forward "<hr>" nil t)
		(replace-match "<hr/>"))
	      (concat "\n#+BEGIN_HTML\n<div class=\"bibliography\">\n" (buffer-string) "\n</div>\n#+END_HTML\n"))))
	 (latexp ;; Latex export
	  (concat "\n#+LATEX: \\\\bibliographystyle{" style "}"
		  "\n#+LATEX: \\\\bibliography{" file "}\n"))))))
    ;; Convert cites to links in html
    (goto-char (point-min))
    (when htmlp
      (while (re-search-forward "\\\\cite{\\(\\w+\\)}" nil t)
	(let* ((cn (match-string 1))
	       (cv (assoc cn oebp-cite-plist)))
	  (replace-match 
	   (concat "\[_{}[[" cn "][" (if cv (cdr cv) cn) "]]\]")))))))


(add-hook 'org-export-preprocess-hook 'org-export-bibtex-preprocess)

(provide 'org-exp-bibtex)

;;; org-exp-bibtex.el ends here
