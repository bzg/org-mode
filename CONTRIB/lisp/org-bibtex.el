;;; org-bibtex.el --- Org links to BibTeX entries
;;
;; Copyright 2007 Bastien Guerry
;;
;; Author: bzg AT altern DOT org
;; Version: 0.2
;; Keywords: org, wp, remember
;; URL: http://www.cognition.ens.fr/~guerry/u/org-bibtex.el
;;
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; The Org mode already lets you store/insert links to BibTeX entries.
;;
;; But what if you want to insert the author or the title of a BibTeX
;; item in a *remember* buffer?  This library lets you deal with this 
;; by adding more properties to the BibTeX link.
;;
;; The available properties for each entry are listed here:
;;
;; :author        :publisher      :volume      :pages
;; :editor        :url            :number      :journal
;; :title         :year           :series      :address
;; :booktitle     :month          :annote      :abstract
;; :key           :btype
;; 
;; Here is an example of a remember template that use some of this
;; information (:author :year :title :journal :pages):
;; 
;; (setq org-remember-templates 
;;   '((?b "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
;;          In %:journal, %:pages.")))
;; 
;; Let's say you want to remember this BibTeX entry:
;; 
;; @Article{dolev83,
;;   author = 	 {Danny Dolev and Andrew C. Yao},
;;   title = 	 {On the security of public-key protocols},
;;   journal = 	 {IEEE Transaction on Information Theory},
;;   year = 	 1983,
;;   volume =	 2,
;;   number =	 29,
;;   pages =	 {198--208},
;;   month =	 {Mars}
;; }
;; 
;; M-x `org-remember' on this entry will produce this buffer:
;; 
;; =====================================================================
;; * READ <== [point here]
;; 
;; [[file:/file.bib::dolev83][Dolev & Yao 1983: security of public key protocols]]
;; 
;; Danny Dolev and Andrew C. Yao (1983): On the security of public-key protocols
;; In IEEE Transaction on Information Theory, 198--208.
;; =====================================================================
;;
;;; History:
;; 
;; This piece of code was inspired by a request of Austin Frank:
;;   http://article.gmane.org/gmane.emacs.orgmode/4112
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-bibtex)

;;; Code:

(provide 'org-bibtex)

(require 'org)

(defvar description nil) ; dynamically scoped in org.el

(org-add-link-type "bibtex" 'org-bibtex-open)
(add-hook 'org-store-link-functions 'org-bibtex-store-link)

;; (defun org-bibtex-publish (path)
;;   "Build the description of the BibTeX entry for publishing."
;;   (let* ((search (when (string-match "::\\(.+\\)\\'" path)
;; 		   (match-string 1 path)))
;; 	 (path (substring path 0 (match-beginning 0)))
;; 	 key)
;;     (with-temp-buffer
;;       (org-open-file path t nil search)
;;       (setq key (org-create-file-search-functions)))
;;     (or description key)))

(defun org-bibtex-open (path)
  "Visit the bibliography entry on PATH."
  (let* ((search (when (string-match "::\\(.+\\)\\'" path)
		   (match-string 1 path)))
	 (path (substring path 0 (match-beginning 0))))
    (org-open-file path t nil search)))

(defun org-bibtex-store-link ()
  "Store a link to a BibTeX entry."
  (when (eq major-mode 'bibtex-mode)
    (let* ((search (run-hook-with-args-until-success
		    'org-create-file-search-functions))
	   (link (concat "file:" (abbreviate-file-name buffer-file-name)
			 "::" search))
	   (entry (mapcar ; repair strings enclosed in "..." or {...}
		   (lambda(c)
		     (if (string-match
			  "^\\(?:{\\|\"\\)\\(.*\\)\\(?:}\\|\"\\)$" (cdr c))
			 (cons (car c) (match-string 1 (cdr c))) c))
		   (save-excursion
		     (bibtex-beginning-of-entry)
		     (bibtex-parse-entry)))))
      (org-store-link-props
       :key (cdr (assoc "=key=" entry))
       :author (or (cdr (assoc "author" entry)) "[no author]")
       :editor (or (cdr (assoc "editor" entry)) "[no editor]")
       :title (or (cdr (assoc "title" entry)) "[no title]")
       :booktitle (or (cdr (assoc "booktitle" entry)) "[no booktitle]")
       :journal (or (cdr (assoc "journal" entry)) "[no journal]")
       :publisher (or (cdr (assoc "publisher" entry)) "[no publisher]")
       :pages (or (cdr (assoc "pages" entry)) "[no pages]")
       :url (or (cdr (assoc "url" entry)) "[no url]")
       :year (or (cdr (assoc "year" entry)) "[no year]")
       :month (or (cdr (assoc "month" entry)) "[no month]")
       :address (or (cdr (assoc "address" entry)) "[no address]")
       :volume (or (cdr (assoc "volume" entry)) "[no volume]")
       :number (or (cdr (assoc "number" entry)) "[no number]")
       :annote (or (cdr (assoc "annote" entry)) "[no annotation]")
       :series (or (cdr (assoc "series" entry)) "[no series]")
       :abstract (or (cdr (assoc "abstract" entry)) "[no abstract]")
       :btype (or (cdr (assoc "=type=" entry)) "[no type]")
       :type "bibtex"
       :link link
       :description description))))

(provide 'org-bibtex)


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


;;; org-bibtex.el ends here
