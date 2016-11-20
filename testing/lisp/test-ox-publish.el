;;; test-ox-publish.el --- Tests for "ox-publish.el" -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun org-test-publish (properties handler)
  "Publish a project defined by PROPERTIES.
Call HANDLER with the publishing directory as its sole argument.
Unless set otherwise in PROPERTIES, `:base-directory' is set to
\"examples/pub/\" sub-directory from test directory and
`:publishing-function' is set to `org-publish-attachment'."
  (let* ((org-publish-use-timestamps-flag nil)
	 (org-publish-cache nil)
	 (base-dir (expand-file-name "examples/pub/" org-test-dir))
	 (pub-dir (make-temp-file "org-test" t))
	 (org-publish-timestamp-directory
	  (expand-file-name ".org-timestamps/" pub-dir))
	 (project
	  `("test" ,@(org-combine-plists
		      `(:base-directory
			,base-dir
			:publishing-function org-publish-attachment)
		      properties
		      `(:publishing-directory ,pub-dir)))))
    (unwind-protect
	(progn
	  (org-publish-projects (list project))
	  (funcall handler pub-dir))
      ;; Clear published data.
      (delete-directory pub-dir t)
      ;; Delete auto-generated site-map file, if applicable.
      (let ((site-map (and (plist-get properties :auto-sitemap)
			   (expand-file-name
			    (or (plist-get properties :sitemap-filename)
				"sitemap.org")
			    base-dir))))
	(when (and site-map (file-exists-p site-map))
	  (delete-file site-map))))))



;;; Site-map

(ert-deftest test-org-publish/sitemap ()
  "Test site-map specifications."
  ;; Site-map creation is controlled with `:auto-sitemap'.  It
  ;; defaults to "sitemap.org".
  (should
   (org-test-publish
    '(:auto-sitemap t)
    (lambda (dir) (file-exists-p (expand-file-name "sitemap.org" dir)))))
  (should-not
   (org-test-publish
    '(:auto-sitemap nil)
    (lambda (dir) (file-exists-p (expand-file-name "sitemap.org" dir)))))
  ;; Site-map file name is controlled with `:sitemap-filename'.
  (should
   (org-test-publish
    '(:auto-sitemap t :sitemap-filename "mysitemap.org")
    (lambda (dir) (file-exists-p (expand-file-name "mysitemap.org" dir)))))
  ;; Site-map title is controlled with `:sitemap-title'.  It defaults
  ;; to the project name.
  (should
   (equal "#+TITLE: Sitemap for project test"
	  (org-test-publish
	   '(:auto-sitemap t)
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-substring (point) (line-end-position)))))))
  (should
   (equal "#+TITLE: My title"
	  (org-test-publish
	   '(:auto-sitemap t :sitemap-title "My title")
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-substring (point) (line-end-position)))))))
  ;; Allowed site-map styles: `list' and `tree'.
  (should
   (equal "
- [[file:a.org][A]]
- [[file:b.org][b]]
- [[file:sub/c.org][C]]"
	  (org-test-publish
	   '(:auto-sitemap t
			   :sitemap-sort-folders ignore
			   :sitemap-style list
			   :exclude "."
			   :include ("a.org" "b.org" "sub/c.org"))
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-substring (line-beginning-position 2) (point-max)))))))
  (should
   (equal "
- [[file:a.org][A]]
- [[file:b.org][b]]
- sub
  - [[file:sub/c.org][C]]"
	  (org-test-publish
	   '(:auto-sitemap t
			   :sitemap-style tree
			   :exclude "."
			   :include ("a.org" "b.org" "sub/c.org"))
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-substring (line-beginning-position 2) (point-max)))))))
  ;; When style is `list', `:sitemap-sort-folders' controls the order
  ;; of appearance of directories among published files.
  (should
   (equal
    "
- sub/
- [[file:a.org][A]]
- [[file:sub/c.org][C]]"
    (org-test-publish
     '(:auto-sitemap t
		     :recursive t
		     :sitemap-style list
		     :sitemap-sort-folders first
		     :exclude "."
		     :include ("a.org" "sub/c.org"))
     (lambda (dir)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name "sitemap.org" dir))
	 (buffer-substring (line-beginning-position 2) (point-max)))))))
  (should
   (equal
    "
- [[file:a.org][A]]
- [[file:sub/c.org][C]]
- sub/"
    (org-test-publish
     '(:auto-sitemap t
		     :recursive t
		     :sitemap-style list
		     :sitemap-sort-folders last
		     :exclude "."
		     :include ("a.org" "sub/c.org"))
     (lambda (dir)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name "sitemap.org" dir))
	 (buffer-substring (line-beginning-position 2) (point-max)))))))
  ;; When style is `list', `:sitemap-sort-folders' can be used to
  ;; toggle visibility of directories in the site-map.
  (should
   (let ((case-fold-search t))
     (string-match-p
      "- sub/$"
      (org-test-publish
       '(:auto-sitemap t
		       :recursive t
		       :sitemap-style list
		       :sitemap-sort-folders t
		       :exclude "."
		       :include ("a.org" "sub/c.org"))
       (lambda (dir)
	 (with-temp-buffer
	   (insert-file-contents (expand-file-name "sitemap.org" dir))
	   (buffer-substring (line-beginning-position 2) (point-max))))))))
  (should-not
   (string-match-p
    "- sub/$"
    (org-test-publish
     '(:auto-sitemap t
		     :recursive t
		     :sitemap-style list
		     :sitemap-sort-folders ignore
		     :exclude "."
		     :include ("a.org" "sub/c.org"))
     (lambda (dir)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name "sitemap.org" dir))
	 (buffer-substring (line-beginning-position 2) (point-max)))))))
  ;; Using `:sitemap-sort-files', files can be sorted alphabetically
  ;; (according to their title, or file name when there is none),
  ;; chronologically a anti-chronologically.
  (should
   (equal
    "
- [[file:a.org][A]]
- [[file:b.org][b]]
- [[file:sub/c.org][C]]"
    (org-test-publish
     '(:auto-sitemap t
		     :recursive t
		     :sitemap-style list
		     :sitemap-sort-folders ignore
		     :sitemap-sort-files alphabetically
		     :exclude "."
		     :include ("a.org" "b.org" "sub/c.org"))
     (lambda (dir)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name "sitemap.org" dir))
	 (buffer-substring (line-beginning-position 2) (point-max)))))))
  (should
   (equal
    "
- [[file:b.org][b]]
- [[file:sub/c.org][C]]
- [[file:a.org][A]]"
    (org-test-publish
     '(:auto-sitemap t
		     :recursive t
		     :sitemap-style list
		     :sitemap-sort-folders ignore
		     :sitemap-sort-files chronologically
		     :exclude "."
		     :include ("a.org" "b.org" "sub/c.org"))
     (lambda (dir)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name "sitemap.org" dir))
	 (buffer-substring (line-beginning-position 2) (point-max)))))))
  (should
   (equal
    "
- [[file:a.org][A]]
- [[file:sub/c.org][C]]
- [[file:b.org][b]]"
    (org-test-publish
     '(:auto-sitemap t
		     :recursive t
		     :sitemap-style list
		     :sitemap-sort-folders ignore
		     :sitemap-sort-files anti-chronologically
		     :exclude "."
		     :include ("a.org" "b.org" "sub/c.org"))
     (lambda (dir)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name "sitemap.org" dir))
	 (buffer-substring (line-beginning-position 2) (point-max)))))))
  ;; `:sitemap-format-entry' formats entries in the site-map whereas
  ;; `:sitemap-function' controls the full site-map.
  (should
   (equal "
- a.org"
	  (org-test-publish
	   '(:auto-sitemap t
			   :exclude "."
			   :include ("a.org")
			   :sitemap-format-entry
			   (lambda (f _s _p) f))
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-substring (line-beginning-position 2) (point-max)))))))
  (should
   (equal "Custom!"
	  (org-test-publish
	   '(:auto-sitemap t
			   :exclude "."
			   :include ("a.org")
			   :sitemap-function (lambda (title _files) "Custom!"))
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-string))))))
  (should
   (equal "[[file:a.org][A]]"
	  (org-test-publish
	   '(:auto-sitemap t
			   :exclude "."
			   :include ("a.org")
			   :sitemap-function
			   (lambda (title files) (org-list-to-generic files nil)))
	   (lambda (dir)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name "sitemap.org" dir))
	       (buffer-string)))))))

(provide 'test-ox-publish)
;;; test-ox-publish.el ends here
