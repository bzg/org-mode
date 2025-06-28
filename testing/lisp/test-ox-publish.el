;;; test-ox-publish.el --- Tests for "ox-publish.el" -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2019  Nicolas Goaziou

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org-test "../testing/org-test")
(require 'ox-publish)

;;; Helper functions

(defun org-test-publish
    (properties handler
                &optional remove-prop timestamp-flag pubdir keep-pubdir-p)
  "Publish a project defined by PROPERTIES.
Call HANDLER with the publishing directory as its sole argument.
Unless set otherwise in PROPERTIES, `:base-directory' is set to
\"examples/pub/\" sub-directory from test directory and
`:publishing-function' is set to `org-publish-attachment'.
Because `org-publish-property' uses `plist-member' to check the
existence of a property, a property with a value nil is different
from a non-existing property.  Properties in REMOVE-PROP will be
removed from the final plist.
Assign optional TIMESTAMP-FLAG to `org-publish-use-timestamps-flag'.
Optional PUBDIR specifies the `:publishing-directory', which
overrides the default of a randomly generated temporary directory.
If optional KEEP-PUBDIR-P is non-nil, keep publishing directory,
including timestamp directory; otherwise, delete it."
  (declare (indent 1))
  (let* ((org-publish-use-timestamps-flag timestamp-flag)
	 (org-publish-cache nil)
	 (base-dir (expand-file-name "examples/pub/" org-test-dir))
	 (pub-dir (or pubdir (make-temp-file "org-test" t)))
	 (org-publish-timestamp-directory
	  (expand-file-name ".org-timestamps/" pub-dir))
         (props (org-plist-delete-all
                 (org-combine-plists
                  `(:base-directory ,base-dir
                    :publishing-function org-publish-attachment)
                  properties
                  `(:publishing-directory ,pub-dir))
                 remove-prop))
	 (project
	  `("test" ,@props)))
    (unwind-protect
	(progn
	  (org-publish-projects (list project))
	  (funcall handler pub-dir))
      (unless keep-pubdir-p
        ;; Clear published data.
        (delete-directory pub-dir t))
      ;; Delete auto-generated site-map file, if applicable.
      (let ((site-map (and (plist-get properties :auto-sitemap)
			   (expand-file-name
			    (or (plist-get properties :sitemap-filename)
				"sitemap.org")
			    base-dir))))
	(when (and site-map (file-exists-p site-map))
	  (delete-file site-map)))
      ;; Delete auto-generated index files, if applicable.
      (when (plist-get properties :makeindex)
        (let ((index.inc (expand-file-name "theindex.inc" base-dir))
              (index.org (expand-file-name "theindex.org" base-dir)))
	  (when (file-exists-p index.inc) (delete-file index.inc))
          (when (file-exists-p index.org) (delete-file index.org)))))))


;;; Mandatory properties

(ert-deftest test-org-publish/base-extension ()
  "Test `:base-extension' specifications."
  ;; Regular tests.
  (should
   (equal '("a.org" "b.org")
	  (org-test-publish '(:base-extension "org")
	    (lambda (dir)
	      (remove ".org-timestamps"
		      (cl-remove-if #'file-directory-p
				    (directory-files dir)))))))
  (should
   (equal '("file.txt")
	  (org-test-publish '(:base-extension "txt")
	    (lambda (dir)
	      (remove ".org-timestamps"
		      (cl-remove-if #'file-directory-p
				    (directory-files dir)))))))
  ;; A nil value is equivalent to ".org".
  (should
   (equal '("a.org" "b.org")
	  (org-test-publish '(:base-extension nil)
	    (lambda (dir)
	      (remove ".org-timestamps"
		      (cl-remove-if #'file-directory-p
				    (directory-files dir)))))))
  ;; Symbol `any' includes all files, even those without extension.
  (should
   (equal '("a.org" "b.org" "file.txt" "noextension")
	  (org-test-publish '(:base-extension any)
	    (lambda (dir)
	      (remove ".org-timestamps"
		      (cl-remove-if #'file-directory-p
				    (directory-files dir)))))))

  ;; Check the default transformation function,
  ;; org-html-publish-to-html. Because org-test-publish uses
  ;; org-publish-attachment by default, we must not just override with
  ;; nil but tell it to remove the :publishing-function from the list.
  (should
   (let ((func (lambda (dir)
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "a.html" dir))
                   (buffer-string)))))
    (equal (org-test-publish nil func '(:publishing-function))
           (org-test-publish '(:publishing-function org-html-publish-to-html) func)))))


;;; Publish cache

(defun org-test-publish-touch (file)
  "Change the modification time of FILE."
  (let ((buf (get-file-buffer file)))
    (when buf
      (kill-buffer buf)))
  (find-file file)
  (set-buffer-modified-p t)
  (save-buffer 0))

(ert-deftest test-org-publish/publish-cache ()
  "Test publish cache based on timestamps.
Publish a source file, which includes a config file, to HTML.
Test updates of source and config file."
  (let* ((base (expand-file-name "examples/pub-cache/" org-test-dir))
         (source (expand-file-name "source.org" base))
         (config (expand-file-name "config.org" base))
         (pub-dir (make-temp-file "org-test" t))
         (html (expand-file-name "source.html" pub-dir))
         (plist `(:publishing-function org-html-publish-to-html
                                       :base-extension nil
                                       :base-directory ,base
                                       :exclude "."
			               :include ("source.org")))
         (handler (lambda (dir)
	            (remove ".org-timestamps"
		            (cl-remove-if #'file-directory-p
				          (directory-files dir))))))
    (should
     ;; Publish HTML from source.org for the first time.
     (equal '("source.html")
	    (org-test-publish plist handler nil t pub-dir t)))
    (let ((hmtime (org-publish-cache-mtime-of-src html)))
      (sleep-for 0.1)
      ;; Publish again, without source changes.
      ;; Should not publish, but keep the HTML file unchanged.
      (org-test-publish plist handler nil t pub-dir t)
      (should
       (equal hmtime
              (org-publish-cache-mtime-of-src html)))

      ;; Pretend the source has changed.
      ;; Publish again, with a new mtime.
      (org-test-publish-touch source)
      (org-test-publish plist handler nil t pub-dir t)
      (let ((hmtime2 (org-publish-cache-mtime-of-src html)))
        (should
         (time-less-p hmtime hmtime2))

        (sleep-for 0.1)
        ;; Publish again, without source changes.
        ;; Does not publish, but keeps the HTML file unchanged.
        (org-test-publish plist handler nil t pub-dir t)
        (should
         (equal hmtime2
                (org-publish-cache-mtime-of-src html)))

        ;; Pretend file config.org has changed.
        ;; Publish again, with a new mtime.
        (org-test-publish-touch config)
        (org-test-publish plist handler nil t pub-dir t)
        (let ((hmtime3 (org-publish-cache-mtime-of-src html)))
          (should
           (time-less-p hmtime2 hmtime3))

          (sleep-for 0.1)
          ;; Publish again, without source changes.
          ;; Should not publish, but keep the HTML file unchanged.
          (org-test-publish plist handler nil t pub-dir t)
          (should
           (equal hmtime3
                  (org-publish-cache-mtime-of-src html))))))
    (delete-directory pub-dir t)))


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
			      :sitemap-function (lambda (_title _f) "Custom!"))
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
			      (lambda (_title f) (org-list-to-generic f nil)))
	    (lambda (dir)
	      (with-temp-buffer
		(insert-file-contents (expand-file-name "sitemap.org" dir))
		(buffer-string)))))))

;;; Index creation

(ert-deftest test-org-publish/index ()
  "Test index creation."
  ;; Index creation is controlled with `:makeindex' (nil by default).
  ;; If t, generate theindex.inc (and theindex.org if it does not exist).
  ;; Index terms are only collected via `org-publish-org-to' (thus,
  ;; `org-publish-attachment' is not good enough).
  (should
   (org-test-publish
       '(:makeindex t)
     (lambda (dir) (file-exists-p (expand-file-name "theindex.org" dir)))))
  (should-not
   (org-test-publish
       '(:makeindex nil)
     (lambda (dir) (file-exists-p (expand-file-name "theindex.org" dir)))))
  (let ((base (expand-file-name "examples/pub/" org-test-dir))
        (correct "* B
  - [[file:index/file1.org::*Section on bar][bar]]
  - [[file:index/file2.org::*Section on baz][baz]]
* F
  - [[file:index/file1.org::*Section on foo][foo]]
  - [[file:index/file2.org::*Section on foo][foo]]
"))
    (should
     (equal correct
	    (org-test-publish
	        '(:makeindex t
                             :publishing-function org-html-publish-to-html
			     :exclude "."
			     :include ("index/file1.org" "index/file2.org"))
	      (lambda (_)
	        (with-temp-buffer
		  (insert-file-contents
                   (expand-file-name "theindex.inc" base))
		  (buffer-string))))))

    ;; Check that the index not only contains terms for file2.org but
    ;; also for index/file1.org, even when the latter file is skipped
    ;; as unchanged.
    (let ((pub-dir (make-temp-file "org-test" t))
          (incorrect "* B
  - [[file:index/file2.org::*Section on baz][baz]]
* F
  - [[file:index/file2.org::*Section on foo][foo]]
"))
      ;; First publication, ignore results.
      (org-test-publish
	  '(:makeindex t
                       :publishing-function org-html-publish-to-html
		       :exclude "."
		       :include ("index/file1.org" "index/file2.org"))
        (lambda (_)) nil t pub-dir t)
      ;; Pretend that a new Emacs session started as follows:
      ;; Use empty transient cache.
      ;; Touch file2.org as changed, while file1.org is skipped.
      (setq org-publish-transient-cache nil)
      (org-test-publish-touch (expand-file-name "index/file2.org" base))
      (let ((result
             (org-test-publish
	         '(:makeindex t
                              :publishing-function org-html-publish-to-html
			      :exclude "."
			      :include ("index/file1.org" "index/file2.org"))
	       (lambda (_)
	         (with-temp-buffer
		   (insert-file-contents (expand-file-name "theindex.inc" base))
		   (buffer-string)))
               nil t pub-dir)))
        ;; Fails up to Org 9.7.23.
        (should-not
         (equal incorrect result))
        (should
         (equal correct result))))))


;;; Cross references

(ert-deftest test-org-publish/resolve-external-link ()
  "Test `org-publish-resolve-external-link' specifications."
  ;; FIXME: Disabling because of CI-only failures that I was not able
  ;; track.
  (skip-unless nil)
  ;; Function should preserve internal reference when used between
  ;; published files.
  (should
   (apply
    #'equal
    (let* (;; (ids nil)
	   (backend
	    (org-export-create-backend
	     :transcoders
	     `((headline . ,(lambda (h c i)
			      (concat (org-export-get-reference h i) " " c)))
	       (paragraph . ,(lambda (_p c _i) c))
	       (section . ,(lambda (_s c _i) c))
	       (link . ,(lambda (l _c _i)
			  (let ((option (org-element-property :search-option l))
			        (path (org-element-property :path l)))
			    (and option
				 (org-publish-resolve-external-link
				  option path))))))))
	   (publish
	    (lambda (plist filename pub-dir)
	      (org-publish-org-to backend filename ".test" plist pub-dir))))
      (org-test-publish
	  (list :publishing-function (list publish))
	(lambda (dir)
	  (cl-subseq
	   (split-string
	    (mapconcat (lambda (f) (org-file-contents (expand-file-name f dir)))
		       (directory-files dir nil "\\.test\\'")
		       " "))
	   1 3))))))
  ;; When optional argument PREFER-CUSTOM is non-nil, use custom ID
  ;; instead of internal reference, whenever possible.
  (should
   (equal
    '("a1" "b1")
    (let* ((ids nil)
	   (link-transcoder
	    (lambda (l _c _i)
	      (let ((option (org-element-property :search-option l))
		    (path (org-element-property :path l)))
		(push (org-publish-resolve-external-link option path t)
		      ids)
		"")))
	   (backend
	    (org-export-create-backend
	     :transcoders `((headline . (lambda (h c i) c))
			    (paragraph . (lambda (p c i) c))
			    (section . (lambda (s c i) c))
			    (link . ,link-transcoder))))
	   (publish
	    (lambda (plist filename pub-dir)
	      (org-publish-org-to backend filename ".test" plist pub-dir))))
      (org-test-publish (list :publishing-function (list publish)
			      :exclude "."
			      :include '("a.org" "b.org"))
	#'ignore)
      (sort ids #'string<)))))


;;; Tools

(ert-deftest test-org-publish/get-project-from-filename ()
  "Test `org-publish-get-project-from-filename' specifications."
  ;; Check base directory.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "a.org" base))
	  (org-publish-project-alist `(("p" :base-directory ,base))))
     (org-publish-get-project-from-filename file)))
  ;; Return nil if no appropriate project is found.
  (should-not
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  ;; (file (expand-file-name "a.org" base))
	  (org-publish-project-alist `(("p" :base-directory ,base))))
     (org-publish-get-project-from-filename "/other/file.org")))
  ;; Return the first project effectively publishing the provided
  ;; file.
  (should
   (equal "p2"
	  (let* ((base (expand-file-name "examples/pub/" org-test-dir))
		 (file (expand-file-name "a.org" base))
		 (org-publish-project-alist
		  `(("p1" :base-directory "/other/")
		    ("p2" :base-directory ,base)
		    ("p3" :base-directory ,base))))
	    (car (org-publish-get-project-from-filename file)))))
  ;; When :recursive in non-nil, allow files in sub-directories.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "sub/c.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :recursive t))))
     (org-publish-get-project-from-filename file)))
  (should-not
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "sub/c.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :recursive nil))))
     (org-publish-get-project-from-filename file)))
  ;; Also, when :recursive is non-nil, follow symlinks to directories.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "link/link.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :recursive t))))
     (org-publish-get-project-from-filename file)))
  (should-not
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "link/link.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :recursive nil))))
     (org-publish-get-project-from-filename file)))
  ;; Check :base-extension.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "file.txt" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :base-extension "txt"))))
     (org-publish-get-project-from-filename file)))
  (should-not
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "file.txt" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :base-extension "org"))))
     (org-publish-get-project-from-filename file)))
  ;; When :base-extension has the special value `any', allow any
  ;; extension, including none.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "file.txt" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :base-extension any))))
     (org-publish-get-project-from-filename file)))
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "noextension" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :base-extension any))))
     (org-publish-get-project-from-filename file)))
  ;; Pathological case: Handle both :extension any and :recursive t.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "sub/c.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :recursive t :base-extension any))))
     (org-publish-get-base-files (org-publish-get-project-from-filename file))))
  ;; Check :exclude property.
  (should-not
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "a.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :exclude "a"))))
     (org-publish-get-project-from-filename file)))
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "a.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :exclude "other"))))
     (org-publish-get-project-from-filename file)))
  ;; The regexp matches against relative file name, not absolute one.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "a.org" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :exclude "examples/pub"))))
     (org-publish-get-project-from-filename file)))
  ;; Check :include property.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "file.txt" base))
	  (org-publish-project-alist
	   `(("p" :base-directory ,base :include (,file)))))
     (org-publish-get-project-from-filename file)))
  ;; :include property has precedence over :exclude one.
  (should
   (let* ((base (expand-file-name "examples/pub/" org-test-dir))
	  (file (expand-file-name "a.org" base))
	  (org-publish-project-alist
	   `(("p"
	      :base-directory ,base
	      :include (,(file-name-nondirectory file))
	      :exclude "a"))))
     (org-publish-get-project-from-filename file)))
  ;; With optional argument, return a meta-project publishing provided
  ;; file.
  (should
   (equal "meta"
	  (let* ((base (expand-file-name "examples/pub/" org-test-dir))
		 (file (expand-file-name "a.org" base))
		 (org-publish-project-alist
		  `(("meta" :components ("p"))
		    ("p" :base-directory ,base))))
	    (car (org-publish-get-project-from-filename file t))))))

(ert-deftest test-org-publish/file-relative-name ()
  "Test `org-publish-file-relative-name' specifications."
  ;; Turn absolute file names into relative ones if file belongs to
  ;; base directory.
  (should
   (equal "a.org"
	  (let* ((base (expand-file-name "examples/pub/" org-test-dir))
		 (file (expand-file-name "a.org" base)))
	    (org-publish-file-relative-name file `(:base-directory ,base)))))
  (should
   (equal "pub/a.org"
	  (let* ((base (expand-file-name "examples/" org-test-dir))
		 (file (expand-file-name "pub/a.org" base)))
	    (org-publish-file-relative-name file `(:base-directory ,base)))))
  ;; Absolute file names that do not belong to base directory are
  ;; unchanged.
  (should
   (equal "/name.org"
	  (let ((base (expand-file-name "examples/pub/" org-test-dir)))
	    (org-publish-file-relative-name "/name.org"
					    `(:base-directory ,base)))))
  ;; Relative file names are unchanged.
  (should
   (equal "a.org"
	  (let ((base (expand-file-name "examples/pub/" org-test-dir)))
	    (org-publish-file-relative-name "a.org" `(:base-directory ,base))))))


(provide 'test-ox-publish)
;;; test-ox-publish.el ends here
