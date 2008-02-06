;;; org-export-freemind - exporting utilities from org-mode to freemind

;; Copyright (C) 2007 Marco Vezzoli

;; Author: marco vezzoli <noise.otn@alice.it>
;; Created:
;; Version: 0.1.0
;; Keywords: org-mode export freemind
;; Commentary:

;; This file is *not* part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Code:
(defgroup org-export-freemind ()
  "This group let you customize your own export into freemind format"
  :group 'org-export)

(defcustom org-freemind-icons-alist
  '(("TODO" . "button_cancel")
    ("SPEC" . "pencil")
    ("WIP"  . "pencil")
    ("TEST" . "xmag")
    ("DONE" . "button_ok"))
  "change the icon according to a regular expression"
  :type '(alist :key-type regexp 
		:value-type string)
  :group 'org-export-freemind)

(defcustom org-freemind-cloud-alist
  '((":PROJECT:" . "ccffcc")
    (":MEETING:" . "ccccff"))
  "create a cloud with the defined color if title match a regexp"
  :type '(alist :key-type regexp :value-type string)
  :group 'org-export-freemind)

(defun org-export-as-freemind (&optional buffer outbuffer)
  "Export the org buffer as FreeMind XML.
"
  (interactive (list (current-buffer) ()))
  ;; A quickie abstraction

  ;; Output everything as FreeMind
  (with-current-buffer (get-buffer buffer)
    (goto-char (point-min))  ;; CD:  beginning-of-buffer is not allowed.
    (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	   (title       (or (plist-get opt-plist :title)
			    (file-name-sans-extension
			     (file-name-nondirectory buffer-file-name))))
	   (filename (concat (file-name-as-directory
			      (org-export-directory :xoxo opt-plist))
			     title
			     ".mm"))
	   (out (if (bufferp outbuffer)
		    outbuffer
		    (find-file-noselect filename)))
	   (last-level 0)
	   (hanging-li nil))

      ;; Check the output buffer is empty.
      ;; Kick off the output
      (unless (bufferp outbuffer)
	  (progn
	    (with-current-buffer out (erase-buffer))
	    (org-export-as-xoxo-insert-into out "<map version='0.8.0'>\n")))
      (org-export-as-xoxo-insert-into out 
	"<node TEXT='" title "'"
	(if (bufferp outbuffer)
	    " FOLDED='true'" "")
	">\n")
      (if (bufferp outbuffer)
	  (org-export-as-xoxo-insert-into out "<cloud COLOR='#ccffff'/>\n"))
      (while (re-search-forward "^\\(\\*+\\) \\(.+\\)" (point-max) 't)
        (let* ((hd (match-string-no-properties 1))
               (level (length hd))
               (text (match-string-no-properties 2)))
	  (save-excursion
	    (goto-char (match-end 0))
	    (catch 'loop
	      (while 't
		(forward-line)
		(if (looking-at "^[ \t]\\(.*\\)")
		    ()
		  (throw 'loop "")))))

          ;; Handle level rendering
	  (cond
           ((> level last-level)
	    (let ((rept (- level last-level 1))
		  (value ()))
	      (dotimes (i rept value)
		(org-export-as-xoxo-insert-into out "<node FOLDED='false' TEXT='.'>\n")))
            (org-export-as-xoxo-insert-into out "\n<node FOLDED='true' TEXT='"))

           ((< level last-level)
	    (let ((rept (+ (- last-level level) 1))
		  (value ()))
	      (dotimes (i rept value)
		(org-export-as-xoxo-insert-into out "</node>\n")))
	    (org-export-as-xoxo-insert-into out "<node FOLDED='true' TEXT='"))

           ((equal level last-level)
	    (org-export-as-xoxo-insert-into out "</node>\n<node FOLDED='true' TEXT='")))

          (setq last-level level)

          ;; And output the new node
	  (let* ((heading
		  (concat "<html><h3>" 
			  (replace-regexp-in-string 
			   ":.*:" 
			   (lambda (x) 
			     (concat "<font color='red'>" x "</font>")) 
			   text)
			  "</h3></html>"))
		 (html-quoted-heading (org-html-expand heading))
		 (exp-quote-heading (replace-regexp-in-string "'" "&quot;" html-quoted-heading)))
	    (org-export-as-xoxo-insert-into out exp-quote-heading "'>\n"))

	  (dolist (rule org-freemind-icons-alist)
	    (if (string-match (car rule) text)
		(org-export-as-xoxo-insert-into out "<icon BUILTIN='" (cdr rule) "'/>\n")))
	  (dolist (rule org-freemind-cloud-alist)
	    (when (string-match (car rule) text)
	      (progn
		(org-export-as-xoxo-insert-into out 
		  "<cloud COLOR='#" (cdr rule) "'/>\n")
		(message (cdr rule))
		)))
	  ))

      ;; Finally finish off the map
      (let ((value ()))
	(org-export-as-xoxo-insert-into out "\n")
	(dotimes (i last-level value)
	  (org-export-as-xoxo-insert-into out "</node>\n")))
      (org-export-as-xoxo-insert-into out "</node>\n")

      ;; Finish the buffer off and clean it up.
      (unless (bufferp outbuffer)
	(progn
	  (org-export-as-xoxo-insert-into out "</map>\n")
	  (switch-to-buffer-other-window out)
	  (indent-region (point-min) (point-max) nil)
	  (save-buffer)
	  (goto-char (point-min))
	  )))))

(defun org-export-as-freemind-agenda-files ()
  "Export all agenda files into Freemind format
each files is saved with .mm extension
into the XOXO publishing directory"
  (interactive)
  (dolist (file org-agenda-files)
    (org-check-agenda-file file)
    (set-buffer (org-get-agenda-file-buffer file))
    (org-export-as-freemind (current-buffer))
    ))

(defun org-export-as-freemind-agenda-files-one-file (filename)
  "Export all agenda files into FreeMind format.
All results are grouped in a single .mm file"
  (interactive "FFile to save: ")
  (let* ((title (file-name-sans-extension
		 (file-name-nondirectory filename)))
	 (out (find-file-noselect filename)))
    (with-current-buffer out (erase-buffer))
    (org-export-as-xoxo-insert-into out "<map version='0.8.0'><node TEXT='" title "'>\n")
    (dolist (file org-agenda-files)
      (org-check-agenda-file file)
      (set-buffer (org-get-agenda-file-buffer file))
      (org-export-as-freemind (current-buffer) out)
      )
    (org-export-as-xoxo-insert-into out "</node></map>\n")
    (switch-to-buffer-other-window out)
    (indent-region (point-min) (point-max) nil)
    (save-buffer)
    (goto-char (point-min))
    ))

(define-key org-mode-map "\C-c\C-xf" 'org-export-as-freemind)
;;; org-export-freemind ends here