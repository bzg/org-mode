;;; test-org-agenda.el --- Tests for org-agenda.el -*- lexical-binding: t ; -*-

;; Copyright (C) 2017 Marco Wahl

;; Author: Marco Wahl <marcowahlsoft@gmail.com>

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

;;; Commentary:

;; Unit tests for Org Agenda.

;;; Code:

(require 'org-test)
(require 'org-agenda)


;; General auxilliaries

;; (possibly better move to some location in the source.)

;; Prefixing with '---' on this page.

;; Evaluate the following function for no brainer function naming.
(defun ---sha1-as-defun-name-39e8857766df959d8b52f9c38739f5a77c392ec0 ()
  "Insert the sha1 of the function text in front of arglist.
The function text starts at the argument list and ends at the
last paren (exclusive).
Use this function if you are too lazy to invent a function name."
  (interactive)
  (let* ((start (progn
		  (beginning-of-defun)
		  (search-forward-regexp "\(" nil nil 2)
		  (backward-char)
		  (point)))
	 (end (progn
		(end-of-defun)
		(backward-char)
		(point)))
	 (sha1 (sha1 (current-buffer) start end)))
    (goto-char start)
    (insert sha1 " ")
    (backward-word)))

(defun ---kill-all-agendas ()
  "Kill all agenda buffers."
  (mapc #'kill-buffer
        (cl-remove-if-not
         (lambda (x)
           (set-buffer x)
           (eq major-mode 'org-agenda-mode))
         (buffer-list))))

(defun ---agenda-buffers ()
    "Return agenda buffers in a list."
    (cl-remove-if-not
     (lambda (x)
       (set-buffer x)
       (eq major-mode 'org-agenda-mode))
     (buffer-list)))


;; Test the Agenda

(ert-deftest org-agenda-90c5dce0435b74ba7e9682a4a9a393aeea741739 ()
  "Empty agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (---agenda-buffers)) nil "precondition violation")
  (let ((org-agenda-span 'day)
        org-agenda-files)
    (org-agenda-list)
    (set-buffer org-agenda-buffer-name)
    (should (= 2 (count-lines (point-min) (point-max)))))
  (---kill-all-agendas))

(ert-deftest org-agenda-668f0e69003051b79eb421146f7626ac9438c105 ()
  "One informative line in the agenda."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (---agenda-buffers)) nil "precondition violation")
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org" org-test-dir))))
    (org-agenda-list nil  "<2017-03-10 Fri>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max)))))
  (---kill-all-agendas))

(ert-deftest org-agenda-8e6c85e9ff1ea9fed0ae0fa04ff9a3dace6c9d17 ()
  "Agenda buffer name after having created one sticky agenda buffer."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (---agenda-buffers)) nil "precondition violation")
  (let ((org-agenda-span 'day)
	(buf (get-buffer org-agenda-buffer-name))
        org-agenda-files)
    (when buf (kill-buffer buf))
    (org-test-with-temp-text "<2017-03-17 Fri>"
			     (org-follow-timestamp-link) 	; creates a sticky agenda.
			     )
    (---kill-all-agendas)
    (org-agenda-list)
    (should (= 1 (length (---agenda-buffers))))
    (should (string= "*Org Agenda*"
		     (buffer-name (car (---agenda-buffers))))))
  (---kill-all-agendas))

(ert-deftest org-agenda-9fa27658bf61d8fe2c5b6f9177e9e8ce07f11f7b ()
  "Agenda buffer name of sticky agenda after reload."
  (cl-assert (not org-agenda-sticky) nil "precondition violation")
  (cl-assert (not (---agenda-buffers)) nil "precondition violation")
  (org-toggle-sticky-agenda)
  (let (org-agenda-files)
    (org-agenda-list)
    (let* ((agenda-buffer-name
	    (progn
	      (assert (= 1 (length (---agenda-buffers))))
	      (buffer-name (car (---agenda-buffers))))))
      (set-buffer agenda-buffer-name)
      (org-agenda-redo)
      (should (= 1 (length (---agenda-buffers))))
      (should (string= agenda-buffer-name
                       (buffer-name (car (---agenda-buffers)))))))
  (org-toggle-sticky-agenda)
  (---kill-all-agendas))


(provide 'test-org-agenda)

;;; test-org-agenda.el ends here
