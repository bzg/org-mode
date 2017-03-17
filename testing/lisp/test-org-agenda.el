;;; test-org-agenda.el --- Tests for org-agenda.el -*- lexical-binding: t; -*-

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


;; Auxilliaries for set-up and tear-down and more.

(defun -sha1-as-defun-name-accc70505c6664ed226e3afa45ca0ecc95a35e83 ()
  "Name a function with the sha1 of the function text.
Use this function if you are too lazy to invent a function name.
The function text starts at the argument list and ends at the
last paren (exclusive)."
  (interactive)
  (save-excursion
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
      ;; relying on (point) is within the defun
      (progn
        (beginning-of-defun)
        (search-forward-regexp " ")
        (skip-chars-forward " \t")
        (just-one-space))
      (unless (= ?\( (char-after))
        (delete-region (point) (progn (forward-word) (point)))
        (just-one-space))
      (insert sha1 " "))))

(defun -kill-all-agendas ()
  "Kill all agenda buffers."
  (mapc #'kill-buffer
        (cl-remove-if-not
         (lambda (x)
           (set-buffer x)
           (eq major-mode 'org-agenda-mode))
         (buffer-list))))


;; Test the Agenda

(ert-deftest org-e9d91f5add1245445ba773dd74ac534273113ca5 ()
  "Empty agenda."
  (let ((org-agenda-span 'day)
        org-agenda-files)
    (org-agenda-list)
    (set-buffer org-agenda-buffer-name)
    (should (= 2 (count-lines (point-min) (point-max))))))

(ert-deftest org-a0116aeccdedc04580e42933a16c2893d76ee6bc ()
  "One informative line in the agenda."
  (let ((org-agenda-span 'day)
	(org-agenda-files `(,(expand-file-name "examples/agenda-file.org" org-test-dir))))
    (org-agenda-list nil  "<2017-03-10 Fri>")
    (set-buffer org-agenda-buffer-name)
    (should (= 3 (count-lines (point-min) (point-max))))))

(ert-deftest org-165802102bb2d2accf16ff0ae362ef51945ae69f ()
  "Agenda buffer name after having created one sticky agenda buffer."
  (-kill-all-agendas)
  ;; (setq org-agenda-buffer-name "*Org Agenda*")
  (let ((org-agenda-span 'day)
	(org-agenda-buffer-name "*Org Agenda*")
	(default-org-agenda-buffer-name org-agenda-buffer-name)
	(buf (get-buffer org-agenda-buffer-name))
        org-agenda-files)
    (when buf (kill-buffer buf))
    (org-test-with-temp-text "<2017-03-17 Fri>"
      (org-follow-timestamp-link) 	; creates a sticky agenda.
      )
    (-kill-all-agendas)
    (org-agenda-list)
    (let ((agenda-buffers
	   (cl-remove-if-not
	    (lambda (x)
	      (set-buffer x)
	      (eq major-mode 'org-agenda-mode))
	    (buffer-list))))
      (should (= 1 (length agenda-buffers)))
      (should (string= default-org-agenda-buffer-name
                       (buffer-name (car agenda-buffers))))))
  (-kill-all-agendas))


(provide 'test-org-agenda)

;;; test-org-agenda.el ends here
