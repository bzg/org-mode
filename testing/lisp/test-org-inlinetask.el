;;; test-org-inlinetask.el --- Tests for org-inlinetask.el

;; Copyright (c)  Marco Wahl
;; Authors: Marco Wahl

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

;;; Comments:

;; Tests for org-inlinetask.el.

;;; Code:

(require 'org-inlinetask)


;;; Test movement

(ert-deftest test-org-inlinetask/goto-end ()
  "Tests around org-inlinetask."
  ;; Goto end.
  (should
     (equal "** H\n***** I\n***** END<point>\nfoo"
  	    (let ((org-inlinetask-min-level 5)
  		  (org-adapt-indentation t))
  	      (org-test-with-temp-text
		  "** H\n<point>***** I\n***** END\nfoo"
		(org-inlinetask-goto-end)
		(insert "<point>")
		(buffer-string)))))

  ;; Goto end.  End is buffer end.
  (should
     (equal "** H\n***** I\n***** END<point>"
  	    (let ((org-inlinetask-min-level 5)
  		  (org-adapt-indentation t))
  	      (org-test-with-temp-text
		  "** H\n<point>***** I\n***** END"
		(org-inlinetask-goto-end)
		(insert "<point>")
		(buffer-string)))))

  ;; Goto end.  Starting somewhere.
  (should
     (equal "** H\n***** I\n***** END<point>\n***** I\n***** END"
  	    (let ((org-inlinetask-min-level 5)
  		  (org-adapt-indentation t))
  	      (org-test-with-temp-text
		  "** H\n****<point>* I\n***** END\n***** I\n***** END"
		(org-inlinetask-goto-end)
		(insert "<point>")
		(buffer-string))))))

(provide 'test-org-inlinetask)

;;; test-org-inlinetask.el ends here
