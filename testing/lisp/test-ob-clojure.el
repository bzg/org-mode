;;; test-ob-clojure.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2025 Free Software Foundation, Inc.
;; Authors: stardiviner

;; This file is not part of GNU Emacs.

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

;;; Comments:

;; Org tests for ob-clojure.el live here

;;; Code:

(unless (featurep 'ob-clojure)
  (signal 'missing-test-dependency "Support for Clojure code blocks"))

;; FIXME: The old tests where totally off.  We need to write new tests.

(provide 'test-ob-clojure)

 ;;; test-ob-clojure.el ends here
