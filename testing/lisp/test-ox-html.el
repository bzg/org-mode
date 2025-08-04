;;; test-ox-html.el --- Tests for ox-html.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Rudolf Adamkovič

;; Author: Rudolf Adamkovič <rudolf@adamkovic.org>

;; This file is part of GNU Emacs.

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

(require 'ox-html)


;;; Loading MathJax

(ert-deftest ox-html/mathjax-path-none ()
  "Test that MathJax does not load when not needed."
  (should-not
   (org-test-with-temp-text "No LaTeX here."
     (let ((export-buffer "*Test HTML Export*")
           (org-export-show-temporary-export-buffer nil))
       (org-export-to-buffer 'html export-buffer
         nil nil nil nil nil
         #'html-mode)
       (with-current-buffer export-buffer
         (let ((case-fold-search t))
           (search-forward "MathJax" nil t)))))))

(ert-deftest ox-html/mathjax-path-default ()
  "Test the default path from which MathJax loads."
  (should
   (= 1 (org-test-with-temp-text "$x$"
          (let ((export-buffer "*Test HTML Export*")
                (org-export-show-temporary-export-buffer nil))
            (org-export-to-buffer 'html export-buffer
              nil nil nil nil nil
              #'html-mode)
            (with-current-buffer export-buffer
              (how-many (rx "<script
  id=\"MathJax-script\"
  async
  src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\">
</script>"))))))))

(ert-deftest ox-html/mathjax-path-custom ()
  "Test a customized path from which MathJax loads."
  (should
   (= 1 (org-test-with-temp-text "$x$"
          (let ((export-buffer "*Test HTML Export*")
                (org-export-show-temporary-export-buffer nil)
                (org-html-mathjax-options
                 '((path "./mathjax/es5/tex-mml-chtml.js"))))
            (org-export-to-buffer 'html export-buffer
              nil nil nil nil nil
              #'html-mode)
            (with-current-buffer export-buffer
              (how-many (rx "<script
  id=\"MathJax-script\"
  async
  src=\"./mathjax/es5/tex-mml-chtml.js\">
</script>"))))))))

(ert-deftest ox-html/mathjax-path-in-buffer ()
  "Test a in-buffer customized path from which MathJax loads."
  (should
   (= 1 (org-test-with-temp-text "
#+HTML_MATHJAX: path: ./mathjax/es5/tex-mml-chtml.js
$x$"
          (let ((export-buffer "*Test HTML Export*")
                (org-export-show-temporary-export-buffer nil))
            (org-export-to-buffer 'html export-buffer
              nil nil nil nil nil
              #'html-mode)
            (with-current-buffer export-buffer
              (how-many (rx "<script
  id=\"MathJax-script\"
  async
  src=\"./mathjax/es5/tex-mml-chtml.js\">
</script>"))))))))


;;; Configuring MathJax with options

(ert-deftest ox-html/mathjax-options-default ()
  "Test the default MathJax options."
  (should
   (= 1 (org-test-with-temp-text "$x$"
          (let ((export-buffer "*Test HTML Export*")
                (org-export-show-temporary-export-buffer nil))
            (org-export-to-buffer 'html export-buffer
              nil nil nil nil nil
              #'html-mode)
            (with-current-buffer export-buffer
              (how-many (rx "<script>
  window.MathJax = {
    tex: {
      ams: {
        multlineWidth: '85%'
      },
      tags: 'ams',
      tagSide: 'right',
      tagIndent: '.8em'
    },
    chtml: {
      scale: 1.0,
      displayAlign: 'center',
      displayIndent: '0em'
    },
    svg: {
      scale: 1.0,
      displayAlign: 'center',
      displayIndent: '0em'
    },
    output: {
      font: 'mathjax-modern',
      displayOverflow: 'overflow'
    }
  };
</script>"))))))))

(ert-deftest ox-html/mathjax-options-custom ()
  "Test customized MathJax options."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               '((path "<unused>")      ; tested elsewhere
                 (scale 0.5)
                 (align "right")
                 (font "mathjax-euler")
                 (overflow "scale")
                 (tags "all")
                 (indent "1em")
                 (multlinewidth "100%")
                 (tagindent "2em")
                 (tagside "left"))))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx "<script>
  window.MathJax = {
    tex: {
      ams: {
        multlineWidth: '100%'
      },
      tags: 'all',
      tagSide: 'left',
      tagIndent: '2em'
    },
    chtml: {
      scale: 0.5,
      displayAlign: 'right',
      displayIndent: '1em'
    },
    svg: {
      scale: 0.5,
      displayAlign: 'right',
      displayIndent: '1em'
    },
    output: {
      font: 'mathjax-euler',
      displayOverflow: 'scale'
    }
  };
</script>"))))))))

(ert-deftest ox-html/mathjax-options-in-buffer ()
  "Test in-buffer customized MathJax options."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: scale: 0.5
#+HTML_MATHJAX: align: right
#+HTML_MATHJAX: font: mathjax-euler
#+HTML_MATHJAX: overflow: scale
#+HTML_MATHJAX: tags: all
#+HTML_MATHJAX: indent: 1em
#+HTML_MATHJAX: multlinewidth: 100%
#+HTML_MATHJAX: tagindent: 2em
#+HTML_MATHJAX: tagside: left"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx "<script>
  window.MathJax = {
    tex: {
      ams: {
        multlineWidth: '100%'
      },
      tags: 'all',
      tagSide: 'left',
      tagIndent: '2em'
    },
    chtml: {
      scale: 0.5,
      displayAlign: 'right',
      displayIndent: '1em'
    },
    svg: {
      scale: 0.5,
      displayAlign: 'right',
      displayIndent: '1em'
    },
    output: {
      font: 'mathjax-euler',
      displayOverflow: 'scale'
    }
  };
</script>"))))))))


;;; Converting legacy MathJax scales

;; Define a legacy scale as any scale given as a percentage string,
;; such as "150", instead of a unit-interval float, such as 1.5.

(ert-deftest ox-html/mathjax-legacy-scale-default ()
  "Test the legacy scale conversion with the old default value."
  (should
   (= 2
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(scale "100") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "scale: 1.0" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-scale-custom ()
  "Test the legacy scale conversion with a non-default value."
  (should
   (= 2
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(scale "10") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "scale: 0.1" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-scale-in-buffer ()
  "Test the legacy scale conversion with an in-buffer value."
  (should
   (= 2
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: scale: 10"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "scale: 0.1" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-scale-message ()
  "Test the legacy scale conversion message."
  (should
   (= 1
      (seq-count
       (lambda (message)
         (string= "Converting legacy MathJax scale: 20 to 0.2"
                  message))
       (org-test-capture-warnings
         (org-test-with-temp-text "$x$"
           (let ((export-buffer "*Test HTML Export*")
                 (org-export-show-temporary-export-buffer nil)
                 (org-html-mathjax-options
                  (cons '(scale "20") org-html-mathjax-options)))
             (org-export-to-buffer 'html export-buffer
               nil nil nil nil nil
               #'html-mode))))))))

(ert-deftest ox-html/mathjax-legacy-scale-message-in-buffer ()
  "Test the legacy scale conversion message for an in-buffer value."
  (should
   (seq-count
    (lambda (message)
      (string= "Converting legacy MathJax scale: 20 to 0.2"
               message))
    (org-test-capture-warnings
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: scale: 20"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)))))))

(ert-deftest ox-html/mathjax-legacy-scale-ignore ()
  "Test the legacy scale conversion ignores small values."
  (should
   (= 2
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options '((scale "9"))))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "scale: 9" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-scale-invalid ()
  "Test the legacy scale conversion with an invalid value."
  (should
   (= 2
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(scale "xxx") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "scale: 1.0" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-scale-invalid-message ()
  "Test the invalid legacy scale conversion message."
  (should
   (= 1
      (seq-count
       (lambda (message)
         (string= "Non-numerical MathJax scale: xxx"
                  message))
       (org-test-capture-warnings
         (org-test-with-temp-text "$x$"
           (let ((export-buffer "*Test HTML Export*")
                 (org-export-show-temporary-export-buffer nil)
                 (org-html-mathjax-options
                  (cons '(scale "xxx") org-html-mathjax-options)))
             (org-export-to-buffer 'html export-buffer
               nil nil nil nil nil
               #'html-mode))))))))


;;; Converting legacy MathJax auto-numbering

;; NOTE: AMS stands for American Mathematical Society.

(ert-deftest ox-html/mathjax-legacy-autonumber-ams ()
  "Test legacy auto-numbering, when AMS."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(autonumber "AMS") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "tags: 'ams'" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-ams-in-buffer ()
  "Test legacy auto-numbering, when AMS in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: autonumber: AMS"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "tags: 'ams'" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-none ()
  "Test legacy auto-numbering, when disabled."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(autonumber "None") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "tags: 'none'" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-none-in-buffer ()
  "Test legacy auto-numbering, when disabled in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: autonumber: None"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "tags: 'none'" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-all ()
  "Test legacy auto-numbering, when enabled."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(autonumber "All") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "tags: 'all'" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-all-in-buffer ()
  "Test legacy auto-numbering, when enabled in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: autonumber: All"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "tags: 'all'" (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-message ()
  "Test legacy auto-numbering conversion message."
  (should
   (= 1
      (seq-count
       (lambda (message)
         (string= "Converting legacy MathJax option: autonumber"
                  message))
       (org-test-capture-warnings
         (org-test-with-temp-text "$x$"
           (let ((export-buffer "*Test HTML Export*")
                 (org-export-show-temporary-export-buffer nil)
                 (org-html-mathjax-options
                  (cons '(autonumber "AMS") org-html-mathjax-options)))
             (org-export-to-buffer 'html export-buffer
               nil nil nil nil nil
               #'html-mode))))))))

(ert-deftest ox-html/mathjax-legacy-autonumber-message-in-buffer ()
  "Test legacy auto-numbering conversion message."
  (should
   (= 1
      (seq-count
       (lambda (message)
         (string= "Converting legacy MathJax option: autonumber"
                  message))
       (org-test-capture-warnings
         (org-test-with-temp-text "$x$
#+HTML_MATHJAX: autonumber: AMS"
           (let ((export-buffer "*Test HTML Export*")
                 (org-export-show-temporary-export-buffer nil))
             (org-export-to-buffer 'html export-buffer
               nil nil nil nil nil
               #'html-mode))))))))


;;; Converting legacy MathJax fonts

(ert-deftest ox-html/mathjax-legacy-font-tex ()
  "Test legacy font, when TeX."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "TeX") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-tex'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-tex-in-buffer ()
  "Test legacy font, when TeX in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: TeX"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-tex'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-stix-web ()
  "Test legacy font, when STIX-Web."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "STIX-Web") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-stix2'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-stix-web-in-buffer ()
  "Test legacy font, when STIX-Web in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: STIX-Web"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-stix2'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-asana-math ()
  "Test legacy font, when Asana-Math."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "Asana-Math") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-asana'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-asana-math-in-buffer ()
  "Test legacy font, when Asana-Math in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: Asana-Math"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-asana'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-neo-euler ()
  "Test legacy font, when Neo-Euler."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "Neo-Euler") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-euler'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-neo-euler-in-buffer ()
  "Test legacy font, when Neo-Euler in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: Neo-Euler"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-euler'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-gyre-pagella ()
  "Test legacy font, when Gyre-Pagella."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "Gyre-Pagella") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-pagella'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-gyre-pagella-in-buffer ()
  "Test legacy font, when Gyre-Pagella in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: Gyre-Pagella"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-pagella'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-gyre-termes ()
  "Test legacy font, when Gyre-Termes."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "Gyre-Termes") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-termes'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-gyre-termes-in-buffer ()
  "Test legacy font, when Gyre-Termes in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: Gyre-Termes"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-termes'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-latin-modern ()
  "Test legacy font, when Latin-Modern."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(font "Latin-Modern") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-modern'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-font-latin-modern-in-buffer ()
  "Test legacy font, when Latin-Modern in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: font: Latin-Modern"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "font: 'mathjax-modern'"
                               (or "," "\n"))))))))))


;;; Converting legacy MathJax line breaks

(ert-deftest ox-html/mathjax-legacy-line-breaks-true ()
  "Test legacy line breaks, when true."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (append '((linebreaks "true")
                         (overflow "overflow"))
                       org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "displayOverflow: 'linebreak'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-line-breaks-true-in-buffer ()
  "Test legacy line breaks, when true in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: linebreaks: true"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(overflow "overflow") org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "displayOverflow: 'linebreak'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-line-breaks-false ()
  "Test legacy line breaks, when false."
  (should
   (= 1
      (org-test-with-temp-text "$x$"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (append '((linebreaks "false")
                         (overflow "linebreak"))
                       org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "displayOverflow: 'overflow'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-line-breaks-false-in-buffer ()
  "Test legacy line breaks, when true in-buffer."
  (should
   (= 1
      (org-test-with-temp-text "$x$
#+HTML_MATHJAX: linebreaks: false"
        (let ((export-buffer "*Test HTML Export*")
              (org-export-show-temporary-export-buffer nil)
              (org-html-mathjax-options
               (cons '(overflow "linebreak")
                     org-html-mathjax-options)))
          (org-export-to-buffer 'html export-buffer
            nil nil nil nil nil
            #'html-mode)
          (with-current-buffer export-buffer
            (how-many (rx (seq "displayOverflow: 'overflow'"
                               (or "," "\n"))))))))))

(ert-deftest ox-html/mathjax-legacy-line-breaks-message ()
  "Test the legacy line breaks conversion message."
  (should
   (= 1
      (seq-count
       (lambda (message)
         (string= "Converting legacy MathJax option: linebreaks"
                  message))
       (org-test-capture-warnings
         (org-test-with-temp-text "$x$"
           (let ((export-buffer "*Test HTML Export*")
                 (org-export-show-temporary-export-buffer nil)
                 (org-html-mathjax-options (cons '(linebreaks "true")
                                                 org-html-mathjax-options)))
             (org-export-to-buffer 'html export-buffer
               nil nil nil nil nil
               #'html-mode))))))))

(ert-deftest ox-html/mathjax-legacy-line-breaks-message-in-buffer ()
  "Test the legacy scale conversion message for an in-buffer value."
  (should
   (= 1
      (seq-count
       (lambda (message)
         (string= "Converting legacy MathJax option: linebreaks"
                  message))
       (org-test-capture-warnings
         (org-test-with-temp-text "$x$
#+HTML_MATHJAX: linebreaks: true"
           (let ((export-buffer "*Test HTML Export*")
                 (org-export-show-temporary-export-buffer nil))
             (org-export-to-buffer 'html export-buffer
               nil nil nil nil nil
               #'html-mode))))))))


;;; Rendering checkboxes

(ert-deftest ox-html/checkbox-ascii ()
  "Test ascii checkbox rendering"
  (skip-unless (libxml-available-p))
  (should
   (equal
    `(html nil
           (body nil
                 (ul ((class . "org-ul"))
                     (li ((class . "off"))
                         (code nil ,(format "[%c]" (char-from-name "NO-BREAK SPACE"))) " not yet")
                     "
"
                     (li ((class . "on"))
                         (code nil "[X]") " I am done")
                     "
"
                     (li ((class . "trans"))
                         (code nil "[-]") " unclear")
                     "
")))
    (org-test-with-temp-text "
- [ ] not yet
- [X] I am done
- [-] unclear
"
      (let ((export-buffer "*Test HTML Export*")
            (org-export-show-temporary-export-buffer nil))
        (org-export-to-buffer 'html export-buffer
          nil nil nil t nil)
        (with-current-buffer export-buffer
          (libxml-parse-html-region (point-min) (point-max))))))))

(ert-deftest ox-html/checkbox-html ()
  "Test HTML checkbox rendering"
  (skip-unless (libxml-available-p))
  (should
   (equal
    '(ul ((class . "org-ul"))
         (li ((class . "off"))
             (input ((type . "checkbox"))) " not yet")
         (li ((class . "on"))
             (input ((type . "checkbox") (checked . "checked"))) " I am done")
         (li ((class . "trans"))
             (input ((type . "checkbox"))) " unclear"))
    (org-test-with-temp-text "
- [ ] not yet
- [X] I am done
- [-] unclear
"
      (let ((export-buffer "*Test HTML Export*")
            (org-export-show-temporary-export-buffer nil))
        (org-export-to-buffer 'html export-buffer
          nil nil nil t '(:html-checkbox-type html))
        (with-current-buffer export-buffer
          (libxml-parse-xml-region (point-min) (point-max))))))))

(ert-deftest ox-html/checkbox-unicode ()
  "Test HTML checkbox rendering"
  (skip-unless (libxml-available-p))
  (should
   (equal
    '(ul ((class . "org-ul"))
         (li ((class . "off")) "☐ not yet")
         (li ((class . "on")) "☑ I am done")
         (li ((class . "trans")) "☐ unclear"))
    (org-test-with-temp-text "
- [ ] not yet
- [X] I am done
- [-] unclear
"
      (let ((export-buffer "*Test HTML Export*")
            (org-export-show-temporary-export-buffer nil))
        (org-export-to-buffer 'html export-buffer
          nil nil nil t '(:html-checkbox-type unicode))
        (with-current-buffer export-buffer
          (libxml-parse-xml-region (point-min) (point-max))))))))


;;; Rendering Timestamps

(ert-deftest ox-html/plain-timestamps ()
  "Test rendering of timestamps (outside of clock/planning)"
  (org-test-with-temp-text "
- [2025-01-31 Fri]
- [2025-01-31 Fri 14:00]
- <2025-02-18 Tue>
- <2025-02-18 Tue 23:59>
- [2025-02-17 Tue 17:00]--[2025-02-17 Fri 19:00]
"
    (let ((export-buffer "*Test HTML Export")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil t)
      (with-current-buffer export-buffer
        (mapc (lambda (s) (should (search-forward s nil t)))
              '("<span class=\"timestamp\">[2025-01-31 Fri]</span>"
                "<span class=\"timestamp\">[2025-01-31 Fri 14:00]</span>"
                "<span class=\"timestamp\">&lt;2025-02-18 Tue&gt;</span>"
                "<span class=\"timestamp\">&lt;2025-02-18 Tue 23:59&gt;</span>"
                "<span class=\"timestamp\">[2025-02-17 Mon 17:00]&ndash;[2025-02-17 Mon 19:00]</span>"))))))

(ert-deftest ox-html/clock ()
  "Test rendering of clock elements"
  (org-test-with-temp-text "
* Test
:LOGBOOK:
CLOCK: [2025-02-21 Fri 17:43]--[2025-02-21 Fri 17:48] =>  0:05
:END:
"
    (let ((export-buffer "*Test HTML Export")
          (org-export-show-temporary-export-buffer nil)
          (org-export-with-drawers t)
          (org-export-with-clocks t))
      (org-export-to-buffer 'html export-buffer
        nil nil nil t)
      (with-current-buffer export-buffer
        (should (search-forward
                 "<span class=\"timestamp-kwd\">CLOCK:</span> <span class=\"timestamp\">[2025-02-21 Fri 17:43]&ndash;[2025-02-21 Fri 17:48] </span> <span class=\"timestamp\">(0:05)</span>"
                 nil t))))))

(ert-deftest ox-html/planning ()
  "Test rendering of timestamps in planning elements"
  (org-test-with-temp-text "
* Some Item
SCHEDULED: <2025-03-26 Wed> DEADLINE: <2025-03-27 Thu 13:00> CLOSED: [2025-03-25 Tue 19:09]
"
    (let ((export-buffer "*Test HTML Export")
          (org-export-show-temporary-export-buffer nil)
          (org-export-with-planning t))
      (org-export-to-buffer 'html export-buffer
        nil nil nil t)
      (with-current-buffer export-buffer
        (mapc (lambda (s) (should (search-forward s nil t)))
              '("<span class=\"timestamp-kwd\">CLOSED:</span> <span class=\"timestamp\">[2025-03-25 Tue 19:09]</span>"
                "<span class=\"timestamp-kwd\">DEADLINE:</span> <span class=\"timestamp\">&lt;2025-03-27 Thu 13:00&gt; </span>"
                "<span class=\"timestamp-kwd\">SCHEDULED:</span> <span class=\"timestamp\">&lt;2025-03-26 Wed&gt; </span>"))))))

(ert-deftest ox-html/html5-fancy-timestamps ()
  "Test rendering of timestamps with fancy HTML5 enabled"
  (org-test-with-temp-text "
[2025-06-25 Wed]
<2025-06-25 Wed 19:10>
"
   (let ((export-buffer "*Test HTML Export")
         (org-export-show-temporary-export-buffer nil)
         (org-html-doctype "html5")
         (org-html-html5-fancy t))
     (org-export-to-buffer 'html export-buffer
       nil nil nil t)
     (with-current-buffer export-buffer
       (mapc (lambda (s)
               (should (search-forward s nil t)))
             '("<span class=\"timestamp-wrapper\"><time class=\"timestamp\" datetime=\"2025-06-25\">[2025-06-25 Wed]</time></span>"
               "<span class=\"timestamp-wrapper\"><time class=\"timestamp\" datetime=\"2025-06-25T19:10:00\">&lt;2025-06-25 Wed 19:10&gt;</time></span>"))))))


;;; Postamble Format

(ert-deftest ox-html/postamble-default ()
  "Test default postamble"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil nil)
      (with-current-buffer export-buffer
        (should (= 1 (how-many "Validate")))
        (should (= 1 (how-many "Created: ")))))))


(ert-deftest ox-html/postamble-custom ()
  "Test custom postamble"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble "Foobar"))
      (with-current-buffer export-buffer
        (should (= 0 (how-many "Validate")))
        (should (= 0 (how-many "Created: ")))
        (should (= 1 (how-many "Foobar")))))))

(ert-deftest ox-html/postamble-custom-format ()
  "Test a html-postamble option (not -format) containing a format string"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble "Author=%a"
                          :author "Madame Orange"))
      (with-current-buffer export-buffer
        (should (= 0 (how-many "Validate")))
        (should (= 0 (how-many "Created: ")))
        (should (= 1 (how-many "Author=Madame Orange")))))))

(ert-deftest ox-html/postamble-none ()
  "Test no postamble"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble nil))
      (with-current-buffer export-buffer
        (should (= 0 (how-many "Validate")))
        (should (= 0 (how-many "Created: ")))))))

(ert-deftest ox-html/postamble-format-wrong-config ()
  "Test a html-postamble-format option, with incomplete config.

This option is only picked up when html-postamble is set to
T. This test leaves it unset, which means it is set to 'auto,
which will make ox-html skip the html-postamble-format option
entirely."
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble-format (("en" "Foobar"))))
      (with-current-buffer export-buffer
        (should (= 1 (how-many "Validate")))
        (should (= 1 (how-many "Created: ")))
        (should (= 0 (how-many "Foobar")))))))

(ert-deftest ox-html/postamble-format-proper-config ()
  "Test a html-postamble-format option which is just a string"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble-format (("en" "Foobar"))
                          :html-postamble t))
      (with-current-buffer export-buffer
        (should (= 0 (how-many "Validate")))
        (should (= 0 (how-many "Created: ")))
        (should (= 1 (how-many "Foobar")))))))

(ert-deftest ox-html/postamble-format-conflict ()
  "Test conflicting postamble and postamble-format configs"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble-format "The format string"
                          :html-postamble "Regular postamble"))
      (with-current-buffer export-buffer
        (should (= 0 (how-many "Validate")))
        (should (= 0 (how-many "Created: ")))
        (should (= 0 (how-many "The format string")))
        (should (= 1 (how-many "Regular postamble")))))))

(ert-deftest ox-html/postamble-format-author ()
  "Test a html-postamble-format option containing the author"
  (org-test-with-temp-text "Test, hi"
    (let ((export-buffer "*Test HTML Export*")
          (org-export-show-temporary-export-buffer nil))
      (org-export-to-buffer 'html export-buffer
        nil nil nil nil '(:html-postamble-format (("en" "Author=%a"))
                          :html-postamble t
                          :author "Monsieur Oeuf"))
      (with-current-buffer export-buffer
        (should (= 0 (how-many "Validate")))
        (should (= 0 (how-many "Created: ")))
        (should (= 1 (how-many "Author=Monsieur Oeuf")))))))

(ert-deftest ox-html/test-normalize-string-or-function ()
  ;; Test cases for `org-element-normalize-string-or-function'
  (should (string= (org-html-normalize-string-or-function
                    (lambda (_res) "abcdefg") nil) "abcdefg\n"))
  (should (string= (org-html-normalize-string-or-function "abcdefg")
                   "abcdefg\n"))
  (should (= (org-html-normalize-string-or-function 123 nil) 123)))


;;; Rendering Table of Contents list

(ert-deftest org-html/test-toc-text ()
  "Test the generation of HTML TOC lists by `org-html--toc-text'."
  ;; Test 1: Standard TOC (scope is nil)
  (let ((toc-entries '(("1" . 1) ("1.1" . 2) ("2" . 1)))
        (expected "\n<ul>\n<li>1\n<ul>\n<li>1.1</li>\n</ul>\n</li>\n<li>2</li>\n</ul>\n"))
    (should (string= (org-html--toc-text toc-entries nil) expected)))
  ;; Test 2: TOC starting with a non-toplevel headline.
  ;; This case, specific to a global TOC (scope is nil), checks
  ;; if the function correctly wraps the output in outer lists
  ;; to represent the skipped headline levels.
  (let ((toc-entries '(("1" . 2) ("1.1" . 3) ("2" . 1)))
        (expected "\n<ul>\n<li>\n<ul>\n<li>1\n<ul>\n<li>1.1</li>\n</ul>\n</li>\n</ul>\n</li>\n<li>2</li>\n</ul>\n"))
    (should (string= (org-html--toc-text toc-entries nil) expected))))

(provide 'test-ox-html)
;;; test-ox-html.el ends here
