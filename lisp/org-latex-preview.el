;;; org-latex-preview.el --- LaTeX previews for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Authors: TEC <contact@tecosaur.net> and Karthik Chikmagalur
;; Keywords: tex, extensions, tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Library to support LaTeX previews in Org mode.
;;
;; This library provides the command `org-latex-preview' to generate
;; or hide preview images for LaTeX fragments or environments in Org
;; buffers.
;;
;; The minor-mode `org-latex-preview-mode' can be turned on for more
;; dynamic and interactive previews:
;; - LaTeX preview images will be hidden and shown again when the
;;   cursor enters and exits the corresponding LaTeX fragment.
;; - Previews will be automatically regenerated when the cursor leaves
;;   an edited fragment.
;; - Previews for LaTeX fragments in text yanked or inserted into the
;;   buffer will be automatically generated
;; - If `org-latex-preview-mode-display-live' is set, previews will be
;;   continuously updated and displayed next to the fragment as they
;;   are edited.
;;
;; Additionally, this library provides general purpose functions for
;; generating, displaying or managing latex previews anywhere in Emacs:
;; - `org-latex-preview-place' can be used to place image previews
;;   over the text of LaTeX fragments or environments in any buffer
;;   in Emacs.  This works in any major-mode.
;; - `org-latex-preview-create-images' accepts a list of LaTeX
;;   fragment strings and returns a list of images.
;; - `org-latex-preview-cache-images' produces preview images for all
;;   LaTeX fragments in an Org parse-tree.  This is intended for use
;;   by Org export backends.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-compat)
(require 'ox-latex)

(declare-function org-persist-read "org-persist")
(declare-function org-persist-register "org-persist")
(declare-function org-persist-unregister "org-persist")
(declare-function eldoc--invoke-strategy "eldoc")

;;;###autoload
(defvar org-latex-preview--dvisvgm3-minor-version
  (or (and (executable-find "dvisvgm")
           (with-temp-buffer
             (call-process "dvisvgm" nil t nil "--version")
             (let ((ver (version-to-list
                         (string-trim (buffer-string) "dvisvgm "))))
               (and (= (car ver) 3) (cadr ver)))))
      -1)
  "The minor version of dvisvgm, if dvisvgm 3 is installed.  Otherwise -1.")

(defgroup org-latex-preview nil
  "Options for generation of LaTeX previews in Org mode."
  :tag "Org LaTeX Preview"
  :group 'org)


;;;; User options
;;;###autoload
(defcustom org-latex-preview-appearance-options
  '( :foreground auto :background "Transparent"
     :scale 1.0 :zoom 1.0 :page-width 0.6)
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  The foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
             `auto' means use the foreground from the text face.
:background  The background color, or \"Transparent\".
             `default' means use the background of the default face.
             `auto' means use the background from the text face.
:scale       A scaling factor for the size of the images, to get more pixels
:zoom        when the image has associated font-relative height information,
             the display size is scaled by this factor.
:page-width  The width of the LaTeX document fragments are compiled in.
             Either:
             - A string giving a LaTeX dimension (e.g. \"12cm\").
             - A floating point value between 0.0 and 1.0,
               this sets the text width to this ratio of the page width.
             - nil, in which case the default text width is unmodified."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type 'plist)

(defcustom org-latex-preview-process-default
  (if (executable-find "dvisvgm") 'dvisvgm 'dvipng)
  "The default process to convert LaTeX fragments to image files.
All available processes and theirs documents can be found in
`org-latex-preview-process-alist', which see."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type 'symbol)

;;;###autoload
(defcustom org-latex-preview-process-alist
    `((dvipng
       :programs ("latex" "dvipng")
       :description "dvi > png"
       :message "you need to install the programs: latex and dvipng."
       :image-input-type "dvi"
       :image-output-type "png"
       :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
       :image-converter ("dvipng --follow -D %D -T tight --depth --height -o %B-%%09d.png %f")
       :transparent-image-converter
       ("dvipng --follow -D %D -T tight -bg Transparent --depth --height -o %B-%%09d.png %f"))
      (dvisvgm
       :programs ("latex" "dvisvgm")
       :description "dvi > svg"
       :message "you need to install the programs: latex and dvisvgm."
       :image-input-type "dvi"
       :image-output-type "svg"
       :latex-compiler ("%l -interaction nonstopmode -output-directory %o %f")
       ;; The --optimise, --clipjoin, and --relative flags cause dvisvgm to
       ;; do some extra work to tidy up the SVG output, but barely add to
       ;; the overall dvisvgm runtime (<1% increace, from testing).
       :image-converter
       ,(list (concat "dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts"
                      (if (>= org-latex-preview--dvisvgm3-minor-version 2)
                          " -v3 --message='processing page {?pageno}: output written to {?svgpath}'" "")
                      " --bbox=preview -o %B-%%9p.svg %f")))
      (imagemagick
       :programs ("pdflatex" "convert")
       :description "pdf > png"
       :message "you need to install the programs: latex and imagemagick."
       :image-input-type "pdf"
       :image-output-type "png"
       :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
       :image-converter
       ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png")))
  "Definitions of external processes for LaTeX previewing.
Org mode can use some external commands to generate TeX snippet's images for
previewing or inserting into HTML files, e.g., \"dvipng\".  This variable tells
`org-latex-preview-create-image' how to call them.

The value is an alist with the pattern (NAME . PROPERTIES).  NAME is a symbol.
PROPERTIES accepts the following attributes:

  :programs           list of strings, required programs.
  :description        string, describe the process.
  :message            string, message it when required programs cannot be found.
  :image-input-type   string, input file type of image converter (e.g., \"dvi\").
  :image-output-type  string, output file type of image converter (e.g., \"png\").
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by option `org-latex-preview-preamble',
                      `org-latex-default-packages-alist' and
                      `org-latex-packages-alist', which see.
  :latex-compiler list of LaTeX commands, as strings or a function.
                      Each of them is given to the shell.
                      Place-holders \"%t\", \"%b\" and \"%o\" are
                      replaced with values defined below.
                      When a function, that function should accept the
                      file name as its single argument.
  :image-converter list of image converter commands strings or a
                      function.  Each of them is given to the shell
                      and supports any of the following place-holders
                      defined below.
                      When a function, that function should accept the
                      file name as its single argument.

If set, :transparent-image-converter is used instead of :image-converter to
convert an image when the background color is nil or \"Transparent\".

Place-holders used by `:image-converter' and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders used only by `:latex-compiler':

  %l   LaTeX compiler command string
  %L   LaTeX compiler command name

Place-holders used only by `:image-converter':

  %B    absolute base name of input file
  %D    dpi, used to adjust image size by some processing commands."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(alist :tag "LaTeX to image backends"
                :value-type (plist))
  :risky t)

(defcustom org-latex-preview-compiler-command-map
  '(("pdflatex" . "latex")
    ("xelatex" . "xelatex -no-pdf")
    ("lualatex" . "dvilualatex"))
  "An alist mapping from each of `org-latex-compilers' to command strings.
Each key is a LaTeX compiler name, for each compiler in
`org-latex-compilers', and the value the command that should be used
when producing a preview (optionally including flags).

This should only ever be changed in the event that PDF, not DVI output
is required."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(alist :tag "Compiler"
          :value-type (string :type "command")))

(defcustom org-latex-preview-cache 'persist
  "Persist produced LaTeX previews across Emacs sessions.

It can be set to
- the cache directory path (relative or absolute path, a string),
- the symbol persist to cache using org-persist,
- or the symbol temp to cache in a temporary directory.  In this case
  the images will be accessible for the duration of the Emacs session
  only.

It may also be set to the symbol live, this is for tracking live updates
and meant for internal use only."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(choice (const :tag "Use org-mode's persistent cache system" persist)
                 (const :tag "Use the system temporary directory" temp)
                 (string :tag "Path to cache directory")))

(defcustom org-latex-preview-persist-expiry 7
  "A homologue of `org-persist-default-expiry' for preview data.
This is only relevant when `org-latex-preview-cache' is set to
persist."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(choice (const :tag "Never" never)
          (const :tag "Always" nil)
          (number :tag "Keep N days")
          (function :tag "Function")))

(defcustom org-latex-preview-numbered t
  "Whether to calculate and apply correct equation numbering.
When nil, equation numbering is disabled and a diamond symbol is
shown in place of the equation number.

When non-nil, equation numbering is tracked across the document.

Alternatively, when set to the symbol \"preview\" numbering will
simply be left as the automatic LaTeX numbering generated when
previewing the batch of fragments.  This may be mostly-correct,
or mostly-incorrect depending on the situation."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(choice (const :tag "No" nil)
          (const :tag "Preview " preview)
          (const :tag "Yes" t)))

(defcustom org-latex-preview-process-active-indicator 'fringe
  "The style of visual indicator for LaTeX currently being processed.
This sets the method used to indicated that a LaTeX fragment is
currently being processed for display.

There are three recognised value symbols:
- nil, do not indicate fragment processing.
- face, apply a special face to fragments that are being processed.
  You can customize the face `org-latex-preview-processing-face' to
  change how it appears.
- fringe, apply a fringe marker to lines where fragments are being
  processed."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(choice
          (const :tag "No indicator" nil)
          (const :tag "Fringe marker" fringe)
          (const :tag "Processing face" face)))

(defcustom org-latex-preview-mode-ignored-environments '("figure")
  "List of LaTeX environments that should not be automatically previewed."
  :type '(repeat string)
  :package-version '(Org . "10.0")
  :group 'org-latex-preview)

(defcustom org-latex-preview-mode-ignored-commands nil
  "List of movement commands that should not affect preview display.

When these commands are invoked, they will not cause previews to
be revealed when using `org-latex-preview-mode'."
  :type '(repeat symbol)
  :package-version '(Org . "10.0")
  :group 'org-latex-preview)

(defcustom org-latex-preview-process-finish-functions nil
  "Abnormal hook run after preview generation.

Each function in this hook is called with three arguments:

- The exit-code of the preview generation process.  More
  specifically, this is the exit-code of the image-converter, the
  final process in the chain of processes that generates a LaTeX
  preview image.

- The process buffer.

- The processing-info plist, containing the state of the LaTeX
  preview process.  See `org-latex-preview--create-image-async'
  for details.

This hook can be used for introspection of or additional
processing after the LaTeX preview process."
  :group 'org-latex-preview
  :type 'hook)

(defcustom org-latex-preview-overlay-update-functions nil
  "Abnormal hook run after a preview overlay is updated.

Each function in this hook is called with one argument, the
overlay that was updated."
  :group 'org-latex-preview
  :type 'hook)

(defcustom org-latex-preview-overlay-close-functions nil
  "Abnormal hook run after placing a LaTeX preview image.

This hook typically runs when the cursor is moved out of a LaTeX
fragment or environment with `org-latex-preview-mode'
active, causing the display of text contents to be replaced by
the corresponding preview image.

Functions in this hook are called with one argument, the overlay
that the cursor moved out of."
  :group 'org-latex-preview
  :type 'hook)

(defcustom org-latex-preview-overlay-open-functions nil
  "Abnormal hook run after hiding a LaTeX preview image.

This hook typically runs when the cursor is moved into a LaTeX
fragment or environment with `org-latex-preview-mode'
active, causing the display of the preview image to be replaced
by the corresponding LaTeX fragment text.

Functions in this hook are called with one argument, the overlay
that the cursor moved into."
  :group 'org-latex-preview
  :type 'hook)

(defface org-latex-preview-processing-face '((t :inherit shadow))
  "Face applied to LaTeX fragments for which a preview is being generated.

See `org-latex-preview-process-active-indicator'."
  :group 'org-faces)

(defconst org-latex-preview--image-log "*Org Preview Convert Output*"
  "Buffer name for Preview image conversion output.")
(defconst org-latex-preview--latex-log "*Org Preview LaTeX Output*"
  "Buffer name for Preview LaTeX output.")

(defconst org-latex-preview--temp-cache-dir
  (expand-file-name "org-latex-preview" (temporary-file-directory))
  "Folder used to cache temp-stored previews.")

(defconst org-latex-preview-live--cache-dir
  (expand-file-name "org-latex-preview-mode-display-live"
                    (temporary-file-directory))
  "Folder used to cache live previews.")

(defcustom org-latex-preview-preamble "\\documentclass{article}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\usepackage{xcolor}"
  "The document header used for processing LaTeX fragments.

It is imperative that this header make sure that no page number appears
on the page.  The package defined in the variables
`org-latex-default-packages-alist' and `org-latex-packages-alist' will
replace the placeholder \"[PACKAGES]\" in this header.  See
`org-splice-latex-header' for more details on the available placeholders
and template expansion."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type 'string)

(defcustom org-latex-preview-process-precompile t
  "Use LaTeX header precompilation when previewing fragments.
This causes a slight delay the first time `org-latex-pdf-process'
is called in a buffer, but subsequent calls will be faster.

This requires the LaTeX package \"mylatexformat\" to be installed."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type 'boolean)

(defcustom org-latex-preview-mode-track-inserts t
  "Whether `org-latex-preview-mode' should apply to newly inserted fragments.

When `org-latex-preview-mode' is on, existing LaTeX previews
will be automatically hidden/shown on cursor movement and
regenerated after edits.  This option controls how newly inserted
and edited fragments are previewed.

The following values are supported:

- t: Generate previews for newly inserted fragments.

- nil: Do not generate previews for newly inserted fragments.

Existing previews are always updated after the cursor moves out of them.

Note: Insertion tracking does not work across indirect buffers.  LaTeX
preview images are generated only for the current buffer, and not any
indirect clones, or for the base buffer if the current buffer is a
clone.  This is currently a limitation of the LaTeX preview system."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(choice
          (const :tag "Track inserts" t)
          (const :tag "Don't track inserts" nil)))

(defconst org-latex-preview--tentative-math-re
  "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"
  "Regexp whith will match all instances of LaTeX math.
Note that this will also produce false postives, and
`org-element-context' should be used to verify that matches are
indeed LaTeX fragments/environments.")

(defconst org-latex-preview--ignored-faces
  '(org-indent whitespace-face)
  "Faces that should not affect the color of preview overlays.")

(defconst org-latex-preview--svg-fg-standin "#000001"
  "Hex color that is used as a stand-in for the current color.
The entire purpose of this is to be replaced by \"currentColor\"
in `org-latex-preview--svg-make-fg-currentColor', and so it
should be a color that is extremely likely not otherwise found in
the image.")

(defconst org-latex-preview--overlay-priority -80
  "The priority used with preview overlays.")


;;;; Code for `org-latex-preview-mode':
;;
;; The boundaries of latex preview image overlays are automatically
;; extended to track changes in the underlying text by the functions
;; `org-latex-preview-mode--insert-front-handler' and
;; `org-latex-preview-mode--insert-behind-handler'.  These are placed
;; in the `insert-in-front-hooks' and `insert-behind-hooks' properties
;; of the image overlays. See (info "(elisp) Overlay Properties").
;;
;; Additionally, when an overlay's text is modified,
;; `org-latex-preview-mode--mark-overlay-modified', placed in the
;; overlay's modification hook, notes this in the overlay's
;; `org-preview-state' property.
;;
;; This code examines the previous and current cursor positions after
;; each command.  It uses the variables
;; `org-latex-preview-mode--from-overlay' and
;; `org-latex-preview-mode--marker' to track this.
;;
;; If the cursor has moved out of or into a latex preview overlay, the
;; overlay is changed to display or hide its image respectively.  The
;; functions `org-latex-preview-mode--handle-pre-cursor' and
;; `org-latex-preview-mode--handle-post-cursor' do this.  These are
;; placed in `pre-command-hook' and `post-command-hook' respectively.
;;
;; When the cursor positions pre- and post-command are inside an
;; overlay, it uses the overlay property `org-view-text' to check if
;; the source and destination overlays are distinct.  If they are it
;; shows and hides images as appropriate.  Hidden images are stored in
;; the overlay property `org-preview-image'.
;;
;; If the latex fragment text for an existing overlay is modified, a
;; new preview image will be generated automatically.  The
;; modification state of the overlay is stored in the overlay property
;; `org-preview-state', and the function
;; `org-latex-preview-mode--close-previous-overlay' handles the
;; recompilation.
;;
;; When the user option `org-latex-preview-mode-track-inserts' is
;; non-nil, previews are auto-generated for latex fragments as they
;; are inserted into the buffer.  This work is handled by
;; `org-latex-preview-mode--detect-fragments-in-change', which is added to
;; `after-change-functions'.  It does this by placing dummy overlays
;; that don't display images, but are marked as having been modified.
;;
;; LaTeX preview overlay property summary:
;;
;; org-view-text: non-nil if the overlay text (LaTeX fragment text) is
;; visible, nil otherwise.
;;
;; org-preview-state: modified or nil.  modified when the text of the
;; overlay has been modified.
;;
;; org-preview-image: Temporary store for the overlay preview image
;; spec when displaying the overlay text.
;;
;; org-hidden-face: Temporary store for the overlay's face when
;; displaying the overlay text.  This is relevant for SVG preview
;; images, which inherit their face from the overlay face property.

(defun org-latex-preview--ensure-overlay (beg end)
  "Build an overlay between BEG and END."
  (let (ov)
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o 'org-overlay-type)
                'org-latex-overlay)
        (if (or ov (not (and (= beg (overlay-start o))
                             (= end (overlay-end o)))))
            (delete-overlay o)
          (setq ov o)
          ;; Reset all potentially modified properties.
          (overlay-put ov 'face nil)
          (overlay-put ov 'help-echo nil)     ;tooltip error display
          (overlay-put ov 'before-string nil) ;error fringe marker
          ;; (overlay-put ov 'org-hidden-face nil)   ;(re)store svg face
          ;; We do not set the display property of preview image
          ;; overlays to nil when ensuring that an overlay exists.
          ;; This causes flicker during regeneration as the the
          ;; underlying text is shown and then replaced with the new
          ;; image.
          ;;
          ;; We also do not reset the image spec stored in the
          ;; `org-preview-image' property, or the state of the preview
          ;; stored in the `org-view-text' property, as persisting the
          ;; state of an already existing overlay is required for live
          ;; previews.
          (overlay-put ov 'org-preview-state nil)))) ;is fragment modified?
    (unless ov
      (setq ov (make-overlay beg end nil 'front-advance))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'priority org-latex-preview--overlay-priority)
      (overlay-put ov 'modification-hooks
                   (list #'org-latex-preview-mode--mark-overlay-modified))
      (overlay-put ov 'insert-in-front-hooks
                   (list #'org-latex-preview-mode--insert-front-handler))
      (overlay-put ov 'insert-behind-hooks
                   (list #'org-latex-preview-mode--insert-behind-handler))
      (run-hook-with-args 'org-latex-preview-overlay-update-functions ov))
    ov))

(defun org-latex-preview--indicate-processing (ov &optional on)
  "Modify OV to provide visual indication of LaTeX fragment preview generation.

When `org-latex-preview-process-active-indicator' is set to fringe, a
triangle in the left fringe will be shown or hidden depending on ON.

When `org-latex-preview-process-active-indicator' is set to face, the
overlay face is set to `org-latex-preview-processing-face'."
  (pcase org-latex-preview-process-active-indicator
    ('fringe
     (overlay-put
      ov 'before-string
      (and on (propertize "!" 'display
                          `(left-fringe right-triangle
                            fringe)))))
    ('face
     (overlay-put ov 'face (and on 'org-latex-preview-processing-face)))))

(defun org-latex-preview-mode--mark-overlay-modified (ov after-p _beg _end &optional _l)
  "When AFTER-P mark OV as modified and display nothing."
  (when after-p
    (unless (eq (overlay-get ov 'org-preview-state) 'modified)
      (overlay-put ov 'org-preview-state 'modified)
      (overlay-put ov 'face nil)
      (overlay-put ov 'display nil))))

(defun org-latex-preview--update-overlay (ov path-info)
  "Update the overlay OV to show the image specified by PATH-INFO."
  (let* ((zoom (or (plist-get org-latex-preview-appearance-options :zoom) 1.0))
         (height (plist-get (cdr path-info) :height))
         (depth (plist-get (cdr path-info) :depth))
         (errors (plist-get (cdr path-info) :errors))
         (image-type (plist-get (cdr path-info) :image-type))
         (image-display
          (and (car path-info)
               (list 'image
                     :type image-type
                     :file (car path-info)
                     :height (and height (cons (* height zoom) 'em))
                     :ascent (if (and depth height)
                                 ;; The baseline seems to tend to sit slightly
                                 ;; lower than it should be, and a very mild
                                 ;; bias seems to improve the visual result.
                                 ;; From testing with a collecting of LaTeX
                                 ;; maths fonts (cm, cmbright, arev, pxfonts,
                                 ;; notomath, nextxsf, eulervm) decreacing the
                                 ;; depth measurement by 0.02pt in the baseline
                                 ;; calculation seems to work well.
                                 ;; I have yet to come across any situation
                                 ;; where this results in a negative depth,
                                 ;; however we may as well ensure that never
                                 ;; occurs.
                                 (round (* 100 (- 1 (/ (max 0.0 (- depth 0.02))
                                                       height))))
                               'center)))))
    (overlay-put ov 'org-preview-image image-display)
    (cond
     ((eq image-type 'svg)
      (overlay-put
       ov 'org-hidden-face
       (or (and errors 'error)
           (org-latex-preview--face-around
            (overlay-start ov) (overlay-end ov)))))
     (errors
      (overlay-put
       ov 'before-string
       (propertize "!" 'display
                   `(left-fringe exclamation-mark error)))))
    (when org-latex-preview-process-active-indicator
      (org-latex-preview--indicate-processing ov))
    ;; This is a temporary measure until a more sophisticated
    ;; interface for errors is available in Org.
    (when errors
      (overlay-put
       ov 'help-echo
       (if (bound-and-true-p tooltip-mode)
           errors
         (concat (propertize "! " 'face '(bold error))
                 (substring (replace-regexp-in-string "[\n\r\t ]+" " " errors) 2)))))
    (unless (overlay-get ov 'org-view-text) ;Live previewing this element, update in background
      (when image-display (overlay-put ov 'display image-display))
      (overlay-put ov 'face (overlay-get ov 'org-hidden-face)))
    (run-hook-with-args 'org-latex-preview-overlay-update-functions ov)))

(defun org-latex-preview--face-around (start end)
  "Return the relevant face symbol(s) around the region START to END.
A relevant face symbol before START is prefered, with END
examined if none could be found, and finally the default face
used as the final fallback.

Faces in `org-latex-preview--ignored-faces' are ignored."
  (let ((face (or (and (> start (point-min))
                       (not (eq (char-before start) ?\n))
                       (get-text-property (1- start) 'face))
                  (and (> (point-max) end)
                       (not (eq (char-after end) ?\n))
                       (get-text-property end 'face))))
        (normalising-face
         (if (>= emacs-major-version 29) 'default '(:inherit default :extend t))))
    (cond
     ((consp face)
      (nconc (cl-set-difference face org-latex-preview--ignored-faces) (list normalising-face)))
     ((and face (not (memq face org-latex-preview--ignored-faces)))
      (list face normalising-face))
     (t normalising-face))))

(defvar-local org-latex-preview-mode--from-overlay nil
  "Whether the cursor if starting from within a preview overlay.")
(defvar-local org-latex-preview-mode--marker nil
  "Marker to keep track of the previous cursor position.
This helps with tracking cursor movement into and out of preview overlays.")
(defvar-local org-latex-preview-mode--inhibit nil
  "Delay the state machine that decides to auto-generate preview fragments.")

(defsubst org-latex-preview-mode--move-into (ov)
  "Adjust column when moving into the overlay OV from below."
  (when (> (marker-position org-latex-preview-mode--marker)
           (line-end-position))
    (goto-char (overlay-end ov))
    (goto-char (max (line-beginning-position)
                    (overlay-start ov)))))

(defun org-latex-preview-mode--handle-pre-cursor ()
  "Record the previous state of the cursor position.

This keeps track of the cursor relative to the positions of
Org latex preview overlays.

This is intended to be placed in `pre-command-hook'."
  (if org-latex-preview-mode--inhibit
      (setq org-latex-preview-mode--inhibit nil)
    (setq org-latex-preview-mode--from-overlay
          (cl-loop for o in (overlays-at (point))
                   thereis (eq (overlay-get o 'org-overlay-type)
                               'org-latex-overlay)))
    (set-marker org-latex-preview-mode--marker (point))))

(defun org-latex-preview-mode--handle-post-cursor ()
  "Toggle or generate LaTeX previews based on cursor movement.

If the cursor is moving into a preview overlay, \"open\" it to
display the underlying latex fragment.  If the cursor is moving
out of a preview overlay, show the image again or generate a new
one as appropriate.

This is intended to be placed in `post-command-hook'."
  (let ((into-overlay-p
         (cl-loop for o in (overlays-at (point))
                  thereis (eq (overlay-get o 'org-overlay-type)
                              'org-latex-overlay))))
    (cond
     ((and into-overlay-p org-latex-preview-mode--from-overlay)
      (unless (or (get-char-property (point) 'org-view-text)     ;Moved within Same overlay
                  (= (point) org-latex-preview-mode--marker) ;Did not move point
                  (invisible-p (point))) ;Overlay in invisible region
        ;; Jumped from overlay to overlay
        (org-latex-preview-mode--close-previous-overlay)
        (org-latex-preview-mode--open-this-overlay)))
     ((and into-overlay-p (not org-latex-preview-mode--from-overlay))
      (unless (invisible-p (point)) ;Overlay in invisible region
        ;; Moved into overlay
        (org-latex-preview-mode--open-this-overlay)))
     (org-latex-preview-mode--from-overlay
      ;; Moved out of overlay
      (org-latex-preview-mode--close-previous-overlay)))
    (set-marker org-latex-preview-mode--marker (point))))

(defun org-latex-preview-mode--detect-fragments-in-change (beg end _)
  "Examine the content between BEG and END, and preview LaTeX fragments found.
This is only active when either
`org-latex-preview-mode-track-inserts' or
`org-latex-preview-mode-display-live' is enabled."
  (when (or org-latex-preview-mode-track-inserts org-latex-preview-mode-display-live)
    (let ((initial-point (point))
          fragments)
      (save-excursion
        ;; Find every location in the changed region where a backslash
        ;; is succeeded by a parenthesis or square bracket, and check
        ;; for a LaTeX fragment.
        (goto-char beg)
        (unless (eobp)
          (while (search-forward "\\" end t)
            (and (memq (char-after) '(?\( ?\) ?\[ ?\]))
                 (push (org-latex-preview-mode--maybe-track-element-here
                        'latex-fragment initial-point)
                       fragments))))
        ;; Find every location in the changed region where a parenthesis
        ;; or square bracket is preceeded by a backslash, and check for
        ;; a LaTeX fragment.
        (goto-char beg)
        (unless (bobp)
          (while (re-search-forward "[][()]" end t)
            (and (eq (char-before (1- (point))) ?\\)
                 (push (org-latex-preview-mode--maybe-track-element-here
                        'latex-fragment initial-point)
                       fragments))))
        ;; Check for LaTeX environments on lines affected by the change.
        ;; Start by finding all affected lines with at least four
        ;; characters of content. Then we can check if the line starts
        ;; with "\beg" or "\end", and if so check for a LaTeX environment.
        (goto-char beg)
        (forward-line 0)
        (skip-chars-forward " \t")
        (when (< (point) end)
          (let ((line-start-positions (and (> (point-max) (+ 4 (point)))
                                           (list (point)))))
            (while (and (< (point) end) (= 0 (forward-line 1)))
              (skip-chars-forward " \t")
              (when (> (point-max) (+ 4 (point)))
                (push (point) line-start-positions)))
            (dolist (line-start line-start-positions)
              (goto-char line-start)
              (and (eq (char-after) ?\\)
                   (member (buffer-substring (point) (+ (point) 4))
                           '("\\beg" "\\end"))
                   (push (org-latex-preview-mode--maybe-track-element-here
                          'latex-environment initial-point)
                         fragments))))))
      (when (setq fragments (delq nil fragments))
        (when (and org-latex-preview-numbered
                   (cl-find 'latex-environment fragments
                            :key #'org-element-type :test #'eq))
          (nconc fragments (org-latex-preview--get-numbered-environments
                         end nil)))
        (org-latex-preview--place-from-elements
         org-latex-preview-process-default
         fragments)))))

(defun org-latex-preview-mode--maybe-track-element-here (type pos)
  "Check for an org element of TYPE at `point' and ensure an overlay exists.
If POS lies within the element, nil is returned.  Otherwise the
element is returned to be used to generate a preview.

If an org-latex-overlay is already present, nothing is done."
  (and (cl-loop for o in (overlays-at (point))
                never (eq (overlay-get o 'org-overlay-type)
                          'org-latex-overlay))
       (when-let* ((element (org-element-context))
                   ((eq (org-element-type element) type))
                   (elem-beg (or (org-element-property :post-affiliated element)
                                 (org-element-property :begin element)))
                   (elem-end (org-with-wide-buffer
                               (goto-char (org-element-property :end element))
                               (skip-chars-backward "\n\r\t ")
                               (point)))
                   ((not (and (eq type 'latex-environment)
                              (save-excursion
                                (goto-char elem-beg)
                                (looking-at "\\\\begin{\\([^}]+\\)}"))
                              (member (match-string 1)
                                      org-latex-preview-mode-ignored-environments))))
                   (ov (org-latex-preview--ensure-overlay elem-beg elem-end)))
         (overlay-put ov 'org-preview-state 'modified)
         (if (<= elem-beg pos elem-end)
             (progn
               (overlay-put ov 'org-view-text t)
               ;; Record a position safely inside the created overlay
               (set-marker org-latex-preview-mode--marker
                           (min pos (1- elem-end)))
               (setq org-latex-preview-mode--from-overlay t)
               nil)
           (setq org-latex-preview-mode--inhibit t)
           element))))

(defun org-latex-preview-mode--open-this-overlay ()
  "Open Org latex preview image overlays.

If there is a latex preview image overlay at point, hide the
image and display its text."
  (dolist (ov (overlays-at (point)))
    (when (and (eq (overlay-get ov 'org-overlay-type)
                   'org-latex-overlay)
               (not (memq this-command
                          org-latex-preview-mode-ignored-commands)))
      (overlay-put ov 'display nil)
      (overlay-put ov 'org-view-text t)
      (when-let* ((f (overlay-get ov 'face)))
        (overlay-put ov 'org-hidden-face f)
        (overlay-put ov 'face nil))
      (org-latex-preview-mode--move-into ov)
      (setq org-latex-preview-mode--from-overlay nil)
      (run-hook-with-args 'org-latex-preview-overlay-open-functions ov))))

(defun org-latex-preview-mode--close-previous-overlay ()
  "Close Org latex preview image overlays.

If there is a latex preview image overlay at the previously
recorded cursor position, hide its text and display the
image.  The preview image is regenerated if necessary."
  (dolist (ov (overlays-at (marker-position org-latex-preview-mode--marker)))
    (when (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
      (overlay-put ov 'org-view-text nil)
      (if (eq (overlay-get ov 'org-preview-state) 'modified)
          ;; It may seem odd to use an timer for this action, but by
          ;; introducing a brief window for Emacs to deal with input
          ;; events triggered during prior processing the perceptible
          ;; delay is reduced.  Setting an 0.01s timer isn't
          ;; necesarily the optimal duration, but from a little
          ;; testing it appears to be fairly reasonable.
          (run-at-time 0.01 nil #'org-latex-preview-mode--regenerate-overlay ov)
        (when-let* ((f (overlay-get ov 'org-hidden-face)))
          (unless (eq f 'org-latex-preview-processing-face)
            (overlay-put ov 'face f))
          (overlay-put ov 'org-hidden-face nil))
        (overlay-put ov 'display (overlay-get ov 'org-preview-image)))
      (run-hook-with-args 'org-latex-preview-overlay-close-functions ov))))

(defun org-latex-preview-mode--regenerate-overlay (ov &optional inhibit-renumbering)
  "Regenerate the LaTeX fragment under overlay OV.

When `org-latex-preview-numbered' is non-nil, and the overlay
being updated is for an environment, other numbered environments
will be updated should their numbering change.

Numbering checking and updating can be prevented by setting
INHIBIT-RENUMBERING to a non-nil value."
  (with-current-buffer (overlay-buffer ov)
    (let* ((start (overlay-start ov))
           (end (overlay-end ov))
           (fragment (save-excursion
                       (goto-char start)
                       (org-element-context)))
           (others (and org-latex-preview-numbered
                        (not inhibit-renumbering)
                        (eq (org-element-type fragment) 'latex-environment)
                        (org-latex-preview--get-numbered-environments end nil))))
      (if (memq (org-element-type fragment)
                '(latex-fragment latex-environment))
          (if (org-latex-preview--empty-fragment-p
               (org-element-property :value fragment))
              (progn (delete-overlay ov)
                     (org-latex-preview--ensure-overlay start end))
            (org-latex-preview--place-from-elements
             org-latex-preview-process-default
             (cons fragment others)))
        (delete-overlay ov)
        (when others
          (org-latex-preview--place-from-elements
           org-latex-preview-process-default
           others))))))

(defun org-latex-preview-mode--insert-front-handler
    (ov after-p _beg end &optional _length)
  "Extend Org LaTeX preview text boundaries when editing previews.

OV is the overlay displaying the preview.  For the meaning of
AFTER-P, END and the other arguments, see the
`modification-hooks' property of overlays in the Elisp
manual: (elisp) Overlay Properties."
  (when after-p
    (unless undo-in-progress
      (if (eq (overlay-get ov 'org-preview-state) 'active)
          (move-overlay ov end (overlay-end ov))))))

(defun org-latex-preview-mode--insert-behind-handler
    (ov after-p beg _end &optional _length)
  "Extend Org LaTeX preview text boundaries when editing previews.

OV is the overlay displaying the preview.  For the meaning of
AFTER-P, BEG and the other arguments, see the
`modification-hooks' property of overlays in the Elisp
manual: (elisp) Overlay Properties."
  (when after-p
    (unless undo-in-progress
      (if (eq (overlay-get ov 'org-preview-state) 'active)
          (move-overlay ov (overlay-end ov) beg)))))

(defun org-latex-preview-mode--org-cycle (state)
  "Close preview overlays at point when cycling visibility.

Check using STATE if the region containing the overlay is hidden
by `org-cycle', and close any open preview overlays."
  (when (memq state '(overview content folded children))
    (org-latex-preview-mode--close-previous-overlay)))

;;;###autoload
(define-minor-mode org-latex-preview-mode
  "Minor mode to automatically preview LaTeX fragments.

When LaTeX preview mode is on, LaTeX fragments in Org
buffers are automatically previewed after being inserted, and
hidden when the cursor moves into them.  This allows one to
seamlessly edit and preview LaTeX in Org buffers.

To enable auto-toggling of the preview images without
auto-generating them or vice-versa, customize the variable
`org-latex-preview-mode-track-inserts'.

To enable previews of LaTeX fragments while writing them,
customize the variable `org-latex-preview-mode-display-live'."
  :global nil
  (if org-latex-preview-mode
      (progn
        (setq org-latex-preview-mode--marker (make-marker))
        (add-hook 'pre-command-hook #'org-latex-preview-mode--handle-pre-cursor nil 'local)
        (org-latex-preview-mode--handle-pre-cursor) ; Invoke setup before the hook even fires.
        (add-hook 'post-command-hook #'org-latex-preview-mode--handle-post-cursor nil 'local)
        (add-hook 'after-change-functions #'org-latex-preview-mode--detect-fragments-in-change nil 'local)
        (add-hook 'org-cycle-hook 'org-latex-preview-mode--org-cycle nil 'local)
        (when org-latex-preview-mode-display-live (org-latex-preview-live--setup)))
    (remove-hook 'pre-command-hook #'org-latex-preview-mode--handle-pre-cursor 'local)
    (remove-hook 'post-command-hook #'org-latex-preview-mode--handle-post-cursor 'local)
    (remove-hook 'after-change-functions #'org-latex-preview-mode--detect-fragments-in-change 'local)
    (remove-hook 'org-cycle-hook 'org-latex-preview-mode--org-cycle 'local)
    (org-latex-preview-live--teardown)))


;;;; Code for "live" preview generation
;;
;; When `org-latex-preview-mode' is turned on and
;; `org-latex-preview-mode-display-live' is non-nil, previews are
;; generated in the background with each change to the LaTeX fragment
;; being edited.  This continuously updated preview is shown to the
;; right of the LaTeX fragment, or under the LaTeX environment being
;; edited.  Alternatively, it can be shown using Eldoc (see
;; `org-latex-preview-mode-display-type').
;;
;; The code works as follows (simplified description):

;; - When the cursor enters a fragment and
;;   `org-latex-preview-mode' is active, the corresponding
;;   overlay is "opened" up and the preview image is hidden.  At this
;;   time, the `after-string' property of this overlay is updated to
;;   show the existing preview image.
;;
;; - A handler is added to `after-change-functions' to regenerate the
;;   preview for the fragment when its contents change.
;;
;; - When the preview is regenerated, the `after-string' property of
;;   the preview overlay is updated to show the new image.  This
;;   regeneration is modulated with a debounce
;;   `org-latex-preview-mode-update-delay' and a dynamically updated
;;   throttle `org-latex-preview-mode-update-throttle'.
;;
;; - When the cursor exits the boundaries of the fragment, the
;;   `after-string' property of the preview overlay is removed.
;;
;; - Preview images for live previews of fragments in "intermediate"
;;   states, before the fragment is exited, are stored in
;;   `org-latex-preview-live--cache-dir' instead of
;;   `org-latex-preview-cache'.  This cache rolls over when
;;   `org-latex-preview-live--max-cache-count' live-preview images
;;   have been accumulated.

(defvar-local org-latex-preview-live--docstring " "
  "String that holds the live LaTeX preview image as a text property.")

(defvar-local org-latex-preview-live--element-type nil
  "The type of the element being previewed live.

It is set to either latex-fragment or latex-environment.")

(defvar-local org-latex-preview-live--generator nil
  "Function to regenerate live previews.

This buffer-local variable holds a closure with captured state for
debouncing and throttling the regeneration of live previews as LaTeX
fragments are modified.")

(defcustom org-latex-preview-mode-display-live '(block edit-special)
  "Whether LaTeX previews should be generated during writing.
This affects the behaviour of `org-latex-preview-mode'.

It is either a boolean value (preview everything or nothing), or
a list of context symbols within which live previews should be
shown.

The availible contexts are:
- inline, for inline LaTeX fragments
- block, for LaTeX environments
- edit-special, for `org-edit-special' buffers

Note: Live preview updates of the LaTeX fragment or environment being
edited apply only in the current buffer, and not in any indirect clones.
Previews of the same fragment in indirect buffers (or the base buffer if
this is an indirect buffer) can thus go out of date.  Clear and
regenerate previews manually in the indirect buffer to resolve this.
This is currently a limitation of the LaTeX preview system."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type '(choice
          (const :tag "Everywhere" t)
          (const :tag "Never" nil)
          (set :greedy t :tag "Contexts"
               (const :tag "Inline LaTeX fragments" inline)
               (const :tag "LaTeX environments" block)
               (const :tag "When using org-edit-special" edit-special))))

(defcustom org-latex-preview-mode-display-type 'buffer
  "How to display live-updating previews of LaTeX snippets.

This option is meaningful when live previews are enabled via
`org-latex-preview-mode-display-live'.

The currently supported options are the symbols

- buffer: Display live previews next to or under the LaTeX
  fragment in the Org buffer.

- eldoc: Display live previews using Eldoc.  Requires
  `eldoc-mode' to be turned on in the Org buffer.  Note that
  Eldoc in turn offers various display functions, such as the
  echo area, the dedicated `eldoc-doc-buffer' and more."
  :group 'org-latex-preview
  :type  '(choice
           (const :tag "Display next to fragment" buffer)
           (const :tag "Display in Eldoc" eldoc)))

(defcustom org-latex-preview-mode-update-delay 1.0
  "Idle time before regenerating LaTeX previews.

When `org-latex-preview-mode-display-live' is non-nil and
`org-latex-preview-mode' is active, live previews are
updated when there have been no changes to the LaTeX fragment or
environment for at least this much time."
  :group 'org-latex-preview
  :package-version '(Org . "10.0")
  :type 'number)

(defvar org-latex-preview-mode-update-throttle 1.0
  "Throttle time for live LaTeX previews.

When `org-latex-preview-mode-display-live' is non-nil and
`org-latex-preview-mode' is active, live previews are
updated no more than once in this interval of time.

Its value is updated dynamically based on the average fragment
preview time.")

(defvar-local org-latex-preview-live--preview-times
    (make-vector 3 1.0)
  "Vector containing the last three preview run times in this buffer.")

(defvar-local org-latex-preview-live--preview-times-index 0
  "Index for computing moving average of preview processing times.

This is used to set the throttle duration of live preview regeneration.")

(defvar-local org-latex-preview-live--last-hash nil
  "Last hashed fragment when live-previewing.")

(defvar org-latex-preview-live--cache-count 0
  "Running count of the number of live previews cached.")

(defconst org-latex-preview-live--max-cache-count 1024
  "The maximum number of live previews to cache.")

(defun org-latex-preview-live--update-times (latest-duration)
  "Update preview times given the last preview took LATEST-DURATION seconds."
  (aset org-latex-preview-live--preview-times
        (% (cl-incf org-latex-preview-live--preview-times-index) 3)
        latest-duration)
  (setq-local org-latex-preview-mode-update-throttle
              (/ (apply #'+ (append org-latex-preview-live--preview-times nil))
                 3)))

(defun org-latex-preview-live--record-hook (exit-code _buf extended-info)
  "A hook for `org-latex-preview-process-finish-functions' to track preview time.
Called with EXIT-CODE and EXTENDED-INFO from the async process."
  (when (= exit-code 0)
    ;; Save the first previewed element's hash.  When live-previewing
    ;; this should always be the element being live-previewed, even
    ;; when there are numbering changes.
    (when-let* ((ov (cl-loop for o in (overlays-at (point))
                             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
                             return o))
                ((overlay-get ov 'org-view-text)))
      (setq org-latex-preview-live--last-hash
            (plist-get
             (car (plist-get extended-info :fragments))
             :key))
      ;; Update run times to dynamically set throttle
      (org-latex-preview-live--update-times
       (- (float-time) (plist-get extended-info :start-time))))))

(defun org-latex-preview-live--debounce (func duration)
  "Return a debounced FUNC with DURATION applied."
  (let ((debounce-timer))
    (lambda (&rest args)
      (if (timerp debounce-timer)
          (timer-set-time debounce-timer (+ (float-time) duration))
        (setq debounce-timer
              (run-at-time
               duration nil
               (lambda ()
                 (cancel-timer debounce-timer)
                 (setq debounce-timer nil)
                 (apply func args))))))))

(defun org-latex-preview-live--throttle (func)
  "Return a throttled FUNC.

Ensures that FUNC runs at the end of the throttle duration."
  (let ((waiting))
    (lambda (&rest args)
      (unless waiting
        (apply func args)
        (setq waiting t)
        (run-at-time
         org-latex-preview-mode-update-throttle nil
         (lambda ()
           (setq waiting nil)
           (apply func args)))))))

(defun org-latex-preview-live--clearout-overlay (ov)
  "Clear out the live LaTeX preview for the preview overlay OV."
  (setq org-latex-preview-live--element-type nil)
  (overlay-put ov 'after-string nil)
  ;; When exiting live-preview, move the final image to the main cache
  (when-let* ((org-latex-preview-live--last-hash)
              ((overlay-get ov 'org-preview-image))
              (path-info (org-latex-preview--get-cached
                          org-latex-preview-live--last-hash 'live)))
    ;; Copy final image data to main cache location
    (when (equal (file-name-directory (car path-info))
                 org-latex-preview-live--cache-dir)
      (org-latex-preview--cache-image
       org-latex-preview-live--last-hash (car path-info) (cdr path-info))
      ;; Replace live-preview image with the image from the main cache
      (run-at-time 0.01 nil #'org-latex-preview-mode--regenerate-overlay ov))
    ;; Reset the last live-previewed hash
    (setq org-latex-preview-live--last-hash nil)))

(defun org-latex-preview-live--regenerate (&rest _)
  "Regenerate the LaTeX preview overlay that overlaps BEG and END.

This is meant to be run via the `after-change-functions' hook in
Org buffers when using live-updating LaTeX previews."
  (let ((ov (cl-loop for o in (overlays-at (point))
                     if (and (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
                             (overlay-get o 'org-preview-state))
                     return o))
        (org-latex-preview-cache 'live))
    (when ov
      ;; The following checks are redundant and can make
      ;; throttling inconsistent:
      ;; (<= (overlay-start ov) beg)
      ;; (>= (overlay-end ov) end)
      (org-latex-preview-mode--regenerate-overlay ov t)
      (unless (and (overlay-buffer ov) (overlay-get ov 'org-preview-image))
        (org-latex-preview-live--clearout-overlay ov)))))

(defun org-latex-preview-live--update-props (image-spec &optional box-face)
  "Update the live preview string with the IMAGE-SPEC display property.

BOX-FACE is the face to apply in addition."
  (let ((l (length org-latex-preview-live--docstring)))
    (put-text-property
     (1- l) l 'display image-spec
     org-latex-preview-live--docstring)
    (when box-face
      (put-text-property
       (1- l) l 'face box-face
       org-latex-preview-live--docstring))))

(defun org-latex-preview-live--setup-overlay (&optional ov)
  "Set up a live preview for the LaTeX fragment with overlay OV."
  (when-let*
      ((ov (or ov (cl-loop
                   for o in (overlays-at (point))
                   if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
                   return o)))
       (image (overlay-get ov 'org-preview-image))
       (end (overlay-end ov)))
    (let ((latex-env-p
           (progn
             (unless org-latex-preview-live--element-type
               (let* ((elm (org-element-context)))
                 ;; Treat \[ ... \] as a latex-environment for the
                 ;; purposes of live-previews.
                 (setq org-latex-preview-live--element-type
                       (or (and (string-prefix-p
                                 "\\[" (org-element-property :value elm))
                                'latex-environment)
                           (org-element-type elm)))))
             (eq org-latex-preview-live--element-type
                 'latex-environment))))
      (when (or (eq org-latex-preview-mode-display-live t)
                (memq (if latex-env-p 'block 'inline) org-latex-preview-mode-display-live))
        (when (eq org-latex-preview-mode-display-type 'buffer)
          (unless (overlay-get ov 'after-string)
            ;; NOTE: The latex-env-specific string includes a zero
            ;; width space char.  This is to force the box around the
            ;; image to render correctly.
            (setq org-latex-preview-live--docstring
                  (concat (and latex-env-p "\n​") " "))
            (overlay-put ov 'org-view-text t)
            (overlay-put ov 'after-string org-latex-preview-live--docstring)))
        (org-latex-preview-live--update-props image '(:box t))))))

(defun org-latex-preview-live--update-overlay (ov)
  "Update the live LaTeX preview for overlay OV."
  (when (and (memq ov (overlays-at (point)))
             (overlay-get ov 'org-view-text))
    (if (or (overlay-get ov 'after-string)
            (eq org-latex-preview-mode-display-type
                'eldoc))
        (org-latex-preview-live--update-props
         (overlay-get ov 'org-preview-image))
      (org-latex-preview-live--setup-overlay ov))))

;; Code for previews in org-src buffers
(defun org-latex-preview-live--src-buffer-setup ()
  "Set up the org-src buffer for live LaTeX previews.

When `org-latex-preview-mode' is turned on, and
`org-latex-preview-mode-display-live' is either t or containes
\"edit-special\", live LaTeX previews are generated when editing
a LaTeX fragment or environment using `org-edit-special'.

If the source Org buffer is visible, these previews are displayed
over the original fragment.  Otherwise previews are displayed in
the org-src buffer.

This is meant to be called via `org-src-mode-hook'."
  (defvar org-src--beg-marker)
  (when (and (derived-mode-p (org-src-get-lang-mode "latex"))
             (buffer-local-value 'org-latex-preview-mode
                                 (marker-buffer org-src--beg-marker))
             (let ((live (buffer-local-value 'org-latex-preview-mode-display-live
                                             (marker-buffer org-src--beg-marker))))
               (or (eq live t) (memq 'edit-special live))))
    (let* ((org-buf (marker-buffer org-src--beg-marker))
           (src-buf (current-buffer))
           ;; Do not use (org-element-property :begin element) to
           ;; find the bounds -- this is fragile under typos.
           (beg (point-min)) (end (point-max))
           preamble element skip-env-p numbering-offsets ov orig-ov)
      (setq org-latex-preview-mode--marker (point-marker))
      ;; Copy the LaTeX preview overlay from the source Org buffer
      ;; into the org-src buffer and show a preview image over the
      ;; former:
      (with-current-buffer org-buf
        (setq element (org-element-context))
        (let ((elem-start (or (org-element-property :post-affiliated element)
                              (org-element-property :begin element)))
              (elem-end (org-with-wide-buffer
                         (goto-char (org-element-property :end element))
                         (skip-chars-backward "\n\r\t ")
                         (point))))
          (pcase (org-element-type element)
            ('export-block (setq skip-env-p t))
            ('latex-environment
             (setq skip-env-p
                   (and (save-excursion
                          (goto-char elem-start)
                          (looking-at "\\\\begin{\\([^}]+\\)}"))
                        (member (match-string 1) org-latex-preview-mode-ignored-environments)))))
          (unless skip-env-p
            (setq orig-ov
                  (or (cl-loop for o in (overlays-at (point))
                               if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
                               return o)
                      (org-latex-preview--ensure-overlay elem-start elem-end)))
            (org-latex-preview-live--clearout-overlay orig-ov)
            (setq ov (copy-overlay orig-ov)
                  preamble (or org-latex-preview--preamble-content
                               (setq org-latex-preview--preamble-content
                                     (org-latex-preview--get-preamble))))
            (overlay-put ov 'org-view-text t)
            (move-overlay ov beg end src-buf))
          (org-latex-preview-mode--close-previous-overlay)))
      (unless skip-env-p
        (or ov (setq ov (org-latex-preview--ensure-overlay beg end)))
        (org-latex-preview-mode--open-this-overlay)
        ;; Adjust numbering if required
        (when (and org-latex-preview-numbered
                   (eq (org-element-type element) 'latex-environment))
          (with-current-buffer org-buf
            (when-let* ((numbering-table (org-latex-preview--environment-numbering-table)))
              (setq numbering-offsets (list (gethash element numbering-table))))))

        (let* ((element-type
                (with-current-buffer org-buf
                  (or (and (string-prefix-p
                            "\\[" (org-element-property :value element))
                           'latex-environment)
                      (org-element-type element)))))
          ;; Set the element type ahead of time since we cannot call
          ;; org-element-context in the org-src buffer
          (setq org-latex-preview-live--element-type element-type)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'org-latex-preview-live--update-overlay nil 'local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    (lambda (ov)           ;Always keep this overlay "open"
                      (save-excursion
                        (goto-char (overlay-start ov))
                        (org-latex-preview-mode--open-this-overlay)))
                    nil 'local)
          ;; Set the preview regenerator function in the org-src buffer
          (setq-local
           org-latex-preview-live--generator
           (thread-first
             (lambda (&rest _)
               (when (eq (current-buffer) src-buf)
                 (if (window-live-p (get-buffer-window org-buf))
                     ;; Show preview in Org buffer
                     (let* ((content
                             (string-trim (buffer-substring-no-properties
                                           (point-min) (point-max)))))
                       (with-current-buffer org-buf
                         (org-latex-preview-place
                          org-latex-preview-process-default
                          (list (list (overlay-start orig-ov)
                                      (overlay-end orig-ov)
                                      content))
                          numbering-offsets))
                       (org-latex-preview-live--clearout-overlay ov)
                       (setq org-latex-preview-live--element-type element-type))
                   ;; Else show preview in org-src buffer, next to the fragment
                   (org-latex-preview-place
                    org-latex-preview-process-default
                    (list (list (point-min) (point-max)))
                    numbering-offsets preamble)
                   (unless (overlay-get ov 'org-preview-image)
                     (org-latex-preview-live--setup-overlay ov)))))
             (org-latex-preview-live--throttle)
             (org-latex-preview-live--debounce
              org-latex-preview-mode-update-delay))))
        (add-hook 'after-change-functions org-latex-preview-live--generator 90 'local)
        ;; Show live preview if available
        (unless (window-live-p (get-buffer-window org-buf))
          (org-latex-preview-live--setup-overlay ov))))))

;; Eldoc support for live previews
(defun org-latex-preview-live--display-in-eldoc (callback)
  "Eldoc documentation function for live LaTeX previews.

CALLBACK is supplied by Eldoc, see
`eldoc-documentation-functions'."
  (when (and org-latex-preview-live--docstring
             (cl-loop for o in (overlays-at (point))
                      thereis (eq (overlay-get o 'org-overlay-type)
                                  'org-latex-overlay)))
    (funcall callback org-latex-preview-live--docstring)))

(defun org-latex-preview-live--update-eldoc (_ov)
  "Force eldoc to update when a preview is updated."
  (eldoc--invoke-strategy nil))

;; Live preview setup and teardown.
(defun org-latex-preview-live--setup ()
  "Set up hooks for live LaTeX previews.

See `org-latex-preview-mode-display-live' for details."
  (setq org-latex-preview-live--docstring " ")
  (setq-local org-latex-preview-live--generator
              (thread-first #'org-latex-preview-live--regenerate
                            (org-latex-preview-live--throttle)
                            (org-latex-preview-live--debounce
                             org-latex-preview-mode-update-delay)))
  (when (eq org-latex-preview-mode-display-type 'eldoc)
    (add-hook 'eldoc-documentation-functions #'org-latex-preview-live--display-in-eldoc nil t)
    (add-hook 'org-latex-preview-overlay-update-functions #'org-latex-preview-live--update-eldoc nil 'local))
  (add-hook 'org-src-mode-hook #'org-latex-preview-live--src-buffer-setup)
  (add-hook 'org-latex-preview-overlay-close-functions #'org-latex-preview-live--clearout-overlay nil 'local)
  (add-hook 'org-latex-preview-overlay-open-functions #'org-latex-preview-live--setup-overlay nil 'local)
  (add-hook 'after-change-functions org-latex-preview-live--generator 90 'local)
  (add-hook 'org-latex-preview-process-finish-functions #'org-latex-preview-live--record-hook nil 'local)
  (add-hook 'org-latex-preview-overlay-update-functions #'org-latex-preview-live--update-overlay nil 'local))

(defun org-latex-preview-live--teardown ()
  "Remove hooks for live LaTeX previews.

See `org-latex-preview-mode-display-live' for details."
  (when-let* ((ov (cl-loop for o in (overlays-at (point))
                           if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
                           return o)))
    (org-latex-preview-live--clearout-overlay ov))
  (when (eq org-latex-preview-mode-display-type 'eldoc)
    (remove-hook 'eldoc-documentation-functions #'org-latex-preview-live--display-in-eldoc t)
    (remove-hook 'org-latex-preview-overlay-update-functions #'org-latex-preview-live--update-eldoc 'local))
  (remove-hook 'org-latex-preview-overlay-close-functions #'org-latex-preview-live--clearout-overlay 'local)
  (remove-hook 'org-latex-preview-overlay-open-functions #'org-latex-preview-live--setup-overlay 'local)
  (remove-hook 'after-change-functions org-latex-preview-live--generator 'local)
  (remove-hook 'org-latex-preview-overlay-update-functions #'org-latex-preview-live--update-overlay 'local)
  (remove-hook 'org-latex-preview-process-finish-functions #'org-latex-preview-live--record-hook 'local)
  (setq-local org-latex-preview-live--generator nil
              org-latex-preview-live--last-hash nil))


;;;; Code for `org-latex-preview' processing pipeline
(defun org-latex-preview-clear-overlays (&optional beg end)
  "Remove all overlays with LaTeX fragment images in current buffer.
When optional arguments BEG and END are non-nil, remove all
overlays between them instead.  Return a non-nil value when some
overlays were removed, nil otherwise."
  (let ((overlays
         (cl-remove-if-not
          (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
          (overlays-in (or beg (point-min)) (or end (point-max))))))
    (mapc #'delete-overlay overlays)
    overlays))

;;;###autoload
(defun org-latex-preview (&optional mode)
  "Generate or hide LaTeX fragment previews.

The particular behaviour depends on MODE (which recognises a
number of symbols and prefix arguments), or when MODE is nil the
context of the current buffer when called.

- point: Toggle preview of the LaTeX fragment at point
- region: Display previews of all fragments in the selected region
- section: Display previews of all fragments in the current section
- buffer: Display previews of all fragments in the buffer
- clear-region: Clear all previews in the current region
- clear-section: Clear all previews in the current section
- clear-buffer: Clear all previews in the buffer

When MODE is nil and the cursor is on a LaTeX fragment, toggle
previewing of the LaTeX fragment at point.  If the there is no
preview, generate a preview and overlay it on the text.  Remove
the preview image otherwise.  If there is no fragment at point,
display previews for all fragments in the current section, or
active region should it exist.

Prefix arguments are handled as follows:
- `\\[universal-argument]' is equivalent to setting MODE to clear-region (if a region
   is currently active) or clear-section
- `\\[universal-argument] \\[universal-argument]' is equivalent to setting MODE to buffer
- `\\[universal-argument] \\[universal-argument] \\[universal-argument]' is equivalent to setting MODE to clear-buffer

MODE can also be a org-element LaTeX environment or fragment, which
will be treated as \"point\"."
  (interactive "P" org-mode)
  (when (display-graphic-p)
    (unless (and mode (symbolp mode))
      (setq mode
            (pcase (car-safe mode)
              (64 'clear-buffer)
              (16 'buffer)
              (4 (if (use-region-p) 'clear-region 'clear-section))
              ((guard (use-region-p)) 'region)
              (_ (if-let* ((datum (org-element-context)) ; Auto, i.e. element at point or section
                           ((memq (org-element-type datum)
                                  '(latex-environment latex-fragment))))
                     datum 'section)))))
    (pcase mode
      ('buffer
       (org-latex-preview--preview-region
        org-latex-preview-process-default (point-min) (point-max)))
      ('clear-buffer
       (org-latex-preview-clear-overlays (point-min) (point-max))
       (message "LaTeX previews removed from buffer"))
      ('section
       (let ((beg (if (org-before-first-heading-p) (point-min)
                    (save-excursion
                      (org-with-limited-levels (org-back-to-heading t) (point)))))
             (end (org-with-limited-levels (org-entry-end-position))))
         (org-latex-preview--preview-region
          org-latex-preview-process-default beg end)))
      ('clear-section
       (org-latex-preview-clear-overlays
        (if (org-before-first-heading-p) (point-min)
          (save-excursion
            (org-with-limited-levels (org-back-to-heading t) (point))))
        (org-with-limited-levels (org-entry-end-position))))
      ('region
       (org-latex-preview--preview-region
        org-latex-preview-process-default (region-beginning) (region-end)))
      ('clear-region
       (org-latex-preview-clear-overlays (region-beginning) (region-end)))
      ('point
       (when-let* ((datum (org-element-context))
                   ((memq (org-element-type datum) '(latex-environment latex-fragment))))
         (org-latex-preview--mode-aware-toggle datum)))
      ((guard (memq (org-element-type mode) '(latex-environment latex-fragment)))
       (org-latex-preview--mode-aware-toggle mode))
      (bad-value (error "Invalid `org-latex-preview' mode argument: %S" bad-value)))))

(defun org-latex-preview--mode-aware-toggle (datum)
  "Toggle the preview of the LaTeX fragment/environment DATUM.
This is done with care to work nicely with `org-latex-preview-mode',
should it be enabled."
  (let* ((beg (org-element-property :begin datum))
         (end (org-element-property :end datum))
         (ov (cl-some
              (lambda (o)
                (and (eq (overlay-get o 'org-overlay-type)
                         'org-latex-overlay)
                     o))
              (overlays-at beg))))
    ;; If using org-latex-preview-mode, an overlay will already exist
    ;; but not be showing an image.  We can detect this situtation via
    ;; the org-preview-state overlay property, and in such cases the
    ;; most reasonable action is to just (re)generate the preview
    ;; image.
    (cond
     ;; When not using preview-mode.
     ((not org-latex-preview-mode)
      (if (org-latex-preview-clear-overlays beg end)
          (message "LaTeX preview removed")
        (org-latex-preview--place-from-elements
         org-latex-preview-process-default (list datum))))
     ;; When using preview-mode, but no current preview.
     ((not ov)
      (org-latex-preview--place-from-elements
       org-latex-preview-process-default (list datum))
      (message "Creating LaTeX preview"))
     ;; When on a just written/edited fragment that should be previewed.
     ((eq (overlay-get ov 'org-preview-state) 'modified)
      (org-latex-preview-mode--regenerate-overlay ov)
      (overlay-put ov 'org-view-text t))
     ;; When on an unmodified fragment that is currently showing an image,
     ;; clear the image.
     ((overlay-get ov 'display)
      (org-latex-preview-clear-overlays beg end)
      (message "LaTeX preview removed"))
     ;; Since we're on an unmodified fragment but not showing an image,
     ;; let's try to show the image if possible.
     (ov
      (overlay-put ov 'org-view-text t)
      (unless (overlay-get ov 'after-string) ;Live preview being shown
        (overlay-put ov 'face (overlay-get ov 'org-hidden-face))
        (overlay-put ov 'display (overlay-get ov 'org-preview-image)))))))

(defun org-latex-preview-collect-fragments (&optional beg end)
  "Collect all LaTeX maths fragments/environments between BEG and END."
  (let (fragments)
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward org-latex-preview--tentative-math-re end t)
        (let ((obj (org-element-context)))
          (when (and (memq (org-element-type obj)
                           '(latex-fragment latex-environment))
                     ;; Avoid duplicating nested latex environments
                     (not (and fragments
                               (= (org-element-property :begin obj)
                                  (org-element-property :begin (car fragments))))))
            (push obj fragments)))))
    (nreverse fragments)))

(defun org-latex-preview--preview-region (processing-type &optional beg end)
  "Produce image overlays of LaTeX math fragments between BEG and END.

The LaTeX fragments are processed using PROCESSING-TYPE, a key of
`org-latex-preview-process-alist'."
  (when (fboundp 'clear-image-cache)
    (clear-image-cache))
  ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
  (when (memq processing-type '(dvipng dvisvgm imagemagick))
    ;; FIXME: `overlay-recenter' is a noop since Emacs 29.1
    (overlay-recenter (or end (point-max))))
  (let ((ws (if (window-live-p (get-buffer-window (current-buffer)))
                (window-start) 0)))
    (if (assq processing-type org-latex-preview-process-alist)
        (org-latex-preview--place-from-elements
         processing-type
         (nconc (org-latex-preview-collect-fragments (max ws beg) end)
                (when (< beg ws)
                  (org-latex-preview-collect-fragments beg (1- ws)))))
      (error "Unknown conversion process %s for previewing LaTeX fragments"
             processing-type))))

(defun org-latex-preview--construct-entries
    (elements &optional construct-numbering-p parse-tree)
  "Constuct a well formatted list of entries and (optinally) numbering offsets.
This operates by processing ELEMENTS.  When CONSTRUCT-NUMBERING-P is non-nil,
the number offsets will also be calculated, using PARSE-TREE if given."
  (let ((numbering-table (and construct-numbering-p
                              (cl-find 'latex-environment elements
                                       :key #'org-element-type :test #'eq)
                              (org-latex-preview--environment-numbering-table
                               parse-tree)))
        entries numbering-offsets)
    (dolist (element elements)
      (let ((beg (or (org-element-property :post-affiliated element)
                     (org-element-property :begin element)))
            (end (org-with-wide-buffer
                   (goto-char (org-element-property :end element))
                   (skip-chars-backward "\n\r\t ")
                   (point)))
            (content (org-element-property :value element)))
        (push (list beg end content) entries)
        (when numbering-table
          (push (and (eq (org-element-type element) 'latex-environment)
                     (gethash element numbering-table))
                numbering-offsets))))
    (list (nreverse entries) (nreverse numbering-offsets))))

(defun org-latex-preview--empty-fragment-p (content)
  "Test if the LaTeX string CONTENT is an empty LaTeX fragment (e.g. \\[\\])."
  (let ((content-point 0)
        (content-max (1- (length content)))
        (only-blanks t))
    (cond
     ((eq (aref content 0) ?$)
      (if (eq (aref content 1) ?$)
          (setq content-point 2 content-max (- content-max 2))
        (setq content-point 1 content-max (- content-max 1))))
     ((eq (aref content 0) ?\\)
      (setq content-point 2 content-max (- content-max 2))))
    (while (and only-blanks (<= content-point content-max))
      (if (memq (aref content content-point) '(?\s ?\t ?\n ?\r))
          (cl-incf content-point)
        (setq only-blanks nil)))
    only-blanks))

(defun org-latex-preview--place-from-elements (processing-type elements)
  "Preview LaTeX math fragments ELEMENTS using PROCESSING-TYPE."
  (apply #'org-latex-preview-place processing-type
         (org-latex-preview--construct-entries
          elements org-latex-preview-numbered)))

;;;###autoload
(defun org-latex-preview-place (processing-type entries &optional numbering-offsets latex-preamble)
  "Preview LaTeX math fragments ENTRIES using PROCESSING-TYPE.
Each entry of ENTRIES should be a list of 2-3 items, either
  (BEG END), or
  (BEG END VALUE)
Where BEG and END are the positions in the buffer, and the LaTeX previewed
is either the substring between BEG and END or (when provided) VALUE."
  (unless latex-preamble
    (setq latex-preamble
          (or org-latex-preview--preamble-content
              (setq org-latex-preview--preamble-content
                    (org-latex-preview--get-preamble)))))
  (let* ((processing-info
          (cdr (assq processing-type org-latex-preview-process-alist)))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         (numbering-offsets (cons nil numbering-offsets))
         fragment-info prev-fg prev-bg)
    (save-excursion
      (dolist (entry entries)
        (pcase-let* ((`(,beg ,end ,provided-value) entry)
                     (value (or provided-value
                                (buffer-substring-no-properties beg end)))
                     (`(,fg ,bg) (org-latex-preview--colors-around beg end))
                     (number (car (setq numbering-offsets (cdr numbering-offsets))))
                     (hash (org-latex-preview--hash
                            processing-type latex-preamble value imagetype fg bg number))
                     (options (org-combine-plists
                               org-latex-preview-appearance-options
                               (list :foreground fg
                                     :background bg
                                     :number number
                                     :continue-color
                                     (and (equal prev-bg bg)
                                          (equal prev-fg fg)))
                               (and (eq processing-type 'dvisvgm)
                                    (list :foreground
                                          org-latex-preview--svg-fg-standin)))))
          (if-let* ((path-info (org-latex-preview--get-cached hash)))
              (org-latex-preview--update-overlay
               (org-latex-preview--ensure-overlay beg end)
               path-info)
            (unless (org-latex-preview--empty-fragment-p value)
              (push (list :string (org-latex-preview--tex-styled
                                   processing-type value options)
                          :overlay (org-latex-preview--ensure-overlay beg end)
                          :key hash)
                    fragment-info)))
          (setq prev-fg fg prev-bg bg))))
    (when fragment-info
      (org-latex-preview--create-image-async
       processing-type
       (nreverse fragment-info)
       :latex-preamble latex-preamble
       :appearance-options org-latex-preview-appearance-options
       :place-preview-p t))))

(defun org-latex-preview--colors-around (start end &optional options)
  "Find colors for LaTeX previews occuping the region START to END.

OPTIONS is a plist containing foreground/background specifications."
  (let* ((face (org-latex-preview--face-around start end))
         (options (or options org-latex-preview-appearance-options))
         (fg (pcase (plist-get options :foreground)
               ('auto
                (org-latex-preview--resolved-faces-attr face :foreground))
               ('default (face-attribute 'default :foreground nil))
               (color color)))
         (bg (pcase (plist-get options :background)
               ('auto
                (org-latex-preview--resolved-faces-attr face :background))
               ('default (face-attribute 'default :background nil))
               (color color))))
    (list fg bg)))

(defun org-latex-preview--resolved-faces-attr (face attr)
  "Find ATTR of the FACE text property.
This is surprisingly complicated given the various forms of output
\\=(get-text-property pos \\='face) can produce.

Faces in `org-latex-preview--ignored-faces' are ignored."
  (when (consp face)
    (setq face (cl-set-difference face org-latex-preview--ignored-faces))
    (when (= (length face) 1)
      (setq face (car face))))
  (cond
   ((not face)
    (face-attribute 'default attr))
   ((not (consp face)) ; Spec like org-level-1.
    (face-attribute face attr nil 'default))
   ((keywordp (car face)) ; Spec like (:inherit org-block :extend t).
    (or (plist-get face attr)
        (face-attribute 'default attr)))
   ((consp (car face)) ; Spec like ((:inherit default :extend t) org-block).
    (or (plist-get (car face) attr)
        (face-attribute (cadr face) attr nil
                        (append (cddr face) '(default)))))
   ((symbolp (car face)) ; Spec like (org-level-1 default).
    (face-attribute (car face) attr nil (append (cdr face) '(default))))))

(defun org-latex-preview--hash (processing-type preamble string imagetype fg bg &optional number)
  "Return a SHA1 hash for referencing LaTeX fragments when previewing them.

PROCESSING-TYPE is the type of process used to create the
preview, see `org-latex-preview-process-default'.

PREAMBLE is the LaTeX preamble used in the generated LaTeX document.

STRING is the string to be hashed, typically the contents of a
LaTeX fragment.

IMAGETYPE is the type of image to be created, see
`org-latex-preview-process-alist'.

FG and BG are the foreground and background colors for the
image.

NUMBER is the equation number that should be used, if applicable."
  (sha1 (prin1-to-string
         (list processing-type
               preamble
               string
               (if (equal imagetype "svg")
                   'svg
                 (list (plist-get
                        org-latex-preview-appearance-options :scale)
                       fg))
               bg
               number))))

(defconst org-latex-preview--numbered-environments-single
  '("equation" "math" "displaymath" ; latex.ltx
    "dmath" ; breqn.sty
    "empheq") ; empheq.sty
  "List of LaTeX environments that contain a single numbered equation.")

(defconst org-latex-preview--numbered-environments-multi
  '("eqnarray" ; latex.ltx
    "align" "alignat" "flalign" "gather" "multiline" ; amsmath.sty
    "xalignat" "xxalignat" "subequations" ; amsmath.sty
    "dseries" "dgroup" "darray") ; breqn.sty
  "List of LaTeX environments that contain multiple numbered equations.")

(defconst org-latex-preview--numbered-environments-all
  (append org-latex-preview--numbered-environments-single
          org-latex-preview--numbered-environments-multi)
  "List of LaTeX environments which produce numbered equations.")

(defun org-latex-preview--environment-numbering-table (&optional parse-tree)
  "Create a hash table from numbered equations to their initial index.
If the org-element cache is active or PARSE-TREE is provided, the
hash table will use `eq' equality, otherwise `equal' will be
used.  When PARSE-TREE is provided, it is passed onto
`org-latex-preview--get-numbered-environments'."
  (let ((table (make-hash-table
                :test (if (or parse-tree (org-element--cache-active-p))
                          #'eq #'equal)))
        (counter 1))
    (save-match-data
      (dolist (element (org-latex-preview--get-numbered-environments
                        nil nil parse-tree))
        (puthash element counter table)
        (cl-incf counter (org-latex-preview--count-numbered-equations element))))
    table))

(defvar org-latex-preview--numbering-count-buffer nil
  "Buffer used for equation numbering calculations.")

(defun org-latex-preview--count-numbered-equations (element)
  "Count the number of numbered equations within Org ELEMENT.
Note: this function changes the current match data."
  (or (org-element-cache-get-key element :latex-numbered-equation-count)
      (let ((count 0)
            environment-name)
        (with-current-buffer (org-element-property :buffer element)
          (save-excursion
            (goto-char (or (org-element-property :post-affiliated element)
                           (org-element-property :begin element)))
            (when (looking-at "\\\\begin{\\([^}]+\\)}")
              (setq environment-name (match-string 1)))
            (cond
             ((member environment-name org-latex-preview--numbered-environments-single)
              (unless (or (search-forward "\\nonumber" (org-element-property :end element) t)
                          (search-forward "\\tag{" (org-element-property :end element) t))
                (cl-incf count 1)))
             ((member environment-name org-latex-preview--numbered-environments-multi)
              (cl-incf count (org-latex-preview--count-numbered-equations-multi
                              element environment-name))))))
        (org-element-cache-store-key element :latex-numbered-equation-count count)
        count)))

(defun org-latex-preview--count-numbered-equations-multi (element environment-name)
  "Count the number of numbered equations within Org ELEMENT.
Element is assumed to be a LaTeX ENVIRONMENT-NAME environment.
Note: this function changes the current match data."
  (let ((temp-buffer (or (and (buffer-live-p org-latex-preview--numbering-count-buffer)
                              org-latex-preview--numbering-count-buffer)
                         (setq org-latex-preview--numbering-count-buffer
                               (get-buffer-create " *Org LaTeX preview numbering calculation*" t))))
        (count-instances
         (lambda (needle)
           (let ((count 0))
             (save-excursion
               (while (search-forward needle nil t)
                 (cl-incf count))
               count))))
        (count 0))
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert-buffer-substring-no-properties
       (org-element-property :buffer element)
       (or (org-element-property :post-affiliated element)
           (org-element-property :begin element))
       (org-element-property :end element))
      (goto-char (point-min))
      (org-skip-whitespace)
      ;; Remove the "outer" environment, so we can then
      ;; easily delete all inner environments.
      (delete-region (point-min) (+ (point) 8 (length environment-name))) ; 8 = (length "\\begin{}")
      (goto-char (point-max))
      (when (search-backward (format "\\end{%s}" environment-name) nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      ;; Remove all "inner" environments.  We do this since
      ;; they can contain false-positive indicators of numbered
      ;; equations.
      (goto-char (point-min))
      (while (re-search-forward "\\\\begin{\\([^}]+\\)}" nil t)
        (let* ((start (match-beginning 0))
               (max-end (point-max))
               (env-name (match-string 1))
               (begin-template (format "\\begin{%s}" env-name))
               (end-template (format "\\end{%s}" env-name))
               (env-depth 1))
          (while (> env-depth 0)
            (let ((next-begin (or (save-excursion (search-forward begin-template nil t)) max-end))
                  (next-end (or (save-excursion (search-forward end-template nil t)) max-end)))
              (cond
               ((= next-begin next-end)
                (setq env-depth 0))
               ((< next-end next-begin)
                (cl-decf env-depth))
               (t (cl-incf env-depth)))
              (goto-char (min next-begin next-end))))
          (delete-region start (match-end 0))))
      ;; Count all hard newlines, as they indicate the start of a new equation,
      ;; then decrement by the number of \nonumber and \tag-d instances.
      (goto-char (point-min))
      (cl-incf count (1+ (funcall count-instances "\\\\")))
      (cl-decf count (funcall count-instances "\\nonumber"))
      (cl-decf count (funcall count-instances "\\tag{"))
      (erase-buffer))
    count))

(defun org-latex-preview--get-numbered-environments (&optional beg end parse-tree)
  "Find all numbered environments between BEG and END.
If PARSE-TREE is provided, it will be used insead of
`org-element-cache-map'."
  (cond
   (parse-tree
    (org-element-map
        parse-tree
        '(latex-environment)
      (lambda (datum)
        (and (<= (or beg (point-min)) (org-element-property :begin datum)
                 (org-element-property :end datum) (or end (point-max)))
             (let* ((content (org-element-property :value datum))
                    (env (and (string-match "\\`\\\\begin{\\([^}]+\\)}" content)
                              (match-string 1 content))))
               (and (member env org-latex-preview--numbered-environments-all)
                    datum))))))
   (t
    (org-element-cache-map
     (lambda (datum)
       (and (<= (or beg (point-min)) (org-element-property :begin datum)
                (org-element-property :end datum) (or end (point-max)))
            (let* ((content (org-element-property :value datum))
                   (env (and (string-match "\\`\\\\begin{\\([^}]+\\)}" content)
                             (match-string 1 content))))
              (and (member env org-latex-preview--numbered-environments-all)
                   datum))))
     :granularity 'element
     :restrict-elements '(latex-environment)
     :from-pos beg
     :to-pos (or end (point-max-marker))))))

(cl-defun org-latex-preview-cache-images
    (parse-tree &optional export-info
                &key preamble processing-type foreground
                background page-width scale image-dir
                &allow-other-keys)
  "Create preview images for LaTeX fragments in PARSE-TREE synchronously.

Returns a hash table.  Each key is a LaTeX fragment or
environment in PARSE-TREE, and correspdonding value is a list
containing image information.  This list has the format
 (path . image-info).

Note: It is assumed that the current buffer is the origin of PARSE-TREE.

 For example:
  (\"/path/.../to/image.svg\"
    :type svg
    :height 1.4
    :width 7.5
    :depth 0.2
    :errors nil)

The geometry information here is in em units.

EXPORT-INFO is a plist holding export options.

PREAMBLE is the LaTeX preamble to use for preview generation.  If
nil, a preamble will be consructed from the contents of the
current buffer if it is in Org mode.

PROCESSING-TYPE is a symbol specifying a external command used to
generate the images.  For supported image converters see
`org-latex-preview-process-default'.

The keyword arguments FOREGROUND, BACKGROUND, PAGE-WIDTH and
SCALE are as in `org-latex-preview-appearance-options' and will
default to its values, which see.

IMAGE-DIR, if non-nil, is a directory to contain the preview
images.  It will be created if necessary.  If IMAGE-DIR is nil,
image are cached as per `org-latex-preview-cache', which see."
  (declare (indent 1))
  (let* ((preamble
          (or preamble
              org-latex-preview--preamble-content
              (setq org-latex-preview--preamble-content
                    (org-latex-preview--get-preamble))))
         (appearance-options
          (org-combine-plists
           org-latex-preview-appearance-options
           (nconc (and foreground (list :foreground foreground))
                  (and background (list :background background))
                  (and page-width (list :page-width page-width))
                  (and scale (list :scale scale)))))
         (elements
          (org-element-map parse-tree
              '(latex-fragment latex-environment)
            #'identity export-info))
         (entries-and-numbering
          (org-latex-preview--construct-entries
           elements t parse-tree))
         (processing-type (or processing-type
                              (plist-get export-info :with-latex)
                              org-latex-preview-process-default))
         (processing-info
          (cdr (assq processing-type org-latex-preview-process-alist)))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         (numbering-offsets (cons nil (cadr entries-and-numbering)))
         (element-preview-hash-table (make-hash-table :test #'eq :size (length elements)))
         fragment-info prev-fg prev-bg)

    ;; Create fragment info for the preview process
    (cl-loop
     for entry in (car entries-and-numbering)
     for element in elements
     for (beg end provided-value) = entry
     for value = (or provided-value (buffer-substring-no-properties beg end))
     for (fg bg) = (org-latex-preview--colors-around beg end appearance-options)
     for number = (car (setq numbering-offsets (cdr numbering-offsets)))
     for hash = (org-latex-preview--hash processing-type preamble value imagetype fg bg number)
     for options = (org-combine-plists
                    appearance-options
                    (list :foreground fg :background bg
                          :number number
                          :continue-color
                          (and (equal prev-bg bg)
                               (equal prev-fg fg))))
     do
     (puthash element hash element-preview-hash-table)
     (unless (org-latex-preview--get-cached hash)
       (push (list :string (org-latex-preview--tex-styled
                            processing-type value options)
                   :overlay (org-latex-preview--ensure-overlay beg end)
                   :key hash)
             fragment-info))
     (setq prev-fg fg prev-bg bg))

    (when image-dir
      (unless (file-name-absolute-p image-dir)
        (let* ((out-file (plist-get export-info :output-file))
               (out-dir (and out-file (file-name-directory out-file))))
          (setq image-dir (expand-file-name image-dir out-dir))))
      (unless (file-directory-p image-dir)
        (make-directory image-dir 'parents)))

    ;; Generate fragment previews
    (let ((org-latex-preview-cache (or image-dir org-latex-preview-cache)))
      (when fragment-info
        (apply #'org-async-wait-for
               (org-latex-preview--create-image-async
                processing-type
                (nreverse fragment-info)
                :latex-preamble preamble
                :appearance-options appearance-options))))

    ;; Fragments generated, update the hash table
    (cl-loop for element in elements
             for hash = (gethash element element-preview-hash-table)
             for (source-file . image-info) = (org-latex-preview--get-cached hash image-dir)
             if (and source-file (file-exists-p source-file))
             do (puthash element (cons source-file image-info) element-preview-hash-table)
             else do
             (display-warning '(org latex-preview get-cache)
                              (format "No image generated for fragment:\n%s"
                                      (org-element-property :value element)))
             (remhash element element-preview-hash-table)
             finally return element-preview-hash-table)))

(cl-defun org-latex-preview-create-images
    (strings &key preamble processing-type foreground
             background page-width scale &allow-other-keys)
  "Return a LaTeX preview image path or list of image paths.

STRINGS is a string or list of strings.

For PREAMBLE, PROCESSING-TYPE, FOREGROUND, BACKGROUND, PAGE-WIDTH and
SCALE see `org-latex-preview-cache-images'."
  (declare (indent 1))
  (let* ((return-type (type-of strings))
         (strings (ensure-list strings))
         (preamble (or preamble
                       (with-temp-buffer (org-latex-preview--get-preamble))))
         (appearance-options
          (org-combine-plists
           org-latex-preview-appearance-options
           (nconc (and foreground (list :foreground foreground))
                  (and background (list :background background))
                  (and page-width (list :page-width page-width))
                  (and scale (list :scale scale)))))
         (processing-type (or processing-type org-latex-preview-process-default))
         (processing-info
          (cdr (assq processing-type org-latex-preview-process-alist)))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         ;; (numbering-offsets (cons nil (cadr entries-and-numbering)))
         element-hash-list fragment-info)

    ;; Create fragment info for the preview process
    (cl-loop
     for value in strings
     with fg = (pcase (plist-get appearance-options :foreground)
                 ((or 'auto 'default) (face-attribute 'default :foreground))
                 (color color))
     with bg = (pcase (plist-get appearance-options :background)
                 ((or 'auto 'default) (face-attribute 'default :background))
                 (color color))
     with number = nil ;; (car (setq numbering-offsets (cdr numbering-offsets)))
     for hash = (org-latex-preview--hash processing-type preamble value imagetype fg bg number)
     for options = (org-combine-plists
                    appearance-options
                    (list :foreground fg :background bg
                          :number number
                          :continue-color t))
     do
     (push hash element-hash-list)
     (unless (org-latex-preview--get-cached hash)
       (push (list :string (org-latex-preview--tex-styled
                            processing-type value options)
                   :key hash)
             fragment-info)))

    ;; Generate fragment previews
    (when fragment-info
      (apply #'org-async-wait-for
             (org-latex-preview--create-image-async
              processing-type
              (nreverse fragment-info)
              :latex-preamble preamble
              :appearance-options appearance-options)))

    ;; Fragments generated, collect files
    (cl-loop for hash in (nreverse element-hash-list)
             for value in strings
             for (image-file . image-info) = (org-latex-preview--get-cached hash)
             if (and image-file (file-exists-p image-file))
             collect image-file into image-files
             else do (display-warning '(org-latex-preview get-cache)
                                      (format "No image generated for fragment:\n%s" value))
             and collect nil into image-files
             finally return
             (if (eq return-type 'cons) image-files (car-safe image-files)))))


(cl-defun org-latex-preview--create-image-async
    (processing-type fragments-info &key latex-processor latex-preamble appearance-options place-preview-p)
  "Preview FRAGMENTS-INFO asynchronously with method PROCESSING-TYPE.

FRAGMENTS-INFO is a list of plists, each of which provides
information on an individual fragment and should have the
following structure:
  (:string fragment-string :overlay fragment-overlay :key fragment-hash)
where
- fragment-string is the literal content of the fragment
- fragment-overlay is the overlay placed for the fragment
- fragment-hash is a string that uniquely identifies the fragment

The FRAGMENTS-INFO plists will be modified during processing to hold
more information on the fragments.

When PLACE-PREVIEW-P is true, it will be set in the extended info
plist passed to filters, and is expected to result in the newly
generated fragment image being placed in the buffer.

LATEX-PROCESSOR is a member of `org-latex-compilers' which is guessed if unset.

When provided, LATEX-PREAMBLE overrides the default LaTeX preamble.

APPEARANCE-OPTIONS is a plist specifying global geometry and
appearance info for the preview run.  It defaults to
`org-latex-preview-appearance-options', which see for recognized
keys.

Returns a list of async tasks started."
  (let* ((processing-type
          (or processing-type org-latex-preview-process-default))
         (latex-processor
          (or latex-processor
              (and (derived-mode-p 'org-mode)
                   (cdr (assoc "LATEX_COMPILER"
                               (org-collect-keywords
                                '("LATEX_COMPILER") '("LATEX_COMPILER")))))
              org-latex-compiler))
         (processing-info
          (nconc (list :latex-processor latex-processor)
                 (and latex-preamble (list :latex-header latex-preamble))
                 (alist-get processing-type org-latex-preview-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) "")))
    ;; xelatex produces .xdv (eXtended dvi) files, not .dvi, so as a special
    ;; case we check for xelatex + dvi and if so switch the file extension to xdv.
    (when (and (equal latex-processor "xelatex")
               (equal (plist-get processing-info :image-input-type) "dvi"))
      (setq processing-info
            (plist-put (copy-sequence processing-info) :image-input-type "xdv")))
    (dolist (program programs)
      (org-check-external-command program error-message))
    (when (and org-latex-preview-process-active-indicator place-preview-p)
      (dolist (fragment fragments-info)
        (org-latex-preview--indicate-processing
         (plist-get fragment :overlay) 'on)))
    ;; At this point we will basically construct a tree of async calls:
    ;;
    ;; dvisvgm case:
    ;; └─ Compile tex file ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;;    └─ (success or failure)
    ;;       └─Extact images ⟶ stdout to `org-latex-preview--dvisvgm-filter'
    ;;         ├─ (success)
    ;;         │  ├─ Call `org-latex-preview--check-all-fragments-produced',
    ;;         │  │  which can rerun the async tree if needed.
    ;;         │  └─ Delete tempfiles (`org-latex-preview--cleanup-callback').
    ;;         └─ (failure)
    ;;            └─ Run `org-latex-preview--failure-callback' (remove overlays and emit msg).
    ;;
    ;; dvipng case:
    ;; ├─ Compile tex file ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;; └─ Extract images ("--follow" tex ouput) ⟶ stdout to `org-latex-preview--dvipng-filter'
    ;;    ├─ (success)
    ;;    │  ├─ Call `org-latex-preview--check-all-fragments-produced',
    ;;    │  │  which can rerun the async tree if needed.
    ;;    │  └─ Delete tempfiles (`org-latex-preview--cleanup-callback')
    ;;    └─ (failure)
    ;;       └─ Run `org-latex-preview--failure-callback' (remove overlays and emit msg).
    ;;
    ;; generic case:
    ;; └─ Compile tex file ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;;    └─ (success or failure)
    ;;       └─Extact images
    ;;         ├─ (success)
    ;;         │  ├─ Call `org-latex-preview--generic-callback'.
    ;;         │  ├─ Delete tempfiles (`org-latex-preview--cleanup-callback')
    ;;         │  └─ Call `org-latex-preview--check-all-fragments-produced',
    ;;         │     which can rerun the async tree if needed.
    ;;         └─ (failure)
    ;;            └─ Run `org-latex-preview--failure-callback' (remove overlays and emit msg).
    ;;
    ;; With continuous, synchronous processing:
    ;;
    ;; ⟶ stdout to `org-latex-preview--latex-preview-filter'
    ;;   ├─ read preview fontsize
    ;;   ├─ capture compilation errors
    ;;   └─ (conditionally) update overlays in buffer with png images and metadata
    ;;
    ;; ⟶ stdout to `org-latex-preview--dvisvgm-filter'
    ;;   ├─ read preview image metadata
    ;;   ├─ edit svgs and adjust colors
    ;;   ├─ cache svgs with org-persist or in /tmp
    ;;   └─ update overlays in buffer with svg images and metadata
    ;;
    ;; ⟶ stdout to `org-latex-preview--dvipng-filter'
    ;;   ├─ read preview image metadata
    ;;   ├─ cache pngs with org-persist or in /tmp
    ;;   └─ update overlays in buffer with png images and metadata
    ;;
    ;; Note that the image extraction processes run even if the tex
    ;; compilation "fails".  This is because preview-latex can exit
    ;; with code 1 even when the tex compilation succeeds in producing
    ;; DVI output.  This is by preview.sty's design, and we
    ;; accommodate it.
    ;;
    ;; -----
    ;;
    ;; All processes in this tree share state in the extended-info
    ;; plist, which is passed to both the functions that create the
    ;; async task specs and all the callbacks.  When initiating the
    ;; compilation, extended-info is populated with the following
    ;; keys:
    ;;
    ;; :processor       One of the keys (processing methods) in
    ;;                  `org-latex-preview-process-alist', such as
    ;;                  dvipng, dvisvgm or imagemagick.
    ;;
    ;; :latex-processor LaTeX compiler in use, see `org-latex-compilers'
    ;; :latex-header    LaTeX preamble for compilation
    ;;
    ;; All keys from the corresponding entry of
    ;; org-latex-preview-process-alist, which see:
    ;; :programs, :description, :message, :image-input-type, :image-output-type
    ;; :latex-compiler, :image-converter, :transparent-image-converter,
    ;; :post-clean
    ;;
    ;; :org-buffer      Buffer from which preview is dispatched, not
    ;;                  necessarily an Org mode buffer.
    ;; :texfile         LaTeX file path for preview generation
    ;; :place-preview-p Whether previews should be placed in the buffer
    ;; :cache-location  Where to store preview images
    ;; :start-time      Compilation process start time
    ;; :appearance-options Preview image size, scale and other
    ;;                     options, see `org-latex-preview-appearance-options'
    ;; :proc-buffers    List of process buffers created when running
    ;;                  the tree
    ;; :fragments  List of fragment information plists, see FRAGMENT-INFO
    ;;             Each fragment plist has the keys
    ;;             :string  fragment contents
    ;;             :overlay overlay placed over fragment region
    ;;             :key     hash of fragment contents and compilation settings
    ;;
    ;; The following keys are added when processing the outputs:
    ;;
    ;; :fontsize    Fontsize as reported by preview.sty
    ;; :tightpage   Margin information (for cropping by the :image-converter)
    ;;              as reported by preview.sty
    ;;
    ;;  Additionally, the following keys are added to each fragment in
    ;;  :fragments
    ;;
    ;;  :errors     LaTeX or :image-converter errors for the fragment
    ;;  :path       Path to generated preview image
    ;;  :height     Height of image (em units)
    ;;  :width      Width of image (em units)
    ;;  :depth      Depth below baseline
    (let* ((cache-location org-latex-preview-cache) ; Save the current value
           (extended-info
            (append processing-info
                    (list :processor processing-type
                          :fragments fragments-info
                          :org-buffer (current-buffer)
                          :texfile (org-latex-preview--create-tex-file
                                    processing-info fragments-info appearance-options)
                          :appearance-options appearance-options
                          :place-preview-p place-preview-p
                          :cache-location cache-location
                          :start-time (float-time))))
           (tex-compile-async
            (org-latex-preview--tex-compile-async extended-info))
           (img-extract-async
            (org-latex-preview--image-extract-async extended-info)))
      (when-let* ((cache-base-dir
                   (cond
                    ((eq cache-location 'temp)
                     org-latex-preview--temp-cache-dir)
                    ((eq cache-location 'live)
                     org-latex-preview-live--cache-dir))))
        (unless (file-directory-p cache-base-dir)
          (make-directory cache-base-dir t)))
      (plist-put (cddr img-extract-async) :success
                 (list                  ; The order is important here.
                  #'org-latex-preview--check-all-fragments-produced
                  #'org-latex-preview--cleanup-callback))
      (plist-put (cddr img-extract-async) :failure
                 (list
                  #'org-latex-preview--failure-callback
                  #'org-latex-preview--cleanup-callback))
      (pcase processing-type
        ('dvipng
         (plist-put (cddr img-extract-async) :filter
                    #'org-latex-preview--dvipng-filter))
        ('dvisvgm
         (plist-put (cddr img-extract-async) :filter
                    #'org-latex-preview--dvisvgm-filter))
        (_
         (push #'org-latex-preview--generic-callback
               (plist-get (cddr img-extract-async) :success))))
      (when org-latex-preview-process-finish-functions
        ;; Extra callbacks to run after image generation
        (push #'org-latex-preview--run-finish-functions
              (plist-get (cddr img-extract-async) :success))
        (push #'org-latex-preview--run-finish-functions
              (plist-get (cddr img-extract-async) :failure)))
      (if (and (eq processing-type 'dvipng)
               (member "--follow" (cadr img-extract-async)))
          (list (org-async-call tex-compile-async)
                (org-async-call img-extract-async))
        (plist-put (cddr tex-compile-async) :success img-extract-async)
        (plist-put (cddr tex-compile-async) :failure img-extract-async)
        (list (org-async-call tex-compile-async))))))

(defun org-latex-preview--run-finish-functions (&rest args)
  "Run hooks after preview image generation, with arguments ARGS."
  (apply #'run-hook-with-args
         'org-latex-preview-process-finish-functions
         args))

(defun org-latex-preview--failure-callback (exit-code buf extended-info)
  "Clear overlays corresponding to previews that failed to generate with EXIT-CODE.

BUF is the process buffer, and EXTENDED-INFO contains the information
needed to identify such previews."
  (message "Creating LaTeX preview images failed (exit code %d). See buffer %s for details"
           exit-code
           (if (pcase (plist-get extended-info :processor)
                 ('dvisvgm (eq exit-code 252)) ; Input file does not exist.
                 ('dvipng ; Same check, just a bit more involved.
                  (and (eq exit-code 1)
                       (with-current-buffer buf
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward ": No such file or directory" nil t))))))
               (propertize org-latex-preview--latex-log 'face 'warning)
             (propertize (buffer-name buf) 'face 'warning)))
  (with-current-buffer (plist-get extended-info :org-buffer)
    (cl-loop for fragment in (plist-get extended-info :fragments)
             for path = (plist-get fragment :path)
             when (not path)
             for ov = (plist-get fragment :overlay)
             when ov do
             ;; ;TODO: Other options here include:
             ;; ;Fringe marker
             ;; (overlay-put ov 'before-string
             ;;              (propertize "!" 'display
             ;;                          `(left-fringe exclamation-mark
             ;;                            warning)))
             ;; ;Special face
             ;; (unless (overlay-get ov 'face)
             ;;   (overlay-put ov 'face 'org-latex-preview-processing-face))
             ;;
             ;; ;Note: ov has buffer extended-info, no need to set current-buffer
             (let ((start (overlay-start ov))
                   (end (overlay-end ov)))
               (delete-overlay ov)
               (when org-latex-preview-mode
                 (org-latex-preview--ensure-overlay start end))))))

(defvar-local org-latex-preview--preamble-content nil
  "Cache of the LaTeX preamble for snippet preview.")

(defun org-latex-preview--clear-preamble-cache ()
  "Set `org-latex-preview--preamble-content' to nil."
  (setq org-latex-preview--preamble-content nil))

(add-hook 'org-mode-hook #'org-latex-preview--clear-preamble-cache)

(defconst org-latex-preview--single-eqn-format
  "\n\\makeatletter
\\renewcommand{\\theequation}{\\(\\diamond\\)\\ifnum\\value{equation}>1%
\\,+\\,\\@arabic{\\numexpr\\value{equation}-1\\relax}\\fi}
\\makeatother"
  "A LaTeX preamble snippet that sets \"◇\"-based equation numbers.")

(defun org-latex-preview--get-preamble (&optional buf)
  "Obtain the LaTeX preview for snippet preview in BUF."
  (with-current-buffer (or buf (current-buffer))
    (org-fold-core-ignore-modifications
      (let ((org-inhibit-startup t)
            (info (org-combine-plists
                   (org-export--get-export-attributes
                    (org-export-get-backend 'latex))
                   (org-export--get-buffer-attributes)
                   '(:time-stamp-file nil)))
            org-export-replace-macros
            org-export-use-babel
            org-export-expand-links
            org-export-process-citations
            org-mode-hook
            org-latex-precompile
            ;; (org-latex-conditional-features
            ;;  (cl-remove-if
            ;;   (lambda (feat)
            ;;     (plist-get (alist-get (cdr feat)
            ;;                           org-latex-feature-implementations)
            ;;                :not-preview))
            ;;   org-latex-conditional-features))
            )
        (org-export-with-buffer-copy
         :drop-narrowing t
         (font-lock-mode -1)
         (setq info
               (org-export--annotate-info (org-export-get-backend 'latex) info))
         (concat
          (org-latex-make-preamble
           (org-combine-plists
            (org-export-get-environment
             (org-export-get-backend 'latex))
            '(:time-stamp-file nil))
           org-latex-preview-preamble 'snippet)
          (and (not org-latex-preview-numbered)
               org-latex-preview--single-eqn-format)))))))

(defconst org-latex-preview--include-preview-string
  "\n\\usepackage[active,tightpage,auctex,dvips]{preview}\n"
  "A LaTeX preamble snippet that includes preview.sty for previews.

The options passed to preview.sty are:
- active: activate preview package

- auctex: Produce fake error messages at the start and end of
  every preview environment.  We use this to help with parsing
  the tex compilation stdout using regexes.

- tightpage: (effectively) shrink page sizes to fit each
  previewed math environment.

- dvips: Only required for xelatex runs.  Add (as a side effect)
  preview geometry data to the output XDV file.

This is an abridged summary.  See the documentation of
preview.sty for more details.")

(defun org-latex-preview--create-tex-file (processing-info fragments appearance-options)
  "Create a LaTeX file based on PROCESSING-INFO and FRAGMENTS.

More specifically, a preamble will be generated based on
PROCESSING-INFO.  Then, if `org-latex-preview-process-precompile' is
non-nil, a precompiled format file will be generated if needed
and used.  Otherwise the preamble is used normally.

Within the body of the created LaTeX file, each of
FRAGMENTS will be placed in order, wrapped within a
\"preview\" environment.

APPEARANCE-OPTIONS is used to set the page width, see
`org-latex-preview-appearance-options'.

The path of the created LaTeX file is returned."
  (let* ((header
          (concat
           (plist-get processing-info :latex-header)
           org-latex-preview--include-preview-string))
         (textwidth
          ;; We can fetch width info from APPEARANCE-OPTIONS, but it's
          ;; possible that an old config using `org-format-latex-options'
          ;; won't have :page-width set, and so we need a default too.
          (let ((w (or (plist-get appearance-options :page-width) 0.6)))
            (cond
             ((stringp w)
              (format "\n\\setlength{\\textwidth}{%s}\n" w))
             ((and (floatp w) (<= 0.0 w 1.0))
              (format "\n\\setlength{\\textwidth}{%s\\paperwidth}\n" w)))))
         (relative-file-p
          (string-match-p "\\(?:\\\\input{\\|\\\\include{\\)[^/]" header))
         (remote-file-p (file-remote-p default-directory))
         (tex-temp-name
          (expand-file-name
           (concat (make-temp-name "org-tex-") ".tex")
           (and remote-file-p (temporary-file-directory))))
         (write-region-inhibit-fsync t)
         (coding-system-for-write buffer-file-coding-system)
         (precompile-failed-msg))
    (when (and relative-file-p remote-file-p)
      (error "Org LaTeX Preview does not currently support \\input/\\include in remote files"))
    (when org-latex-preview-process-precompile
      (pcase (plist-get processing-info :latex-processor)
        ("pdflatex"
         (if-let* ((format-file (org-latex-preview--precompile
                                 processing-info header (not relative-file-p))))
             (setq header (concat "%& " (file-name-sans-extension format-file)))
           (setq precompile-failed-msg
                 (concat
                  (format "Precompile failed for buffer %s."
                          (plist-get processing-info :org-buffer))
                  (when-let* ((filename (buffer-file-name
                                         (plist-get processing-info :org-buffer))))
                    (format " (File %s)" filename))
                  (condition-case nil
                      (unless (= 0 (call-process "kpsewhich" nil nil nil "preview.sty"))
                        "\nThe LaTeX package \"preview\" is required for precompilation, but could not be found")
                    (file-missing "\nPlease ensure that the LaTeX package \"preview\" is installed"))))))
        ((or "xelatex" "lualatex")
         (setq precompile-failed-msg
               (concat
                (plist-get processing-info :latex-processor)
                " does not support precompilation."))))
      (when precompile-failed-msg
        (display-warning
         '(org latex-preview disable-local-precompile)
         (concat
          precompile-failed-msg
          " Disabling LaTeX preview precompile in this buffer.\n To re-enable, run `(setq-local org-latex-preview-process-precompile t)' or reopen this buffer."))
        (setq-local org-latex-preview-process-precompile nil)))
    (with-temp-file tex-temp-name
      (insert header)
      ;; The \abovedisplayskip length must be set after \begin{document} because
      ;; it is usually set during the font size intialisation that occurs at
      ;; \begin{document}.  We can either modify the \normalsize command to set
      ;; the \abovedisplayskip length, or just set it after \begin{document}.
      (insert textwidth
              "\n\\begin{document}\n\n"
              "\\setlength\\abovedisplayskip{0pt}"
              " % Remove padding before equation environments.\n\n")
      (dolist (fragment-info fragments)
        (insert
         "\n\\begin{preview}\n"
         (plist-get fragment-info :string)
         "\n\\end{preview}\n"))
      (insert "\n\\end{document}\n"))
    tex-temp-name))

(defun org-latex-preview--tex-compile-async (extended-info)
  "Create an `org-async-call' spec to compile the texfile in EXTENDED-INFO."
  (let* ((tex-process-buffer            ;Create a new buffer if a process is running
          (if (and (buffer-live-p (get-buffer org-latex-preview--latex-log))
                   (get-buffer-process org-latex-preview--latex-log))
              (let ((buf (generate-new-buffer org-latex-preview--latex-log t)))
                (prog1 buf
                  (buffer-disable-undo buf)
                  (plist-put extended-info :proc-buffers (list buf))))
            (with-current-buffer        ;Else reuse the standard process buffer
                (get-buffer-create org-latex-preview--latex-log t)
              (setq buffer-undo-list t)
              (erase-buffer)
              (current-buffer))))
         (tex-compile-commands-fmt (plist-get extended-info :latex-compiler))
         (texfile (plist-get extended-info :texfile))
         (texfile-base (file-name-base texfile))
         (outputfile (concat texfile-base "."
                             (plist-get extended-info :image-input-type)))
         (org-tex-compiler
          (cdr (assoc (plist-get extended-info :latex-processor)
                      org-latex-preview-compiler-command-map)))
         (tex-command-spec
          `((?o . ,(shell-quote-argument (temporary-file-directory)))
            (?b . ,(shell-quote-argument texfile-base))
            (?f . ,(shell-quote-argument texfile))
            (?O . ,(shell-quote-argument
                    (expand-file-name outputfile (temporary-file-directory))))
            (?l . ,org-tex-compiler)
            (?L . ,(car (split-string org-tex-compiler)))))
         (tex-formatted-commands
          (mapcar (lambda (cmd) (split-string-shell-command
                            (format-spec cmd tex-command-spec)))
                  tex-compile-commands-fmt)))
    (unless org-tex-compiler
      (user-error "No `org-latex-preview-compiler-command-map' entry found for LaTeX processor %S, it should be a member of `org-latex-compilers' %S"
                  (plist-get extended-info :latex-processor)
                  org-latex-compilers))
    (with-current-buffer tex-process-buffer
      (erase-buffer)
      (insert "RUNNING: "
              (format-spec (car (last tex-compile-commands-fmt)) tex-command-spec)
              "\n")
      (add-text-properties (point-min) (1- (point))
                           '(face ((:height 0.8) font-lock-comment-face header-line))))
    (list 'org-async-task
          (if (cdr-safe tex-formatted-commands)
              (cons 'org-async-chain tex-formatted-commands)
            (car tex-formatted-commands))
          :buffer tex-process-buffer
          :info extended-info
          :filter #'org-latex-preview--latex-preview-filter
          :failure
          (lambda (exit-code buf _info)
            ;; With how preview.sty works, an exit code of 1 is expectd.
            (unless (eq exit-code 1)
              (message "LaTeX compilation for preview failed (error code %d).  See buffer %s for details"
                       exit-code (propertize (buffer-name buf) 'face 'warning)))))))

(defun org-latex-preview--image-extract-async (extended-info)
  "Create an `org-async-call' spec to extract images according to EXTENDED-INFO."
  (let* ((img-process-buffer            ;Create a new buffer if a process is running
          (if (and (buffer-live-p (get-buffer org-latex-preview--image-log))
                   (get-buffer-process org-latex-preview--image-log))
              (let ((buf (generate-new-buffer org-latex-preview--image-log t)))
                (prog1 buf
                  (buffer-disable-undo buf)
                  (push buf (plist-get extended-info :proc-buffers))))
            (with-current-buffer        ;Else reuse the standard process buffer
                (get-buffer-create org-latex-preview--image-log t)
              (setq buffer-undo-list t)
              (erase-buffer)
              (current-buffer))))
         (appearance-options (plist-get extended-info :appearance-options))
         (img-extract-command
          (pcase
              (or (and (string= (plist-get appearance-options :background)
                                "Transparent")
                       (plist-get extended-info :transparent-image-converter))
                  (plist-get extended-info :image-converter))
            ((and (pred stringp) cmd) cmd)
            ((and (pred consp) cmds)
             (when (> (length cmds) 1)
               (warn "Preview converter must now be a single command.  %S will be ignored."
                     (cdr cmds)))
             (car cmds))))
         (dpi (* 1.4 ; This factor makes it so generated PNGs are not blury
                     ; at the displayed resulution.
                 (or (plist-get appearance-options :scale) 1.0)
                 (if (display-graphic-p)
                     (org-latex-preview--get-display-dpi)
                   140.0)))
         (texfile (plist-get extended-info :texfile))
         (texfile-base (file-name-base texfile))
         (outputfile-base (expand-file-name texfile-base (temporary-file-directory)))
         (img-command-spec
          `((?o . ,(shell-quote-argument (temporary-file-directory)))
            (?b . ,(shell-quote-argument (file-name-base texfile)))
            (?B . ,(shell-quote-argument outputfile-base))
            (?D . ,(shell-quote-argument (format "%s" dpi)))
            ;; Unused, but we supported %S in the past, so keep
            ;; in the code.
            (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
            (?f . ,(shell-quote-argument
                    (expand-file-name
                     (concat texfile-base
                             "." (plist-get extended-info :image-input-type))
                     (temporary-file-directory))))
            (?O . ,(shell-quote-argument
                    (concat outputfile-base
                            (pcase (plist-get extended-info :processor)
                              ((or 'dvipng 'imagemagick) "-%09d.png")
                              ('dvisvgm "-%9p.svg")
                              (_ (concat "." (plist-get extended-info :image-output-type)))))))))
         (img-formatted-command
          (split-string-shell-command
           (format-spec img-extract-command img-command-spec))))
    (with-current-buffer org-latex-preview--image-log
      (erase-buffer)
      (insert "RUNNING: "
              (format-spec img-extract-command img-command-spec)
              "\n")
      (add-text-properties (point-min) (1- (point))
                           '(face ((:height 0.8) font-lock-comment-face header-line))))
    (list 'org-async-task
          img-formatted-command
          :buffer img-process-buffer
          :info extended-info
          :failure
          (format "Creating LaTeX preview images failed (exit code %%d). See buffer %s for details"
                  (propertize (buffer-name img-process-buffer) 'face 'warning)))))

(defun org-latex-preview--cleanup-callback (_exit-code _stdout extended-info)
  "Schedule cleanup with EXTENDED-INFO."
  (run-with-idle-timer
   1.0 nil
   #'org-latex-preview--do-cleanup
   extended-info))

(defun org-latex-preview--do-cleanup (extended-info)
  "Delete files after image creation, in accord with EXTENDED-INFO."
  (let* ((texfile (plist-get extended-info :texfile))
         (proc-buffers (plist-get extended-info :proc-buffers))
         (outputs-no-ext (expand-file-name (file-name-base texfile)
                                           (temporary-file-directory)))
         (images
          (mapcar
           (lambda (fragment-info)
             (plist-get fragment-info :path))
           (plist-get extended-info :fragments)))
         (clean-exts
          (or (plist-get extended-info :post-clean)
              '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                ".svg" ".png" ".jpg" ".jpeg" ".out"))))
    (when (file-exists-p texfile) (delete-file texfile))
    (when proc-buffers
      (unless (cl-some (lambda (fragment-info)
                         (plist-get fragment-info :errors))
                       (plist-get extended-info :fragments))
        (mapc #'kill-buffer proc-buffers)))
    (dolist (img images)
      (and img (delete-file img)))
    (dolist (ext clean-exts)
      (when (file-exists-p (concat outputs-no-ext ext))
        (delete-file (concat outputs-no-ext ext))))))

(defun org-latex-preview--generic-callback (_exit-code _stdout extended-info)
  "Place generated images, in accord with EXTENDED-INFO."
  (let* ((texfile (plist-get extended-info :texfile))
         (outputs-no-ext (expand-file-name (file-name-base texfile)
                                           (temporary-file-directory)))
         (images
          (file-expand-wildcards
           (concat outputs-no-ext "*." (plist-get extended-info :image-output-type))
           'full))
         (cache-location (plist-get extended-info :cache-location)))
    (save-excursion
      (cl-loop
       for fragment-info in (plist-get extended-info :fragments)
       for image-file in images
       for ov = (plist-get fragment-info :overlay)
       for cached-img =
       (org-latex-preview--cache-image
        (plist-get fragment-info :key)
        image-file
        (org-latex-preview--display-info
         extended-info fragment-info)
        cache-location)
       do
       (plist-put fragment-info :path image-file)
       (when (plist-get extended-info :place-preview-p)
         (org-latex-preview--update-overlay ov cached-img))))))

(defun org-latex-preview--check-all-fragments-produced (_exit-code _stdout extended-info)
  "Check each of the fragments in EXTENDED-INFO has a path.
Should this not be the case, the fragment immediately before the first
fragment without a path is marked as erronious, and the remaining
fragments are regenerated."
  (let ((fragments (cons nil (copy-sequence (plist-get extended-info :fragments))))
        (cache-location (plist-get extended-info :cache-location)))
    (while (cdr fragments)
      (if (or (plist-get (cadr fragments) :path)
              (plist-get (cadr fragments) :errors))
          (setq fragments (cdr fragments))
        ;; If output ends prematurely, this is most likely due to an issue with
        ;; the last "succesfully" produced fragment, and so we mark it as erronious
        ;; and attempt to re-generate the rest.
        (when-let* ((bad-fragment (car fragments)))
          (plist-put bad-fragment :errors
                     (concat (when-let* ((current-error (plist-get bad-fragment :errors)))
                               (concat current-error "\n\n"))
                             "Preview generation catastrophically failed after this fragment."))
          (org-latex-preview--remove-cached
           (plist-get bad-fragment :key)
           cache-location)
          (if (plist-get extended-info :place-preview-p)
              (org-latex-preview--update-overlay
               (plist-get bad-fragment :overlay)
               (org-latex-preview--cache-image
                (plist-get bad-fragment :key)
                (plist-get bad-fragment :path)
                (org-latex-preview--display-info
                 extended-info bad-fragment)
                cache-location))
            (org-latex-preview--cache-image
             (plist-get bad-fragment :key)
             (plist-get bad-fragment :path)
             (org-latex-preview--display-info
              extended-info bad-fragment)
             cache-location)))
        ;; Re-generate the remaining fragments.
        (org-latex-preview--create-image-async
         (plist-get extended-info :processor)
         (cdr fragments)
         :latex-processor (plist-get extended-info :latex-processor)
         :latex-preamble (plist-get extended-info :latex-header)
         :appearance-options (plist-get extended-info :appearance-options)
         :place-preview-p (plist-get extended-info :place-preview-p))
        (setq fragments nil)))))

(defun org-latex-preview--display-info (extended-info fragment-info)
  "From FRAGMENT-INFO and EXTENDED-INFO obtain display-relevant information."
  (let ((image-type (intern (plist-get extended-info :image-output-type)))
        info)
    (setq info (plist-put info :image-type image-type))
    (dolist (key '(:width :height :depth))
      (when-let* ((val (plist-get fragment-info key)))
        (plist-put info key val)))
    (plist-put info :errors (plist-get fragment-info :errors))
    info))

;; TODO: Figure out why this factor is needed.
(defconst org-latex-preview--shameful-magic-tex-scaling-factor
  1.01659593
  "Extra factor for aligning preview image baselines.

Sometimes a little sprinkling of pixie dust is needed to get
things just right.  Even just 2.7% magic can suffice.

This is the ratio of image sizes as reported by preview.sty and
computed by dvisvgm.  The latter is correct.")

(defconst org-latex-preview--tex-scale-divisor
  (* 65781.76 org-latex-preview--shameful-magic-tex-scaling-factor)
  "Base pt to point conversion for preview.sty output.

This is the product of three scaling quantities:

- Point to scaled point ratio: 1:65536
- Base point to scaled point ratio: 72:72.27
- The magic scaling factor
  (see `org-latex-preview--shameful-magic-tex-scaling-factor').")

(defun org-latex-preview--latex-preview-filter (_proc _string extended-info)
  "Examine the stdout from LaTeX compilation with preview.sty.

- The detected fontsize is directly entered into EXTENDED-INFO.
- The tightpage bounds information is captured and stored in EXTENDED-INFO.
- Fragment geometry and alignment info is computed using the
  tightpage info and page geometry reported by preview.sty.
- Fragment errors are put into the :errors slot of the relevant
fragments in EXTENDED-INFO."
  (unless (plist-get extended-info :fontsize)
    (when (save-excursion
            (re-search-forward "^Preview: Fontsize \\([0-9]+\\)pt$" nil t))
      (plist-put extended-info :fontsize (string-to-number (match-string 1)))
      ;; Since at this point can infer that the preamble logging is complete,
      ;; we can also check for hyperref and warn if it seems to be used,
      ;; as it is currently known to cause issues.
      (save-excursion
        (goto-char (point-min))
        (when (if (and (buffer-local-value 'org-latex-preview-process-precompile
                                           (plist-get extended-info :org-buffer))
                       (re-search-forward "^PRELOADED FILES:" nil t))
                  (re-search-forward "^ *hyperref\\.sty" nil t)
                (re-search-forward "^(.*hyperref/hyperref\\.sty" nil t))
          (display-warning
           '(org latex-preview hyperref)
           "Hyperref seems to be loaded, this is known to cause issues with the reported size information"
           :warning)))))
  (let ((preview-start-re
         "^! Preview: Snippet \\([0-9]+\\) started.\n<-><->\n *\nl\\.\\([0-9]+\\)[^\n]+\n")
        (preview-end-re
         "\\(?:^Preview: Tightpage.*$\\)?\n! Preview: Snippet [0-9]+ ended.(\\([0-9]+\\)\\+\\([0-9]+\\)x\\([0-9]+\\))")
        (fragments (plist-get extended-info :fragments))
        (tightpage-info (plist-get extended-info :tightpage))
        (concurrentp (eq (plist-get extended-info :processor) 'dvipng))
        preview-marks fragments-to-show)
    (beginning-of-line)
    (save-excursion
      (while (re-search-forward preview-start-re nil t)
        (push (list (match-beginning 0)
                    (match-end 0)
                    (string-to-number (match-string 1)) ; Preview index.
                    (1+ (string-to-number (match-string 2)))) ; Base line number.
              preview-marks)))
    (setq preview-marks (nreverse preview-marks))
    (while preview-marks
      (goto-char (caar preview-marks))
      (unless tightpage-info
        (save-excursion
          (when (re-search-forward
                 "^Preview: Tightpage \\(-?[0-9]+\\) *\\(-?[0-9]+\\) *\\(-?[0-9]+\\) *\\(-?[0-9]+\\)"
                 (or (caadr preview-marks) (point-max)) t)
            (setq tightpage-info
                  (mapcar #'string-to-number
                          ;; left-margin bottom-margin
                          ;; right-margin top-margin
                          (list (match-string 1) (match-string 2)
                                (match-string 3) (match-string 4))))
            (plist-put extended-info :tightpage tightpage-info))))
      ;; Check for processed fragment
      (if (re-search-forward preview-end-re (or (caadr preview-marks) (point-max)) t)
          (let ((fragment-info (nth (1- (nth 2 (car preview-marks))) fragments))
                (errors-substring
                 (save-match-data
                   (string-trim
                    (buffer-substring (cadar preview-marks)
                                      (match-beginning 0));; In certain situations we can end up with non-error
                    ;; logging informattion within the preview output.
                    ;; To make sure this is not captured, we rely on the fact
                    ;; that LaTeX error messages have a consistent format
                    ;; and start with an exclamation mark "!".  Thus, we
                    ;; can safely strip everything prior to the first "!"
                    ;; from the output.
                    "[^!]*")))
                depth)
            ;; Gather geometry and alignment info
            (if tightpage-info
                (progn
                  (setq depth
                        (/ (- (string-to-number (match-string 2))
                              (nth 1 tightpage-info))
                           org-latex-preview--tex-scale-divisor
                           (or (plist-get fragment-info :fontsize) 10)))
                  (plist-put fragment-info :depth depth)
                  (plist-put fragment-info :height
                             (+ (or depth 0)
                                (/ (+ (string-to-number (match-string 1))
                                      (nth 3 tightpage-info))
                                   org-latex-preview--tex-scale-divisor
                                   (or (plist-get fragment-info :fontsize) 10))))
                  (plist-put fragment-info :width
                             (/ (+ (string-to-number (match-string 3))
                                   (nth 2 tightpage-info)
                                   (- (nth 1 tightpage-info)))
                                org-latex-preview--tex-scale-divisor
                                (or (plist-get fragment-info :fontsize) 10))))
              (cl-loop for (geom . match-index)
                       in '((:height . 1) (:depth . 2) (:width . 3))
                       do
                       (plist-put fragment-info geom
                                  (/ (string-to-number (match-string match-index))
                                     org-latex-preview--tex-scale-divisor
                                     (or (plist-get fragment-info :fontsize) 10)))))
            (plist-put fragment-info :errors
                       (and (not (string-blank-p errors-substring))
                            (replace-regexp-in-string
                             "^l\\.[0-9]+"
                             (lambda (linum)
                               (format "l.%d"
                                       (- (string-to-number (substring linum 2))
                                          (nth 3 (car preview-marks)))))
                             errors-substring)))
            (when (and concurrentp (plist-get fragment-info :path))
              ;; path has been recorded by dvipng filter, can display image
              (push fragment-info fragments-to-show)))
        (goto-char (caar preview-marks)))
      (setq preview-marks (cdr preview-marks)))
    (when fragments-to-show
      (org-latex-preview--place-images
       extended-info (nreverse fragments-to-show)))))

(defun org-latex-preview--dvisvgm-filter (_proc _string extended-info)
  "Look for newly created images in the dvisvgm stdout buffer.
Any matches found will be matched against the fragments recorded in
EXTENDED-INFO, and displayed in the buffer."
  (let ((dvisvgm-processing-re "^processing page \\([0-9]+\\)[\n:]")
        (fragments (plist-get extended-info :fragments))
        page-marks fragments-to-show)
    (beginning-of-line)
    (save-excursion
      (while (re-search-forward dvisvgm-processing-re nil t)
        (push (cons (string-to-number (match-string 1))
                    (match-beginning 0))
              page-marks)))
    (setq page-marks (nreverse page-marks))
    (while page-marks
      (let ((start (cdar page-marks))
            (end (or (cdadr page-marks) (point-max)))
            (page (caar page-marks))
            fragment-info)
        (goto-char start)
        (when (save-excursion
                (re-search-forward "output written to \\(.*.svg\\)$" end t))
          (setq fragment-info (nth (1- page) fragments))
          (plist-put fragment-info :path (match-string 1))
          (when (save-excursion
                  (re-search-forward "^  page is empty" end t))
            (unless (plist-get fragment-info :errors)
              (plist-put fragment-info :errors "Image file not produced."))
            (plist-put fragment-info :path nil))
          (push fragment-info fragments-to-show)
          (goto-char end)))
      (setq page-marks (cdr page-marks)))
    (when fragments-to-show
      (setq fragments-to-show (nreverse fragments-to-show))
      (if (>= org-latex-preview--dvisvgm3-minor-version 1)
          (mapc #'org-latex-preview--await-fragment-existance fragments-to-show)
        (mapc #'org-latex-preview--svg-make-fg-currentColor fragments-to-show))
      (org-latex-preview--place-images extended-info fragments-to-show))))

(defun org-latex-preview--await-fragment-existance (svg-fragment)
  "Wait until SVG-FRAGMENT's file exists (up to 1s).
This is needed to accomidate for asyncronicity in file writing."
  (let ((path (plist-get svg-fragment :path)))
    (unless (or (not path) (file-exists-p path))
      (catch 'svg-exists
        (dotimes (_ 1000) ; Check for svg existance over 1s.
          (when (file-exists-p path)
            (throw 'svg-exists t))
          (sleep-for 0.001))))))

(defun org-latex-preview--svg-make-fg-currentColor (svg-fragment)
  "Replace the foreground color in SVG-FRAGMENT's file with \"currentColor\".
The foreground color is guessed to be the first specified <g>
fill color, which appears to be a reliable heuristic from a few
tests with the output of dvisvgm."
  (let ((write-region-inhibit-fsync t)
        ;; dvisvgm produces UTF-8 encoded files, so we might as well
        ;; avoid calling `find-auto-coding'.
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        ;; Prevent any file handlers (specifically
        ;; `image-file-handler') from being called.
        (file-name-handler-alist nil)
        (path (plist-get svg-fragment :path)))
    (when path
      (catch 'svg-exists
        (dotimes (_ 1000)           ; Check for svg existance over 1s.
          (when (file-exists-p path)
            (throw 'svg-exists t))
          (sleep-for 0.001)))
      (with-temp-buffer
        (insert-file-contents path)
        (unless ; When the svg is incomplete, wait for it to be completed.
            (string= (buffer-substring (max 1 (- (point-max) 6)) (point-max))
                     "</svg>")
          (catch 'svg-complete
            (dotimes (_ 1000) ; Check for complete svg over 1s.
              (if (string= (buffer-substring (max 1 (- (point-max) 6)) (point-max))
                           "</svg>")
                  (throw 'svg-complete t)
                (erase-buffer)
                (sleep-for 0.001)
                (insert-file-contents path)))
            (erase-buffer)))
        (goto-char (point-min))
        (if (or (= (buffer-size) 0)
                (re-search-forward "<svg[^>]*>\n<g[^>]*>\n</svg>" nil t))
            ;; We never want to show an empty SVG, instead it is better to delete
            ;; it and leave the LaTeX fragment without an image overlay.
            ;; This also works better with other parts of the system, such as
            ;; the display of errors.
            (delete-file path)
          (while (search-forward org-latex-preview--svg-fg-standin nil t)
            (replace-match "currentColor" t t))
          (write-region nil nil path nil 0))))))

(defun org-latex-preview--dvipng-filter (_proc _string extended-info)
  "Look for newly created images in the dvipng stdout buffer.
Any matches found will be matched against the fragments recorded in
EXTENDED-INFO, and displayed in the buffer."
  (let ((outputs-no-ext (expand-file-name
                         (file-name-base
                          (plist-get extended-info :texfile))
                         (temporary-file-directory)))
        (fragments (plist-get extended-info :fragments))
        fragments-to-show page-info-end)
    (while (search-forward "]" nil t)
      (setq page-info-end (point))
      (save-excursion
        (backward-list)
        (if (re-search-forward "\\=\\[\\([0-9]+\\) " page-info-end t)
            (let* ((page (string-to-number (match-string 1)))
                   (fragment-info (nth (1- page) fragments)))
              (plist-put fragment-info :path
                         (format "%s-%09d.png" outputs-no-ext page))
              (when (plist-get fragment-info :height)
                ;; geometry has been recorded by latex filter, can display image
                (push fragment-info fragments-to-show))))))
    (when fragments-to-show
      (org-latex-preview--place-images
       extended-info (nreverse fragments-to-show)))))


(defun org-latex-preview--place-images (extended-info &optional fragments)
  "Cache and place images for FRAGMENTS, according to their data and EXTENDED-INFO.
Should FRAGMENTS not be explicitly provided, all of the fragments
listed in EXTENDED-INFO will be used.

If this is an export run, images will only be cached, not placed."
  (let ((fragments (or fragments (plist-get extended-info :fragments)))
        (cache-location (plist-get extended-info :cache-location)))
    (if (plist-get extended-info :place-preview-p)
        (when (buffer-live-p (plist-get extended-info :org-buffer))
          (with-current-buffer (plist-get extended-info :org-buffer)
            (save-excursion
              (cl-loop
               for fragment-info in fragments
               for image-file = (plist-get fragment-info :path)
               for ov = (plist-get fragment-info :overlay)
               when (overlay-buffer ov)
               do (org-latex-preview--update-overlay
                   ov
                   (org-latex-preview--cache-image
                    (plist-get fragment-info :key)
                    image-file
                    (org-latex-preview--display-info
                     extended-info fragment-info)
                    cache-location))))))
      (dolist (fragment-info fragments)
        (org-latex-preview--cache-image
         (plist-get fragment-info :key)
         (plist-get fragment-info :path)
         (org-latex-preview--display-info
          extended-info fragment-info)
         cache-location)))))

(defconst org-latex-preview--cache-name "LaTeX preview cached image data"
  "The name used for Org LaTeX Preview objects in the org-persist cache.")

(defvar org-latex-preview--table nil
  "Hash table to hold LaTeX preview image metadata.

This is only used for non-persist image caching, used when
`org-latex-preview-cache' is not set to persist.")

(defun org-latex-preview--cache-image (key path info &optional cache-location)
  "Save the image at PATH with associated INFO in the cache indexed by KEY.
Return (path . info).

INFO is typically a plist containing image geometry information.

The caching location is set by CACHE-LOCATION, which defaults to
`org-latex-preview-cache'.  It should be the symbol \"persist\",
\"temp\", \"live\", or an existing directory path as a string."
  (if (not path)
      (ignore
       (display-warning
        '(org latex-preview put-cache)
        (format "Tried to cache %S without a path, skipping. This should not happen, please report it as a bug to the Org mailing list (M-x org-submit-bug-report)." key)
        :warning))
    (pcase (or cache-location org-latex-preview-cache)
      ('persist
       (let ((label-path-info
              (or (org-persist-read org-latex-preview--cache-name
                                    (list :key key)
                                    nil nil :read-related t)
                  (org-persist-register `(,org-latex-preview--cache-name
                                          (file ,path)
                                          (elisp-data ,info))
                                        (list :key key)
                                        :expiry org-latex-preview-persist-expiry
                                        :write-immediately t))))
         (cons (cadr label-path-info) info)))
      ((and dir (or 'temp 'live (pred stringp)))
       (unless org-latex-preview--table
         (setq org-latex-preview--table (make-hash-table :test 'equal :size 240)))
       (when (eq dir 'live)
         (org-latex-preview--increment-live-cache))
       (let ((new-path
              (expand-file-name
               (concat "org-tex-" key "." (file-name-extension path))
               (cond
                ((eq dir 'temp)
                 org-latex-preview--temp-cache-dir)
                ((eq dir 'live)
                 org-latex-preview-live--cache-dir)
                (t dir)))))
         (copy-file path new-path 'replace)
         (puthash key (cons new-path info)
                  org-latex-preview--table)))
      (bad (error "Invalid cache location: %S (must be persist, temp, or a string)" bad)))))

(defun org-latex-preview--increment-live-cache ()
  "Increment the live cache count, and perhaps roll it over."
  (when (> (cl-incf org-latex-preview-live--cache-count)
           org-latex-preview-live--max-cache-count)
    (delete-directory org-latex-preview-live--cache-dir t)
    (maphash
     (lambda (key cache)
       (when (string-prefix-p
              org-latex-preview-live--cache-dir (car cache))
         (remhash key org-latex-preview--table)))
     org-latex-preview--table)
    (make-directory org-latex-preview-live--cache-dir)
    (setq org-latex-preview-live--cache-count 1)))

(defun org-latex-preview--get-cached (key &optional cache-location)
  "Retrieve the image path and info associated with KEY.
The result, should it exist, will be of the form (path . info).

The caching location checked is set by CACHE-LOCATION, which
defaults to `org-latex-preview-cache'.  org-persist is checked
when the cache location is set to the symbol \"persist\",
otherwise an entry is looked for in `org-latex-preview--table'.

Example result:
  (\"/path/.../to/.../image.svg\"
   :type svg
   :height 1.4
   :width 7.6
   :depth 0.2
   :errors nil)"
  (let ((result
         (cond
          ((eq (or cache-location org-latex-preview-cache) 'persist)
           (and-let*
               ((label-path-info
                 (org-persist-read org-latex-preview--cache-name
                                   (list :key key)
                                   nil nil :read-related t))
                ;; While /in theory/ this check isn't needed, sometimes the
                ;; org-persist cache can be modified outside the current Emacs
                ;; process.  When this occurs the metadata of the fragment can
                ;; still exist in `org-persist--index', but the image file is
                ;; gone.  This condition can be detected by checking if the
                ;; `cadr' is nil (indicating the image has gone AWOL).
                (path (cadr label-path-info)))
             (cons path (caddr label-path-info))))
          (org-latex-preview--table
           (gethash key org-latex-preview--table)))))
    (if (and result (file-exists-p (car result)))
        result
      (org-latex-preview--remove-cached key cache-location)
      nil)))

(defun org-latex-preview--remove-cached (key &optional cache-location)
  "Remove the fragment cache associated with KEY.

The caching location used is set by CACHE-LOCATION, which
defaults to `org-latex-preview-cache'.  org-persist is used when
the cache location is set to the symbol \"persist\", otherwise an
entry is looked for in `org-latex-preview--table'."
  (cond
   ((eq (or org-latex-preview-cache cache-location) 'persist)
    (org-persist-unregister org-latex-preview--cache-name
                            (list :key key)
                            :remove-related t))
   (org-latex-preview--table
    (when-let* ((cache (gethash key org-latex-preview--table)))
      (remhash key org-latex-preview--table)
      (when (file-exists-p (car cache))
        (delete-file (car cache)))))))

(defun org-latex-preview-clear-cache (&optional beg end clear-entire-cache)
  "Clear LaTeX preview cache for fragments between BEG and END.

Interactively, act on
- the region if it is active,
- the fragment at point if in a fragment,
- the whole buffer otherwise.

When CLEAR-ENTIRE-CACHE is non-nil (interactively set by \\[universal-argument]),
the *entire* preview cache will be cleared, and `org-persist-gc' run."
  (interactive
   (if current-prefix-arg
       (list nil nil (y-or-n-p "This will clear the systemwide LaTeX preview cache, continue? "))
     (let ((context (if (derived-mode-p 'org-mode)
                        (org-element-context)
                      (user-error "This command must be run in an org-mode buffer"))))
       (cond
        ((use-region-p)
         (list (region-beginning) (region-end)))
        ((memq (org-element-type context)
               '(latex-fragment latex-environment))
         (list (org-element-property :begin context)
               (org-element-property :end context)))
        (t (list nil nil)))))
   org-mode)
  ;; Clear the precompile cache if clearing the whole buffer or everything.
  (when (or clear-entire-cache (not (or beg end)))
    (or org-latex-preview--preamble-content
        (setq org-latex-preview--preamble-content
              (org-latex-preview--get-preamble)))
    (let* ((full-preamble
            (concat org-latex-preview--preamble-content
                    org-latex-preview--include-preview-string))
           (compilers (assoc org-latex-compiler
                             org-latex-preview-compiler-command-map))
           (latex-precompiler
            (if (string= (thread-first (alist-get org-latex-preview-process-default
                                                  org-latex-preview-process-alist)
                                       (plist-get :image-input-type))
                         "pdf")
                (car compilers) (cdr compilers))))
      (dolist (compiler org-latex-compilers)
        (org-latex--remove-cached-preamble
         compiler full-preamble nil latex-precompiler)
        (org-latex--remove-cached-preamble
         compiler full-preamble t latex-precompiler))))
  (org-latex-preview-clear-overlays beg end)
  (if clear-entire-cache
      ;; FIXME: Perform this search using org-persist itself
      (let ((n 0))
        (dolist (collection org-persist--index)
          (when (equal (cadar (plist-get collection :container))
                       org-latex-preview--cache-name)
            (org-latex-preview--remove-cached
             (plist-get (plist-get collection :associated) :key)
             'persist)
            (cl-incf n)))
        (when org-latex-preview--table
          (maphash
           (lambda (_ cache)
             (when (file-exists-p (car cache))
               (delete-file (car cache))
               (cl-incf n)))
           org-latex-preview--table)
          (setq org-latex-preview--table nil))
        (if (= n 0)
            (message "The Org LaTeX preview cache was already empty.")
          (org-persist-gc)
          (message "Cleared all %d entries fom the Org LaTeX preview cache." n)))
    (let ((imagetype
           (or (plist-get (alist-get org-latex-preview-process-default
                                     org-latex-preview-process-alist)
                          :image-output-type)
               "png"))
          (numbering-table
           (and org-latex-preview-numbered
                (org-latex-preview--environment-numbering-table))))
      (dolist (element (org-latex-preview-collect-fragments beg end))
        (pcase-let* ((begin (or (org-element-property :post-affiliated element)
                                (org-element-property :begin element)))
                     (end (org-with-wide-buffer
                            (goto-char (org-element-property :end element))
                            (skip-chars-backward "\n\r\t ")
                            (point)))
                     (`(,fg ,bg) (org-latex-preview--colors-around begin end))
                     (value (org-element-property :value element))
                     (number (and numbering-table
                                  (eq (org-element-type element)
                                      'latex-environment)
                                  (gethash element numbering-table))))
          (org-latex-preview--remove-cached
           (org-latex-preview--hash
            org-latex-preview-process-default
            org-latex-preview--preamble-content
            value imagetype fg bg number))))
      (message "Cleared LaTeX preview cache for %s."
               (if (or beg end) "region" "buffer"))))
  (when (or clear-entire-cache (not (or beg end)))
    (org-latex-preview--clear-preamble-cache)))

(defun org-latex-preview--precompile (processing-info preamble &optional tempfile-p)
  "Precompile/dump LaTeX PREAMBLE text.

The path to the format file (.fmt) is returned.  If the format
file could not be found in the persist cache, it is generated
according to PROCESSING-INFO and stored.

If TEMPFILE-P is non-nil, then it is assumed the preamble does
not contain any relative references to other files.

This is intended to speed up Org's LaTeX preview generation
process."
  (org-latex--precompile
   (list :latex-compiler (plist-get processing-info :latex-processor))
   preamble
   tempfile-p
   (let* ((compilers
           (assoc (plist-get processing-info :latex-processor)
                  org-latex-preview-compiler-command-map))
          (org-tex-compiler
           (if (string= (plist-get processing-info :image-input-type) "pdf")
               (car compilers)
             (cdr compilers))))
     `((?l . ,org-tex-compiler)
       (?L . ,(car (split-string org-tex-compiler)))))))

(defun org-latex-preview--tex-styled (processing-type value appearance-options)
  "Apply LaTeX style commands to VALUE based on APPEARANCE-OPTIONS.
If PROCESSING-TYPE is dvipng, the colours are set with DVI
\"\\special\" commands instead of \"\\color\" and
\"\\pagecolor\".

VALUE is the math fragment text to be previewed.

APPEARANCE-OPTIONS is the plist in the form of
`org-latex-preview-appearance-options' with customized color
information for this run."
  (let* ((fg (pcase (plist-get appearance-options :foreground)
               ('default (org-latex-preview--format-color (org-latex-preview--attr-color :foreground)))
               ((pred null) (org-latex-preview--format-color "Black"))
               (color (org-latex-preview--format-color color))))
         (bg (pcase (plist-get appearance-options :background)
               ('default (org-latex-preview--attr-color :background))
               ("Transparent" nil)
               (bg (org-latex-preview--format-color bg))))
         (num (or (plist-get appearance-options :number)
                  (and (not (eq org-latex-preview-numbered 'preview))
                       1))))
    (concat (and (not (plist-get appearance-options :continue-color))
                 (if (eq processing-type 'dvipng)
                     (concat (and fg (format "\\special{color rgb %s}"
                                             (subst-char-in-string
                                              ?, ?\s fg)))
                             (and bg (format "\\special{background rgb %s}"
                                             (subst-char-in-string
                                              ?, ?\s bg))))
                   (concat
                    (and bg (format "\\pagecolor[rgb]{%s}" bg))
                    (and fg (format "\\color[rgb]{%s}" fg))
                    (and fg
                         (eq processing-type 'dvisvgm)
                         (>= org-latex-preview--dvisvgm3-minor-version 1)
                         "\\special{dvisvgm:currentcolor on}"))))
            (and num (format "\\setcounter{equation}{%d}" (1- num)))
            "%\n"
            value)))

(defun org-latex-preview--get-display-dpi ()
  "Get the DPI of the display.
The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (let ((mm-height (or (display-mm-height) 0))
            (mm-per-inch 25.4))
        (if (= 0 mm-height)
            140 ; Fallback reasonable DPI
          (round (/ (display-pixel-height) (/ mm-height mm-per-inch)))))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun org-latex-preview--attr-color (attr)
  "Return a RGB color for the LaTeX color package based on ATTR."
  (org-latex-preview--format-color (face-attribute 'default attr nil)))

(defvar org-latex-preview--format-color-cache
  '(("unspecified-fg" . "1.000,1.000,1.000")
    ("unspecified-bg" . "0.000,0.000,0.000"))
  "Cache for `org-latex-preview--format-color'.
Because `org-latex-preview--format-color' is called multiple
times for every fragment, even though only few colors will be
used it can be worth storing the results to avoid re-computing.")

(defun org-latex-preview--format-color (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (or (alist-get color-name org-latex-preview--format-color-cache nil nil #'equal)
      (cdar (push (cons color-name
                        (apply #'format "%.3f,%.3f,%.3f"
                               (mapcar
                                (lambda (v) (/ v 65535.0))
                                (color-values color-name))))
                  org-latex-preview--format-color-cache))))

(provide 'org-latex-preview)
;;; org-latex-preview.el ends here
