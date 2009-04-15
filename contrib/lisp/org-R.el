;;; org-R.el --- Computing and data visualisation in Org-mode using R

;; Copyright (C) 2009
;;   Free Software Foundation, Inc.

;; Author: Dan Davison <davison@stats.ox.ac.uk>
;; Keywords: org, R, ESS, tables, graphics
;; Homepage: http://www.stats.ox.ac.uk/~davison/software/org-R
;; Version: 0.06 2009-04-15
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file allows R (http://www.r-project.org) code to be applied to
;; emacs org-mode (http://orgmode.org) tables. When the result of the
;; analysis is a vector or matrix, it is output back into the org-mode
;; buffer as a new org table. Alternatively the R code may be used to
;; plot the data in the org table. It requires R to be running in an
;; inferior-ess-mode buffer (install Emacs Speaks Statistics
;; http://ess.r-project.org and issue M-x R).
;; 
;;
;; The user interface is via two different options lines in the org
;; buffer. As is conventional in org-mode, these are lines starting
;; with `#+'. Lines starting with #+R: specify options in the
;; standard org style (option:value) and are used to specify certain
;; off-the-shelf transformations and plots of the table data. The
;; #+R: line is also used to specify the data to be analysed
;; (either an org table or a csv file), and to restrict the analysis
;; to certain columns etc. In lines starting #+RR: you can supply
;; literal R code, giving you full control over what you do with the
;; table. With point in the first #+R line, M-x org-R-apply
;; makes happen whatever has been specified in those lines. 

;; The documentation is currently the Worg tutorial:
;;
;; http://orgmode.org/worg/org-tutorials/org-R/org-R.php
;;
;; changelog:
;; 2009-04-05 two bug fixes in org-R-eval contributed by David Moffat
;; 


(defconst org-R-skeleton-funcall-1-arg
  "%s(x[%s]%s)"
  "Skeleton of a call to an R function.
E.g. barplot(x[,3:5], names.arg=rownames(x))")

(defconst org-R-skeleton-funcall-2-args
  "%s(x[,%s], x[,%s]%s)"
  "Skeleton of a call to an R function which can take x and y
  args.")

(defconst org-R-write-org-table-def
  "write.org.table <- function (x, write.rownames = TRUE) 
{
    if(!is.null(dim(x)) && length(dim(x)) > 2)
        stop(\"Object must be 1- or 2-dimensional\") ;
    if(is.vector(x) || is.table(x) || is.factor(x) || is.array(x)) 
        x <- as.matrix(x) ;
    if(!(is.matrix(x) || inherits(x, c('matrix', 'data.frame')))) {
       invisible() ;
       print(x) ;
       stop(\"Object not recognised as 1- or 2-dimensional\") ;
    } ;
    if(is.null(colnames(x))) 
        colnames(x) <- rep('', ncol(x)) ;
    if(write.rownames) 
        x <- cbind(rownames(x), x) ;
    cat('|', paste(colnames(x), collapse = ' | '), '|\\n') ;
    cat('|', paste(rep('----', ncol(x)), collapse = '+'), '|\\n', sep = '') ;
    invisible(apply(x, 1, function(row) cat('|', paste(row, collapse = ' | '), '|\\n'))) ;
}"
  "Definition of R function to write org table representation of R objects.
To see a more human-readable version of this, look at the code,
or type dput(write.org.table) RET at the R (inferior-ess-mode
buffer) prompt.")

(defun org-R-apply-maybe ()
  (if (save-excursion
	(beginning-of-line 1)
	(looking-at "#\\+RR?:"))
      (progn (call-interactively 'org-R-apply)
	     t) ;; to signal that we took action
    nil)) ;; to signal that we did not

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-R-apply-maybe)


(defun org-R-apply ()
  "Construct and evaluate an R function call.
Construct an R function corresponding to the #+R: and #+RR:
lines. R must be currently running in an inferior-ess-mode
buffer. The function evaluates any user-supplied R code in the
#+RR: line before the off-the-shelf actions specified in the #+R:
line. The user-supplied R code can operate on a variable called x
that is the org table represented as a data frame in R. Text
output from the R process may be inserted into the org buffer, as
an org table where appropriate."
  (interactive)
  (require 'ess)
  (save-excursion 
    (beginning-of-line)
    (unless (looking-at "#\\+RR?:") (error "Point must be in a #+R or #+RR line"))
    (while (looking-at "#\\+RR?:") (forward-line -1))
    (forward-line)
    ;; For the rest of the code in this file we are based at the
    ;; beginning of the first #+R line

    ;; FIXME: if point is at the beginning of the #+RR? lines when
    ;; this function is called, then tabular output gets inserted,
    ;; leaving point up at the top of the tabular output.

    (let* ((options (org-R-get-options))
	   (code (org-R-construct-code options))
	   (infile (plist-get options :infile))
	   (ext (if infile (file-name-extension infile)))
	   csv-file)

      (if (string-equal ext "csv")
	  (setq csv-file infile)
	(setq csv-file
	      (org-R-export-to-csv
	       (make-temp-file "org-R-tmp" nil ".csv") options)))
      
      (org-R-eval code csv-file options)

      (delete-other-windows) ;; FIXME
      (if (plist-get options :showcode) (org-R-showcode code)))))

(defun org-R-apply-throughout-subtree ()
  "Call org-R-apply in every org-R block in current subtree."
  ;; This currently relies on re-search-forward leaving point after
  ;; the #+RR?: If point were at the beginning of the line, then
  ;; tabular input would get inserted leaving point above the #+RR?:,
  ;; and this would loop infinitely. Same for org-R-apply-to-buffer.
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (while (re-search-forward 
	    "^#\\+RR?:"
	    (save-excursion (org-end-of-subtree)) t)
      (org-R-apply)
      (forward-line)
      (while (looking-at "#\\+RR?")
	(forward-line)))))

(defun org-R-apply-throughout-buffer ()
  "Call org-R-apply in every org-R block in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+RR?:" nil t)
      (org-R-apply)
      (forward-line)
      (while (looking-at "#\\+RR?")
	(forward-line)))))
  
(defun org-R-construct-code (options)
  "Construct the R function that implements the requested
behaviour.
The body of this function derives from two sources:

1. Explicit R code which is read from lines starting with
#+RR: by org-R-get-user-code, and

2. Off-the-shelf code corresponding to options specified in the
#+R: line. This code is constructed by
org-R-off-the-shelf-code."
  (let ((user-code (org-R-get-user-code))
	(action (plist-get options :action)))

    (if (or (eq action 'tabulate) (eq action 'transpose))
	(setq options (plist-put options :output-to-buffer t)))
    (format "function(x){%sx}"
	    (concat
	     (when user-code (concat user-code ";"))
	     (when action (concat (org-R-off-the-shelf-code options) ";"))))))

(defun org-R-get-user-code (&optional R)
  "Read user-supplied R code from #+RR: lines."
  (let ((case-fold-search t))
    (save-excursion
      (while (looking-at "^#\\+\\(RR?:\\) *\\(.*\\)")
	(if (string= "RR:" (match-string 1))
	    (setq R (concat R (when R ";") (match-string 2))))
	(forward-line))))
  R)

(defun org-R-off-the-shelf-code (options)
  "Return R code implementing the actions requested in the
#+R: lines."
  
  ;; This is a somewhat long function as it deals with several
  ;; different cases, corresponding to all the off-the-shelf actions
  ;; that have been implemented.
  
  (let* ((action (plist-get options :action))
	 (cols (plist-get options :columns))
	 (ncols (org-R-number-of-columns cols))
	 (nxcols (nth 0 ncols))
	 (nycols (nth 1 ncols))
	 (cols-R (org-R-make-index-vectors cols))
	 (xcols-R (nth 0 cols-R))
	 (ycols-R (nth 1 cols-R))
	 seq args largs extra-code title colour matrix-index)
    
    ;; I want this to affect options outside this function.  Will it
    ;; necessarily do so? (not if plist-put adds to head of the
    ;; plist?)
    (setq options (plist-put options :nxcols nxcols))
    
    (cond ((eq action 'points)
	   (setq action 'plot)
	   (setq options (plist-put options :lines nil)))
	  ((eq action 'lines)
	   (setq action 'plot)
	   (setq options (plist-put options :lines t))))
    
    (if (and (setq title (plist-get options :title)) (symbolp title))
	(setq title symbol-name title))
    
    (setq args (plist-put args :main (concat "\"" title "\"")))
    
    (if (setq colour (or (plist-get options :colour)
			 (plist-get options :color)
			 (plist-get options :col)))
	(setq args
	      (plist-put args :col
			 (concat "\"" (if (symbolp colour) (symbol-name colour) colour) "\""))))
      
    (setq largs
	  (if (setq legend (plist-get options :legend))
	      (plist-put largs :x
			 (concat "\"" (if (symbolp legend) (symbol-name legend) legend) "\""))
	    (plist-put largs :x "\"topright\"")))

    (cond
     ((null ycols-R)
      ;; single set of columns; implicit x values
      (if (null xcols-R)
	  (setq xcols-R "" matrix-index "")
	(setq matrix-index (concat "," xcols-R)))
      (cond

       ;;----------------------------------------------------------------------

       ((eq action 'barplot)
	(if (eq nxcols 1)
	    (progn
	      (setq args (plist-put args :names.arg "rownames(x)"))
	      (setq args (org-R-set-user-supplied-args args (plist-get options :args)))
	      (format org-R-skeleton-funcall-1-arg
		      "barplot" xcols-R
		      (concat ", " (org-R-plist-to-R-args args))))
	  
	  (setq args (plist-put args :names.arg "colnames(x)"))
	  (setq args (plist-put args :col "seq(nrow(x))"))
	  (setq args (plist-put args :beside "TRUE"))

	  (setq largs (plist-put largs :bty "\"n\""))
	  ;; (setq largs (plist-put largs :lwd 10))
	  (setq largs (plist-put largs :col "seq(nrow(x))"))
	  (setq largs (plist-put largs :legend "rownames(x)"))
	  
	  (setq args (org-R-set-user-supplied-args args (plist-get options :args)))
	  
	  (concat (format org-R-skeleton-funcall-1-arg 
			  "barplot(as.matrix" matrix-index
			  (concat "), " (org-R-plist-to-R-args args)))
		  "; legend(" (org-R-plist-to-R-args largs) ")")))

       ;;----------------------------------------------------------------------

       ((eq action 'density)
	(if (and nxcols (> nxcols 1))
	  (error "Multiple columns not implemented for action:%s" action))

	(setq args (plist-put args :xlab (concat "colnames(x)["xcols-R"]")))
	(setq args (org-R-set-user-supplied-args args (plist-get options :args)))

	(format org-R-skeleton-funcall-1-arg
		"plot(density" matrix-index
		(concat "), " (org-R-plist-to-R-args args))))

       ;;----------------------------------------------------------------------

       ((eq action 'hist)
	(if (and nxcols (> nxcols 1))
	  (error "Multiple columns not implemented for action:%s" action))
	(setq args (plist-put args :xlab (concat "colnames(x)["xcols-R"]")))
	(setq args (org-R-set-user-supplied-args args (plist-get options :args)))
	(setq args (concat ", " (org-R-plist-to-R-args args)))
	(format org-R-skeleton-funcall-1-arg "hist" matrix-index args))
       
       ;;----------------------------------------------------------------------

       ((eq action 'image)
	(format org-R-skeleton-funcall-1-arg "image(as.matrix" matrix-index ")"))

       ;;----------------------------------------------------------------------

       ((eq action 'plot)
	(setq seq (concat "seq_along("xcols-R")"))

	(setq args (plist-put args :type (if (plist-get options :lines) "\"l\"" "\"p\"")))
	(setq args (plist-put args :ylab (concat "colnames(x)["xcols-R"]")))
	(setq args (concat ", " (org-R-plist-to-R-args args)))

	(concat (format org-R-skeleton-funcall-1-arg
			(if (eq nxcols 1) "plot" "matplot") matrix-index args)
		extra-code))
	
       ;;----------------------------------------------------------------------

       ((eq action 'tabulate)
	(concat
	 (if (plist-get options :sort)
	     (format org-R-skeleton-funcall-1-arg
		     "x <- sort(table" xcols-R "), decreasing=TRUE")
	   (format org-R-skeleton-funcall-1-arg "x <- table" matrix-index ""))
	 (if (eq nxcols 1) "; x <- data.frame(value=names(x), count=x[])")))
       
       ;;----------------------------------------------------------------------

       ((eq action 'transpose)
	(format org-R-skeleton-funcall-1-arg "x <- t" matrix-index ""))
     
       ;;----------------------------------------------------------------------
       
       ;; Don't recognise action: option, try applying it as the name of an R function.
       
       (t (format org-R-skeleton-funcall-1-arg
		  (concat "x <- " (symbol-name action)) matrix-index ""))))
     
     ;;----------------------------------------------------------------------

     (ycols-R
      ;; x and y columns specified
      (cond
       
       ;;----------------------------------------------------------------------
       
       ((eq action 'plot)
	(unless (eq nxcols 1) (error "Multiple x-columns not implemented for action:plot"))
	
	(setq args
	      (plist-put
	       args :ylab
	       (concat "if(length("ycols-R") == 1) colnames(x)["ycols-R"] else ''")))
	(setq args (plist-put args :xlab (concat "colnames(x)["xcols-R"]")))

	(setq args (plist-put args :type (if (plist-get options :lines) "\"l\"" "\"p\"")))
	
	(setq args (concat ", " (org-R-plist-to-R-args args)))
	(setq seq (concat "seq_along("ycols-R")"))

	(setq largs (plist-put largs :col seq))
	(setq largs (plist-put largs :lty seq))
	(setq largs (plist-put largs :bty "\"n\""))
	(setq largs (plist-put largs :legend (concat "colnames(x)["ycols-R"]")))
	
	(setq extra-code
	      (concat "; "
		      "if(length("ycols-R") > 1) "
		      "legend(" (org-R-plist-to-R-args largs) ")"))
	
	(concat (format org-R-skeleton-funcall-2-args
			(if (and (eq nxcols 1) (eq nycols 1)) "plot" "matplot")
			xcols-R ycols-R args)
		extra-code))
       
       ;;----------------------------------------------------------------------
       
       (t (error "action:%s requires a single set of columns" (symbol-name action))))))))

(defun org-R-set-user-supplied-args (args user-args)
  "Set user-supplied values in arguments plist."
  (while (setq prop (pop user-args))
    (setq args (plist-put args prop (pop user-args))))
  args)
  
(defun org-R-plist-to-R-args (plist)
  "Convert a plist into a string of R arguments."
  (let (arg-string arg)
    (while (setq arg (pop plist))
      (string-match ":\\(\.*\\)" (symbol-name arg))
      (setq arg (match-string 1 (symbol-name arg)))
      (setq arg-string
	    (concat
	     (if arg-string (concat arg-string ", "))
	     (format "%s=%s" arg (pop plist)))))
    arg-string))

(defun org-R-alist-to-R-args (alist)
  "Convert an alist of (argument . val) pairs into a string of R arguments.
The alist is something like
      '((arg1 . 1)
	(arg2 . a)) 
This isn't used, but it seems much nicer than
my plist equivalent. Is there a better way to write the plist
version?
"
  (mapconcat
   'identity
   (mapcar (lambda(pair) (format "%s = %s" (car pair) (cdr pair))) alist)
   ", "))

(defun org-R-make-index-vectors (cols)
  "Construct R indexing vectors as strings from lisp form.

COLS is the lisp form given by the `columns:' option. It may
take the following forms:

1. integer atom        - the number of the column
2. symbol/string atom  - the name of the column
3. list of length 1    - same as 1 or 2 above
4. list of length > 1  - specification of multiple columns as 1 or 2 above, unless it is
5. list of 2 lists     - each list specifies (possibly multiple) columns

In cases 1-4 this function returns a list of length 1, containing
the R index vector as a string. In case 5 this function returns a
list of two such index vectors.

In cases 1 - 4, when a bivariate plot is requested such as by
`action:lines', the x values are implicit, i.e
1,2,...,number-of-rows.

In case 4, an attempt is made to do something sensible with the
multiple columns, e.g. for `action:lines' they will be plotted
together on the same graph against the implicit x values, and for
`action:barplot' the bars corresponding to a single row will be
stacked on top of each other, or placed side by side, depending
on the value of the `beside' option.

For `action:tabulate', if 2 columns are selected, a
two-dimensional table is created. If more than 2, then the
appropriately dimensioned table is computed and inserted using
the standard text representation of multi-dimensional arrays used
by R (as org does not currently have tables of dimension > 2).

The straightforward case of case 5 is that both lists are of
length 1. For `action:plot' and `action:lines' these specify the
y and x coordinates of the points to be plotted or joined by
lines. 

The intention is that `org-R-apply' does something
corresponding to what would happen if you did the following in R:

fun(x=tab[,xcols], y=tab[,ycols])

where fun is the R function implementing the desired
action (plotting/computation), tab is the org table, xcols are
the columns specified in cases 1-4 above, and ycols are the
second set of columns which might have been specified under case
5 above. For relevant R documentation see the help page
associated with the function xy.coords, e.g. by typing ?xy.coords
at the R prompt.

The following won't work with case 5: `tabulate'
"
  (defun org-R-make-index-vector (cols)
    "Return the R indexing vector (as a string) corresponding to
the lisp form COLS. In this function, COLS is a either a list of
atoms, or an atom, i.e. in the form of cases 1-4"
    (when cols
      (let (to-stringf)
	(unless (listp cols) (setq cols (list cols)))
	(setq to-stringf 
	      (cond ((car (mapcar 'symbolp cols))
		     (lambda (symbol) (concat "\"" (symbol-name symbol) "\"")))
		    ((car (mapcar 'integerp cols))
		     'int-to-string)
		    ((car (mapcar 'stringp cols))
		     (lambda (string) (concat "\"" string "\"")))
		    (t (error "Column selection should be symbol, integer or string: %S" cols))))
	(concat (when (> (length cols) 1) "c(")
		(mapconcat to-stringf cols ",")
		(when (> (length cols) 1) ")")))))

  (if (and (listp cols) (listp (car cols)))
      (mapcar 'org-R-make-index-vector cols) ;; case 5
    (list (org-R-make-index-vector cols))))  ;; other cases

(defun org-R-number-of-columns (cols)
  (defun f (c) (if (listp c) (length c) 1))
  (if (and (listp cols) (listp (car cols)))
      (mapcar 'f cols)
    (list (f cols))))
  

(defun org-R-eval (R-function csv-file options)
  "Apply an R function to tabular data and receive output as an org table.

R-FUNCTION is a string; it may be simply the name of an
appropriate R function (e.g. \"summary\", \"plot\"), or a
user-defined anonymous function of the form
\"(function(data.frame) {...})\". It will receive as its first
argument the org table as an R 'data frame' -- a table-like
structure which can have columns containing different types of
data -- numeric, character etc.

The R function may produce graphical and/or text output. If it
produces text output, and the replace:t is specified, and if
there is a table immediately above the #+R lines, then it is
replaced by the text output. Otherwise the text output is
inserted above the #+R lines.
"
  (let ((transit-buffer "org-R-transit")
	(infile (plist-get options :infile))
	(output-file (plist-get options :outfile))
	(title (plist-get options :title))
	output-format graphics-output-file width height)

    (unless (not output-file)
      ;; We are writing output to file. Determine file format and
      ;; location, and open graphics device if necessary.
      (if (string-match
	   "\\(.*\.\\)?\\(org\\|png\\|jpg\\|jpeg\\|pdf\\|ps\\|bmp\\|tiff\\)$"
	   output-file)
	  (setq output-format (match-string 2 output-file))
	(error "Did not recognise file name suffix %s as available output format"
	       (match-string 2 output-file)))
      (unless (match-string 1 output-file)
	;; only suffix provided: store in org-attach dir
	(require 'org-attach)
	(let ((temporary-file-directory (org-attach-dir t)))
	  (setq output-file
		(make-temp-file
		 "org-R-output-" nil (concat "." output-format)))))
      ;;; MdQ bug fix.
      ;;; If a filename is given, make sure it's absolute,
      ;;; as ess-execute needs that later.
      (if (match-string 1 output-file)
	  (setq output-file (expand-file-name output-file)) )

      (if (eq output-format "jpg") (setq output-format "jpeg"))
      (setq graphics-output-file (not (string-equal output-format "org")))
      (if graphics-output-file ;; open the graphics device
	  (ess-execute
	   (concat output-format "(file=\"" output-file "\""
		   (if (setq width (plist-get options :width))
		       (format ", width=%d" width))
		   (if (setq height (plist-get options :height))
		       (format ", height=%d" height)) ")"))))

    ;; Apply R code to table (which is now stored as a csv file)
    ;; does it matter whether this uses ess-command or ess-execute?

    ;; First evaluate function definition for R -> org table conversion
    ;;; MdQ bug fix.
    ;;; The following save-excursion has been brought up to here
    ;;; so that the two ess-execute commands are now within it.
    ;;; This is because they have the side effect of changing current
    ;;; buffer to the transit-buffer, which causes error of deleting
    ;;; the wrong table there, instead of in the org buffer.
    (save-excursion
      (ess-execute (replace-regexp-in-string "\n" " " org-R-write-org-table-def)
		   nil transit-buffer)

      ;; FIXME: why not eval the function def together with the function call
      ;; as in the commented out line below (it didn't work for some reason)
      (ess-execute
       (concat
	;; (replace-regexp-in-string "\n" " " org-R-write-org-table-def) ";"
	(org-R-make-expr R-function csv-file options)) nil transit-buffer)

      ;;       (set-buffer (concat "*" transit-buffer "*"))
      (unless (or (looking-at "$")
		  (string-equal (buffer-substring-no-properties 1 2) "|"))
	(error "Error in R evaluation:\n%s" (buffer-string))))


    (if csv-file
	(unless (and infile
		     (string-equal (file-name-extension infile) "csv"))
	  (delete-file csv-file)))

    (if graphics-output-file (ess-execute "dev.off()")) ;; Close graphics device

    (unless (or graphics-output-file
		(not (plist-get options :output-to-buffer)))
      ;; Send tabular output to a org buffer as new org
      ;; table. Recall that we are currently at the beginning of the
      ;; first #+R line
      (if (and output-file graphics-output-file)
	  (error "output-to-buffer and graphics-output-file both t"))
		
      (save-excursion
	(if output-file
	    (progn (set-buffer (find-file-noselect output-file))
		   (delete-region (point-min) (point-max)))
	  (if (plist-get options :replace)
	      (progn ;; kill a table iff in one or one ends on the previous line
		(delete-region (org-table-begin) (org-table-end))
		(save-excursion
		  (forward-line -1)
		  (if (looking-at "#\\+TBLNAME")
		      (delete-region (point) (1+ (point-at-eol))))))))
	(if title (insert "#+TBLNAME:" title "\n"))
	(insert-buffer-substring (concat "*" transit-buffer "*"))
	(org-table-align)
	(if output-file (save-buffer))))

    ;; We might be linking to graphical output, or to org output in
    ;; another file. Either way, point is still at the beginning of
    ;; the first #+R line.
    (unless (not output-file)
      (save-excursion
	(forward-line -1)
	(if (looking-at "\\[\\[file:")
	    (delete-region (point) (1+ (point-at-eol)))))
      (insert (org-make-link-string
	       (concat "file:" output-file)
	       (unless (plist-get options :inline)
		 (or title (concat output-format " output")))) "\n"))

    (kill-buffer (concat "*" transit-buffer "*"))))


(defun org-R-export-to-csv (csv-file options)
  "Find and export org table to csv.

If the intable: option has not been supplied, then the table must
end on the line immediately above the #+R lines. Otherwise,
the remote table referenced by the intable: option is found using
org-R-find-table. If options:infile has been set then this is the
org file containing the table. See the docstring of
org-R-find-table for details."
  (let ((tbl-name-or-id (plist-get options :intable))
	(org-file (plist-get options :infile)) tbl-marker)
    
    (if (and org-file
	     (not (string-equal (file-name-extension org-file) "org")))
	(error "File %s extension is not .csv so should be .org"))

    (save-excursion
      (if tbl-name-or-id
	  ;; a remote table has been specified -- move into it
	  (progn
	    (if org-file (set-buffer (find-file-noselect org-file)))
	    (setq tbl-marker (org-R-find-table tbl-name-or-id 'marker))
	    (set-buffer (marker-buffer tbl-marker))
	    (goto-char (marker-position tbl-marker)))
	(forward-line -1)) ;; move into table above
      (if (looking-at "[ \t]*|")
	  (progn (org-table-export csv-file "orgtbl-to-csv") csv-file)
	nil))))

(defun org-R-find-table (name-or-id &optional markerp)
  "Return location of a table.

NAME-OR-ID may be the name of a
table in the current file as set by a \"#+TBLNAME:\" directive.
The first table following this line will then be used.
Alternatively, it may be an ID referring to any entry, perhaps in
a different file.  In this case, the first table in that entry
will be referenced. The location is returned as a marker pointing
to the beginning of the first line of the table.

This is taken from the first part of org-table-get-remote-range
in org-table.el.
"
  (cond
   ((symbolp name-or-id) (setq name-or-id (symbol-name name-or-id)))
   ((numberp name-or-id) (setq name-or-id (number-to-string name-or-id))))
  (save-match-data
    (let ((id-loc nil) (case-fold-search t) buffer loc)
      (save-excursion
	(save-restriction
	  (widen)
	  (save-excursion
	    (goto-char (point-min))
	    (if (re-search-forward
		 (concat "^#\\+TBLNAME:[ \t]*" (regexp-quote name-or-id) "[ \t]*$")
		 nil t)
		;; OK, we've found a matching table name in this buffer.
		(setq buffer (current-buffer) loc (match-beginning 0))
	      ;; It's not a table name in this buffer. It must be an entry id.
	      ;; obtain a marker pointing to it.
	      (setq id-loc (org-id-find name-or-id 'marker)
		    buffer (marker-buffer id-loc)
		    loc (marker-position id-loc))
	      (move-marker id-loc nil))) ;; disable the marker
	  ;; (switch-to-buffer buffer)
	  (set-buffer buffer)
	  ;; OK, so now we're in the right buffer, and loc is either
	  ;; the beginning of the #+TBLNAME line, or the location of the entry
	  ;; either way we need to search forward to get to the beginning of the table
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char loc)
	      (forward-char 1)
	      ;; The following regexp search finds the beginning of
	      ;; the next table in this entry. If it gets to the next
	      ;; entry before the next table, then it signals failure.
	      (unless (and (re-search-forward "^\\(\\*+ \\)\\|[ \t]*|" nil t)
			   (not (match-beginning 1)))
		(error "Cannot find a table at NAME or ID %s" name-or-id))
	      (if markerp
		  (move-marker (make-marker) (point-at-bol) (current-buffer))
		(error "Option to return cons cell not implemented.
                        It should return (file-name . position) to be 
                        consistent with functions in org-id.el")))))))))

(defun org-R-make-expr (R-function csv-file options)
  "Construct R code to read data, analyse it and write output."

  (let ((rownames (plist-get options :rownames))
	(colnames (plist-get options :colnames))
	(action (plist-get options :action))
	(replace (plist-get options :replace)))

    (if (and csv-file (symbolp csv-file))
	(setq csv-file (symbol-name csv-file)))

    (format "write.org.table((%s)(%s), write.rownames=%s)"
	    R-function
	    (if csv-file
		(format
		 "read.csv(\"%s\", header=%s, row.names=%s)"
		 csv-file
		  
		 ;; Do we treat first row as colnames? Yes by default
		 ;; FIXME: should really check for hline
		 (if colnames "TRUE" "FALSE")

		 ;; Do we use a column as rownames? Not unless rownames: is specified
		 (if rownames "1" "NULL"))
	      "NULL")

	    ;; Do we write rownames into org table?
	    (cond ((eq action 'tabulate)
		   (if (eq (plist-get options :nxcols) 1) "FALSE" "TRUE"))
		  ((eq action 'transpose) (if colnames "TRUE" "FALSE"))
		  (rownames "TRUE")
		  (t "TRUE")))))
	    
(defun org-R-get-options ()
  "Parse the #+R: lines and return the options and values as a p-list."
  (let ((opts '(
		(:infile . "infile")
		(:intable . "intable")
		(:rownames . "rownames")
		(:colnames . "colnames")
		(:columns . "columns")

		(:action . "action")
		(:args . "args")
		
		(:outfile . "outfile")
		(:replace . "replace")
		(:title . "title")
		(:legend . "legend")
		(:colour . "colour")
		(:color . "color")
		(:col . "col")
		(:height . "height")
		(:width . "width")
		(:lines . "lines")
		(:sort . "sort")
		(:inline . "inline")

		(:output-to-buffer . "output-to-buffer")
		
		(:showcode . "showcode")))
	(regexp ":\\(\"[^\"]*\"\\|(([^)]*) *([^)]*))\\|([^)]*)\\|[^ \t\n\r;,.]*\\)")
	(case-fold-search t) p)

    ;; FIXME: set default options properly
    (setq p (plist-put p :output-to-buffer t)) ;; FIXME: hack: null options plist is bad news
    (setq p (plist-put p :replace t))
    (setq p (plist-put p :rownames nil))
    (setq p (plist-put p :colnames t))
    (setq p (plist-put p :inline nil))

    (save-excursion
      (while (looking-at "^#\\+\\(RR?:+\\) *\\(.*\\)")
	(if (string= "R:" (match-string 1))
	    (setq p (org-R-add-options-to-plist p (match-string 2) opts regexp)))
	(forward-line)))
    p))

(defun org-R-add-options-to-plist (p opt-string op regexp)
  "Parse a #+R: line and set values in the property list p.
This function is adapted from similar functions in org-exp.el
and org-plot.el. It might be a good idea to have a single
function serving these three files' needs."
  ;; Adapted from org-exp.el and org-plot.el
  (let (o)
    (when opt-string
      (while (setq o (pop op))
	(if (string-match 
	     (concat (regexp-quote (cdr o)) regexp)
	     opt-string)
	    (setq p (plist-put p (car o)
			       (car (read-from-string
				     (match-string 1 opt-string)))))))))
  p)


(defun org-R-sanitise-options (options)
  (error "not used yet")
  (let (should-be-strings '(title legend colour color col csv)))
  )
(defun org-R-showcode (R)
  "Display R function constructed by org-R in a new R-mode
buffer."
  (split-window-vertically)
  (switch-to-buffer "*org-table.R*")
  (kill-region (point-min) (point-max))
  (R-mode)
  (insert (replace-regexp-in-string 
	   ";" "\n" (replace-regexp-in-string "\\([{}]\\)" "\n\\1\n" R)))
  ;; (mark-whole-buffer)
  ;; (indent-region)
  ;; why doesn't that do what I hoped?
  )

(defun org-R-get-remote-range (name-or-id form)
  "Get a field value or a list of values in a range from table at ID.

This is a refactoring of Carsten's original version. I have
extracted the first bit of his function and named it
org-R-find-table (which would presumably be called something like
org-table-find-table or org-id-find-table if this were accepted).

---

Get a field value or a list of values in a range from table at ID.

NAME-OR-ID may be the name of a table in the current file as set by
a \"#+TBLNAME:\" directive.  The first table following this line
will then be used.  Alternatively, it may be an ID referring to
any entry, possibly in a different file.  In this case, the first table
in that entry will be referenced.
FORM is a field or range descriptor like \"@2$3\" or or \"B3\" or
\"@I$2..@II$2\".  All the references must be absolute, not relative.

The return value is either a single string for a single field, or a
list of the fields in the rectangle."

  (let ((tbl-marker (org-R-find-table name-or-id 'marker))
	org-table-column-names org-table-column-name-regexp
	org-table-local-parameters org-table-named-field-locations
	org-table-current-line-types org-table-current-begin-line
	org-table-current-begin-pos org-table-dlines
	org-table-hlines org-table-last-alignment
	org-table-last-column-widths org-table-last-alignment
	org-table-last-column-widths tbeg)

    (save-excursion
      (set-buffer (marker-buffer tbl-marker))
      (goto-char (marker-position tbl-marker))
      (org-table-get-specials)
      (setq form (org-table-formula-substitute-names form))
      (if (and (string-match org-table-range-regexp form)
	       (> (length (match-string 0 form)) 1))
	  (save-match-data
	    (org-table-get-range (match-string 0 form) (point) 1))
	form))))

(provide 'org-R)
