##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS=emacs

# Where local software is found
prefix=/usr/local

# Where local lisp files go.
lispdir   = $(prefix)/share/emacs/site-lisp

# Where info files go.
infodir = $(prefix)/share/info

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Using emacs in batch mode.

BATCH=$(EMACS) -batch -q -no-site-file -eval                             			\
  "(setq load-path (cons (expand-file-name \"./lisp/\") (cons \"$(lispdir)\" load-path)))"

# Specify the byte-compiler for compiling org-mode files
ELC= $(BATCH) -f batch-byte-compile

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf

# How to create directories
MKDIR = mkdir -p

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections
TEXI2HTMLNOPSLIT = makeinfo --html --no-split --number-sections

# How to copy the lisp files and elc files to their distination.
CP = cp -p

# Name of the program to install info files
INSTALL_INFO=install-info

