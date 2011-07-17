##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS   = emacs

# Where local software is found
prefix  = /usr/share

# Where local lisp files go.
lispdir = $(prefix)/emacs/site-lisp/org

# Where info files go.
infodir = $(prefix)/info

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Using emacs in batch mode.

BATCH   = $(EMACS) -batch -q -no-site-file -eval          \
          "(setq load-path (cons (expand-file-name \".\") \
                                 (cons \"$(lispdir)\" load-path)))"

# Specify the byte-compiler for compiling org-mode files
ELC     = $(BATCH) -f batch-byte-compile

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf

# How to make a pdf file from a tex file
PDFTEX = pdftex

# How to create directories
MKDIR   = mkdir -p

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections

# How to find files
FIND    = find

# How to remove files
RM      = rm -f

# How to remove files recursively
RMR     = rm -fr

# How to copy the lisp files and elc files to their destination.
# CP    = cp -p         # try this if there is no install
CP      = install -p

# Name of the program to install info files
INSTALL_INFO = install-info

