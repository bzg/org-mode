##----------------------------------------------------------------------
##  NEVER EDIT THIS FILE, PUT ANY ADAPTATIONS INTO local.mk
##----------------------------------------------------------------------
##  CHECK AND ADAPT THE FOLLOWING DEFINITIONS
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS	= emacs

# Where local software is found
prefix	= /usr/share

# Where local lisp files go.
lispdir= $(prefix)/emacs/site-lisp/org

# Where local data files go.
datadir = $(prefix)/emacs/etc/org

# Where info files go.
infodir = $(prefix)/info

# where to create temporary files for the testsuite
TMPDIR ?= /tmp
testdir = $(TMPDIR)/tmp-orgtest

##----------------------------------------------------------------------
## YOU MAY NEED TO ADAPT THESE DEFINITIONS
##----------------------------------------------------------------------

# Using emacs in batch mode.
BATCH	= $(EMACS) -batch -Q \
	  -L . \
	  --eval '(defconst org-release "$(ORGVERSION)-Make")' \

# How to run tests
BTEST_EXTRA = # placeholder
BTEST	= $(EMACS) -batch \
	  $(BTEST_EXTRA) \
	  -L lisp/ \
	  --eval '(defconst org-release "$(ORGVERSION)-Test")' \
	  -l testing/org-test.el \
	  -eval "(setq org-confirm-babel-evaluate nil)" \
	  -f org-test-run-batch-tests

# How to byte-compile the whole source directory
ELCDIR	= $(BATCH) \
	  --eval '(batch-byte-recompile-directory 0)'

# How to byte-compile a single source file
ELC	= $(BATCH) -f batch-byte-compile

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf --batch --clean

# How to make a pdf file from a tex file
PDFTEX = pdftex

# How to create directories
MKDIR	= mkdir -p

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections

# How to find files
FIND	= find

# How to remove files
RM	= rm -f

# How to remove files recursively
RMR	= rm -fr

# How to stream edit a file
SED	= sed

# How to copy the lisp files and elc files to their destination.
# CP	= cp -p	# try this if there is no install
CP	= install -p

# How to obtain administrative privileges
# SUDO	= 	# leave blank if you don't need this
SUDO	= sudo

# Name of the program to install info files
# INSTALL_INFO = ginstall-info # Debian: avoid harmless warning message
INSTALL_INFO = install-info
