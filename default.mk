##----------------------------------------------------------------------
##  NEVER EDIT THIS FILE, PUT ANY ADAPTATIONS INTO local.mk
##----------------------------------------------------------------------
##  CHECK AND ADAPT THE FOLLOWING DEFINITIONS
##-8<-------------------------------------------------------------------

# Override default target if desired or define your own default target
# oldorg:	# have plain "make" do the same things the old Makefile did

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

# Define if you only need info documentation, the default includes html and pdf
# ORG_MAKE_DOC = info # html pdf

# Where to create temporary files for the testsuite
TMPDIR ?= /tmp
testdir = $(TMPDIR)/tmp-orgtest

# Configuration for testing
BTEST_PRE   = # add options before standard load-path
BTEST_POST  = # add options after standard load path
              # -L <path-to>/ert      # needed for Emacs23, Emacs24 has ert built in
              # -L <path-to>/htmlize  # need at least version 1.34 for source code formatting
BTEST_OB_LANGUAGES = awk C fortran maxima lilypond octave python sh # R
              # R is not activated by default because it requires ess to be installed and configured
BTEST_EXTRA = # extra packages to require for testing

##->8-------------------------------------------------------------------
## YOU MAY NEED TO ADAPT THESE DEFINITIONS
##----------------------------------------------------------------------

# How to run tests
req-ob-lang = --eval '(require '"'"'ob-$(ob-lang))'
req-extra   = --eval '(require '"'"'$(req))'
BTEST	= $(EMACS) -batch -Q \
	  $(BTEST_PRE) -L lisp/ -L testing/ $(BTEST_POST) \
	  --eval '(defconst org-release "$(ORGVERSION)-Test")' \
	  -l testing/org-test.el \
	  $(foreach ob-lang,$(BTEST_OB_LANGUAGES),$(req-ob-lang)) \
	  $(foreach req,$(BTEST_EXTRA),$(req-extra)) \
	  --eval '(setq org-confirm-babel-evaluate nil)' \
	  -f org-test-run-batch-tests

# Using emacs in batch mode.
BATCH	= $(EMACS) -batch -Q \
	  -L . \
	  --eval '(defconst org-release "$(ORGVERSION)-Make")' \

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
