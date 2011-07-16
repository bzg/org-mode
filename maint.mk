##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
DOCFILES    = doc/org.texi doc/org.pdf doc/org doc/dir \
              doc/pdflayout.sty doc/.nosearch \
              doc/orgguide.texi doc/orgguide.pdf
CARDFILES   = doc/orgcard.tex doc/orgcard.pdf doc/orgcard_letter.pdf
TEXIFILES   = doc/org.texi
INFOFILES   = doc/org

# Package Manager (ELPA)
PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "nil"

PKG_FILES = $(LISPFILES0)		\
            doc/dir doc/org		\
            doc/pdflayout.sty		\
            doc/org.pdf			\
            doc/orgguide.pdf		\
            doc/orgcard.tex		\
            doc/orgcard.pdf		\
            doc/orgcard_letter.pdf

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

