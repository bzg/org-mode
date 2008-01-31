# Makefile - for the org-mode distribution
#
# Maintainer: Carsten Dominik <dominik@science.uva.nl>
# Version: VERSIONTAG
#
# To install org-mode, edit the Makefile, type `make', then `make install'.
# To create the PDF and HTML documentation files, type `make doc'.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS=emacs

# Where local software is found
prefix=/usr/local

# Where local lisp files go.
lispdir = $(prefix)/share/emacs/site-lisp

# Where info files go.
infodir = $(prefix)/info

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Using emacs in batch mode.
BATCH=$(EMACS) -batch -q

# Specify the byte-compiler for compiling org-mode files
ELC= $(BATCH) -f batch-byte-compile

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf

# How to create directories
MKDIR = mkdir -p

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
#TEXI2HTML = ./texi2html -monolithic -number
TEXI2HTML = makeinfo --html --number-sections --no-split

# How to move the byte compiled files to their destination.  
MV = mv

# How to copy the lisp files to their distination.
CP = cp -p

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
LISPFILES  = org.el org-publish.el org-install.el
ELCFILES   = $(LISPFILES:.el=.elc)
TEXIFILES  = org.texi
INFOFILES  = org
HTMLDIR    = /home/dominik/public_html/Tools/org

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

DISTFILES=  README xemacs ${LISPFILES} org.texi org.pdf org orgcard.tex orgcard.pdf Makefile

all:	$(ELCFILES)

install: install-lisp

doc: org.html org.pdf orgcard.pdf

install-lisp: $(LISPFILES) $(ELCFILES)
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $(LISPFILES) $(lispdir)
	$(CP) $(ELCFILES)  $(lispdir)

install-info: $(INFOFILES)
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(infodir)

install-noutline: xemacs/noutline.elc
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) xemacs/noutline.el xemacs/noutline.elc $(lispdir)

org.elc:		org.el

org-publish.elc:	org-publish.el

org-install.elc:	org-install.el

xemacs/noutline.elc: xemacs/noutline.el

org:	org.texi
	$(MAKEINFO) --no-split org.texi -o org

org.pdf: org.texi
	$(TEXI2PDF) org.texi

org.html: org.texi
	$(TEXI2HTML) -o org.html org.texi

orgcard.dvi: orgcard.tex
	tex orgcard.tex

orgcard.pdf: orgcard.dvi
	dvips -q -f -t landscape orgcard.dvi | gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=orgcard.pdf -c .setpdfwrite -

orgcard.ps: orgcard.dvi
	dvips -t landscape -o orgcard.ps orgcard.dvi 

# Below here are special targets for maintenance only

info:	
	$(MAKEINFO) --no-split org.texi -o org

pdf:	
	$(TEXI2PDF) org.texi

card:	orgcard.pdf orgcard.ps

xcompile:
	xemacs -batch -q -f batch-byte-compile $(LISPFILES)

ecompile:
	emacs -batch -q -f batch-byte-compile $(LISPFILES)

distfile:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	make info
	make doc
	rm -rf org-$(TAG) org-$(TAG).zip
	$(MKDIR) org-$(TAG)
	cp -r $(DISTFILES) org-$(TAG)/
	zip -r org-$(TAG).zip org-$(TAG)
	gtar zcvf org-$(TAG).tar.gz org-$(TAG)

dist:
	make distfile TAG=$(TAG)
	cp org-$(TAG).zip org-$(TAG).tar.gz $(HTMLDIR)
	rm -f $(HTMLDIR)/org.zip $(HTMLDIR)/org.tar.gz
	(cd $(HTMLDIR); ln -s org-$(TAG).zip org.zip)
	(cd $(HTMLDIR); ln -s org-$(TAG).tar.gz org.tar.gz)
	make doc
	cp org.pdf orgcard.pdf org.texi org.html $(HTMLDIR)

minidist:
	rm -f org-$(TAG).zip
	zip org-$(TAG).zip org.el
	scp org-$(TAG).zip remote.science.uva.nl:public_html/Tools/org/

clean:
	rm -f $(ELCFILES) org.pdf org org.html orgcard.pdf orgcard.ps
	rm -f *~ 
	rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs
	rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps

.el.elc:
	$(ELC) $<

