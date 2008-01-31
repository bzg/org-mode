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
# BATCH=$(EMACS) -batch -q
BATCH=$(EMACS) -batch -q -eval "(add-to-list (quote load-path) \"$(lispdir)\")"

# Specify the byte-compiler for compiling org-mode files
ELC= $(BATCH) -f batch-byte-compile

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf

# How to create directories
MKDIR = mkdir -p

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections --no-split

# How to move the byte compiled files to their destination.  
MV = mv

# How to copy the lisp files to their distination.
CP = cp -p

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
LISPFILES  = org.el org-publish.el org-mouse.el org-install.el
ELCFILES   = $(LISPFILES:.el=.elc)
DOCFILES   = org.texi org.pdf org
CARDFILES  = orgcard.tex orgcard.pdf orgcard_letter.pdf
TEXIFILES  = org.texi
INFOFILES  = org
HTMLDIR    = /home/dominik/public_html/Tools/org

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

DISTFILES=  README ${LISPFILES} ${DOCFILES} ${CARDFILES} Makefile
DISTFILES_xemacs=  xemacs/noutline.el xemacs/ps-print-invisible.el xemacs/README

all:	$(ELCFILES)

install: install-lisp

doc: org.html org.pdf orgcard.pdf

p:
	make pdf && open org.pdf

c:
	make card && gv orgcard.ps

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

orgcard_letter.dvi: orgcard.tex
	perl -pe 's/letterpaper=0/letterpaper=1/' orgcard.tex > orgcard_letter.tex
	tex orgcard_letter.tex

orgcard_letter.pdf: orgcard_letter.dvi
	dvips -q -f -t landscape orgcard_letter.dvi | gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=orgcard_letter.pdf -c .setpdfwrite -

orgcard_letter.ps: orgcard_letter.dvi
	dvips -t landscape -o orgcard_letter.ps orgcard_letter.dvi 

# Below here are special targets for maintenance only

info:	
	$(MAKEINFO) --no-split org.texi -o org

pdf:	org.pdf

card:	orgcard.pdf orgcard.ps orgcard_letter.pdf orgcard_letter.ps

xcompile:
	xemacs -batch -q -f batch-byte-compile $(LISPFILES)

ecompile:
	emacs -batch -q -f batch-byte-compile $(LISPFILES)

distfile:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch org.texi orgcard.tex
	make info
	make doc
	rm -rf org-$(TAG) org-$(TAG).zip
	$(MKDIR) org-$(TAG)
	$(MKDIR) org-$(TAG)/xemacs
	cp $(DISTFILES) org-$(TAG)/
	cp $(DISTFILES_xemacs) org-$(TAG)/xemacs/
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

