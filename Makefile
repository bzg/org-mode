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
# BATCH=$(EMACS) -batch -q -eval "(add-to-list (quote load-path) \".\")"

BATCH=$(EMACS) -batch -q -eval                             \
 "(progn (add-to-list (quote load-path) \".\")             \
        (add-to-list (quote load-path) \"$(lispdir)\"))"

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

# How to move the byte compiled files to their destination.  
MV = mv

# How to copy the lisp files to their distination.
CP = cp -p

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
LISPFILES0 = org.el org-publish.el org-mouse.el org-export-latex.el \
	     org-mac-message.el org-irc.el
LISPFILES  = $(LISPFILES0) org-install.el 
ELCFILES   = $(LISPFILES:.el=.elc)
DOCFILES   = org.texi org.pdf org
CARDFILES  = orgcard.tex orgcard.pdf orgcard_letter.pdf
TEXIFILES  = org.texi
INFOFILES  = org
HG_RELEASES = ../org-mode-all-releases-hg/


.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

DISTFILES=  README ${LISPFILES} ${DOCFILES} ${CARDFILES} \
	Makefile dir ChangeLog request-assign-future.txt \
	CONTRIB
DISTFILES_xemacs=  xemacs/noutline.el xemacs/ps-print-invisible.el xemacs/README

all:	$(ELCFILES)

install: install-lisp

doc: org.html org.pdf orgcard.pdf orgcard_letter.pdf

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

org-install.el: $(LISPFILES0) Makefile
	$(BATCH) --eval "(require 'autoload)" \
		--eval '(find-file "org-install.el")'  \
		--eval '(erase-buffer)' \
		--eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPFILES))))' \
		--eval '(insert "\n(provide (quote org-install))\n")' \
		--eval '(save-buffer)'

org.elc:		org.el

org-publish.elc:	org-publish.el

org-install.elc:	org-install.el

xemacs/noutline.elc: xemacs/noutline.el

org:	org.texi
	$(MAKEINFO) --no-split org.texi -o org

org.pdf: org.texi
	$(TEXI2PDF) org.texi

org.html: org.texi
	$(TEXI2HTML) --no-split -o org.html org.texi

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

webfiles:
	(cd ORGWEBPAGE; emacs -batch -l ~/.emacs index.org -f org-publish-current-project)

web:
	make webfiles
	(cd ORGWEBPAGE/tmp; lftp -f ../../../org-mode-proprietary/ftp_upload_website)

html: org.html

html_split: org.texi
	rm -rf manual
	mkdir manual
	$(TEXI2HTML) -o manual org.texi

info:	
	$(MAKEINFO) --no-split org.texi -o org

pdf:	org.pdf

card:	orgcard.pdf orgcard.ps orgcard_letter.pdf orgcard_letter.ps

distfile:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch org.texi orgcard.tex
	make info
	make doc
	make org-install.el
	rm -rf org-$(TAG) org-$(TAG).zip
	$(MKDIR) org-$(TAG)
	$(MKDIR) org-$(TAG)/xemacs
	cp -r $(DISTFILES) org-$(TAG)/
	cp -r $(DISTFILES_xemacs) org-$(TAG)/xemacs/
	zip -r org-$(TAG).zip org-$(TAG)
	gtar zcvf org-$(TAG).tar.gz org-$(TAG)

release:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	make webfiles
	make distfile
	make doc
	make html_split
	rm -rf RELEASEDIR
	$(MKDIR) RELEASEDIR
	cp org-$(TAG).zip org-$(TAG).tar.gz RELEASEDIR
	cp org.pdf orgcard.pdf org.texi org.html RELEASEDIR
	cp ORGWEBPAGE/tmp/*.html   RELEASEDIR
#	cp ORGWEBPAGE/tmp/*.el     RELEASEDIR
	cp ORGWEBPAGE/tmp/*.txt    RELEASEDIR
	cp ORGWEBPAGE/tmp/*.css    RELEASEDIR
#	cp ORGWEBPAGE/tmp/*.jpg    RELEASEDIR
	cp RELEASEDIR/org-$(TAG).zip    RELEASEDIR/org.zip
	cp RELEASEDIR/org-$(TAG).tar.gz RELEASEDIR/org.tar.gz
	(cd $(HG_RELEASES); rm -rf $(DISTFILES) xemacs)
	cp -r org-$(TAG)/* $(HG_RELEASES)
	(cd $(HG_RELEASES); hg addremove; hg ci -m $(TAG); hg tag $(TAG))

trackrelease:
	(cd $(HG_RELEASES); rm -rf $(DISTFILES) xemacs)
	cp -r org-$(TAG)/* $(HG_RELEASES)
	(cd $(HG_RELEASES); hg addremove; hg ci -m $(TAG); hg tag $(TAG))

upload_release:
	(cd RELEASEDIR; lftp -f ../../org-mode-proprietary/ftp_upload_release)

upload_manual:
	lftp -f ../org-mode-proprietary/ftp_upload_manual

relup:
	make release
	make upload_release
	make upload_manual

clean:
	rm -f $(ELCFILES) org.pdf org org.html orgcard.pdf orgcard.ps
	rm -f *~ */*~ */*/*~
	rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs
	rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps
	rm -f orgcard_letter.tex orgcard_letter.pdf
	rm -rf manual
	rm -rf RELEASEDIR

.el.elc:
	$(ELC) $<


push:
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git master

pushtag:
	git-tag -m "Adding tag" -a $(TAG)
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git $(TAG)