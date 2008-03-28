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
LISPF      = org.el \
	     org-compat.el org-macs.el \
	     org-table.el org-exp.el org-remember.el org-agenda.el\
	     org-publish.el org-mouse.el org-export-latex.el \
	     org-bbdb.el org-gnus.el org-info.el org-irc.el \
             org-mac-message.el org-mhe.el org-rmail.el org-vm.el org-wl.el
LISPFILES0 = $(LISPF:%=lisp/%)
LISPFILES  = $(LISPFILES0) lisp/org-install.el
ELCFILES0  = $(LISPFILES0:.el=.elc)
ELCFILES   = $(LISPFILES:.el=.elc)
DOCFILES   = org.texi org.pdf org
CARDFILES  = doc/orgcard.tex doc/orgcard.pdf doc/orgcard_letter.pdf
TEXIFILES  = doc/org.texi
INFOFILES  = org
HG_RELEASES = ../org-mode-all-releases-hg/


.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

DISTFILES=  README ${LISPFILES} ${DOCFILES} ${CARDFILES} \
	Makefile dir ChangeLog request-assign-future.txt \
	CONTRIB
DISTFILES_xemacs=  xemacs/noutline.el xemacs/ps-print-invisible.el xemacs/README

all:	$(ELCFILES)
compile: $(ELCFILES0)

install: install-lisp

doc: doc/org.html doc/org.pdf doc/orgcard.pdf doc/orgcard_letter.pdf

p:
	make pdf && open doc/org.pdf

c:
	make card && gv doc/orgcard.ps

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

autoloads: lisp/org-install.el

lisp/org-install.el: $(LISPFILES0) Makefile
	$(BATCH) --eval "(require 'autoload)" \
		--eval '(find-file "org-install.el")'  \
		--eval '(erase-buffer)' \
		--eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPFILES0))))' \
		--eval '(insert "\n(provide (quote org-install))\n")' \
		--eval '(save-buffer)'
	mv org-install.el lisp

xemacs/noutline.elc: xemacs/noutline.el

doc/org: doc/org.texi
	(cd doc; $(MAKEINFO) --no-split org.texi -o org)

doc/org.pdf: doc/org.texi
	(cd doc; $(TEXI2PDF) org.texi)

doc/org.html: doc/org.texi
	(cd doc; $(TEXI2HTML) --no-split -o org.html org.texi)

doc/orgcard.dvi: doc/orgcard.tex
	(cd doc; tex orgcard.tex)

doc/orgcard.pdf: doc/orgcard.dvi
	dvips -q -f -t landscape doc/orgcard.dvi | gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=doc/orgcard.pdf -c .setpdfwrite -

doc/orgcard.ps: doc/orgcard.dvi
	dvips -t landscape -o doc/orgcard.ps doc/orgcard.dvi 

doc/orgcard_letter.dvi: doc/orgcard.tex
	perl -pe 's/letterpaper=0/letterpaper=1/' doc/orgcard.tex > doc/orgcard_letter.tex
	(cd doc; tex orgcard_letter.tex)

doc/orgcard_letter.pdf: doc/orgcard_letter.dvi
	dvips -q -f -t landscape doc/orgcard_letter.dvi | gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=doc/orgcard_letter.pdf -c .setpdfwrite -

doc/orgcard_letter.ps: doc/orgcard_letter.dvi
	dvips -t landscape -o doc/orgcard_letter.ps doc/orgcard_letter.dvi 

# Below here are special targets for maintenance only

webfiles:
	(cd ORGWEBPAGE; emacs -batch -l ~/.emacs index.org -f org-publish-current-project)

web:
	make webfiles
	(cd ORGWEBPAGE/tmp; lftp -f ../../../org-mode-proprietary/ftp_upload_website)

html: doc/org.html

html_split: doc/org.texi
	rm -rf doc/manual
	mkdir doc/manual
	$(TEXI2HTML) -o doc/manual doc/org.texi

info:	doc/org

pdf:	doc/org.pdf

card:	doc/orgcard.pdf doc/orgcard.ps doc/orgcard_letter.pdf doc/orgcard_letter.ps

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

cleanelc:
	rm -f $(ELCFILES)
cleandoc:
	(cd doc; rm -f org.pdf org org.html orgcard.pdf orgcard.ps)
	(cd doc; rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs)
	(cd doc; rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps)
	(cd doc; rm -f orgcard_letter.tex orgcard_letter.pdf)
	(cd doc; rm -rf manual)
clean:
	make cleanelc
	make cleandoc
	rm -f *~ */*~ */*/*~
	rm -rf RELEASEDIR

.el.elc:
	$(ELC) $<


push:
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git master

pushtag:
	git-tag -m "Adding tag" -a $(TAG)
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git $(TAG)