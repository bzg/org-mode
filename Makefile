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
 "(progn (add-to-list (quote load-path) (expand-file-name \"./lisp/\")) \
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
LISPF      = 	org.el			\
		org-agenda.el		\
	     	org-archive.el		\
		org-bbdb.el		\
		org-bibtex.el		\
	     	org-clock.el		\
	     	org-colview.el		\
	     	org-colview-xemacs.el	\
	     	org-compat.el		\
		org-exp.el		\
		org-export-latex.el	\
		org-faces.el		\
		org-gnus.el		\
		org-id.el		\
		org-info.el		\
		org-jsinfo.el		\
		org-irc.el		\
		org-mac-message.el	\
	     	org-macs.el		\
		org-mew.el              \
		org-mhe.el		\
		org-mouse.el		\
		org-publish.el		\
		org-remember.el		\
		org-rmail.el		\
		org-table.el		\
		org-vm.el		\
		org-wl.el

LISPFILES0 = $(LISPF:%=lisp/%)
LISPFILES  = $(LISPFILES0) lisp/org-install.el
ELCFILES0  = $(LISPFILES0:.el=.elc)
ELCFILES   = $(LISPFILES:.el=.elc)
DOCFILES   = doc/org.texi doc/org.pdf doc/org doc/dir
CARDFILES  = doc/orgcard.tex doc/orgcard.pdf doc/orgcard_letter.pdf
TEXIFILES  = doc/org.texi
INFOFILES  = doc/org


.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

# Additional distribution files
DISTFILES_extra=  Makefile ChangeLog request-assign-future.txt contrib
DISTFILES_xemacs=  xemacs/noutline.el xemacs/ps-print-invisible.el xemacs/README

default: $(ELCFILES)

all:	$(ELCFILES) $(INFOFILES)

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

updateweb:
	ssh cdominik@caprisun.dreamhost.com 'pull-worg-org.sh && publish-worg-org.sh'

html: doc/org.html

html_manual: doc/org.texi
	rm -rf doc/manual
	mkdir doc/manual
	$(TEXI2HTML) -o doc/manual doc/org.texi

info:	doc/org

pdf:	doc/org.pdf

card:	doc/orgcard.pdf doc/orgcard.ps doc/orgcard_letter.pdf doc/orgcard_letter.ps

distfile:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch doc/org.texi doc/orgcard.tex # force update
	make info
	make doc
	make lisp/org-install.el
	rm -rf org-$(TAG) org-$(TAG).zip
	$(MKDIR) org-$(TAG)
	$(MKDIR) org-$(TAG)/xemacs
	$(MKDIR) org-$(TAG)/doc
	$(MKDIR) org-$(TAG)/lisp
	cp -r $(LISPFILES) org-$(TAG)/lisp
	cp -r $(DOCFILES) $(CARDFILES) org-$(TAG)/doc
	cp -r $(DISTFILES_extra) org-$(TAG)/
	cp -r README_DIST org-$(TAG)/README
	cp -r ORGWEBPAGE/Changes.org org-$(TAG)/
	cp -r $(DISTFILES_xemacs) org-$(TAG)/xemacs/
	zip -r org-$(TAG).zip org-$(TAG)
	gtar zcvf org-$(TAG).tar.gz org-$(TAG)

release:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	make distfile
	make doc
	UTILITIES/gplmanual.pl
	make html_manual
	rm -rf RELEASEDIR
	$(MKDIR) RELEASEDIR
	cp org-$(TAG).zip org-$(TAG).tar.gz RELEASEDIR
	cp doc/org.pdf doc/orgcard.pdf doc/org.texi doc/org.html RELEASEDIR
	cp RELEASEDIR/org-$(TAG).zip    RELEASEDIR/org.zip
	cp RELEASEDIR/org-$(TAG).tar.gz RELEASEDIR/org.tar.gz

upload_release:
	(cd RELEASEDIR; lftp -f ../../org-mode-proprietary/ftp_upload_release_legito)

upload_manual:
	lftp -f ../org-mode-proprietary/ftp_upload_manual_legito

relup0:
	make release
	make upload_release

relup:
	make release
	make upload_release
	make upload_manual

db:
	grep -e '(debug)' lisp/*el

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
	rm -f lisp/org-install.el

.el.elc:
	$(ELC) $<


push:
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git master

pushtag:
	git-tag -m "Adding tag" -a $(TAG)
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git $(TAG)

pushreleasetag:
	git-tag -m "Adding release tag" -a release_$(TAG)
	git-push git+ssh://repo.or.cz/srv/git/org-mode.git release_$(TAG)

dummy:
	echo ${prefix}

# Dependencies

lisp/org.elc:           lisp/org-macs.elc lisp/org-compat.elc lisp/org-faces.elc
lisp/org-agenda.elc:       lisp/org.elc
lisp/org-archive.elc:      lisp/org.elc
lisp/org-bbdb.elc:         lisp/org.elc
lisp/org-bibtex.elc:       lisp/org.elc
lisp/org-clock.elc:        lisp/org.elc
lisp/org-colview.elc:      lisp/org.elc
lisp/org-colview-xemacs.elc:      lisp/org.elc
lisp/org-compat.elc:
lisp/org-exp.elc:          lisp/org.elc lisp/org-agenda.elc
lisp/org-export-latex.elc: lisp/org.elc lisp/org-exp.elc
lisp/org-faces.elc:                    lisp/org-macs.elc lisp/org-compat.elc
lisp/org-gnus.elc:         lisp/org.elc
lisp/org-id.elc:           lisp/org.elc
lisp/org-info.elc:         lisp/org.elc
lisp/org-jsinfo.elc:       lisp/org.elc lisp/org-exp.elc
lisp/org-irc.elc:          lisp/org.elc
lisp/org-mac-message.elc:  lisp/org.elc
lisp/org-macs.elc:
lisp/org-mew.elc:          lisp/org.elc
lisp/org-mhe.elc:          lisp/org.elc
lisp/org-mouse.elc:        lisp/org.elc
lisp/org-publish.elc:
lisp/org-remember.elc:     lisp/org.elc
lisp/org-rmail.elc:        lisp/org.elc
lisp/org-table.elc:        lisp/org.elc
lisp/org-vm.elc:           lisp/org.elc
lisp/org-wl.elc:           lisp/org.elc
