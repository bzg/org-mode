.PHONY:	default all up2 update compile lisp\
	install info html pdf card doc install-lisp install-info \
	autoloads cleanall clean cleancontrib cleanelc cleandoc cleanrel
.NOTPARALLEL: .PHONY
# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc
.EXPORT_ALL_VARIABLES:

lisp compile:
	$(MAKE) -C lisp compile

all:	lisp $(INFOFILES)

up2:	update
	sudo ${MAKE} install

update:
	git pull
	${MAKE} clean
	${MAKE} all

install: install-lisp

info:	doc/org

html:	doc/org.html

pdf:	doc/org.pdf doc/orgguide.pdf

card:	doc/orgcard.pdf doc/orgcard_letter.pdf doc/orgcard.txt

doc:	html pdf card

install-lisp:
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(MAKE) -C lisp install

install-info: $(INFOFILES)
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(infodir)
	$(INSTALL_INFO) --infodir=$(infodir) $(INFOFILES)

autoloads: lisp maint.mk
	$(MAKE) -C lisp autoloads

doc/org: doc/org.texi
	(cd doc && $(MAKEINFO) --no-split org.texi -o org)

doc/%.pdf: LC_ALL=C	# work around a bug in texi2dvi
doc/%.pdf: LANG=C	# work around a bug in texi2dvi
doc/%.pdf: doc/%.texi
	(cd doc && $(TEXI2PDF) $(<F))
doc/%.pdf: doc/%.tex
	(cd doc && $(TEXI2PDF) $(<F))

doc/org.html: doc/org.texi
	(cd doc && $(TEXI2HTML) --no-split -o org.html org.texi)
	UTILITIES/manfull.pl doc/org.html

doc/orgcard.txt: doc/orgcard.tex
	(cd doc && perl ../UTILITIES/orgcard2txt.pl orgcard.tex > orgcard.txt)

doc/orgcard_letter.tex: doc/orgcard.tex
	perl -pe 's/\\pdflayout=\(0l\)/\\pdflayout=(1l)/' \
                   doc/orgcard.tex > doc/orgcard_letter.tex

cleanall: clean
	$(MAKE) -C lisp cleanall

clean:	cleandoc cleanrel cleancontrib
	$(MAKE) -C lisp clean
	-$(FIND) . -name \*~ -exec $(RM) {} \;

cleancontrib:
	-$(FIND) contrib -name \*~ -exec $(RM) {} \;

cleandoc:
	-(cd doc && rm -f org.pdf org org.html orgcard.pdf orgguide.pdf)
	-(cd doc && rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs)
	-(cd doc && rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps)
	-(cd doc && rm -f orgcard_letter.tex orgcard_letter.pdf)
	-(cd doc && rm -rf manual)

cleanrel:
	rm -rf RELEASEDIR
	rm -rf org-7.*
	rm -rf org-7*zip org-7*tar.gz
