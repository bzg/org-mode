# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc

default: $(ELCFILES) $(ELCBFILES)

all:	$(ELCFILES) $(ELCBFILES) $(INFOFILES)

up2:	update
	sudo ${MAKE} install

update:
	git pull
	${MAKE} clean
	${MAKE} all

compile: $(ELCFILES0) $(ELCBFILES)

install: install-lisp

doc: doc/org.html doc/org.pdf doc/orgcard.pdf doc/orgcard_letter.pdf doc/orgguide.pdf doc/orgcard.txt

p:
	${MAKE} pdf && open doc/org.pdf

g:
	${MAKE} pdf && open doc/orgguide.pdf

install-lisp: $(LISPFILES) $(ELCFILES)
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $(LISPFILES)  $(lispdir)
	$(CP) $(ELCFILES)   $(lispdir)

install-info: $(INFOFILES)
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(infodir)
	$(INSTALL_INFO) --infodir=$(infodir) $(INFOFILES)

autoloads: lisp/org-install.el

lisp/org-install.el: $(LISPFILES0) Makefile
	$(BATCH) --eval "(require 'autoload)" \
		--eval '(find-file "org-install.el")'  \
		--eval '(erase-buffer)' \
		--eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPFILES0))))' \
		--eval '(insert "\n(provide (quote org-install))\n")' \
		--eval '(save-buffer)'
	mv org-install.el lisp

doc/org: doc/org.texi
	(cd doc && $(MAKEINFO) --no-split org.texi -o org)

doc/org.pdf: doc/org.texi
	(cd doc && $(TEXI2PDF) org.texi)

doc/orgguide.pdf: doc/orgguide.texi
	(cd doc && $(TEXI2PDF) orgguide.texi)

doc/org.html: doc/org.texi
	(cd doc && $(TEXI2HTML) --no-split -o org.html org.texi)
	UTILITIES/manfull.pl doc/org.html

doc/orgcard.pdf: doc/orgcard.tex
	(cd doc && pdftex orgcard.tex)

doc/orgcard.txt: doc/orgcard.tex
	(cd doc && perl ../UTILITIES/orgcard2txt.pl orgcard.tex > orgcard.txt)

doc/orgcard_letter.tex: doc/orgcard.tex
	perl -pe 's/\\pdflayout=\(0l\)/\\pdflayout=(1l)/' \
                   doc/orgcard.tex > doc/orgcard_letter.tex

doc/orgcard_letter.pdf: doc/orgcard_letter.tex
	(cd doc && pdftex orgcard_letter.tex)

