ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

GITVERSION	= $(shell git describe --abbrev=6 HEAD)
ORGVERSION	= $(subst release_,,$(shell git describe --abbrev=0 HEAD))
GITSTATUS	= $(shell git status -uno --porcelain)
DATE            = $(shell date +%Y-%m-%d)
ifneq ("$(GITSTATUS)", "")
  GITVERSION := $(GITVERSION).dirty
endif

.PHONY:		all info html pdf card manual guide install clean cleanall clean-install

all:		info html pdf card

info:		org

html:		org.html

pdf:		org.pdf orgguide.pdf

card:		orgcard.pdf orgcard_letter.pdf orgguide.pdf

manual guide::
	$(RMR) $@ ; $(MKDIR) $@
manual::	org.texi
	$(TEXI2HTML) -o $@ $<
	../UTILITIES/mansplit.pl $@/*
guide::		orgguide.texi
	$(TEXI2HTML) -o $@ $<
	../UTILITIES/guidesplit.pl $@/*

git-describe.texi: org.texi
	@echo "@c automatically generated, do not edit"  > git-describe.texi
	@echo "@set VERSION $(ORGVERSION) ($(GITVERSION))" >> git-describe.texi
	@echo "@set DATE $(DATE)" >> git-describe.texi


install:	org
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) org $(infodir)
	$(INSTALL_INFO) --infodir=$(infodir) org

clean:
	$(RM) org *.pdf *.html *_letter.tex \
	      *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs \
	      *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps git-describe.texi
cleanall:	clean
	$(RMR) guide manual

clean-install:
	$(RM) $(infodir)/org*
	$(INSTALL_INFO) --infodir=$(infodir) --remove org

.SUFFIXES:	# we don't need default suffix rules
.SUFFIXES:	.texi .tex .txt

%:		%.texi
	$(MAKE) git-describe.texi
	$(MAKEINFO) --no-split $< -o $@

%.pdf:		LC_ALL=C	# work around a bug in texi2dvi
%.pdf:		LANG=C		# work around a bug in texi2dvi
%.pdf:		%.texi
	$(TEXI2PDF) $<
%.pdf:		%.tex
	PDFLATEX=$(PDFTEX) $(TEXI2PDF) $<

%.html:		%.texi
	$(TEXI2HTML) --no-split -o $@ $<
	../UTILITIES/manfull.pl $@

%.txt:		%.tex
	perl ../UTILITIES/orgcard2txt.pl $< > $@

%_letter.tex:	%.tex
	$(SED) -e 's/\\pdflayout=(0l)/\\pdflayout=(1l)/' \
                   $< > $@
