#----------------------------------------------------------------------
# This file is used for maintenance of org on the server.
#----------------------------------------------------------------------
.PHONY:	helpserver \
	release rel-dirty rel-up cleanrel \
	elpa elpa-dirty elpa-up \
	doc-up \
	upload-release upload-elpa upload-doc upload \
	tagwarn version

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info release             - clean up, create the distribution archives)
	$(info elpa                - clean up, create the ELPA archive)
	$(info upload-release      - clean up, populate the server with arhives)
	$(info upload-elpa         - clean up, populate the server with ELPA)
	$(info upload-doc          - clean up, populate the server with docs)
	$(info upload              - clean up, populate the server with everything)

helpserver::
	@echo ""

#----------------------------------------------------------------------

SERVROOT ?= /var/www/orgmode.org
SERVERMK ?= true # or just any value at all, really

#----------------------------------------------------------------------

ORGCOMM  = README lisp/
ORGFULL  = $(ORGCOMM) Makefile request-assign-future.txt \
		      mk/default.mk mk/targets.mk mk/version.mk \
		      mk/org-fixup.el \
		      etc/ contrib/ doc/
ORGFULL := $(ORGFULL:%/=%/*)
ORGELPA  = $(ORGCOMM) doc/dir doc/org doc/orgcard.pdf \
		      etc/styles/ org-pkg.el
ORGELPA := $(ORGELPA:%/=%/*)
ORGELPAPLUS := $(ORGELPA:org-pkg%=orgplus-pkg%)

release:	cleanall info pdf card rel-dirty tagwarn
rel-dirty rel-up:	ORGDIR=org-$(GITVERSION:release_%=%)
rel-dirty:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-dist version autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGRZIP)
	ln -s . $(ORGDIR)
	tar -zcf $(ORGDIR).tar.gz $(foreach dist, $(ORGFULL), $(ORGDIR)/$(dist))
	zip -r9  $(ORGDIR).zip    $(foreach dist, $(ORGFULL), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR)
rel-up:	info pdf card rel-dirty
	$(CP) $(ORGDIR).tar.gz $(ORGDIR).zip $(SERVROOT)/

PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "" # marmalade chokes on explicit "nil"

elpa:		cleanall info card elpa-dirty
elpa-dirty elpa-up:	ORGDIR=org-$(PKG_TAG)
elpa-dirty:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpa version autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGZIP)
	ln -s . $(ORGDIR)
	echo "(define-package \"org\" \"$(PKG_TAG)\" \"$(PKG_DOC)\" $(PKG_REQ))" \
	  > org-pkg.el
	tar --exclude=Makefile --transform='s:\(lisp\|doc\)/::' -cf $(ORGDIR).tar \
	  $(foreach dist, $(ORGELPA), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR) org-pkg.el
elpa-up:	info card elpa-dirty
	$(CP) $(ORGDIR).tar $(SERVROOT)/pkg/daily/


elpaplus:		cleanall info card elpaplus-dirty
elpaplus-dirty elpaplus-up:	ORG_ADD_CONTRIB=org-*
elpaplus-dirty elpaplus-up:	ORGDIR=orgplus-$(PKG_TAG)
elpaplus-dirty:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpaplus version autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGZIP)
	ln -s . $(ORGDIR)
	echo "(define-package \"orgplus\" \"$(PKG_TAG)\" \"$(PKG_DOC)\" $(PKG_REQ))" \
	  > orgplus-pkg.el
	tar --exclude=Makefile --transform='s:\(lisp\|doc\)/::' -cf $(ORGDIR).tar \
	  $(foreach dist, $(ORGELPAPLUS), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR) orgplus-pkg.el
	@$(MAKE) cleanlisp
elpaplus-up:	info card elpaplus-dirty
	$(CP) $(ORGDIR).tar $(SERVROOT)/pkg/daily/

tagwarn:
	$(if $(filter-out $(ORGVERSION), $(GITVERSION)), \
	  $(info  ======================================================) \
	  $(info  =                                                    =) \
	  $(info  = A release should only be made from a revision that =) \
	  $(info  = has an annotated tag!                              =) \
	  $(info  =                                                    =) \
	  $(info  ======================================================))

version:
	@echo ORGVERSION=$(ORGVERSION) GITVERSION=$(GITVERSION)$(ORGDIST)
	@echo "ORGVERSION	?= $(ORGVERSION)"  > mk/version.mk
	@echo "GITVERSION	?= $(GITVERSION)" >> mk/version.mk

cleanall clean:	cleanrel
cleanrel:
	-$(RM) org-$(PKG_TAG)* org-$(DISTVERSION)* org-*.zip org-*.tar* mk/version.mk

doc-up:	info pdf card html
	$(MAKE) -C doc manual guide
	$(CP) doc/org.html $(SERVROOT)
	$(CP) doc/manual/* $(SERVROOT)/manual
	$(CP) doc/guide/*  $(SERVROOT)/guide

upload:			cleanall elpa-up rel-up doc-up elpaplus-up
upload-elpa:		cleanall elpa-up
upload-elpaplus:	cleanall elpaplus-up
upload-release:		cleanall rel-up
upload-doc:		cleanall doc-up
