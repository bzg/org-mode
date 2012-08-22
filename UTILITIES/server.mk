#----------------------------------------------------------------------
# This file is used for maintenance of org on the server.
#----------------------------------------------------------------------
.PHONY:	helpserver release rel-dirty cleanrel tagwarn

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info release               - clean up and create TAR/ZIP release archives)
	$(info elpa                  - clean up and create ELPA TAR archive)
helpserver::
	@echo ""

#----------------------------------------------------------------------

ORGCOMM  = README request-assign-future.txt lisp/ doc/
ORGFULL  = $(ORGCOMM) Makefile default.mk targets.mk etc/ contrib/
ORGFULL := $(ORGFULL:%/=%/*)
ORGELPA  = $(ORGCOMM) etc/styles/ org-pkg.el
ORGELPA := $(ORGELPA:%/=%/*)

release:	ORG_MAKE_DOC=info pdf card # do not make HTML documentation
release:	cleanall doc autoloads rel-dirty
rel-dirty:	ORGDIR=org-$(GITVERSION:release_%=%)
rel-dirty:
	-@$(RM) $(ORGDIR) $(ORGRTAR) $(ORGRZIP)
	ln -s . $(ORGDIR)
	tar -zcf $(ORGDIR).tar.gz $(foreach dist, $(ORGFULL), $(ORGDIR)/$(dist))
	zip -r9  $(ORGDIR).zip    $(foreach dist, $(ORGFULL), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR)
	$(if $(filter-out $(ORGVERSION), $(GITVERSION)), \
	    @$(MAKE) tagwarn)
	@echo ORGVERSION=$(ORGVERSION) GITVERSION=$(GITVERSION)

PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "nil"

elpa:		ORG_MAKE_DOC=info pdf card # do not make HTML documentation
elpa:		cleanall doc elpa-dirty
elpa-dirty:	ORGDIR=org-$(PKG_TAG)
elpa-dirty:	autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGZIP)
	ln -s . $(ORGDIR)
	echo "(define-package \"org\" \"$(PKG_TAG)\" \"$(PKG_DOC)\" $(PKG_REQ))" >org-pkg.el
	tar --exclude=Makefile --xform='s:\(lisp\|doc\)/::' -cf $(ORGDIR).tar \
	  $(foreach dist, $(ORGELPA), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR) org-pkg.el
	$(if $(filter-out $(ORGVERSION), $(GITVERSION)), \
	    @$(MAKE) tagwarn)
	@echo ORGVERSION=$(ORGVERSION) GITVERSION=$(GITVERSION)

tagwarn:
	$(info  ======================================================)
	$(info  =                                                    =)
	$(info  = A release should only be made from a revision that =)
	$(info  = has an annotated tag!                              =)
	$(info  =                                                    =)
	$(info  ======================================================)
	@echo ""

clean:	cleanrel
cleanrel:
	$(RM) org-7.* org-20??????*
