#----------------------------------------------------------------------
# This file is used for maintenance of org on the server.
#----------------------------------------------------------------------
.PHONY:	helpserver reltest rel-dirty warn

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info reltest               - clean up and create TAR/ZIP release archives)
helpserver::
	@echo ""

#----------------------------------------------------------------------

ORGDIR = org-$(GITVERSION)
ORGTAR = $(ORGDIR).tar.gz
ORGZIP = $(ORGDIR).zip
ORGDIST = README Makefile default.mk targets.mk request-assign-future.txt \
	  lisp/ etc/ doc/ contrib/

ORG_MAKE_DOC = info pdf card # do not make HTML documentation for release

reltest:	cleanall doc autoloads rel-dirty
rel-dirty:
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGZIP)
	ln -s . $(ORGDIR)
	tar -zcf $(ORGTAR) $(foreach dist, $(ORGDIST), $(ORGDIR)/$(dist))
	zip -r9 $(ORGZIP) $(foreach dist, $(ORGDIST), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR)
	$(if $(filter-out $(ORGVERSION), $(GITVERSION)), \
	    @$(MAKE) warn)
	@echo ORGVERSION=$(ORGVERSION) GITVERSION=$(GITVERSION)

warn:
	$(info  ======================================================)
	$(info  =                                                    =)
	$(info  = A release should only be made from a revision that =)
	$(info  = has an annotated tag!                              =)
	$(info  =                                                    =)
	$(info  ======================================================)
