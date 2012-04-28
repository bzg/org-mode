.NOTPARALLEL:	# always run this make serially
.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

LISPV = org-version.el
LISPI = org-install.el
LISPA = $(LISPV) $(LISPI)
LISPF = $(filter-out $(LISPA),$(wildcard *.el))
LISPC = $(LISPF:%el=%elc)

NOSTOP = all compile compile-dirty
DOSTOP = compile-stop compile-stop-dirty

.PHONY:	$(NOSTOP) $(DOSTOP) \
		autoloads $(LISPA) \
		install clean cleanauto cleanall clean-install \

# do not clean here, done in toplevel make
$(NOSTOP) $(DOSTOP)::	$(LISPV)
	$(ELCDIR)
$(NOSTOP)::
	$(MAKE) cleanauto

autoloads:	$(LISPA)

$(LISPV):
	@echo "org-version: $(ORGVERSION) ($(GITVERSION))"
	@$(MAKE_ORG_VERSION)

$(LISPI):	$(LISPV)
	@echo "org-install: $(ORGVERSION) ($(GITVERSION))"
	$(MAKE_ORG_INSTALL)

install:	$(LISPF) compile
	if [ ! -d $(DESTDIR)$(lispdir) ] ; then \
	  $(MKDIR) $(DESTDIR)$(lispdir) ; \
	fi ;
	$(MAKE) autoloads
	$(CP) $(LISPC) $(LISPF) $(LISPA) $(DESTDIR)$(lispdir)
	$(MAKE) cleanauto

cleanauto clean cleanall::
	$(RM) $(LISPA) $(LISPA:%el=%elc)
clean cleanall::
	$(RM) *.elc

clean-install:
	if [ -d $(DESTDIR)$(lispdir) ] ; then \
	  $(RM) $(DESTDIR)$(lispdir)/org*.el* $(DESTDIR)$(lispdir)/ob*.el* ; \
	fi ;
