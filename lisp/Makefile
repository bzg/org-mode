.NOTPARALLEL:	# always run this make serially
.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

LISPV = org-version.el
LISPI = org-install.el
LISPA = $(LISPV) $(LISPI)
LISPF = $(filter-out $(LISPA),$(wildcard *.el))
LISPC = $(filter-out $(LISPN:%el=%elc),$(LISPF:%el=%elc))

.PHONY:	all compile compile-dirty \
		autoloads \
		install clean cleanauto cleanall clean-install

# do not clean here, done in toplevel make
all compile::	autoloads
all compile compile-dirty::	 $(LISPI) $(LISPV)
	$(ELCDIR)

autoloads:	cleanauto $(LISPI) $(LISPV)

$(LISPV):	$(LISPF)
	@echo "org-version: $(ORGVERSION) ($(GITVERSION))"
	@$(RM) $(@)
	@$(MAKE_ORG_VERSION)

$(LISPI):	$(LISPV) $(LISPF)
	@echo "org-install: $(ORGVERSION) ($(GITVERSION))"
	@$(RM) $(@)
	@$(MAKE_ORG_INSTALL)

install:	$(LISPF) compile
	if [ ! -d $(DESTDIR)$(lispdir) ] ; then \
	  $(MKDIR) $(DESTDIR)$(lispdir) ; \
	fi ;
	$(CP) $(LISPC) $(LISPF) $(LISPA) $(DESTDIR)$(lispdir)

cleanauto clean cleanall::
	$(RM) $(LISPA) $(LISPA:%el=%elc)
clean cleanall::
	$(RM) *.elc

clean-install:
	if [ -d $(DESTDIR)$(lispdir) ] ; then \
	  $(RM) $(DESTDIR)$(lispdir)/org*.el* $(DESTDIR)$(lispdir)/ob*.el* ; \
	fi ;
