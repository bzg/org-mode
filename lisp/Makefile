.NOTPARALLEL:	# always run this make serially
.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

LISPV = org-version.el
LISPI = org-install.el
LISPA = $(LISPV) $(LISPI)
LISPF = $(filter-out $(LISPA),$(sort $(wildcard *.el)))
LISPC = $(filter-out $(LISPN:%el=%elc),$(LISPF:%el=%elc))

.PHONY:	all compile compile-dirty \
	dirall source slint1 slint3 slint4 \
	compile-single compile-source \
	compile-slint1 compile-slint2 compile-slint3 compile-slint4 \
	autoloads \
	install clean cleanauto cleanall cleanelc clean-install

# do not clean here, done in toplevel make
all compile compile-dirty::	 autoloads
	$(MAKE) compile-$(_COMPILE_)

compile-dirall:	dirall
compile-single: $(LISPC)
compile-source:	source dirall
compile-slint1:	dirall slint1
compile-slint2:	source dirall slint1
compile-slint3:	slint3 dirall
compile-slint4:	slint4 dirall

# internal
dirall:
	@$(info ==================== $@ ====================)
	@$(ELCDIR)
source: cleanelc
	@$(info ==================== $@ ====================)
	@$(foreach elc,$(LISPC),$(MAKE) $(elc) && $(RM) $(elc);)
slint1:
	@$(info ==================== $@ ====================)
	@$(foreach elc,$(LISPC),$(RM) $(elc); $(MAKE) $(elc);)
slint3:
	@$(info ==================== $@ ====================)
	-@$(ELINTL) $(foreach el,$(LISPF),$(ELINTF))
slint4:
	@$(info ==================== $@ ====================)
	-@$(foreach el,$(LISPF),$(ELINTL) $(ELINTF);)

%.elc:	%.el
	@$(info Compiling single $(abspath $<)...)
	-@$(ELC) $<

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
clean cleanall cleanelc::
	$(RM) *.elc

clean-install:
	if [ -d $(DESTDIR)$(lispdir) ] ; then \
	  $(RM) $(DESTDIR)$(lispdir)/org*.el* $(DESTDIR)$(lispdir)/ob*.el* ; \
	fi ;
