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
	compile-single compile-source compile-slint1 compile-slint2 \
	autoloads \
	install clean cleanauto cleanall cleanelc clean-install

# do not clean here, done in toplevel make
all compile compile-dirty::	 autoloads
	$(MAKE) compile-$(_COMPILE_)

compile-dirall:
	@$(ELCDIR)
compile-single: $(LISPC)
compile-source: cleanelc
	@$(foreach elc,$(LISPC),$(MAKE) $(elc) && $(RM) $(elc);)
compile-slint1:	compile-dirall
	@$(foreach elc,$(LISPC),$(RM) $(elc); $(MAKE) $(elc);)
compile-slint2:
	$(MAKE) compile-source compile-slint1

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
