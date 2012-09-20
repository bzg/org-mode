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
_ORGCM_ = dirall single source slint1 slint2
-include local.mk

.PHONY:	all compile compile-dirty \
	$(_ORGCM_) $(_ORGCM_:%=compile-%) \
	autoloads addcontrib \
	install clean cleanauto cleanall cleanelc clean-install

# do not clean here, done in toplevel make
all compile compile-dirty::	 autoloads
ifeq ($(filter-out $(_ORGCM_),$(ORGCM)),)
	$(MAKE) compile-$(ORGCM)
else
	$(error ORGCM has illegal value $(ORGCM) (valid: $(_ORGCM_)))
endif

compile-dirall:	dirall
compile-single: single $(LISPC)
compile-source:	source dirall
compile-slint1:	dirall slint1
compile-slint2:	source dirall slint1

# internal
dirall:
	@$(info ==================== $@ ====================)
	@$(ELCDIR)
single:
	@$(info ==================== $@ ====================)
source: cleanelc
	@$(info ==================== $@ ====================)
	@$(foreach elc,$(LISPC),$(MAKE) $(elc) && $(RM) $(elc);)
slint1:
	@$(info ==================== $@ ====================)
	@$(foreach elc,$(LISPC),$(RM) $(elc); $(MAKE) $(elc);)

%.elc:	%.el
	@$(info Compiling single $(abspath $<)...)
	-@$(ELC) $<

addcontrib:
ifneq ($(ORG_ADD_CONTRIB),)
	$(CP) $(wildcard \
		$(addsuffix .el, \
		$(addprefix ../contrib/lisp/, \
		$(basename \
		$(notdir $(ORG_ADD_CONTRIB)))))) .
endif

autoloads:	cleanauto addcontrib $(LISPI) $(LISPV)

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
