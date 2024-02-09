.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

LISPV 	:= org-version.el
LISPI 	:= org-loaddefs.el
LISPA 	:= $(LISPV) $(LISPI)
LISPB 	:= $(LISPA:%el=%elc) org-install.elc
LISPF 	:= $(filter-out $(LISPA),$(sort $(wildcard *.el)))
LISPC 	:= $(filter-out $(LISPB) $(LISPN:%el=%elc),$(LISPF:%el=%elc))
LISPN 	:= $(filter-out $(LISPB) $(LISPN:%el=%eln),$(LISPF:%el=%eln))
_ORGCM_ := dirall single native source slint1 slint2
-include local.mk

.PHONY:	all compile compile-dirty \
	$(_ORGCM_) $(_ORGCM_:%=compile-%) \
	autoloads \
	install clean cleanauto cleanall cleanelc clean-install

# do not clean here, done in toplevel make
all compile compile-dirty::	 | autoloads
ifeq ($(filter-out $(_ORGCM_),$(ORGCM)),)
	$(MAKE) compile-$(ORGCM)
else
	$(error ORGCM has illegal value $(ORGCM) (valid: $(_ORGCM_)))
endif

compile-dirall:	dirall
compile-single: $(LISPC) | single
compile-native: $(LISPN) | native
compile-source:	| source dirall
compile-slint1:	| dirall slint1
compile-slint2:	| source dirall slint1

# internal
dirall:
	@$(info ==================== $@ ====================)
	@$(ELCDIR)
single:
	@$(info ==================== $@ ====================)
native:
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

%.eln: %.el
	@$(info Native compiling single $(abspath $<)...)
	-@$(ELN) $<

autoloads:	cleanauto $(LISPI) $(LISPV)

$(LISPV):	$(LISPF)
	@echo "org-version: $(ORGVERSION) ($(GITVERSION))"
	@$(RM) $(@)
	@$(MAKE_ORG_VERSION)

$(LISPI):	$(LISPV) $(LISPF)
	@echo "org-loaddefs: $(ORGVERSION) ($(GITVERSION))"
	@$(RM) $(@)
	@$(MAKE_ORG_INSTALL)

install:	 compile $(LISPF)
	if [ ! -d $(DESTDIR)$(lispdir) ] ; then \
	  $(MKDIR) $(DESTDIR)$(lispdir) ; \
	fi ;
	$(CP) $(LISPC) $(LISPF) $(LISPA) $(DESTDIR)$(lispdir)

cleanauto clean cleanall::
	$(RM) $(LISPA) $(LISPB)
clean cleanall cleanelc::
	$(RM) *.elc

clean-install:
	if [ -d $(DESTDIR)$(lispdir) ] ; then \
	  $(RM) $(DESTDIR)$(lispdir)/org*.el* $(DESTDIR)$(lispdir)/ob*.el* $(DESTDIR)$(lispdir)/ol*.el* $(DESTDIR)$(lispdir)/ox*.el* ; \
	fi ;
