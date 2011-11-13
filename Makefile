# Makefile - for the org-mode distribution
#
# Maintainer: Carsten Dominik <dominik@science.uva.nl>
# Version: VERSIONTAG
#

# Describe valid make targets for org-mode.
.PHONY:	targets help
targets help:
	$(info )
	$(info make               - show this help)
	$(info make all           - cleanly compile Org ELisp files and documentation)
	$(info )
	$(info Installation)
	$(info ============)
	$(info make install       - install Org, both ELisp and Info files)
	$(info make install-lisp  - install Org, only ELisp files)
	$(info make install-info  - install Org, only Info file)
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info make docs          - make all documentation)
	$(info make info          - make Info documentation)
	$(info make html          - make HTML documentation)
	$(info make pdf           - make pdf documentation)
	$(info make card          - make refcards documentation)
	$(info )
	$(info make clean         - clean Org ELisp and documentation files)
	$(info make compile       - cleanly compile Org ELisp files)
	$(info make compile-dirty - compile Org ELisp without cleaning)
	$(info )
	$(info make clean-install - remove installed Org ELisp and documentation files)
	@echo ""

 include default.mk
-include local.mk	# optional local customization
 include maint.mk
 include targets.mk
 include maint-targets.mk
