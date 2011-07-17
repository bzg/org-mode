# Makefile - for the org-mode distribution
#
# Maintainer: Carsten Dominik <dominik@science.uva.nl>
# Version: VERSIONTAG
#

# Describe valid make targets for org-mode.
.PHONY:	targets help
targets help:
	$(info )
	$(info make              - show this help)
	$(info )
	$(info make clean        - clean Elisp and documentation files)
	$(info make all          - compile Org ELisp files and documentation)
	$(info )
	$(info make docs         - make all documentation)
	$(info make info         - make Info documentation)
	$(info make html         - make HTML documentation)
	$(info make pdf          - make pdf documentation)
	$(info make card         - make refcards documentation)
	$(info )
	$(info make install      - install Org, both ELisp and Info files)
	$(info make install-lisp - install Org ELisp files)
	$(info make install-info - install Org Info file)
	@echo ""

 include default.mk
-include local.mk
 include maint.mk
 include targets.mk
 include maint-targets.mk
