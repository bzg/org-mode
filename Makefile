# Makefile - for the org-mode distribution
#
# Maintainer: Carsten Dominik <dominik@science.uva.nl>
# Version: VERSIONTAG
#

# Describe valid make targets for org-mode.
.PHONY:	targets help
targets help:
	@echo "make - compile Org ELisp files"
	@echo "make clean - clean Elisp and documentation files"
	@echo "make all - compile Org ELisp files and documentation"
	@echo ""
	@echo "make doc - make all documentation"
	@echo "make info - make Info documentation"
	@echo "make html - make HTML documentation"
	@echo "make pdf - make pdf documentation"
	@echo "make card - make refcards documentation"
	@echo ""
	@echo "make install - install Org"
	@echo "make install-lisp - install Org ELisp files"
	@echo "make install-info - install Org Info file"

 include default.mk
-include local.mk
 include maint.mk
 include targets.mk
 include maint-targets.mk
