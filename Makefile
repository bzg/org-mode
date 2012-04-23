# Makefile - for the org-mode distribution
#
# This file is not part of GNU Emacs

# set up environment
 include default.mk	# defaults, customizable via "local.mk"
-include local.mk	# optional local customization, use default.mk as template

# default target is "all" unless overridden in local.mk
all::

# Describe valid make targets for org-mode.
.PHONY:	targets help
targets help helpall::
	$(info )
	$(info Getting Help)
	$(info ============)
	$(info )
	$(info make help          - show brief help)
	$(info make targets       - dito)
	$(info make helpall       - show extended help)
	$(info )
	$(info Build and Check)
	$(info ===============)
	$(info make               - build Org ELisp and all documentation)
	$(info make all           - dito)
	$(info make compile       - build Org ELisp files)
	$(info make autoloads     - create org-install.el to load org in-place)
	$(info make check         - build Org ELisp files and run test suite)
helpall::
	$(info make test          - dito)
	$(info make compile-dirty - build only stale Org ELisp files)
	$(info make test-dirty    - check without building first)
	$(info )
	$(info Compatibility)
	$(info =============)
	$(info make oldorg        - what the old make did: compile autoloads info)
	$(info )
	$(info Convenience)
	$(info ===========)
	$(info make up0           - pull from upstream)
	$(info make up1           - pull from upstream, build and check)
	$(info make up2           - pull from upstream, build, check and install)
	$(info make update        - pull from upstream and build)
	$(info make update2       - pull from upstream, build and install)
	$(info make local.mk      - create new local.mk as template for adaptation)
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info make clean         - remove built Org ELisp files and documentation)
	$(info make cleanall      - remove everything that can be built and all remnants)
	$(info make cleandirs     - clean in etc/, lisp/ and doc/)
	$(info make cleancontrib  - remove remnants in contrib/)
	$(info make cleandoc      - remove built documentation)
	$(info make cleandocs     - dito)
	$(info make cleanlisp     - remove built Org ELisp files)
	$(info make cleanelc      - dito)
	$(info make cleanrel      - remove release remnants)
	$(info make cleantest     - remove check remnants)
	$(info make clean-install - remove previous Org installation)
	$(info )
	$(info Documentation)
	$(info =============)
targets help helpall::
	$(info make doc           - build all documentation)
helpall::
	$(info make docs          - dito)
targets help helpall::
	$(info make info          - build Info documentation)
helpall::
	$(info make html          - build HTML documentation)
	$(info make pdf           - build PDF documentation)
	$(info make card          - build reference cards)
	$(info make refcard       - dito)
targets help helpall::
	$(info )
	$(info Installation)
	$(info ============)
	$(info make install       - build and install Org)
helpall::
	$(info make install-etc   - build and install files in /etc)
	$(info make install-lisp  - build and install Org Elisp files)
	$(info make install-info  - build and install Info documentation)
targets help helpall::
	@echo ""

 include targets.mk	# toplevel make machinery
