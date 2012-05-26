.EXPORT_ALL_VARIABLES:
.NOTPARALLEL: .PHONY
# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc

LISPDIRS      = lisp
OTHERDIRS     = doc etc
SUBDIRS       = $(OTHERDIRS) $(LISPDIRS)
INSTSUB       = $(SUBDIRS:%=install-%)
ORG_MAKE_DOC ?= info html pdf

ifneq ($(wildcard .git),)
  GITVERSION ?= $(shell git describe --abbrev=6 HEAD)
  ORGVERSION ?= $(subst release_,,$(shell git describe --abbrev=0 HEAD))
  GITSTATUS  ?= $(shell git status -uno --porcelain)
else
  GITVERSION ?= N/A
  ORGVERSION ?= N/A
endif
DATE          = $(shell date +%Y-%m-%d)
ifneq ($(GITSTATUS),)
  GITVERSION := $(GITVERSION:.dirty=).dirty
endif

.PHONY:	all oldorg update update2 up0 up1 up2 compile $(SUBDIRS) \
		check test install info html pdf card doc docs $(INSTSUB) \
		autoloads cleanall clean cleancontrib cleanutils cleanrel clean-install \
		cleanelc cleandirs cleanlisp cleandoc cleandocs cleantest
		compile compile-dirty uncompiled

oldorg:	compile info	# what the old makefile did when no target was specified
uncompiled:	cleanlisp autoloads	# for developing
refcard:	card
update update2::	up0 all

.PRECIOUS:	local.mk
local.mk:
	$(info ======================================================)
	$(info = Invoke "make help" for a synopsis of make targets. =)
	$(info = Created a default local.mk template.               =)
	$(info = Setting "oldorg" as the default target.            =)
	$(info = Please adapt local.mk to your local setup!         =)
	$(info ======================================================)
	-@$(MAKE_LOCAL_MK)

all compile::
	$(foreach dir, doc lisp, $(MAKE) -C $(dir) clean;)
compile compile-dirty::
	$(MAKE) -C lisp $@
all clean-install::
	$(foreach dir, $(SUBDIRS), $(MAKE) -C $(dir) $@;)

check test::	compile
check test test-dirty::
	-$(MKDIR) $(testdir)
	TMPDIR=$(testdir) $(BTEST)
ifeq ($(TEST_NO_AUTOCLEAN),) # define this variable to leave $(testdir) around for inspection
	$(MAKE) cleantest
endif

up0 up1 up2::
	git remote update
	git pull
up1 up2::	all
	$(MAKE) test-dirty
up2 update2::
	$(SUDO) $(MAKE) install

install:	$(INSTSUB)

install-info:	install-doc

doc docs:	$(ORG_MAKE_DOC)

info html pdf card:
	$(MAKE) -C doc $@

$(INSTSUB):
	$(MAKE) -C $(@:install-%=%) install

autoloads: lisp
	$(MAKE) -C $< $@

cleandirs:
	$(foreach dir, $(SUBDIRS), $(MAKE) -C $(dir) cleanall;)

clean:	cleanrel
	$(MAKE) -C lisp clean
	$(MAKE) -C doc clean

cleanall: cleandirs cleantest cleancontrib cleanutils
	-$(FIND) . -name \*~ -o -name \*# -o -name .#\* -exec $(RM) {} \;

cleancontrib:
	-$(FIND) contrib -name \*~ -o -name \*.elc -exec $(RM) {} \;

cleanutils:
	-$(FIND) UTILITIES -name \*~ -o -name \*.elc -exec $(RM) {} \;

cleanrel:
	$(RMR) RELEASEDIR
	$(RMR) org-7.*
	$(RMR) org-7*zip org-7*tar.gz

cleanelc cleanlisp:
	$(MAKE) -C lisp clean
	-$(FIND) lisp -name \*~ -exec $(RM) {} \;

cleandoc cleandocs:
	$(MAKE) -C doc clean
	-$(FIND) doc -name \*~ -exec $(RM) {} \;

cleantest:
	$(RMR) $(testdir)
