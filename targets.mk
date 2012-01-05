.NOTPARALLEL: .PHONY
# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc
.EXPORT_ALL_VARIABLES:

LISPDIRS      = lisp
SUBDIRS       = doc etc $(LISPDIRS)
INSTSUB       = $(SUBDIRS:%=install-%)

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
  GITVERSION := $(GITVERSION).dirty
endif

.PHONY:	default all up2 update compile lisp doc etc \
	test install info html pdf card docs $(INSTSUB) \
	autoloads cleanall clean cleancontrib cleanrel clean-install \
	cleanelc cleanlisp cleandoc cleandocs

all \
compile::	lisp
	$(MAKE) -C $< clean

compile \
compile-dirty::	lisp
	$(MAKE) -C $< $@

all \
clean-install::	$(SUBDIRS)
	$(foreach dir, $?, $(MAKE) -C $(dir) $@;)

test::	all

test \
test-dirty::
	$(BTEST)

up2:	update
	sudo ${MAKE} install

update:
	git remote update
	git pull
	${MAKE} all

install:	$(INSTSUB)

install-info:	install-doc

docs:	info html pdf card

info html pdf card:
	$(MAKE) -C doc $@

$(INSTSUB):
	$(MAKE) -C $(@:install-%=%) install

autoloads: lisp
	$(MAKE) -C $< $@

cleanall: $(SUBDIRS)
	$(foreach dir, $?, $(MAKE) -C $(dir) $@;)
	-$(FIND) . -name \*~ -exec $(RM) {} \;

clean:	cleanrel
	$(MAKE) -C lisp clean
	$(MAKE) -C doc clean
	-$(FIND) . -name \*~ -exec $(RM) {} \;

cleancontrib:
	-$(FIND) contrib -name \*~ -exec $(RM) {} \;

cleanrel:
	$(RMR) RELEASEDIR
	$(RMR) org-7.*
	$(RMR) org-7*zip org-7*tar.gz

cleanelc cleanlisp:
	$(MAKE) -C lisp clean

cleandoc cleandocs:
	$(MAKE) -C doc clean
