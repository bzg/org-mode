.PHONY:	default all up2 update compile lisp doc \
	install info html pdf card doc install-lisp install-info \
	autoloads cleanall clean cleancontrib cleanelc cleandoc cleanrel
.NOTPARALLEL: .PHONY
# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib etc
.EXPORT_ALL_VARIABLES:

LISPDIRS	= lisp #contrib
SUBDIRS		= doc $(LISPDIRS) #contrib

compile:	lisp
	$(MAKE) -C $< $@

all:	$(SUBDIRS)
	$(foreach dir, $?, $(MAKE) -C $(dir) $@;)

up2:	update
	sudo ${MAKE} install

update:
	git pull
	${MAKE} clean
	${MAKE} all

install: install-lisp install-info

docs:	info html pdf card

info html pdf card:
	$(MAKE) -C doc $@

install-lisp:
	$(MAKE) -C lisp install

install-info:
	$(MAKE) -C doc install

autoloads: lisp maint.mk
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
