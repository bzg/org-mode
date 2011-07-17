ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

GITVERSION = $(shell git describe --abbrev=6 HEAD)
ifneq ($(shell git status -uno --porcelain), '')
  GITVERSION := $(GITVERSION).dirty
endif

LISPO = org-install.el
LISPF = $(subst $(LISPO),,$(wildcard *.el))
LISPC = $(LISPF:%el=%elc)

include dependencies.mk

.PHONY:		autoloads compile install clean cleanall

all compile:	$(LISPC)

autoloads:	$(LISPO)

org-install.el:	$(LISPC)
	$(BATCH) \
	 --eval "(require 'autoload)" \
	 --eval '(find-file "$(LISPO)")' \
	 --eval '(erase-buffer)' \
	 --eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPF))))' \
	 --eval '(insert "\n(provide (quote org-install))\n")' \
	 --eval '(save-buffer)'

install:	$(LISPF) compile autoloads
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $(LISPC) $(LISPF) $(LISPO) $(lispdir)
	perl -i -pe 's/^(\(defconst org-git-version ).*/\1 "$(GITVERSION)"/;' $(lispdir)/org.el
	$(MAKE) $(lispdir)/org.elc

clean:
	$(RM) $(LISPC)

cleanall:
	$(RM) $(LISPC) $(LISPO)

$(LISPC):	dependencies.mk

.SUFFIXES:	# we don't need default suffix rules
.SUFFIXES:	.el .elc

.el.elc:
	$(info in subdir lisp)
	$(ELC) $<
