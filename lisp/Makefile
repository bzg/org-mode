ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
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
