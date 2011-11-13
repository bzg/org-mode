.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

LISPO = org-install.el
LISPF = $(subst $(LISPO),,$(wildcard *.el))
LISPC = $(LISPF:%el=%elc)

.PHONY:		all compile compile-dirty autoloads install clean cleanall clean-install

all \
compile \
compile-dirty:
	$(BATCH) --eval '(batch-byte-recompile-directory 0)'

autoloads:	$(LISPO)

org-install.el:	$(LISPF)
	$(BATCH) \
	 --eval "(require 'autoload)" \
	 --eval '(find-file "$(LISPO)")' \
	 --eval '(erase-buffer)' \
	 --eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPF))))' \
	 --eval '(insert "\n(provide (quote org-install))\n")' \
	 --eval '(insert "\n(defconst org-release \"$(ORGVERSION)\"\n  \"The release version of org-mode.  Inserted by installing org-mode\n  or when a release is made.\")\n")' \
	 --eval '(insert "\n(defconst org-git-version \"$(GITVERSION)\"\n  \"The Git version of org-mode.  Inserted by installing org-mode\n  or when a release is made.\")\n")' \
	 --eval '(save-buffer)'

install:	$(LISPF) compile autoloads
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $(LISPC) $(LISPF) $(LISPO) $(lispdir)

clean:
	$(RM) *.elc

cleanall:
	$(RM) *.elc $(LISPO)

clean-install:
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(RM) $(lispdir)/org*.el* $(lispdir)/ob*.el*
