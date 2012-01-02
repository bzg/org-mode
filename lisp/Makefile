ORG-INSTALL = $(BATCH) \
	 --eval "(require 'autoload)" \
	 --eval '(find-file "$(LISPO)")' \
	 --eval '(erase-buffer)' \
	 --eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPF))))' \
	 --eval '(insert "\n(provide (quote org-install))\n")' \
	 --eval '(insert "\n(defconst org-release \"$(ORGVERSION)\"\n  \"The release version of org-mode.  Inserted by installing org-mode\n  or when a release is made.\")\n")' \
	 --eval '(insert "\n(defconst org-git-version \"$(GITVERSION)\"\n  \"The Git version of org-mode.  Inserted by installing org-mode\n  or when a release is made.\")\n")' \
	 --eval '(insert "\n(defconst org-odt-data-dir \"$(datadir)\"\n  \"The location of ODT styles.\")\n")' \
	 --eval '(save-buffer)'
-include local.mk	# optional local customization

.NOTPARALLEL:	# always run this make serially
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
compile-dirty:	cleanauto
	$(ELCDIR)

autoloads:	$(LISPO)

org-install.el:	$(LISPF)
	$(ORG-INSTALL)

install:	$(LISPF) compile autoloads
	if [ ! -d $(DESTDIR)$(lispdir) ] ; then \
	  $(MKDIR) $(DESTDIR)$(lispdir) ; \
	fi ;
	$(CP) $(LISPC) $(LISPF) $(LISPO) $(DESTDIR)$(lispdir)

clean:
	$(RM) *.elc

cleanauto:
	$(RM) $(LISPO) $(LISPO:%el=%elc)

cleanall:
	$(RM) *.elc $(LISPO)

clean-install:
	if [ -d $(DESTDIR)$(lispdir) ] ; then \
	  $(RM) $(DESTDIR)$(lispdir)/org*.el* $(DESTDIR)$(lispdir)/ob*.el* ; \
	fi ;
