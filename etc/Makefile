ETCDIRS = styles
-include local.mk	# optional local customization

.NOTPARALLEL:	# always run this make serially
.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif

.PHONY:		all install clean cleanall clean-install

all:

install:	$(ETCDIRS)
	$(foreach dir, $?, if [ ! -d $(DESTDIR)$(datadir)/$(dir) ]; then $(MKDIR) $(DESTDIR)$(datadir)/$(dir); else true; fi ; $(CP) $(dir)/* $(DESTDIR)$(datadir)/$(dir); )

clean:

cleanall:

clean-install:
	if [ ! -d $(DESTDIR)$(datadir) ]; then $(MKDIR) $(DESTDIR)$(datadir); else true; fi ;
	$(RMR) $(DESTDIR)$(datadir)
