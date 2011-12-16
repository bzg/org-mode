etcdir = $(lispdir)/etc
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
	$(foreach dir, $?, if [ ! -d $(etcdir)/$(dir) ]; then $(MKDIR) $(etcdir)/$(dir); else true; fi ; $(CP) $(dir)/* $(etcdir)/$(dir); )

clean:

cleanall:

clean-install:
	if [ ! -d $(etcdir) ]; then $(MKDIR) $(etcdir); else true; fi ;
	$(RMR) $(etcdir)
