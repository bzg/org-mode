##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
LISPF      = 	org.el			\
		org-agenda.el		\
		org-ascii.el		\
	     	org-attach.el		\
	     	org-archive.el		\
		org-bbdb.el		\
		org-beamer.el		\
		org-bibtex.el		\
	     	org-capture.el		\
	     	org-clock.el		\
	     	org-colview.el		\
	     	org-colview-xemacs.el	\
	     	org-compat.el		\
	     	org-pcomplete.el	\
	     	org-crypt.el		\
	     	org-ctags.el		\
	     	org-datetree.el		\
	     	org-docview.el		\
	     	org-entities.el		\
		org-exp.el		\
		org-exp-blocks.el	\
		org-docbook.el		\
		org-faces.el		\
		org-feed.el		\
		org-footnote.el		\
		org-freemind.el		\
		org-gnus.el		\
		org-habit.el		\
		org-html.el		\
		org-icalendar.el	\
		org-id.el		\
		org-indent.el		\
		org-info.el		\
		org-inlinetask.el	\
		org-jsinfo.el		\
		org-irc.el		\
		org-latex.el		\
		org-list.el		\
		org-mac-message.el	\
	     	org-macs.el		\
		org-mew.el              \
		org-mhe.el		\
		org-mks.el		\
		org-mobile.el		\
		org-mouse.el		\
		org-publish.el		\
		org-plot.el		\
		org-protocol.el		\
		org-remember.el		\
		org-rmail.el		\
		org-special-blocks.el	\
		org-src.el		\
		org-table.el		\
		org-taskjuggler.el	\
		org-timer.el		\
		org-vm.el		\
		org-w3m.el              \
		org-wl.el		\
		org-xoxo.el		\
		ob.el			\
		ob-table.el		\
		ob-lob.el		\
		ob-ref.el		\
		ob-exp.el		\
		ob-tangle.el		\
		ob-comint.el		\
		ob-eval.el		\
		ob-keys.el		\
		ob-awk.el		\
		ob-C.el			\
		ob-calc.el		\
		ob-ditaa.el		\
		ob-haskell.el		\
		ob-perl.el		\
		ob-sh.el		\
		ob-R.el			\
		ob-dot.el		\
		ob-mscgen.el		\
		ob-latex.el		\
		ob-lisp.el		\
		ob-ledger.el		\
		ob-python.el		\
		ob-sql.el		\
		ob-asymptote.el		\
		ob-emacs-lisp.el	\
		ob-matlab.el		\
		ob-ruby.el		\
		ob-sqlite.el		\
		ob-clojure.el		\
		ob-ocaml.el		\
		ob-sass.el		\
		ob-css.el		\
		ob-gnuplot.el		\
		ob-octave.el		\
		ob-screen.el		\
		ob-plantuml.el		\
		ob-org.el		\
		ob-js.el		\
		ob-scheme.el		\
		ob-lilypond.el

LISPFILES0  = $(LISPF:%=lisp/%)
LISPFILES   = $(LISPFILES0) lisp/org-install.el
ELCFILES0   = $(LISPFILES0:.el=.elc)
ELCFILES    = $(LISPFILES:.el=.elc)
DOCFILES    = doc/org.texi doc/org.pdf doc/org doc/dir \
              doc/pdflayout.sty doc/.nosearch \
              doc/orgguide.texi doc/orgguide.pdf
CARDFILES   = doc/orgcard.tex doc/orgcard.pdf doc/orgcard_letter.pdf
TEXIFILES   = doc/org.texi
INFOFILES   = doc/org

# Package Manager (ELPA)
PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "nil"

PKG_FILES = $(LISPFILES0)		\
            doc/dir doc/org		\
            doc/pdflayout.sty		\
            doc/org.pdf			\
            doc/orgguide.pdf		\
            doc/orgcard.tex		\
            doc/orgcard.pdf		\
            doc/orgcard_letter.pdf

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

