# Below here are special targets for maintenance only

html: doc/org.html

html_manual: doc/org.texi
	rm -rf doc/manual
	mkdir doc/manual
	$(TEXI2HTML) -o doc/manual doc/org.texi
	UTILITIES/mansplit.pl doc/manual/*.html

html_guide: doc/orgguide.texi
	rm -rf doc/guide
	mkdir doc/guide
	$(TEXI2HTML) -o doc/guide doc/orgguide.texi
	UTILITIES/guidesplit.pl doc/guide/*.html

info:	doc/org

pdf:	doc/org.pdf doc/orgguide.pdf

card:	doc/orgcard.pdf doc/orgcard_letter.pdf doc/orgcard.txt

testrelease:
	git checkout -b testrelease origin/maint
	git merge -s recursive -X theirs master
	UTILITIES/set-version.pl testing
	git commit -a -m "Release testing"
	make distfile TAG=testversion
	make cleanrel
	rm -rf org-testversion*
	git reset --hard
	git checkout master
	git branch -D testrelease

# The following target makes a full release for the stuff that is
# currently on master.  Do it like this:
#
#   make release TAG=7.01

release:
	git checkout maint
	git merge -s recursive -X theirs master
	UTILITIES/set-version.pl $(TAG)
	git commit -a -m "Release $(TAG)"
	make relup TAG=$(TAG)
	make cleanrel
	rm -rf org-$(TAG)
	rm -f org-$(TAG)*.zip
	rm -f org-$(TAG)*.tar.gz
	make pushreleasetag TAG=$(TAG)
	git push -f origin maint
	git checkout master
	git merge -s ours maint
	UTILITIES/set-version.pl -a $(TAG)
	git commit -a -m "Update website to show $(TAG) as current release"
	git push

# The following target makes a release, but from the stuff that is on
# maint, not from the stuff that is on master.  The idea is that it pushes
# out a minor fix into a minor update, while development on master
# already went full steam ahead.  To make a micro-relesse, cherry-pick
# the necessary changes into maint, then run (with proper version number)
# This is just like release, but we skip  the step which merges master
# into maint.
#
#   make fixrelease TAG=7.01.02

fixrelease:
	git checkout maint
	git merge -s recursive -X theirs master
	UTILITIES/set-version.pl $(TAG)
	git commit -a -m "Release $(TAG)"
	make relup TAG=$(TAG)
	make cleanrel
	rm -rf org-$(TAG)
	rm org-$(TAG)*.zip
	rm org-$(TAG)*.tar.gz
	make pushreleasetag TAG=$(TAG)
	git push -f origin maint
	git checkout master
	git merge -s ours maint
	UTILITIES/set-version.pl -o $(TAG)
	git commit -a -m "Update website to show $(TAG) as current release"
	git push

# ~$ make relup only makes sense from orgmode.org server
# Don't call it from your computer!
relup:
	${MAKE} makerelease
	${MAKE} sync_release
	${MAKE} sync_manual

makerelease:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	${MAKE} distfile
	${MAKE} doc
	UTILITIES/gplmanual.pl
	${MAKE} html_manual
	${MAKE} html_guide
	rm -rf RELEASEDIR
	$(MKDIR) RELEASEDIR
	cp org-$(TAG).zip org-$(TAG).tar.gz RELEASEDIR
	cp doc/org.pdf doc/orgcard.pdf doc/org.texi doc/org.html RELEASEDIR
	cp doc/org_dual_license.texi RELEASEDIR
	cp doc/orgguide.pdf doc/orgcard.txt RELEASEDIR
	cp RELEASEDIR/org-$(TAG).zip    RELEASEDIR/org.zip
	cp RELEASEDIR/org-$(TAG).tar.gz RELEASEDIR/org.tar.gz

# ~$ make sync_release only makes sense from orgmode.org server
# Don't call it from your computer!
sync_release:
	rsync -avuz RELEASEDIR/ /var/www/orgmode.org/

# ~$ make sync_manual only makes sense from orgmode.org server
# Don't call it from your computer!
sync_manual:
	rsync -avuz --delete doc/manual/ /var/www/orgmode.org/manual/
	rsync -avuz --delete doc/guide/ /var/www/orgmode.org/guide/

distfile:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch doc/org.texi doc/orgcard.tex # force update
	${MAKE} cleancontrib
	${MAKE} info
	${MAKE} doc
	${MAKE} lisp/org-install.el
	rm -rf org-$(TAG) org-$(TAG).zip
	$(MKDIR) org-$(TAG)
	$(MKDIR) org-$(TAG)/doc
	$(MKDIR) org-$(TAG)/lisp
	cp -r $(LISPFILES) org-$(TAG)/lisp
	cp -r $(DOCFILES) $(CARDFILES) org-$(TAG)/doc
	cp -r $(DISTFILES_extra) org-$(TAG)/
	cp -r README_DIST org-$(TAG)/README
	zip -r org-$(TAG).zip org-$(TAG)
	tar zcvf org-$(TAG).tar.gz org-$(TAG)

pkg:
	@if [ "X$(PKG_TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	touch doc/org.texi doc/orgcard.tex # force update
	${MAKE} info
	${MAKE} doc
	rm -rf org-$(PKG_TAG) org-$(PKG_TAG).tar
	$(MKDIR) org-$(PKG_TAG)
	cp -r $(PKG_FILES) org-$(PKG_TAG)
	echo "(define-package \"org\" \"$(PKG_TAG)\" \"$(PKG_DOC)\" $(PKG_REQ))" > org-$(PKG_TAG)/org-pkg.el
	tar cf org-$(PKG_TAG).tar org-$(PKG_TAG) --remove-files

cleanall:
	${MAKE} clean
	rm -f lisp/org-install.el

clean:
	${MAKE} cleanelc
	${MAKE} cleandoc
	${MAKE} cleanrel
	rm -f *~ */*~ */*/*~

cleancontrib:
	find contrib -name \*~ -exec rm {} \;

cleanelc:
	rm -f $(ELCFILES)
cleandoc:
	-(cd doc && rm -f org.pdf org org.html orgcard.pdf orgguide.pdf)
	-(cd doc && rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs)
	-(cd doc && rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps)
	-(cd doc && rm -f orgcard_letter.tex orgcard_letter.pdf)
	-(cd doc && rm -rf manual)

cleanrel:
	rm -rf RELEASEDIR
	rm -rf org-7.*
	rm -rf org-7*zip org-7*tar.gz

.el.elc:
	$(ELC) $<


push:
	git push orgmode@orgmode.org:org-mode.git master

pushtag:
	git tag -m "Adding tag" -a $(TAG)
	git push orgmode@orgmode.org:org-mode.git $(TAG)

pushreleasetag:
	git tag -m "Adding release tag" -a release_$(TAG)
	git push orgmode@orgmode.org:org-mode.git release_$(TAG)

