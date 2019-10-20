_bindir = /usr/local/bin/
_testfiles = $(sort $(wildcard ../tests/*.json))
_site_dir = "/usr/share/guile/site/2.2"

.PHONY: install
install:
	@echo "Installing to $(_site_dir)"
	mkdir -p $(_site_dir)/logipp
	install logipp/latex-pp.scm $(_site_dir)/logipp/latex-pp.scm
	install logipp/extras.scm $(_site_dir)/logipp/extras.scm
	install logipp-latex $(_bindir)/logipp-latex

uninstall:
	-$(RM) -r $(_site_dir)/logipp
	-$(RM) $(_bindir)/logipp-latex

tests:
	@for file in $(_testfiles); do \
		GUILE_LOAD_PATH=. ./logipp-latex < $$file; \
		echo ''; \
	done
