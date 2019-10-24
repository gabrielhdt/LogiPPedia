BINDIR    ?= /usr/local/bin
SITE_DIR  ?= $(shell guile -c '(display (%site-dir))')
_testfiles = $(sort $(wildcard ../tests/*.json))
_sttfatest = $(sort $(wildcard ../tests/*+sttfa.json))

STTFASYMS = uri2syms/sttfa-latex.scm

.PHONY: install
install:
	@echo "Installing Guile modules to $(SITE_DIR)"
	install -D -m 644 logipp/latex-pp.scm $(SITE_DIR)/logipp/latex-pp.scm
	install -D -m 644 logipp/extras.scm $(SITE_DIR)/logipp/extras.scm
	@echo "Installing script to $(BINDIR)"
	install -D -m 755 logipp-latex $(BINDIR)/logipp-latex

.PHONY: uninstall
uninstall:
	-$(RM) -r $(SITE_DIR)/logipp
	-$(RM) $(BINDIR)/logipp-latex

.PHONY: tests
tests:
	@for file in $(_testfiles); do \
		GUILE_LOAD_PATH=. ./logipp-latex < $$file; \
		echo ''; \
	done
	@for file in $(_sttfatest); do \
		GUILE_LOAD_PATH=. ./logipp-latex --symbols $(STTFASYMS) < $$file; \
		echo ''; \
	done
