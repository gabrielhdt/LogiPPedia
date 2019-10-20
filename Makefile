PROLOG ?= swipl
BIN = logipp
TESTFILES = $(sort $(wildcard tests/*.json))

$(BIN): to_latex.pl
	$(PROLOG) -o $@ -c $<

.PHONY: tests
tests: to_latex.scm $(BIN) $(TEST)
	@echo "Prolog"
	@for file in $(TESTFILES); do \
		./$(BIN) < $$file ; \
		echo "" ; \
	done

.PHONY: clean
clean:
	-$(RM) $(BIN)
