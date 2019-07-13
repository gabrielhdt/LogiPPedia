PROLOG ?= swipl
BIN = logipp
TESTFILES = $(sort $(wildcard tests/*.json))

$(BIN): to_latex.pl
	$(PROLOG) -o $@ -c $<

.PHONY: clean tests.
tests: $(BIN) $(TEST)
	@for file in $(TESTFILES); do \
		./$(BIN) $$file ; \
	done

clean:
	-$(RM) $(BIN)
