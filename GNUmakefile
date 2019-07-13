PROLOG ?= swipl
BIN = logipp
TEST = tests/nat_le.json

$(BIN): ppterm.pl
	$(PROLOG) -o $@ -c $<

tests: $(BIN) $(TEST)
	./$(BIN) $(TEST)

.PHONY: clean.
	-$(RM) ppttltx
