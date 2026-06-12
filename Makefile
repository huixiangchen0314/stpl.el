EMACS ?= emacs

LOAD_PATH = -L .

TEST_FILES = stpl-tests.el

.PHONY: test clean

%.elc: %.el
	$(EMACS) -Q --batch $(LOAD_PATH) -f batch-byte-compile $<

compile: stpl.elc

test: compile
	$(EMACS) -Q --batch $(LOAD_PATH) -l stpl-tests.el \
		-f ert-run-tests-batch-and-exit
clean:
	rm -f *.elc
