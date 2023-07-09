CHEZ=chez

all: bwog.ss
	$(CHEZ) --program bwog.ss docs

test: tests.ss
	$(CHEZ) --program tests.ss

.PHONY: all test
