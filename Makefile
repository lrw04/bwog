CHEZ=chez

all: bwog.ss
	$(CHEZ) --program bwog.ss

test: tests.ss
	$(CHEZ) --program tests.ss

.PHONY: all test
