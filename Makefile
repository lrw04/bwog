CHEZ=chez

doc: bwog.ss
	$(CHEZ) --program bwog.ss docs

test: tests.ss
	$(CHEZ) --program tests.ss

.PHONY: doc test
