CHEZ=chez

test: tests.ss
	$(CHEZ) --program tests.ss

.PHONY: test
