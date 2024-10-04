CHEZ=chez

doc: bwog.ss
	$(CHEZ) --script bwog.ss docs

.PHONY: doc test
