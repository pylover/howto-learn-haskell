HC = ghc
HCFLAGS = -XArrows


targets = \
	state \
	array \
	monad \
	monadfail \
	arrow \
	transformer


all: $(targets)


$(targets): %: %.hs
	$(HC) $(HCFLAGS) -o $@ $<


clean:
	-rm $(targets)
