HC = ghc


targets = \
	state \
	array \
	monad \
	monadfail \
	transformer


all: $(targets)


$(targets): %: %.hs
	$(HC) -o $@ $<


clean:
	-rm $(targets)
