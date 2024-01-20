HC = ghc


targets = \
	state \
	array \
	monad \
	monadfail


all: $(targets)


$(targets): %: %.hs
	$(HC) -o $@ $<


clean:
	-rm $(targets)
