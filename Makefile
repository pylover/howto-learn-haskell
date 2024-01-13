HC = ghc


targets = \
	state


all : $(targets)


$(targets): %: %.hs
	$(HC) -o $@ $<


clean:
	-rm $(targets)
