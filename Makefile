HC = ghc


targets = \
	monad
	# state \


all : $(targets)


$(targets): %: %.hs
	$(HC) -o $@ $<


clean:
	-rm $(targets)
