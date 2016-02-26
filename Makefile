GHC_OPTS=-Wall

SRC=economy.hs arithmetics.hs

economy: $(SRC)
	ghc $(GHC_OPTS) $^

.PHONY: test
test: $(SRC)
	runhaskell $(GHC_OPTS) arithmetics.hs

.PHONY: deps
deps:
	cabal install optparse-applicative aeson boxes

.PHONY: ghci
ghci: $(SRC)
	ghci $^
