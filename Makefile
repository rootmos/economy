GHC_OPTS=-Wall

economy: economy.hs
	ghc $(GHC_OPTS) $<

.PHONY: deps
deps:
	cabal install optparse-applicative aeson
