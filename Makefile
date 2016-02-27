GHC_OPTS=-Wall

SRC=Main.hs Economy.hs Arithmetics.hs Month.hs
TARGET=economy

$(TARGET): $(SRC)
	ghc $(GHC_OPTS) -o $@ $^

.PHONY: test
test: $(SRC)
	runhaskell $(GHC_OPTS) Arithmetics.hs
	runhaskell $(GHC_OPTS) Month.hs

.PHONY: deps
deps:
	cabal install optparse-applicative aeson boxes

.PHONY: ghci
ghci: $(SRC)
	ghci $^

.PHONY: clean
clean:
	rm *.o *.hi $(TARGET)
