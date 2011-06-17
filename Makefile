run:
	ghci -isrc Brain

clean:
	cabal clean

configure: clean
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/qltg/index.html
