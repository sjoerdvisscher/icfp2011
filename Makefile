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

build: configure
	cabal build

package: build
	cp dist/build/ltg/ltg package/run
	mkdir package/src
	cd package; tar czf ../magic-missiles.tar.gz install run README src
	rm package/run
	rmdir package/src

