run:
	ghci -Wall -isrc MonadBrain

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

profile: clean
	cabal configure --enable-executable-profiling --ghc-option=-rtsopts
	cabal build
	dist/build/ltg/ltg nop stdin +RTS -p -hc -sltg.summary < tests/loop.ltg
	hp2ps ltg.hp
