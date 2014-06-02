default: testconfigure docs

install:
	cabal install

build: configure
	cabal build

configure:
	cabal configure

test: testconfigure
	cabal test

testconfigure:
	cabal configure --enable-tests

docs:
	cabal haddock --internal --hyperlink-source
