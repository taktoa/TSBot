.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests

haddock:
	cabal haddock --hyperlink-source
# dist/doc/html/TSBot/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-tests --jobs --only-dependencies --reorder-goals

nix:
	rm default.nix shell.nix
	cabal2nix TSBot.cabal > default.nix
	cabal2nix --shell TSBot.cabal > shell.nix

repl:
	cabal repl lib:TSBot

run:
	cabal run --jobs TSBot

test:
	cabal test --jobs
	cabal check
