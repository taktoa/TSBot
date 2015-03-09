.PHONY: all bench build clean configure haddock hpc install repl run test
shell = '$$SHELL'
all: install configure build haddock test hpc bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean: nix-clean
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

nix-clean:
	if test -e default.nix; then rm default.nix; fi
	if test -e shell.nix; then rm shell.nix; fi

nix-init: nix-clean
	[ `cabal2nix --version` = "2.0" ] && cabal2nix --shell . > shell.nix;
	[ `cabal2nix --version` = "2.0" ] && cabal2nix . > default.nix;

nix-shell: nix-init
	nix-shell --command 'make install && IN_NIX=nix $(shell)'
	make clean

repl:
	cabal repl lib:TSBot

run:
	cabal run --jobs TSBot

test:
	cabal test --jobs
	cabal check
