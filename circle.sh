#!/bin/bash
ls -lah
nix-env -i cabal2nix
make nix-init
nix-shell --command "make test"
