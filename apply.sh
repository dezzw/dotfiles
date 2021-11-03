#!/bin/sh
pushd ~/.dotfiles/
darwin-rebuild switch --flake .#
popd
