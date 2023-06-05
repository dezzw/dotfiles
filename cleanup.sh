#!/bin/sh

echo "Deleting previous generations before the last 7:"
echo "running: sudo nix-env --delete-generations +7 --profile /nix/var/nix/profiles/system"
sudo nix-env --delete-generations +7 --profile /nix/var/nix/profiles/system
echo
echo "Garbage collecting older than 7d:"
echo "running: sudo nix-collect-garbage --delete-older-than 7d"
sudo nix-collect-garbage --delete-older-than 7d
echo
echo "Hard linking duplicates:"
echo "running: nix store optimise"
nix store optimise
