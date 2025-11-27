#!/usr/bin/env bash
# Helper script to update Rust package versions and hashes in overlays/rust-packages.nix
# Usage: ./scripts/update-rust-package.sh <package-name> [version]

set -euo pipefail

PACKAGE_NAME="${1:-}"
VERSION="${2:-}"

if [[ -z "$PACKAGE_NAME" ]]; then
  echo "Usage: $0 <package-name> [version]"
  echo ""
  echo "Available packages:"
  echo "  - devicon-lookup (owner: coreyja, repo: devicon-lookup)"
  echo "  - emacs-lsp-proxy (owner: jadestrong, repo: lsp-proxy)"
  exit 1
fi

# Package metadata
case "$PACKAGE_NAME" in
  devicon-lookup)
    OWNER="coreyja"
    REPO="devicon-lookup"
    ;;
  emacs-lsp-proxy)
    OWNER="jadestrong"
    REPO="lsp-proxy"
    ;;
  *)
    echo "Unknown package: $PACKAGE_NAME"
    exit 1
    ;;
esac

# Get latest version if not provided
if [[ -z "$VERSION" ]]; then
  echo "Fetching latest version for $PACKAGE_NAME..."
  VERSION=$(curl -s "https://api.github.com/repos/${OWNER}/${REPO}/releases/latest" | \
    jq -r '.tag_name' | sed 's/^v//')
  echo "Found version: $VERSION"
fi

echo "Updating $PACKAGE_NAME to version $VERSION..."

# Fetch source hash
echo "Fetching source hash..."
SOURCE_HASH=$(nix-prefetch-github "$OWNER" "$REPO" --rev "v${VERSION}" 2>/dev/null | \
  jq -r '.sha256' || \
  nix-prefetch-github "$OWNER" "$REPO" --rev "v${VERSION}" | \
  grep -oP 'sha256-\K[^"]+' | head -1)

if [[ -z "$SOURCE_HASH" ]]; then
  echo "Error: Could not fetch source hash"
  exit 1
fi

echo "Source hash: $SOURCE_HASH"

# Update the overlay file
OVERLAY_FILE="overlays/rust-packages.nix"

# Update version
sed -i "s/version = \".*\";  # ${PACKAGE_NAME}/version = \"${VERSION}\";  # ${PACKAGE_NAME}/" "$OVERLAY_FILE" || \
sed -i "/${PACKAGE_NAME} =/,/};/ s/version = \".*\";/version = \"${VERSION}\";/" "$OVERLAY_FILE"

# Update source hash
sed -i "s/hash = \"sha256-.*\";  # ${PACKAGE_NAME} source/hash = \"sha256-${SOURCE_HASH}\";  # ${PACKAGE_NAME} source/" "$OVERLAY_FILE" || \
sed -i "/${PACKAGE_NAME} =/,/};/ s/hash = \"sha256-[^\"]*\";/hash = \"sha256-${SOURCE_HASH}\";/" "$OVERLAY_FILE"

echo ""
echo "Updated $PACKAGE_NAME to version $VERSION"
echo "Source hash updated to: sha256-${SOURCE_HASH}"
echo ""
echo "Note: You'll need to update cargoHash manually by building the package."
echo "Nix will tell you the correct cargoHash if it's wrong."