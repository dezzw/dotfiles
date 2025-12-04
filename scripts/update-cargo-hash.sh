#!/usr/bin/env bash
# Automatically update cargoHash for Rust packages in overlays/rust-packages.nix
# This script extracts the correct hash from Nix error messages and updates the file
# Usage: ./scripts/update-cargo-hash.sh [package-name]
#        If no package is specified, updates all packages

set -euo pipefail

PACKAGE_NAME="${1:-}"
OVERLAY_FILE="overlays/rust-packages.nix"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

cd "$REPO_ROOT"

# Function to update a single package's cargoHash
update_package_hash() {
  local pkg=$1
  
  echo -e "${YELLOW}Updating cargoHash for ${pkg}...${NC}"
  
  # Try to build through home-manager and capture the error
  # Redirect stderr to stdout and capture both
  local error_output
  error_output=$(nix build .#homeConfigurations.desmond.activationPackage 2>&1 || true)
  
  # Extract the hash from the error message
  # Pattern 1: "got:    sha256-..."
  local hash=$(echo "$error_output" | grep -oP "got:\s+sha256-[A-Za-z0-9+/=]+" | head -1 | awk '{print $2}' || echo "")
  
  # Pattern 2: Look for hash mismatch - find the "got:" hash (not the "specified:" one)
  if [[ -z "$hash" ]]; then
    # Find all hashes, filter out fakeHash pattern, take the first real one after "got:"
    hash=$(echo "$error_output" | grep -A 2 "hash mismatch" | grep -oP "sha256-[A-Za-z0-9+/=]+" | grep -v "AAAAAAAA" | head -1 || echo "")
  fi
  
  # Pattern 3: Look for any sha256 hash that's not the fake one
  if [[ -z "$hash" ]]; then
    hash=$(echo "$error_output" | grep -oP "sha256-[A-Za-z0-9+/=]{43,}" | grep -v "AAAAAAAA" | head -1 || echo "")
  fi
  
  if [[ -z "$hash" ]]; then
    echo -e "${RED}Error: Could not extract hash from build output${NC}"
    echo "Last 30 lines of build output:"
    echo "$error_output" | tail -30
    return 1
  fi
  
  echo -e "${GREEN}Found hash: ${hash}${NC}"
  
  # Update the overlay file using sed
  if [[ -f "$OVERLAY_FILE" ]]; then
    # Create a backup
    cp "$OVERLAY_FILE" "${OVERLAY_FILE}.bak"
    
    # Use sed to find the package block and update cargoHash
    # Match lines starting with the package name, then find cargoHash within that block
    local in_pkg=0
    local updated=0
    local temp_file=$(mktemp)
    
    while IFS= read -r line; do
      # Check if we're entering the package block
      if [[ "$line" =~ ^[[:space:]]*${pkg}[[:space:]]*= ]]; then
        in_pkg=1
        echo "$line"
        continue
      fi
      
      # If we're in the package block and find cargoHash, replace it
      if [[ $in_pkg -eq 1 ]] && [[ "$line" =~ cargoHash[[:space:]]*=[[:space:]]* ]]; then
        echo "    cargoHash = \"${hash}\";"
        updated=1
        continue
      fi
      
      # Check if we're leaving the package block
      if [[ $in_pkg -eq 1 ]] && [[ "$line" =~ ^[[:space:]]*\}\; ]]; then
        in_pkg=0
      fi
      
      echo "$line"
    done < "$OVERLAY_FILE" > "$temp_file"
    
    if [[ $updated -eq 1 ]]; then
      mv "$temp_file" "$OVERLAY_FILE"
      rm -f "${OVERLAY_FILE}.bak"
      echo -e "${GREEN}✓ Updated ${pkg} cargoHash to ${hash}${NC}"
    else
      rm -f "$temp_file"
      mv "${OVERLAY_FILE}.bak" "$OVERLAY_FILE"
      echo -e "${RED}Error: Could not find cargoHash for ${pkg} in overlay file${NC}"
      return 1
    fi
  else
    echo -e "${RED}Error: Overlay file not found: ${OVERLAY_FILE}${NC}"
    return 1
  fi
}

# Function to extract all hashes from error output and update all packages
update_all_hashes() {
  echo -e "${YELLOW}Updating cargoHash for all Rust packages...${NC}"
  
  # Try to build and capture all errors
  local error_output
  error_output=$(nix build .#homeConfigurations.desmond.activationPackage 2>&1 || true)
  
  # Extract all hashes from error messages
  # Pattern: look for "got:    sha256-..." lines
  local -a hashes
  mapfile -t hashes < <(echo "$error_output" | grep -oP "got:\s+sha256-[A-Za-z0-9+/=]+" | awk '{print $2}' | grep -v "AAAAAAAA" || true)
  
  # List of packages in order (should match the order of hashes in errors)
  local packages=("devicon-lookup")
  
  # If we got hashes, update packages
  if [[ ${#hashes[@]} -gt 0 ]]; then
    for i in "${!packages[@]}"; do
      local pkg="${packages[$i]}"
      if [[ $i -lt ${#hashes[@]} ]]; then
        local hash="${hashes[$i]}"
        echo -e "${YELLOW}Updating ${pkg} with hash: ${hash}${NC}"
        update_package_hash_with_hash "$pkg" "$hash"
      else
        echo -e "${YELLOW}No hash found for ${pkg}, trying individual update...${NC}"
        update_package_hash "$pkg"
      fi
      echo ""
    done
  else
    # Fallback: update each package individually
    for pkg in "${packages[@]}"; do
      update_package_hash "$pkg"
      echo ""
    done
  fi
  
  echo -e "${GREEN}All hashes updated!${NC}"
}

# Function to update a package with a known hash (used by update_all_hashes)
update_package_hash_with_hash() {
  local pkg=$1
  local hash=$2
  
  echo -e "${GREEN}Updating ${pkg} cargoHash to ${hash}${NC}"
  
  # Update the overlay file
  if [[ -f "$OVERLAY_FILE" ]]; then
    local in_pkg=0
    local updated=0
    local temp_file=$(mktemp)
    
    while IFS= read -r line; do
      if [[ "$line" =~ ^[[:space:]]*${pkg}[[:space:]]*= ]]; then
        in_pkg=1
        echo "$line"
        continue
      fi
      
      if [[ $in_pkg -eq 1 ]] && [[ "$line" =~ cargoHash[[:space:]]*=[[:space:]]* ]]; then
        echo "    cargoHash = \"${hash}\";"
        updated=1
        continue
      fi
      
      if [[ $in_pkg -eq 1 ]] && [[ "$line" =~ ^[[:space:]]*\}\; ]]; then
        in_pkg=0
      fi
      
      echo "$line"
    done < "$OVERLAY_FILE" > "$temp_file"
    
    if [[ $updated -eq 1 ]]; then
      mv "$temp_file" "$OVERLAY_FILE"
      echo -e "${GREEN}✓ Updated ${pkg} cargoHash${NC}"
    else
      rm -f "$temp_file"
      echo -e "${RED}Error: Could not find cargoHash for ${pkg}${NC}"
      return 1
    fi
  fi
}

# Main execution
if [[ -z "$PACKAGE_NAME" ]]; then
  update_all_hashes
else
  update_package_hash "$PACKAGE_NAME"
fi
