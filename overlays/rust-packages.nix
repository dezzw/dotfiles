# Overlay for custom Rust packages
# Uses flake inputs tracking HEAD (latest commit) for automatic updates
# No version numbers needed! Just run: nix flake update devicon-lookup

inputs: final: prev: {
  # devicon-lookup - Emacs package for devicon lookup
  # Repository: https://github.com/coreyja/devicon-lookup
  # Tracking HEAD - updates automatically with: nix flake update devicon-lookup
  devicon-lookup = final.rustPlatform.buildRustPackage {
    pname = "devicon-lookup";
    version = "unstable";

    src = inputs.devicon-lookup;

    # cargoHash - using fakeHash so Nix will show the correct hash in error
    # Run ./scripts/update-cargo-hash.sh to auto-update
    cargoHash = "sha256-FYXInaJZhbDmE9NJKJijHfNqqaYOb5xeaZfKP4BOflE=";
  };

  # Note: emacs-lsp-proxy is now built as part of the Emacs configuration
  # See ~/.emacs.d/flake.nix for the package definition
}
