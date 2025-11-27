# Overlay for custom Rust packages
# Uses flake inputs tracking HEAD (latest commit) for automatic updates
# No version numbers needed! Just run: nix flake update devicon-lookup emacs-lsp-proxy

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

  # emacs-lsp-proxy - LSP proxy for Emacs
  # Repository: https://github.com/jadestrong/lsp-proxy
  # Tracking HEAD - updates automatically with: nix flake update emacs-lsp-proxy
  emacs-lsp-proxy = final.rustPlatform.buildRustPackage {
    pname = "emacs-lsp-proxy";
    version = "unstable";

    src = inputs.emacs-lsp-proxy;

    # cargoHash - using fakeHash so Nix will show the correct hash in error
    # Run ./scripts/update-cargo-hash.sh to auto-update
    cargoHash = "sha256-ITxGdjRuMIzLhBuEEe4+2yg+1oHuIVajHNOGbFJn8qA=";
  };
}
