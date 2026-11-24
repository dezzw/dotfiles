# Overlay for custom Rust packages
# Uses flake inputs tracking HEAD (latest commit) for automatic updates
# No version numbers needed! Just run: nix flake update <input-name>

inputs: final: prev: {
  # devicon-lookup - Emacs package for devicon lookup
  # Repository: https://github.com/coreyja/devicon-lookup
  # Tracking HEAD - updates automatically with: nix flake update devicon-lookup
  devicon-lookup = final.rustPlatform.buildRustPackage {
    pname = "devicon-lookup";
    version = "unstable";

    src = inputs.devicon-lookup;

    cargoHash = "sha256-FYXInaJZhbDmE9NJKJijHfNqqaYOb5xeaZfKP4BOflE=";
  };
}
