# Cursor Agent ACP adapter - Node/npm build from source
# https://github.com/blowmage/cursor-agent-acp-npm
# Tracks latest commit via flake input. Update with: nix flake update cursor-agent-acp-npm

inputs: final: prev: {
  cursor-agent-acp = prev.buildNpmPackage {
    pname = "cursor-agent-acp";
    version = "unstable";

    src = inputs.cursor-agent-acp-npm;

    # Run: nix build .#cursor-agent-acp 2>&1 | grep "got:" to get the correct hash after first build
    npmDepsHash = "sha256-prT/vy1jYBiQ+8lurk1kAeTvXeNj22ftQe3p+n9q0AM=";

    # Bin is dist/bin/cursor-agent-acp.js; build runs tsc
    dontNpmBuild = false;
  };
}
