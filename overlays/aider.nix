final: prev: {
  aider-chat = prev.aider-chat.overrideAttrs (oldAttrs: rec {
    version = "0.77.0";
    src = prev.fetchFromGitHub {
      owner = "Aider-AI";
      repo = "aider";
      rev = "v${version}";
      sha256 = "0j3nlaxf9lzli2nvlixmpfdwxd9ndfdc4mk805di6dhkqxqn8rf0";
    };
  });
}
