{
  description = "Tech Team Haskell Trial - 2025 Edition";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.git-hooks.flakeModule
        inputs.haskell-flake.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { config, pkgs, ... }:
        {
          haskellProjects.default = {
            devShell.mkShellArgs = {
              nativeBuildInputs = [ pkgs.curl ];
              shellHook = config.pre-commit.installationScript;
            };
          };

          pre-commit.settings.hooks = {
            ormolu.enable = true;
            cabal-fmt.enable = true;
            hlint.enable = true;
            nixfmt-rfc-style.enable = true;
            prettier = {
              enable = true;
              excludes = [ "flake.lock" ];
            };
          };
        };
    };
}
