{
  inputs = {
    # Default to the nixos-unstable branch
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";

    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs@{
      self,
      nixpkgs-stable,
      nixpkgs-unstable,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs-unstable = import nixpkgs-unstable {
          inherit system;
        };
        pkgs-stable = import nixpkgs-stable {
          inherit system;
        };
        pythonPackages = pkgs-unstable.python313Packages;

        buildInputs = with pkgs-stable; [

          cabal-install
          cabal2nix
          ghc
          haskell-language-server
          haskellPackages.hoogle
          ormolu

          bat
          dockerfile-language-server-nodejs
          shfmt
          nodePackages.eslint
          vscode-langservers-extracted
        ];

        packageName = "regexicon";
      in

      with pkgs-unstable;
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self rec {
          # Dependency overrides go here
        };
        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.app;
        devShells.default = mkShell {
          inherit buildInputs;
          shellHook = ''
            export LORCAN_FLAKE_NAME="${packageName}"
          '';
        };
      }
    );
}
