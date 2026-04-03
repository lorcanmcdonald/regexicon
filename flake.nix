{
  inputs = {
    # Default to the nixos-unstable branch
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";

    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs@{
      self,
      nixpkgs-stable,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs-stable = import nixpkgs-stable {
          inherit system;
        };

        buildInputs = with pkgs-stable; [

          cabal-install
          cabal2nix
          ghc
          haskell-language-server
          haskellPackages.hoogle
          ormolu
          pcre
          zlib

          typescript-language-server
          nodePackages.typescript-language-server
          nodePackages.eslint
          nodePackages.prettier
          vscode-langservers-extracted

          dockerfile-language-server-nodejs
          shfmt
          nodePackages.eslint
          vscode-langservers-extracted
        ];

        packageName = "regexicon";
      in

      with pkgs-stable;
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
