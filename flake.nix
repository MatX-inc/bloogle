{
  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachSystem [ "aarch64-darwin" "aarch64-linux" "x86_64-linux" ] (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskell.packages.ghc967;
      in
      {
        checks.selftest = pkgs.runCommand "bloogle-test" { } ''
          cd ${self}
          ${self.packages.${system}.default}/bin/bloogle test
          touch $out
        '';

        devShells.default = hs.shellFor {
          packages = _: [ self.packages.${system}.default ];
          nativeBuildInputs = [
            hs.haskell-language-server
            pkgs.cabal-install
          ];
        };

        packages.default = hs.callCabal2nix "bloogle" ./. { };
      }
    );
}
