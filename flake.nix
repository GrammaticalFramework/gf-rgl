{
  description = "Grammatical Framework's Resouce Grammar Library";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gf-core.url ="/Users/timpierson/Work/gf-core";

  outputs = { self, nixpkgs, flake-utils, gf-core }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "gf-rgl";
          src = ./.;
          buildInputs = [gf-core.packages.${system}.gf];
          buildPhase = ''
            mkdir -p build
            ./Setup.sh --dest=build
          '';
          installPhase = ''
            mkdir -p $out
            cp -r build/* $out
          '';
        };
      }
    );
}