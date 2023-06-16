{
  description = "Merlin Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        inherit (pkgs.ocamlPackages) buildDunePackage;
      in
      rec {
        packages = rec {
          default = merlin;
          merlin = buildDunePackage {
            pname = "merlin";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            buildInputs = with pkgs.ocamlPackages; [ menhirSdk menhir ];
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              findlib
              csexp
              yojson
              menhirLib
            ];
            checkInputs = with pkgs.ocamlPackages; [ ppxlib pkgs.jq ];
            doCheck = true;
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = with pkgs.ocamlPackages; [ ocaml-lsp ];
        };
      });
}
