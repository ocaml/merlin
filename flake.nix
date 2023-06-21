{
  description = "Merlin Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.menhir-repository = {
    url = "gitlab:fpottier/menhir/20201216?host=gitlab.inria.fr";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, menhir-repository }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (_: super: {
          ocamlPackages = super.ocamlPackages.overrideScope' (_: osuper: {
            menhirLib = osuper.menhirLib.overrideAttrs (_: {
              version = "20201216";
              src = menhir-repository;
            });
          });
        });
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
