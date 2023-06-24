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
          merlin-lib = buildDunePackage {
            pname = "merlin-lib";
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
            doCheck = true;
          };
          dot-merlin-reader = buildDunePackage {
            pname = "dot-merlin-reader";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            buildInputs = [ merlin-lib ];
            doCheck = true;
          };
          merlin = buildDunePackage {
            pname = "merlin";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            buildInputs = with pkgs.ocamlPackages; [
              merlin-lib
              dot-merlin-reader
            ];
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              dot-merlin-reader
              merlin-lib
              findlib
            ];
            nativeBuildInputs = [
              dot-merlin-reader
            ];
            checkInputs = with pkgs.ocamlPackages; [
              ppxlib
              pkgs.jq
            ];
            # merlin tests rely on wrapper shell script and env vars.
            # TODO: make them work
            doCheck = false;
            meta = with pkgs; {
              mainProgram = "ocamlmerlin";
            };
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = with pkgs.ocamlPackages; [ ocaml-lsp ];
        };
      });
}
