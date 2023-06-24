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
            version = "dev";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              csexp
            ];
            doCheck = true;
          };
          dot-merlin-reader = buildDunePackage {
            pname = "dot-merlin-reader";
            version = "dev";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = [
              pkgs.ocamlPackages.findlib
            ];
            buildInputs = [
              merlin-lib
            ];
            doCheck = true;
          };
          merlin = buildDunePackage {
            pname = "merlin";
            version = "dev";
            src = ./.;
            duneVersion = "3";
            buildInputs = [
              merlin-lib
              dot-merlin-reader
              pkgs.ocamlPackages.menhirLib
              pkgs.ocamlPackages.menhirSdk
              pkgs.ocamlPackages.yojson
            ];
            nativeBuildInputs = [
              pkgs.ocamlPackages.menhir
              pkgs.jq
            ];
            nativeCheckInputs = [ dot-merlin-reader ];
            checkInputs = with pkgs.ocamlPackages; [
              ppxlib
            ];
            doCheck = true;
            checkPhase = ''
              runHook preCheck
              patchShebangs tests/merlin-wrapper
              dune build @check @runtest
              runHook postCheck
           '';
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
