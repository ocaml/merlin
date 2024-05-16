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
        pkgs = nixpkgs.legacyPackages."${system}";

        # Build with OCaml 5.2
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2.overrideScope'
          (_: osuper: {
            # Override menhirLib to the pinned version
            menhirLib = osuper.menhirLib.overrideAttrs (_: {
              version = "20201216";
              src = menhir-repository;
            });

            inherit (packages) merlin-lib dot-merlin-reader merlin;
          });

        inherit (ocamlPackages) buildDunePackage;

        packages = rec {
          default = merlin;
          merlin-lib = buildDunePackage {
            pname = "merlin-lib";
            version = "dev";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = with ocamlPackages; [ csexp ];
            doCheck = true;
          };

          dot-merlin-reader = buildDunePackage {
            pname = "dot-merlin-reader";
            version = "dev";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = [ ocamlPackages.findlib ];
            buildInputs = [ merlin-lib ];
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
              ocamlPackages.menhirLib
              ocamlPackages.menhirSdk
              ocamlPackages.yojson
            ];
            nativeBuildInputs = [ ocamlPackages.menhir pkgs.jq ];
            nativeCheckInputs = [ dot-merlin-reader ];
            checkInputs = with ocamlPackages; [ ppxlib ];
            doCheck = true;
            checkPhase = ''
              runHook preCheck
              patchShebangs tests/merlin-wrapper
              dune build @check @runtest
              runHook postCheck
            '';
            meta = with pkgs; { mainProgram = "ocamlmerlin"; };
          };
        };
      in {
        inherit packages;

        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = with ocamlPackages; [ merlin ];
        };
      });
}
