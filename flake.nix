{
  description = "Merlin Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocamlPackages.overrideScope' (oself: osuper: {
            menhirLib = osuper.menhirLib.overrideAttrs (_: rec {
              version = "20201216";
              src = pkgs.fetchFromGitLab {
                domain = "gitlab.inria.fr";
                owner = "fpottier";
                repo = "menhir";
                rev = version;
                sha256 = "sha256:04lnd3qxwma4l5jcv79f9bbl5849l6bhg2rzrrsvdzabdplfrxcb";
              };
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
