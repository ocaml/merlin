# Release procedure

## Project organization

Merlin maintains multiple branches for multiple versions of the compiler. The current active support branches are:

- `master` with support for OCaml 5.2
- `501` with support for OCaml 5.1
- `414` with support for OCaml 4.14

Development happens on the `master` branch and bug fixes that should be
backported to other supported branches should be added to the appropriate
[project](https://github.com/ocaml/merlin/projects?type=classic).

Major changes were specific to OCaml 5.2 so the subsequent releases have a major
version number at `5` (like `v5.0-502`) while support releases will stay with
the version number `4` (like `v4.15-414` or `v4.15-501`).

## Before the release

- A PR is opened with the updated changelog for each released branch. The
  changelog should contain an appropriate timestamp for the release.
- Additionally, for each non-master released version, the PR also contains the
  required backports.

## Release

We use `dune-release` to release Merlin.

For each released branch:
- Merge the corresponding PR.
- Run `dune-release tag v4.15-414`.
    The tag should always formed in the same way:
    ```
    "v%s-%s" merlin_version ocaml_version
    ```
    For example: `v4.14-414`, `v5.0-502` etc.
- Run `dune-release` and follow the instructions, but say `No` when asked if a PR should be created on opam's repository.
- For multiple releases it is best to group all the releases in a single opam PR. Additionally it is often not necessary to release the `dot-merlin-package`.
- Cherry-pick the commits from every branch created by `dune-release`, make the necessary changes (like removing the `dot-merlin-reader` packages) and open the PR on opam.
