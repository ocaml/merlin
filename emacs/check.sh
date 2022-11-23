#!/bin/sh -e

# Adapted from https://github.com/purcell/package-lint/blob/master/run-tests.sh
EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="package-lint company iedit auto-complete"

ELS_TO_CHECK=*.el
# To reduce the amount of false positives we only package-lint files
# that are actual installable packages.
PKGS_TO_CHECK="merlin.el merlin-ac.el merlin-company.el merlin-iedit.el"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (add-to-list 'package-archives \
    '(\"melpa\" . \"https://melpa.org/packages/\") t) \
  (package-initialize) \
  (package-refresh-contents) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_BYTECOMP_WARN_IGNORE:+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         -L . \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         ${ELS_TO_CHECK}

# package-lint failures are ignored if EMACS_PACKAGE_LINT_IGNORE is nonempty.
# Right now this is always the case because it is difficult to make
# package-lint shut up completely.
EMACS_PACKAGE_LINT_IGNORE=1

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -L . \
         --eval "(require 'package-lint)" \
         -f package-lint-batch-and-exit \
         ${PKGS_TO_CHECK} || [ -n "${EMACS_PACKAGE_LINT_IGNORE:+x}" ]
