## 0.19.2 (2022-04-01)

### Bug fixes

Remove quote characters from the default set of characters before which brackets are closed.

## 0.19.1 (2022-02-17)

### Bug fixes

The extension now tries to look at the syntax tree to figure out when you might be likely to be closing a string, and doesn't duplicate quotes in that case.

Don't close brackets when a composition has started, to avoid breaking dead key input.

## 0.19.0 (2021-08-11)

### Breaking changes

Update dependencies to 0.19.0

## 0.18.0 (2021-03-03)

### Breaking changes

Update dependencies to 0.18.

## 0.17.2 (2021-01-07)

### New features

A new export `insertBracket` can be used to directly invoke the bracket-closing behavior when necessary.

## 0.17.1 (2021-01-06)

### New features

The package now also exports a CommonJS module.

## 0.17.0 (2020-12-29)

### Breaking changes

First numbered release.

