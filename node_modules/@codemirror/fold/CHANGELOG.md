## 0.19.3 (2022-01-24)

### New features

`foldGutter` now takes a `domEventHandlers` option to register additional event handlers for the gutter.

## 0.19.2 (2021-11-30)

### Bug fixes

Use more efficient line iteration in fold gutter.

## 0.19.1 (2021-10-25)

### New features

The `placeholderDOM` option now gets the view and an event handler function as arguments.

## 0.19.0 (2021-08-11)

### Breaking changes

Update dependencies to 0.19.0

## 0.18.2 (2021-08-07)

### New features

The fold gutter now accepts a `markerDOM` option, which can be used to override the way gutter markers are rendered. Fix typo issue

## 0.18.1 (2021-04-30)

### Bug fixes

The fold gutter will now properly update when the editor's language config changes.

Fix an issue where the fold gutter could get out of date when changes below a given line affected the fold marker for that line.

### New features

The package now exports a `foldedRanges` function that can be used to query set of folded ranges in an editor state.

The newly exported `foldEffect` and `unfoldEffect` state effects can be used to control the fold state directly.

## 0.18.0 (2021-03-03)

### Bug fixes

Adds a screen reader announcement when code is folded or unfolded.

## 0.17.1 (2021-01-06)

### New features

The package now also exports a CommonJS module.

## 0.17.0 (2020-12-29)

### Breaking changes

First numbered release.

