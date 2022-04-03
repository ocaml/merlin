## 0.19.1 (2022-02-16)

### Bug fixes

The commenting commands now never do anything in a read-only editor.

Make `toggleComment` use `toggleBlockCommentPerLine` when no line comment syntax is available.

### New features

Add a `toggleBlockCommentByLine` command, which toggles commenting/uncommenting of entire selected lines.

## 0.19.0 (2021-08-11)

### Breaking changes

Update dependencies to 0.19.0

## 0.18.1 (2021-05-10)

### Bug fixes

Toggling line comments will now add comments when a range with both commented and uncommented lines is selected (to allow toggling without changing anything).

## 0.18.0 (2021-03-03)

### Breaking changes

Update dependencies to 0.18.

## 0.17.3 (2021-02-25)

### Bug fixes

Don't change lines that have the end of a range selection directly at their start when line commenting.

## 0.17.2 (2021-02-18)

### Bug fixes

Don't insert multiple line comment markers when there are multiple selections on a line.

Move the cursor after the comment marker when inserting a line comment directly at the cursor.

## 0.17.1 (2021-01-06)

### New features

The package now also exports a CommonJS module.

## 0.17.0 (2020-12-29)

### Breaking changes

First numbered release.

