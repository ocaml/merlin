## 0.19.9 (2022-03-30)

### Bug fixes

Fix another crash in parsing multiple input ranges.

## 0.19.8 (2022-03-29)

### Bug fixes

Fix an issue that caused the stream parser to crash or hang in some situations on multi-range parses.

## 0.19.7 (2022-03-03)

### Bug fixes

Fix a bug that caused the wrong line content to be passed to stream parsers' indent method.

## 0.19.6 (2022-02-15)

### Bug fixes

Fix an infinite loop when a stream parser is stopped at a point beyond its parsing range.

## 0.19.5 (2022-01-28)

### Bug fixes

Fix a bug that could cause pieces of the old syntax tree to inappropriately appear in an updated tree on re-parsing.

## 0.19.4 (2022-01-17)

### New features

Stream parser objects now support a `tokenTable` property that allows you to use non-standard highlighting tags in the parser output.

## 0.19.3 (2021-12-11)

### Bug fixes

Stop parsing at a given line length to avoid freezing the editor on very long lines.

Make sure the stream parser doesn't get stuck at the end of the viewport.

## 0.19.2 (2021-09-06)

### Bug fixes

Fix a bug where tree fragments near changes were reused too eagerly.

## 0.19.1 (2021-08-11)

### Bug fixes

Fix incorrect versions for @lezer dependencies.

## 0.19.0 (2021-08-11)

### Breaking changes

Update dependencies to 0.19.0

## 0.18.2 (2021-04-13)

### Bug fixes

Fix a misformed .d.ts file in the previous release.

## 0.18.1 (2021-03-09)

### Bug fixes

Fix the node offsets in trees produced by a parse starting in the middle of a document.

Fix a bug that would cause a stream parser to get stuck when a nested stream parse ended before the end of the viewport.

## 0.18.0 (2021-03-03)

### Breaking changes

Update dependencies to 0.18.

## 0.17.1 (2021-01-06)

### New features

The package now also exports a CommonJS module.

## 0.17.0 (2020-12-29)

### Breaking changes

First numbered release.

