## 0.19.16 (2022-02-25)

### New features

Tooltip views may now have a `getPos` property, which can be used to override the way the tooltip's position is computed.

## 0.19.15 (2022-02-18)

### Bug fixes

The default hover time has been reduced from 600 to 300 milliseconds.

### New features

`hoverTooltip` now takes a `hoverTime` option, which can be used to control how long the user must hover to see the tooltip.

## 0.19.14 (2022-02-14)

### Bug fixes

Fix an issue where in editors without padding, tooltips for positions right on the start and end of the visible content wouldn't show up.

## 0.19.13 (2022-01-19)

### New features

Tooltips now have an `overlap` option that can be used to disable the moving of overlapping tooltips.

## 0.19.12 (2022-01-11)

### New features

The new `getTooltip` function can retrieve the active tooltip view from a tooltip object.

## 0.19.11 (2022-01-11)

### Bug fixes

Make sure tooltips are repositioned on window resize.

## 0.19.10 (2021-11-23)

### Bug fixes

Make positioning of tooltips that don't fit the available space more well-defined.

### New features

The new `repositionTooltips` function can be used to tell an editor to recompute its tooltip positions.

## 0.19.9 (2021-11-22)

### New features

Add an option `tooltipSpace` to the `tooltips` function that allows configuring of the available space to show tooltips in.

## 0.19.8 (2021-11-19)

### Bug fixes

Make automatic repositioning when tooltips go out of view more robust with an intersection observer.

## 0.19.7 (2021-11-17)

### Bug fixes

Make sure window resizing doesn't make tooltips stick out of the visible viewport.

## 0.19.6 (2021-11-07)

### Bug fixes

Fix an issue where a tooltip arrow wasn't moved along by a horizontal offset.

## 0.19.5 (2021-11-06)

### Bug fixes

Fix breakage of hover tooltips inside shadow roots.

### New features

Adds a `hasHoverTooltips` predicate that tells you if an editor state has any open hover tooltips.

Adds a `closeHoverTooltips` state effect that closes all hover tooltips.

Tooltip views can now provide an `offset` property to change the tooltip position.

## 0.19.4 (2021-10-13)

## 0.19.3 (2021-10-11)

### Bug fixes

Fix an issue where a newly created (or reconfigured) editor wouldn't show its tooltips until the first change or scroll event.

### New features

Tooltips now accept an `arrow` option to show a little triangle between the tooltip and its target position.

## 0.19.2 (2021-09-01)

### Bug fixes

Fix accidental assignment to const.

## 0.19.1 (2021-08-30)

### New features

The new `tooltips` function can be used to configure tooltip behavior. For now, the only option is `position`, which allows you to choose between fixed and absolute positioning.

## 0.19.0 (2021-08-11)

### Bug fixes

Move tooltips to avoid overlapping between them, when necessary.

Make sure tooltips don't stay visible when the editor goes out of view.

### New features

Hover tooltips are now grouped together in a single DOM element when multiple such tooltips are active.

## 0.18.4 (2021-03-15)

### Breaking changes

It is no longer necessary to use the `tooltips` extension when using this package—just providing values through `showTooltip` will implicitly enable the necessary extensions.

Tooltips no longer use the `class` property on the spec object (just apply the class yourself when creating the DOM element).

### Bug fixes

Tooltips in a dark theme that doesn't explicitly style them no longer use the light theme defaults.

### New features

`showTooltip` now accepts null as input value, which doesn't produce a tooltip.

## 0.18.3 (2021-03-14)

### Bug fixes

Fix a crash in tooltip creation.

## 0.18.2 (2021-03-14)

### Bug fixes

Fix an issue where tooltips created in an out-of-view editor show up in the wrong place.

## 0.18.1 (2021-03-04)

### New features

The source callback to `hoverTooltip` may now return a promise.

## 0.18.0 (2021-03-03)

### Breaking changes

Extra CSS classes for tooltips must now be specified with the `class` option. The `style` option no longer exists.

## 0.17.2 (2021-01-14)

### Bug fixes

Fix tooltip positioning on iOS, which still handles position: fixed strangely.

## 0.17.1 (2021-01-06)

### New features

The package now also exports a CommonJS module.

## 0.17.0 (2020-12-29)

### Breaking changes

First numbered release.

