import { EditorView, ViewUpdate } from '@codemirror/view';
import { Extension, Facet, EditorState, StateEffect } from '@codemirror/state';

declare type Rect = {
    left: number;
    right: number;
    top: number;
    bottom: number;
};
/**
Return an extension that configures tooltip behavior.
*/
declare function tooltips(config?: {
    /**
    By default, tooltips use `"fixed"`
    [positioning](https://developer.mozilla.org/en-US/docs/Web/CSS/position),
    which has the advantage that tooltips don't get cut off by
    scrollable parent elements. However, CSS rules like `contain:
    layout` can break fixed positioning in child nodes, which can be
    worked about by using `"absolute"` here.
    
    On iOS, which at the time of writing still doesn't properly
    support fixed positioning, the library always uses absolute
    positioning.
    */
    position?: "fixed" | "absolute";
    /**
    The element to put the tooltips into. By default, they are put
    in the editor (`cm-editor`) element, and that is usually what
    you want. But in some layouts that can lead to positioning
    issues, and you need to use a different parent to work around
    those.
    */
    parent?: HTMLElement;
    /**
    By default, when figuring out whether there is room for a
    tooltip at a given position, the extension considers the entire
    space between 0,0 and `innerWidth`,`innerHeight` to be available
    for showing tooltips. You can provide a function here that
    returns an alternative rectangle.
    */
    tooltipSpace?: (view: EditorView) => {
        top: number;
        left: number;
        bottom: number;
        right: number;
    };
}): Extension;
/**
Describes a tooltip. Values of this type, when provided through
the [`showTooltip`](https://codemirror.net/6/docs/ref/#tooltip.showTooltip) facet, control the
individual tooltips on the editor.
*/
interface Tooltip {
    /**
    The document position at which to show the tooltip.
    */
    pos: number;
    /**
    The end of the range annotated by this tooltip, if different
    from `pos`.
    */
    end?: number;
    /**
    A constructor function that creates the tooltip's [DOM
    representation](https://codemirror.net/6/docs/ref/#tooltip.TooltipView).
    */
    create(view: EditorView): TooltipView;
    /**
    Whether the tooltip should be shown above or below the target
    position. Not guaranteed for hover tooltips since all hover
    tooltips for the same range are always positioned together.
    Defaults to false.
    */
    above?: boolean;
    /**
    Whether the `above` option should be honored when there isn't
    enough space on that side to show the tooltip inside the
    viewport. Not guaranteed for hover tooltips. Defaults to false.
    */
    strictSide?: boolean;
    /**
    When set to true, show a triangle connecting the tooltip element
    to position `pos`.
    */
    arrow?: boolean;
}
/**
Describes the way a tooltip is displayed.
*/
interface TooltipView {
    /**
    The DOM element to position over the editor.
    */
    dom: HTMLElement;
    /**
    Adjust the position of the tooltip relative to its anchor
    position. A positive `x` value will move the tooltip
    horizontally along with the text direction (so right in
    left-to-right context, left in right-to-left). A positive `y`
    will move the tooltip up when it is above its anchor, and down
    otherwise.
    */
    offset?: {
        x: number;
        y: number;
    };
    /**
    By default, a tooltip's screen position will be based on the
    text position of its `pos` property. This method can be provided
    to make the tooltip view itself responsible for finding its
    screen position.
    */
    getCoords?: (pos: number) => Rect;
    /**
    By default, tooltips are moved when they overlap with other
    tooltips. Set this to `true` to disable that behavior for this
    tooltip.
    */
    overlap?: boolean;
    /**
    Called after the tooltip is added to the DOM for the first time.
    */
    mount?(view: EditorView): void;
    /**
    Update the DOM element for a change in the view's state.
    */
    update?(update: ViewUpdate): void;
    /**
    Called when the tooltip has been (re)positioned.
    */
    positioned?(): void;
}
/**
Behavior by which an extension can provide a tooltip to be shown.
*/
declare const showTooltip: Facet<Tooltip | null, readonly (Tooltip | null)[]>;
/**
Enable a hover tooltip, which shows up when the pointer hovers
over ranges of text. The callback is called when the mouse hovers
over the document text. It should, if there is a tooltip
associated with position `pos` return the tooltip description
(either directly or in a promise). The `side` argument indicates
on which side of the position the pointer is—it will be -1 if the
pointer is before the position, 1 if after the position.

Note that all hover tooltips are hosted within a single tooltip
container element. This allows multiple tooltips over the same
range to be "merged" together without overlapping.
*/
declare function hoverTooltip(source: (view: EditorView, pos: number, side: -1 | 1) => Tooltip | null | Promise<Tooltip | null>, options?: {
    /**
    When enabled (this defaults to false), close the tooltip
    whenever the document changes.
    */
    hideOnChange?: boolean;
    /**
    Hover time after which the tooltip should appear, in
    milliseconds. Defaults to 300ms.
    */
    hoverTime?: number;
}): Extension;
/**
Get the active tooltip view for a given tooltip, if available.
*/
declare function getTooltip(view: EditorView, tooltip: Tooltip): TooltipView | null;
/**
Returns true if any hover tooltips are currently active.
*/
declare function hasHoverTooltips(state: EditorState): boolean;
/**
Transaction effect that closes all hover tooltips.
*/
declare const closeHoverTooltips: StateEffect<null>;
/**
Tell the tooltip extension to recompute the position of the active
tooltips. This can be useful when something happens (such as a
re-positioning or CSS change affecting the editor) that could
invalidate the existing tooltip positions.
*/
declare function repositionTooltips(view: EditorView): void;

export { Tooltip, TooltipView, closeHoverTooltips, getTooltip, hasHoverTooltips, hoverTooltip, repositionTooltips, showTooltip, tooltips };
