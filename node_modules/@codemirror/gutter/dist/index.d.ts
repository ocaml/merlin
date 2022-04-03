import { EditorView, BlockInfo, ViewUpdate } from '@codemirror/view';
import { RangeValue, RangeSet } from '@codemirror/rangeset';
import { Facet, Extension, EditorState } from '@codemirror/state';

/**
A gutter marker represents a bit of information attached to a line
in a specific gutter. Your own custom markers have to extend this
class.
*/
declare abstract class GutterMarker extends RangeValue {
    /**
    Compare this marker to another marker of the same type.
    */
    eq(other: GutterMarker): boolean;
    /**
    Render the DOM node for this marker, if any.
    */
    toDOM?(_view: EditorView): Node;
    /**
    This property can be used to add CSS classes to the gutter
    element that contains this marker.
    */
    elementClass: string;
    /**
    Called if the marker has a `toDOM` method and its representation
    was removed from a gutter.
    */
    destroy(dom: Node): void;
}
/**
Facet used to add a class to all gutter elements for a given line.
Markers given to this facet should _only_ define an
[`elementclass`](https://codemirror.net/6/docs/ref/#gutter.GutterMarker.elementClass), not a
[`toDOM`](https://codemirror.net/6/docs/ref/#gutter.GutterMarker.toDOM) (or the marker will appear
in all gutters for the line).
*/
declare const gutterLineClass: Facet<RangeSet<GutterMarker>, readonly RangeSet<GutterMarker>[]>;
declare type Handlers = {
    [event: string]: (view: EditorView, line: BlockInfo, event: Event) => boolean;
};
interface GutterConfig {
    /**
    An extra CSS class to be added to the wrapper (`cm-gutter`)
    element.
    */
    class?: string;
    /**
    Controls whether empty gutter elements should be rendered.
    Defaults to false.
    */
    renderEmptyElements?: boolean;
    /**
    Retrieve a set of markers to use in this gutter.
    */
    markers?: (view: EditorView) => (RangeSet<GutterMarker> | readonly RangeSet<GutterMarker>[]);
    /**
    Can be used to optionally add a single marker to every line.
    */
    lineMarker?: (view: EditorView, line: BlockInfo, otherMarkers: readonly GutterMarker[]) => GutterMarker | null;
    /**
    If line markers depend on additional state, and should be
    updated when that changes, pass a predicate here that checks
    whether a given view update might change the line markers.
    */
    lineMarkerChange?: null | ((update: ViewUpdate) => boolean);
    /**
    Add a hidden spacer element that gives the gutter its base
    width.
    */
    initialSpacer?: null | ((view: EditorView) => GutterMarker);
    /**
    Update the spacer element when the view is updated.
    */
    updateSpacer?: null | ((spacer: GutterMarker, update: ViewUpdate) => GutterMarker);
    /**
    Supply event handlers for DOM events on this gutter.
    */
    domEventHandlers?: Handlers;
}
/**
Define an editor gutter. The order in which the gutters appear is
determined by their extension priority.
*/
declare function gutter(config: GutterConfig): Extension;
/**
The gutter-drawing plugin is automatically enabled when you add a
gutter, but you can use this function to explicitly configure it.

Unless `fixed` is explicitly set to `false`, the gutters are
fixed, meaning they don't scroll along with the content
horizontally (except on Internet Explorer, which doesn't support
CSS [`position:
sticky`](https://developer.mozilla.org/en-US/docs/Web/CSS/position#sticky)).
*/
declare function gutters(config?: {
    fixed?: boolean;
}): Extension;
interface LineNumberConfig {
    /**
    How to display line numbers. Defaults to simply converting them
    to string.
    */
    formatNumber?: (lineNo: number, state: EditorState) => string;
    /**
    Supply event handlers for DOM events on this gutter.
    */
    domEventHandlers?: Handlers;
}
/**
Facet used to provide markers to the line number gutter.
*/
declare const lineNumberMarkers: Facet<RangeSet<GutterMarker>, readonly RangeSet<GutterMarker>[]>;
/**
Create a line number gutter extension.
*/
declare function lineNumbers(config?: LineNumberConfig): Extension;
/**
Returns an extension that adds a `cm-activeLineGutter` class to
all gutter elements on the [active
line](https://codemirror.net/6/docs/ref/#view.highlightActiveLine).
*/
declare function highlightActiveLineGutter(): Extension;

export { GutterMarker, gutter, gutterLineClass, gutters, highlightActiveLineGutter, lineNumberMarkers, lineNumbers };
