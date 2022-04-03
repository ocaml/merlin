import { ViewUpdate, EditorView } from '@codemirror/view';
import { Extension, Facet } from '@codemirror/state';

declare type PanelConfig = {
    /**
    By default, panels will be placed inside the editor's DOM
    structure. You can use this option to override where panels with
    `top: true` are placed.
    */
    topContainer?: HTMLElement;
    /**
    Override where panels with `top: false` are placed.
    */
    bottomContainer?: HTMLElement;
};
/**
Configures the panel-managing extension.
*/
declare function panels(config?: PanelConfig): Extension;
/**
Object that describes an active panel.
*/
interface Panel {
    /**
    The element representing this panel. The library will add the
    `"cm-panel"` DOM class to this.
    */
    dom: HTMLElement;
    /**
    Optionally called after the panel has been added to the editor.
    */
    mount?(): void;
    /**
    Update the DOM for a given view update.
    */
    update?(update: ViewUpdate): void;
    /**
    Called when the panel is removed from the editor or the editor
    is destroyed.
    */
    destroy?(): void;
    /**
    Whether the panel should be at the top or bottom of the editor.
    Defaults to false.
    */
    top?: boolean;
    /**
    An optional number that is used to determine the ordering when
    there are multiple panels. Those with a lower `pos` value will
    come first. Defaults to 0.
    */
    pos?: number;
}
/**
Get the active panel created by the given constructor, if any.
This can be useful when you need access to your panels' DOM
structure.
*/
declare function getPanel(view: EditorView, panel: PanelConstructor): Panel | null;
/**
A function that initializes a panel. Used in
[`showPanel`](https://codemirror.net/6/docs/ref/#panel.showPanel).
*/
declare type PanelConstructor = (view: EditorView) => Panel;
/**
Opening a panel is done by providing a constructor function for
the panel through this facet. (The panel is closed again when its
constructor is no longer provided.) Values of `null` are ignored.
*/
declare const showPanel: Facet<PanelConstructor | null, readonly (PanelConstructor | null)[]>;

export { Panel, PanelConstructor, getPanel, panels, showPanel };
