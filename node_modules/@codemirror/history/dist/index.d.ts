import * as _codemirror_state from '@codemirror/state';
import { Facet, Transaction, StateEffect, Extension, StateField, StateCommand, EditorState } from '@codemirror/state';
import { KeyBinding } from '@codemirror/view';

/**
Transaction annotation that will prevent that transaction from
being combined with other transactions in the undo history. Given
`"before"`, it'll prevent merging with previous transactions. With
`"after"`, subsequent transactions won't be combined with this
one. With `"full"`, the transaction is isolated on both sides.
*/
declare const isolateHistory: _codemirror_state.AnnotationType<"after" | "before" | "full">;
/**
This facet provides a way to register functions that, given a
transaction, provide a set of effects that the history should
store when inverting the transaction. This can be used to
integrate some kinds of effects in the history, so that they can
be undone (and redone again).
*/
declare const invertedEffects: Facet<(tr: Transaction) => readonly StateEffect<any>[], readonly ((tr: Transaction) => readonly StateEffect<any>[])[]>;
interface HistoryConfig {
    /**
    The minimum depth (amount of events) to store. Defaults to 100.
    */
    minDepth?: number;
    /**
    The maximum time (in milliseconds) that adjacent events can be
    apart and still be grouped together. Defaults to 500.
    */
    newGroupDelay?: number;
}
/**
Create a history extension with the given configuration.
*/
declare function history(config?: HistoryConfig): Extension;
/**
The state field used to store the history data. Should probably
only be used when you want to
[serialize](https://codemirror.net/6/docs/ref/#state.EditorState.toJSON) or
[deserialize](https://codemirror.net/6/docs/ref/#state.EditorState^fromJSON) state objects in a way
that preserves history.
*/
declare const historyField: StateField<unknown>;
/**
Undo a single group of history events. Returns false if no group
was available.
*/
declare const undo: StateCommand;
/**
Redo a group of history events. Returns false if no group was
available.
*/
declare const redo: StateCommand;
/**
Undo a selection change.
*/
declare const undoSelection: StateCommand;
/**
Redo a selection change.
*/
declare const redoSelection: StateCommand;
/**
The amount of undoable change events available in a given state.
*/
declare const undoDepth: (state: EditorState) => number;
/**
The amount of redoable change events available in a given state.
*/
declare const redoDepth: (state: EditorState) => number;
/**
Default key bindings for the undo history.

- Mod-z: [`undo`](https://codemirror.net/6/docs/ref/#history.undo).
- Mod-y (Mod-Shift-z on macOS): [`redo`](https://codemirror.net/6/docs/ref/#history.redo).
- Mod-u: [`undoSelection`](https://codemirror.net/6/docs/ref/#history.undoSelection).
- Alt-u (Mod-Shift-u on macOS): [`redoSelection`](https://codemirror.net/6/docs/ref/#history.redoSelection).
*/
declare const historyKeymap: readonly KeyBinding[];

export { history, historyField, historyKeymap, invertedEffects, isolateHistory, redo, redoDepth, redoSelection, undo, undoDepth, undoSelection };
