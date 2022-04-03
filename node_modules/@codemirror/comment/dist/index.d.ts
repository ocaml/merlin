import { StateCommand } from '@codemirror/state';
import { KeyBinding } from '@codemirror/view';

/**
An object of this type can be provided as [language
data](https://codemirror.net/6/docs/ref/#state.EditorState.languageDataAt) under a `"commentTokens"`
property to configure comment syntax for a language.
*/
interface CommentTokens {
    /**
    The block comment syntax, if any. For example, for HTML
    you'd provide `{open: "<!--", close: "-->"}`.
    */
    block?: {
        open: string;
        close: string;
    };
    /**
    The line comment syntax. For example `"//"`.
    */
    line?: string;
}
/**
Comment or uncomment the current selection. Will use line comments
if available, otherwise falling back to block comments.
*/
declare const toggleComment: StateCommand;
/**
Comment or uncomment the current selection using line comments.
The line comment syntax is taken from the
[`commentTokens`](https://codemirror.net/6/docs/ref/#comment.CommentTokens) [language
data](https://codemirror.net/6/docs/ref/#state.EditorState.languageDataAt).
*/
declare const toggleLineComment: StateCommand;
/**
Comment the current selection using line comments.
*/
declare const lineComment: StateCommand;
/**
Uncomment the current selection using line comments.
*/
declare const lineUncomment: StateCommand;
/**
Comment or uncomment the current selection using block comments.
The block comment syntax is taken from the
[`commentTokens`](https://codemirror.net/6/docs/ref/#comment.CommentTokens) [language
data](https://codemirror.net/6/docs/ref/#state.EditorState.languageDataAt).
*/
declare const toggleBlockComment: StateCommand;
/**
Comment the current selection using block comments.
*/
declare const blockComment: StateCommand;
/**
Uncomment the current selection using block comments.
*/
declare const blockUncomment: StateCommand;
/**
Comment or uncomment the lines around the current selection using
block comments.
*/
declare const toggleBlockCommentByLine: StateCommand;
/**
Default key bindings for this package.

 - Ctrl-/ (Cmd-/ on macOS): [`toggleComment`](https://codemirror.net/6/docs/ref/#comment.toggleComment).
 - Shift-Alt-a: [`toggleBlockComment`](https://codemirror.net/6/docs/ref/#comment.toggleBlockComment).
*/
declare const commentKeymap: readonly KeyBinding[];

export { CommentTokens, blockComment, blockUncomment, commentKeymap, lineComment, lineUncomment, toggleBlockComment, toggleBlockCommentByLine, toggleComment, toggleLineComment };
