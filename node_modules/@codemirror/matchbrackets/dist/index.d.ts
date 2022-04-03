import { Extension, EditorState } from '@codemirror/state';

interface Config {
    /**
    Whether the bracket matching should look at the character after
    the cursor when matching (if the one before isn't a bracket).
    Defaults to true.
    */
    afterCursor?: boolean;
    /**
    The bracket characters to match, as a string of pairs. Defaults
    to `"()[]{}"`. Note that these are only used as fallback when
    there is no [matching
    information](https://lezer.codemirror.net/docs/ref/#common.NodeProp^closedBy)
    in the syntax tree.
    */
    brackets?: string;
    /**
    The maximum distance to scan for matching brackets. This is only
    relevant for brackets not encoded in the syntax tree. Defaults
    to 10 000.
    */
    maxScanDistance?: number;
}
/**
Create an extension that enables bracket matching. Whenever the
cursor is next to a bracket, that bracket and the one it matches
are highlighted. Or, when no matching bracket is found, another
highlighting style is used to indicate this.
*/
declare function bracketMatching(config?: Config): Extension;
/**
The result returned from `matchBrackets`.
*/
interface MatchResult {
    /**
    The extent of the bracket token found.
    */
    start: {
        from: number;
        to: number;
    };
    /**
    The extent of the matched token, if any was found.
    */
    end?: {
        from: number;
        to: number;
    };
    /**
    Whether the tokens match. This can be false even when `end` has
    a value, if that token doesn't match the opening token.
    */
    matched: boolean;
}
/**
Find the matching bracket for the token at `pos`, scanning
direction `dir`. Only the `brackets` and `maxScanDistance`
properties are used from `config`, if given. Returns null if no
bracket was found at `pos`, or a match result otherwise.
*/
declare function matchBrackets(state: EditorState, pos: number, dir: -1 | 1, config?: Config): MatchResult | null;

export { Config, MatchResult, bracketMatching, matchBrackets };
