import { Tag } from '@codemirror/highlight';
import { IndentContext, Language } from '@codemirror/language';

/**
Encapsulates a single line of input. Given to stream syntax code,
which uses it to tokenize the content.
*/
declare class StringStream {
    /**
    The line.
    */
    string: string;
    private tabSize;
    /**
    The current indent unit size.
    */
    indentUnit: number;
    /**
    The current position on the line.
    */
    pos: number;
    /**
    The start position of the current token.
    */
    start: number;
    private lastColumnPos;
    private lastColumnValue;
    /**
    True if we are at the end of the line.
    */
    eol(): boolean;
    /**
    True if we are at the start of the line.
    */
    sol(): boolean;
    /**
    Get the next code unit after the current position, or undefined
    if we're at the end of the line.
    */
    peek(): string | undefined;
    /**
    Read the next code unit and advance `this.pos`.
    */
    next(): string | void;
    /**
    Match the next character against the given string, regular
    expression, or predicate. Consume and return it if it matches.
    */
    eat(match: string | RegExp | ((ch: string) => boolean)): string | void;
    /**
    Continue matching characters that match the given string,
    regular expression, or predicate function. Return true if any
    characters were consumed.
    */
    eatWhile(match: string | RegExp | ((ch: string) => boolean)): boolean;
    /**
    Consume whitespace ahead of `this.pos`. Return true if any was
    found.
    */
    eatSpace(): boolean;
    /**
    Move to the end of the line.
    */
    skipToEnd(): void;
    /**
    Move to directly before the given character, if found on the
    current line.
    */
    skipTo(ch: string): boolean | void;
    /**
    Move back `n` characters.
    */
    backUp(n: number): void;
    /**
    Get the column position at `this.pos`.
    */
    column(): number;
    /**
    Get the indentation column of the current line.
    */
    indentation(): number;
    /**
    Match the input against the given string or regular expression
    (which should start with a `^`). Return true or the regexp match
    if it matches.
    
    Unless `consume` is set to `false`, this will move `this.pos`
    past the matched text.
    
    When matching a string `caseInsensitive` can be set to true to
    make the match case-insensitive.
    */
    match(pattern: string | RegExp, consume?: boolean, caseInsensitive?: boolean): boolean | RegExpMatchArray | null;
    /**
    Get the current token.
    */
    current(): string;
}

/**
A stream parser parses or tokenizes content from start to end,
emitting tokens as it goes over it. It keeps a mutable (but
copyable) object with state, in which it can store information
about the current context.
*/
interface StreamParser<State> {
    /**
    Read one token, advancing the stream past it, and returning a
    string indicating the token's style tag—either the name of one
    of the tags in [`tags`](https://codemirror.net/6/docs/ref/#highlight.tags), or such a name
    suffixed by one or more tag
    [modifier](https://codemirror.net/6/docs/ref/#highlight.Tag^defineModifier) names, separated by
    spaces. For example `"keyword"` or "`variableName.constant"`.
    
    It is okay to return a zero-length token, but only if that
    updates the state so that the next call will return a non-empty
    token again.
    */
    token(stream: StringStream, state: State): string | null;
    /**
    This notifies the parser of a blank line in the input. It can
    update its state here if it needs to.
    */
    blankLine?(state: State, indentUnit: number): void;
    /**
    Produce a start state for the parser.
    */
    startState?(indentUnit: number): State;
    /**
    Copy a given state. By default, a shallow object copy is done
    which also copies arrays held at the top level of the object.
    */
    copyState?(state: State): State;
    /**
    Compute automatic indentation for the line that starts with the
    given state and text.
    */
    indent?(state: State, textAfter: string, context: IndentContext): number | null;
    /**
    Default [language data](https://codemirror.net/6/docs/ref/#state.EditorState.languageDataAt) to
    attach to this language.
    */
    languageData?: {
        [name: string]: any;
    };
    /**
    Extra tokens to use in this parser. When the tokenizer returns a
    token name that exists as a property in this object, the
    corresponding tag will be assigned to the token.
    */
    tokenTable?: {
        [name: string]: Tag;
    };
}
/**
A [language](https://codemirror.net/6/docs/ref/#language.Language) class based on a streaming
parser.
*/
declare class StreamLanguage<State> extends Language {
    private constructor();
    static define<State>(spec: StreamParser<State>): StreamLanguage<State>;
    private getIndent;
    get allowsNesting(): boolean;
}

export { StreamLanguage, StreamParser, StringStream };
