import { NodeType, NodeSet, Parser, NodeProp, Tree } from '@lezer/common';
import { tags, styleTags } from '@codemirror/highlight';
import { Language, defineLanguageFacet, indentService, syntaxTree, ParseContext, getIndentUnit, languageDataProp } from '@codemirror/language';

// Counts the column offset in a string, taking tabs into account.
// Used mostly to find indentation.
function countCol(string, end, tabSize, startIndex = 0, startValue = 0) {
    if (end == null) {
        end = string.search(/[^\s\u00a0]/);
        if (end == -1)
            end = string.length;
    }
    let n = startValue;
    for (let i = startIndex; i < end; i++) {
        if (string.charCodeAt(i) == 9)
            n += tabSize - (n % tabSize);
        else
            n++;
    }
    return n;
}
/**
Encapsulates a single line of input. Given to stream syntax code,
which uses it to tokenize the content.
*/
class StringStream {
    /**
    @internal
    */
    constructor(
    /**
    The line.
    */
    string, tabSize, 
    /**
    The current indent unit size.
    */
    indentUnit) {
        this.string = string;
        this.tabSize = tabSize;
        this.indentUnit = indentUnit;
        /**
        The current position on the line.
        */
        this.pos = 0;
        /**
        The start position of the current token.
        */
        this.start = 0;
        this.lastColumnPos = 0;
        this.lastColumnValue = 0;
    }
    /**
    True if we are at the end of the line.
    */
    eol() { return this.pos >= this.string.length; }
    /**
    True if we are at the start of the line.
    */
    sol() { return this.pos == 0; }
    /**
    Get the next code unit after the current position, or undefined
    if we're at the end of the line.
    */
    peek() { return this.string.charAt(this.pos) || undefined; }
    /**
    Read the next code unit and advance `this.pos`.
    */
    next() {
        if (this.pos < this.string.length)
            return this.string.charAt(this.pos++);
    }
    /**
    Match the next character against the given string, regular
    expression, or predicate. Consume and return it if it matches.
    */
    eat(match) {
        let ch = this.string.charAt(this.pos);
        let ok;
        if (typeof match == "string")
            ok = ch == match;
        else
            ok = ch && (match instanceof RegExp ? match.test(ch) : match(ch));
        if (ok) {
            ++this.pos;
            return ch;
        }
    }
    /**
    Continue matching characters that match the given string,
    regular expression, or predicate function. Return true if any
    characters were consumed.
    */
    eatWhile(match) {
        let start = this.pos;
        while (this.eat(match)) { }
        return this.pos > start;
    }
    /**
    Consume whitespace ahead of `this.pos`. Return true if any was
    found.
    */
    eatSpace() {
        let start = this.pos;
        while (/[\s\u00a0]/.test(this.string.charAt(this.pos)))
            ++this.pos;
        return this.pos > start;
    }
    /**
    Move to the end of the line.
    */
    skipToEnd() { this.pos = this.string.length; }
    /**
    Move to directly before the given character, if found on the
    current line.
    */
    skipTo(ch) {
        let found = this.string.indexOf(ch, this.pos);
        if (found > -1) {
            this.pos = found;
            return true;
        }
    }
    /**
    Move back `n` characters.
    */
    backUp(n) { this.pos -= n; }
    /**
    Get the column position at `this.pos`.
    */
    column() {
        if (this.lastColumnPos < this.start) {
            this.lastColumnValue = countCol(this.string, this.start, this.tabSize, this.lastColumnPos, this.lastColumnValue);
            this.lastColumnPos = this.start;
        }
        return this.lastColumnValue;
    }
    /**
    Get the indentation column of the current line.
    */
    indentation() {
        return countCol(this.string, null, this.tabSize);
    }
    /**
    Match the input against the given string or regular expression
    (which should start with a `^`). Return true or the regexp match
    if it matches.
    
    Unless `consume` is set to `false`, this will move `this.pos`
    past the matched text.
    
    When matching a string `caseInsensitive` can be set to true to
    make the match case-insensitive.
    */
    match(pattern, consume, caseInsensitive) {
        if (typeof pattern == "string") {
            let cased = (str) => caseInsensitive ? str.toLowerCase() : str;
            let substr = this.string.substr(this.pos, pattern.length);
            if (cased(substr) == cased(pattern)) {
                if (consume !== false)
                    this.pos += pattern.length;
                return true;
            }
            else
                return null;
        }
        else {
            let match = this.string.slice(this.pos).match(pattern);
            if (match && match.index > 0)
                return null;
            if (match && consume !== false)
                this.pos += match[0].length;
            return match;
        }
    }
    /**
    Get the current token.
    */
    current() { return this.string.slice(this.start, this.pos); }
}

function fullParser(spec) {
    return {
        token: spec.token,
        blankLine: spec.blankLine || (() => { }),
        startState: spec.startState || (() => true),
        copyState: spec.copyState || defaultCopyState,
        indent: spec.indent || (() => null),
        languageData: spec.languageData || {},
        tokenTable: spec.tokenTable || noTokens
    };
}
function defaultCopyState(state) {
    if (typeof state != "object")
        return state;
    let newState = {};
    for (let prop in state) {
        let val = state[prop];
        newState[prop] = (val instanceof Array ? val.slice() : val);
    }
    return newState;
}
/**
A [language](https://codemirror.net/6/docs/ref/#language.Language) class based on a streaming
parser.
*/
class StreamLanguage extends Language {
    constructor(parser) {
        let data = defineLanguageFacet(parser.languageData);
        let p = fullParser(parser), self;
        let impl = new class extends Parser {
            createParse(input, fragments, ranges) {
                return new Parse(self, input, fragments, ranges);
            }
        };
        super(data, impl, docID(data), [indentService.of((cx, pos) => this.getIndent(cx, pos))]);
        self = this;
        this.streamParser = p;
        this.stateAfter = new NodeProp({ perNode: true });
        this.tokenTable = parser.tokenTable ? new TokenTable(p.tokenTable) : defaultTokenTable;
    }
    static define(spec) { return new StreamLanguage(spec); }
    getIndent(cx, pos) {
        let tree = syntaxTree(cx.state), at = tree.resolve(pos);
        while (at && at.type != this.topNode)
            at = at.parent;
        if (!at)
            return null;
        let start = findState(this, tree, 0, at.from, pos), statePos, state;
        if (start) {
            state = start.state;
            statePos = start.pos + 1;
        }
        else {
            state = this.streamParser.startState(cx.unit);
            statePos = 0;
        }
        if (pos - statePos > 10000 /* MaxIndentScanDist */)
            return null;
        while (statePos < pos) {
            let line = cx.state.doc.lineAt(statePos), end = Math.min(pos, line.to);
            if (line.length) {
                let stream = new StringStream(line.text, cx.state.tabSize, cx.unit);
                while (stream.pos < end - line.from)
                    readToken(this.streamParser.token, stream, state);
            }
            else {
                this.streamParser.blankLine(state, cx.unit);
            }
            if (end == pos)
                break;
            statePos = line.to + 1;
        }
        let { text } = cx.lineAt(pos);
        return this.streamParser.indent(state, /^\s*(.*)/.exec(text)[1], cx);
    }
    get allowsNesting() { return false; }
}
function findState(lang, tree, off, startPos, before) {
    let state = off >= startPos && off + tree.length <= before && tree.prop(lang.stateAfter);
    if (state)
        return { state: lang.streamParser.copyState(state), pos: off + tree.length };
    for (let i = tree.children.length - 1; i >= 0; i--) {
        let child = tree.children[i], pos = off + tree.positions[i];
        let found = child instanceof Tree && pos < before && findState(lang, child, pos, startPos, before);
        if (found)
            return found;
    }
    return null;
}
function cutTree(lang, tree, from, to, inside) {
    if (inside && from <= 0 && to >= tree.length)
        return tree;
    if (!inside && tree.type == lang.topNode)
        inside = true;
    for (let i = tree.children.length - 1; i >= 0; i--) {
        let pos = tree.positions[i], child = tree.children[i], inner;
        if (pos < to && child instanceof Tree) {
            if (!(inner = cutTree(lang, child, from - pos, to - pos, inside)))
                break;
            return !inside ? inner
                : new Tree(tree.type, tree.children.slice(0, i).concat(inner), tree.positions.slice(0, i + 1), pos + inner.length);
        }
    }
    return null;
}
function findStartInFragments(lang, fragments, startPos, editorState) {
    for (let f of fragments) {
        let from = f.from + (f.openStart ? 25 : 0), to = f.to - (f.openEnd ? 25 : 0);
        let found = from <= startPos && to > startPos && findState(lang, f.tree, 0 - f.offset, startPos, to), tree;
        if (found && (tree = cutTree(lang, f.tree, startPos + f.offset, found.pos + f.offset, false)))
            return { state: found.state, tree };
    }
    return { state: lang.streamParser.startState(editorState ? getIndentUnit(editorState) : 4), tree: Tree.empty };
}
class Parse {
    constructor(lang, input, fragments, ranges) {
        this.lang = lang;
        this.input = input;
        this.fragments = fragments;
        this.ranges = ranges;
        this.stoppedAt = null;
        this.chunks = [];
        this.chunkPos = [];
        this.chunk = [];
        this.chunkReused = undefined;
        this.rangeIndex = 0;
        this.to = ranges[ranges.length - 1].to;
        let context = ParseContext.get(), from = ranges[0].from;
        let { state, tree } = findStartInFragments(lang, fragments, from, context === null || context === void 0 ? void 0 : context.state);
        this.state = state;
        this.parsedPos = this.chunkStart = from + tree.length;
        for (let i = 0; i < tree.children.length; i++) {
            this.chunks.push(tree.children[i]);
            this.chunkPos.push(tree.positions[i]);
        }
        if (context && this.parsedPos < context.viewport.from - 100000 /* MaxDistanceBeforeViewport */) {
            this.state = this.lang.streamParser.startState(getIndentUnit(context.state));
            context.skipUntilInView(this.parsedPos, context.viewport.from);
            this.parsedPos = context.viewport.from;
        }
        this.moveRangeIndex();
    }
    advance() {
        let context = ParseContext.get();
        let parseEnd = this.stoppedAt == null ? this.to : Math.min(this.to, this.stoppedAt);
        let end = Math.min(parseEnd, this.chunkStart + 2048 /* ChunkSize */);
        if (context)
            end = Math.min(end, context.viewport.to);
        while (this.parsedPos < end)
            this.parseLine(context);
        if (this.chunkStart < this.parsedPos)
            this.finishChunk();
        if (this.parsedPos >= parseEnd)
            return this.finish();
        if (context && this.parsedPos >= context.viewport.to) {
            context.skipUntilInView(this.parsedPos, parseEnd);
            return this.finish();
        }
        return null;
    }
    stopAt(pos) {
        this.stoppedAt = pos;
    }
    lineAfter(pos) {
        let chunk = this.input.chunk(pos);
        if (!this.input.lineChunks) {
            let eol = chunk.indexOf("\n");
            if (eol > -1)
                chunk = chunk.slice(0, eol);
        }
        else if (chunk == "\n") {
            chunk = "";
        }
        return pos + chunk.length <= this.to ? chunk : chunk.slice(0, this.to - pos);
    }
    nextLine() {
        let from = this.parsedPos, line = this.lineAfter(from), end = from + line.length;
        for (let index = this.rangeIndex;;) {
            let rangeEnd = this.ranges[index].to;
            if (rangeEnd >= end)
                break;
            line = line.slice(0, rangeEnd - (end - line.length));
            index++;
            if (index == this.ranges.length)
                break;
            let rangeStart = this.ranges[index].from;
            let after = this.lineAfter(rangeStart);
            line += after;
            end = rangeStart + after.length;
        }
        return { line, end };
    }
    skipGapsTo(pos, offset, side) {
        for (;;) {
            let end = this.ranges[this.rangeIndex].to, offPos = pos + offset;
            if (side > 0 ? end > offPos : end >= offPos)
                break;
            let start = this.ranges[++this.rangeIndex].from;
            offset += start - end;
        }
        return offset;
    }
    moveRangeIndex() {
        while (this.ranges[this.rangeIndex].to < this.parsedPos)
            this.rangeIndex++;
    }
    emitToken(id, from, to, size, offset) {
        if (this.ranges.length > 1) {
            offset = this.skipGapsTo(from, offset, 1);
            from += offset;
            let len0 = this.chunk.length;
            offset = this.skipGapsTo(to, offset, -1);
            to += offset;
            size += this.chunk.length - len0;
        }
        this.chunk.push(id, from, to, size);
        return offset;
    }
    parseLine(context) {
        let { line, end } = this.nextLine(), offset = 0, { streamParser } = this.lang;
        let stream = new StringStream(line, context ? context.state.tabSize : 4, context ? getIndentUnit(context.state) : 2);
        if (stream.eol()) {
            streamParser.blankLine(this.state, stream.indentUnit);
        }
        else {
            while (!stream.eol()) {
                let token = readToken(streamParser.token, stream, this.state);
                if (token)
                    offset = this.emitToken(this.lang.tokenTable.resolve(token), this.parsedPos + stream.start, this.parsedPos + stream.pos, 4, offset);
                if (stream.start > 10000 /* MaxLineLength */)
                    break;
            }
        }
        this.parsedPos = end;
        this.moveRangeIndex();
        if (this.parsedPos < this.to)
            this.parsedPos++;
    }
    finishChunk() {
        let tree = Tree.build({
            buffer: this.chunk,
            start: this.chunkStart,
            length: this.parsedPos - this.chunkStart,
            nodeSet,
            topID: 0,
            maxBufferLength: 2048 /* ChunkSize */,
            reused: this.chunkReused
        });
        tree = new Tree(tree.type, tree.children, tree.positions, tree.length, [[this.lang.stateAfter, this.lang.streamParser.copyState(this.state)]]);
        this.chunks.push(tree);
        this.chunkPos.push(this.chunkStart - this.ranges[0].from);
        this.chunk = [];
        this.chunkReused = undefined;
        this.chunkStart = this.parsedPos;
    }
    finish() {
        return new Tree(this.lang.topNode, this.chunks, this.chunkPos, this.parsedPos - this.ranges[0].from).balance();
    }
}
function readToken(token, stream, state) {
    stream.start = stream.pos;
    for (let i = 0; i < 10; i++) {
        let result = token(stream, state);
        if (stream.pos > stream.start)
            return result;
    }
    throw new Error("Stream parser failed to advance stream.");
}
const noTokens = /*@__PURE__*/Object.create(null);
const typeArray = [NodeType.none];
const nodeSet = /*@__PURE__*/new NodeSet(typeArray);
const warned = [];
const defaultTable = /*@__PURE__*/Object.create(null);
for (let [legacyName, name] of [
    ["variable", "variableName"],
    ["variable-2", "variableName.special"],
    ["string-2", "string.special"],
    ["def", "variableName.definition"],
    ["tag", "typeName"],
    ["attribute", "propertyName"],
    ["type", "typeName"],
    ["builtin", "variableName.standard"],
    ["qualifier", "modifier"],
    ["error", "invalid"],
    ["header", "heading"],
    ["property", "propertyName"]
])
    defaultTable[legacyName] = /*@__PURE__*/createTokenType(noTokens, name);
class TokenTable {
    constructor(extra) {
        this.extra = extra;
        this.table = Object.assign(Object.create(null), defaultTable);
    }
    resolve(tag) {
        return !tag ? 0 : this.table[tag] || (this.table[tag] = createTokenType(this.extra, tag));
    }
}
const defaultTokenTable = /*@__PURE__*/new TokenTable(noTokens);
function warnForPart(part, msg) {
    if (warned.indexOf(part) > -1)
        return;
    warned.push(part);
    console.warn(msg);
}
function createTokenType(extra, tagStr) {
    let tag = null;
    for (let part of tagStr.split(".")) {
        let value = (extra[part] || tags[part]);
        if (!value) {
            warnForPart(part, `Unknown highlighting tag ${part}`);
        }
        else if (typeof value == "function") {
            if (!tag)
                warnForPart(part, `Modifier ${part} used at start of tag`);
            else
                tag = value(tag);
        }
        else {
            if (tag)
                warnForPart(part, `Tag ${part} used as modifier`);
            else
                tag = value;
        }
    }
    if (!tag)
        return 0;
    let name = tagStr.replace(/ /g, "_"), type = NodeType.define({
        id: typeArray.length,
        name,
        props: [styleTags({ [name]: tag })]
    });
    typeArray.push(type);
    return type.id;
}
function docID(data) {
    let type = NodeType.define({ id: typeArray.length, name: "Document", props: [languageDataProp.add(() => data)] });
    typeArray.push(type);
    return type;
}

export { StreamLanguage, StringStream };
