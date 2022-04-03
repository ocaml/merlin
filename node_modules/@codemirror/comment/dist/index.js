/**
Comment or uncomment the current selection. Will use line comments
if available, otherwise falling back to block comments.
*/
const toggleComment = target => {
    let config = getConfig(target.state);
    return config.line ? toggleLineComment(target) : config.block ? toggleBlockCommentByLine(target) : false;
};
function command(f, option) {
    return ({ state, dispatch }) => {
        if (state.readOnly)
            return false;
        let tr = f(option, state);
        if (!tr)
            return false;
        dispatch(state.update(tr));
        return true;
    };
}
/**
Comment or uncomment the current selection using line comments.
The line comment syntax is taken from the
[`commentTokens`](https://codemirror.net/6/docs/ref/#comment.CommentTokens) [language
data](https://codemirror.net/6/docs/ref/#state.EditorState.languageDataAt).
*/
const toggleLineComment = /*@__PURE__*/command(changeLineComment, 0 /* Toggle */);
/**
Comment the current selection using line comments.
*/
const lineComment = /*@__PURE__*/command(changeLineComment, 1 /* Comment */);
/**
Uncomment the current selection using line comments.
*/
const lineUncomment = /*@__PURE__*/command(changeLineComment, 2 /* Uncomment */);
/**
Comment or uncomment the current selection using block comments.
The block comment syntax is taken from the
[`commentTokens`](https://codemirror.net/6/docs/ref/#comment.CommentTokens) [language
data](https://codemirror.net/6/docs/ref/#state.EditorState.languageDataAt).
*/
const toggleBlockComment = /*@__PURE__*/command(changeBlockComment, 0 /* Toggle */);
/**
Comment the current selection using block comments.
*/
const blockComment = /*@__PURE__*/command(changeBlockComment, 1 /* Comment */);
/**
Uncomment the current selection using block comments.
*/
const blockUncomment = /*@__PURE__*/command(changeBlockComment, 2 /* Uncomment */);
/**
Comment or uncomment the lines around the current selection using
block comments.
*/
const toggleBlockCommentByLine = /*@__PURE__*/command((o, s) => changeBlockComment(o, s, selectedLineRanges(s)), 0 /* Toggle */);
/**
Default key bindings for this package.

 - Ctrl-/ (Cmd-/ on macOS): [`toggleComment`](https://codemirror.net/6/docs/ref/#comment.toggleComment).
 - Shift-Alt-a: [`toggleBlockComment`](https://codemirror.net/6/docs/ref/#comment.toggleBlockComment).
*/
const commentKeymap = [
    { key: "Mod-/", run: toggleComment },
    { key: "Alt-A", run: toggleBlockComment }
];
function getConfig(state, pos = state.selection.main.head) {
    let data = state.languageDataAt("commentTokens", pos);
    return data.length ? data[0] : {};
}
const SearchMargin = 50;
/**
Determines if the given range is block-commented in the given
state.
*/
function findBlockComment(state, { open, close }, from, to) {
    let textBefore = state.sliceDoc(from - SearchMargin, from);
    let textAfter = state.sliceDoc(to, to + SearchMargin);
    let spaceBefore = /\s*$/.exec(textBefore)[0].length, spaceAfter = /^\s*/.exec(textAfter)[0].length;
    let beforeOff = textBefore.length - spaceBefore;
    if (textBefore.slice(beforeOff - open.length, beforeOff) == open &&
        textAfter.slice(spaceAfter, spaceAfter + close.length) == close) {
        return { open: { pos: from - spaceBefore, margin: spaceBefore && 1 },
            close: { pos: to + spaceAfter, margin: spaceAfter && 1 } };
    }
    let startText, endText;
    if (to - from <= 2 * SearchMargin) {
        startText = endText = state.sliceDoc(from, to);
    }
    else {
        startText = state.sliceDoc(from, from + SearchMargin);
        endText = state.sliceDoc(to - SearchMargin, to);
    }
    let startSpace = /^\s*/.exec(startText)[0].length, endSpace = /\s*$/.exec(endText)[0].length;
    let endOff = endText.length - endSpace - close.length;
    if (startText.slice(startSpace, startSpace + open.length) == open &&
        endText.slice(endOff, endOff + close.length) == close) {
        return { open: { pos: from + startSpace + open.length,
                margin: /\s/.test(startText.charAt(startSpace + open.length)) ? 1 : 0 },
            close: { pos: to - endSpace - close.length,
                margin: /\s/.test(endText.charAt(endOff - 1)) ? 1 : 0 } };
    }
    return null;
}
function selectedLineRanges(state) {
    let ranges = [];
    for (let r of state.selection.ranges) {
        let fromLine = state.doc.lineAt(r.from);
        let toLine = r.to <= fromLine.to ? fromLine : state.doc.lineAt(r.to);
        let last = ranges.length - 1;
        if (last >= 0 && ranges[last].to > fromLine.from)
            ranges[last].to = toLine.to;
        else
            ranges.push({ from: fromLine.from, to: toLine.to });
    }
    return ranges;
}
// Performs toggle, comment and uncomment of block comments in
// languages that support them.
function changeBlockComment(option, state, ranges = state.selection.ranges) {
    let tokens = ranges.map(r => getConfig(state, r.from).block);
    if (!tokens.every(c => c))
        return null;
    let comments = ranges.map((r, i) => findBlockComment(state, tokens[i], r.from, r.to));
    if (option != 2 /* Uncomment */ && !comments.every(c => c)) {
        return { changes: state.changes(ranges.map((range, i) => {
                if (comments[i])
                    return [];
                return [{ from: range.from, insert: tokens[i].open + " " }, { from: range.to, insert: " " + tokens[i].close }];
            })) };
    }
    else if (option != 1 /* Comment */ && comments.some(c => c)) {
        let changes = [];
        for (let i = 0, comment; i < comments.length; i++)
            if (comment = comments[i]) {
                let token = tokens[i], { open, close } = comment;
                changes.push({ from: open.pos - token.open.length, to: open.pos + open.margin }, { from: close.pos - close.margin, to: close.pos + token.close.length });
            }
        return { changes };
    }
    return null;
}
// Performs toggle, comment and uncomment of line comments.
function changeLineComment(option, state, ranges = state.selection.ranges) {
    let lines = [];
    let prevLine = -1;
    for (let { from, to } of ranges) {
        let startI = lines.length, minIndent = 1e9;
        for (let pos = from; pos <= to;) {
            let line = state.doc.lineAt(pos);
            if (line.from > prevLine && (from == to || to > line.from)) {
                prevLine = line.from;
                let token = getConfig(state, pos).line;
                if (!token)
                    continue;
                let indent = /^\s*/.exec(line.text)[0].length;
                let empty = indent == line.length;
                let comment = line.text.slice(indent, indent + token.length) == token ? indent : -1;
                if (indent < line.text.length && indent < minIndent)
                    minIndent = indent;
                lines.push({ line, comment, token, indent, empty, single: false });
            }
            pos = line.to + 1;
        }
        if (minIndent < 1e9)
            for (let i = startI; i < lines.length; i++)
                if (lines[i].indent < lines[i].line.text.length)
                    lines[i].indent = minIndent;
        if (lines.length == startI + 1)
            lines[startI].single = true;
    }
    if (option != 2 /* Uncomment */ && lines.some(l => l.comment < 0 && (!l.empty || l.single))) {
        let changes = [];
        for (let { line, token, indent, empty, single } of lines)
            if (single || !empty)
                changes.push({ from: line.from + indent, insert: token + " " });
        let changeSet = state.changes(changes);
        return { changes: changeSet, selection: state.selection.map(changeSet, 1) };
    }
    else if (option != 1 /* Comment */ && lines.some(l => l.comment >= 0)) {
        let changes = [];
        for (let { line, comment, token } of lines)
            if (comment >= 0) {
                let from = line.from + comment, to = from + token.length;
                if (line.text[to - line.from] == " ")
                    to++;
                changes.push({ from, to });
            }
        return { changes };
    }
    return null;
}

export { blockComment, blockUncomment, commentKeymap, lineComment, lineUncomment, toggleBlockComment, toggleBlockCommentByLine, toggleComment, toggleLineComment };
