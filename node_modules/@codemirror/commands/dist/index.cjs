'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var state = require('@codemirror/state');
var text = require('@codemirror/text');
var view = require('@codemirror/view');
var matchbrackets = require('@codemirror/matchbrackets');
var language = require('@codemirror/language');
var common = require('@lezer/common');

function updateSel(sel, by) {
    return state.EditorSelection.create(sel.ranges.map(by), sel.mainIndex);
}
function setSel(state, selection) {
    return state.update({ selection, scrollIntoView: true, userEvent: "select" });
}
function moveSel({ state, dispatch }, how) {
    let selection = updateSel(state.selection, how);
    if (selection.eq(state.selection))
        return false;
    dispatch(setSel(state, selection));
    return true;
}
function rangeEnd(range, forward) {
    return state.EditorSelection.cursor(forward ? range.to : range.from);
}
function cursorByChar(view, forward) {
    return moveSel(view, range => range.empty ? view.moveByChar(range, forward) : rangeEnd(range, forward));
}
/**
Move the selection one character to the left (which is backward in
left-to-right text, forward in right-to-left text).
*/
const cursorCharLeft = view$1 => cursorByChar(view$1, view$1.textDirection != view.Direction.LTR);
/**
Move the selection one character to the right.
*/
const cursorCharRight = view$1 => cursorByChar(view$1, view$1.textDirection == view.Direction.LTR);
/**
Move the selection one character forward.
*/
const cursorCharForward = view => cursorByChar(view, true);
/**
Move the selection one character backward.
*/
const cursorCharBackward = view => cursorByChar(view, false);
function cursorByGroup(view, forward) {
    return moveSel(view, range => range.empty ? view.moveByGroup(range, forward) : rangeEnd(range, forward));
}
/**
Move the selection to the left across one group of word or
non-word (but also non-space) characters.
*/
const cursorGroupLeft = view$1 => cursorByGroup(view$1, view$1.textDirection != view.Direction.LTR);
/**
Move the selection one group to the right.
*/
const cursorGroupRight = view$1 => cursorByGroup(view$1, view$1.textDirection == view.Direction.LTR);
/**
Move the selection one group forward.
*/
const cursorGroupForward = view => cursorByGroup(view, true);
/**
Move the selection one group backward.
*/
const cursorGroupBackward = view => cursorByGroup(view, false);
function moveBySubword(view, range, forward) {
    let categorize = view.state.charCategorizer(range.from);
    return view.moveByChar(range, forward, start => {
        let cat = state.CharCategory.Space, pos = range.from;
        let done = false, sawUpper = false, sawLower = false;
        let step = (next) => {
            if (done)
                return false;
            pos += forward ? next.length : -next.length;
            let nextCat = categorize(next), ahead;
            if (cat == state.CharCategory.Space)
                cat = nextCat;
            if (cat != nextCat)
                return false;
            if (cat == state.CharCategory.Word) {
                if (next.toLowerCase() == next) {
                    if (!forward && sawUpper)
                        return false;
                    sawLower = true;
                }
                else if (sawLower) {
                    if (forward)
                        return false;
                    done = true;
                }
                else {
                    if (sawUpper && forward && categorize(ahead = view.state.sliceDoc(pos, pos + 1)) == state.CharCategory.Word &&
                        ahead.toLowerCase() == ahead)
                        return false;
                    sawUpper = true;
                }
            }
            return true;
        };
        step(start);
        return step;
    });
}
function cursorBySubword(view, forward) {
    return moveSel(view, range => range.empty ? moveBySubword(view, range, forward) : rangeEnd(range, forward));
}
/**
Move the selection one group or camel-case subword forward.
*/
const cursorSubwordForward = view => cursorBySubword(view, true);
/**
Move the selection one group or camel-case subword backward.
*/
const cursorSubwordBackward = view => cursorBySubword(view, false);
function interestingNode(state, node, bracketProp) {
    if (node.type.prop(bracketProp))
        return true;
    let len = node.to - node.from;
    return len && (len > 2 || /[^\s,.;:]/.test(state.sliceDoc(node.from, node.to))) || node.firstChild;
}
function moveBySyntax(state$1, start, forward) {
    let pos = language.syntaxTree(state$1).resolveInner(start.head);
    let bracketProp = forward ? common.NodeProp.closedBy : common.NodeProp.openedBy;
    // Scan forward through child nodes to see if there's an interesting
    // node ahead.
    for (let at = start.head;;) {
        let next = forward ? pos.childAfter(at) : pos.childBefore(at);
        if (!next)
            break;
        if (interestingNode(state$1, next, bracketProp))
            pos = next;
        else
            at = forward ? next.to : next.from;
    }
    let bracket = pos.type.prop(bracketProp), match, newPos;
    if (bracket && (match = forward ? matchbrackets.matchBrackets(state$1, pos.from, 1) : matchbrackets.matchBrackets(state$1, pos.to, -1)) && match.matched)
        newPos = forward ? match.end.to : match.end.from;
    else
        newPos = forward ? pos.to : pos.from;
    return state.EditorSelection.cursor(newPos, forward ? -1 : 1);
}
/**
Move the cursor over the next syntactic element to the left.
*/
const cursorSyntaxLeft = view$1 => moveSel(view$1, range => moveBySyntax(view$1.state, range, view$1.textDirection != view.Direction.LTR));
/**
Move the cursor over the next syntactic element to the right.
*/
const cursorSyntaxRight = view$1 => moveSel(view$1, range => moveBySyntax(view$1.state, range, view$1.textDirection == view.Direction.LTR));
function cursorByLine(view, forward) {
    return moveSel(view, range => {
        if (!range.empty)
            return rangeEnd(range, forward);
        let moved = view.moveVertically(range, forward);
        return moved.head != range.head ? moved : view.moveToLineBoundary(range, forward);
    });
}
/**
Move the selection one line up.
*/
const cursorLineUp = view => cursorByLine(view, false);
/**
Move the selection one line down.
*/
const cursorLineDown = view => cursorByLine(view, true);
function cursorByPage(view$1, forward) {
    let { state } = view$1, selection = updateSel(state.selection, range => {
        return range.empty ? view$1.moveVertically(range, forward, view$1.dom.clientHeight) : rangeEnd(range, forward);
    });
    if (selection.eq(state.selection))
        return false;
    let startPos = view$1.coordsAtPos(state.selection.main.head);
    let scrollRect = view$1.scrollDOM.getBoundingClientRect();
    view$1.dispatch(setSel(state, selection), {
        effects: startPos && startPos.top > scrollRect.top && startPos.bottom < scrollRect.bottom
            ? view.EditorView.scrollIntoView(selection.main.head, { y: "start", yMargin: startPos.top - scrollRect.top })
            : undefined
    });
    return true;
}
/**
Move the selection one page up.
*/
const cursorPageUp = view => cursorByPage(view, false);
/**
Move the selection one page down.
*/
const cursorPageDown = view => cursorByPage(view, true);
function moveByLineBoundary(view, start, forward) {
    let line = view.lineBlockAt(start.head), moved = view.moveToLineBoundary(start, forward);
    if (moved.head == start.head && moved.head != (forward ? line.to : line.from))
        moved = view.moveToLineBoundary(start, forward, false);
    if (!forward && moved.head == line.from && line.length) {
        let space = /^\s*/.exec(view.state.sliceDoc(line.from, Math.min(line.from + 100, line.to)))[0].length;
        if (space && start.head != line.from + space)
            moved = state.EditorSelection.cursor(line.from + space);
    }
    return moved;
}
/**
Move the selection to the next line wrap point, or to the end of
the line if there isn't one left on this line.
*/
const cursorLineBoundaryForward = view => moveSel(view, range => moveByLineBoundary(view, range, true));
/**
Move the selection to previous line wrap point, or failing that to
the start of the line. If the line is indented, and the cursor
isn't already at the end of the indentation, this will move to the
end of the indentation instead of the start of the line.
*/
const cursorLineBoundaryBackward = view => moveSel(view, range => moveByLineBoundary(view, range, false));
/**
Move the selection to the start of the line.
*/
const cursorLineStart = view => moveSel(view, range => state.EditorSelection.cursor(view.lineBlockAt(range.head).from, 1));
/**
Move the selection to the end of the line.
*/
const cursorLineEnd = view => moveSel(view, range => state.EditorSelection.cursor(view.lineBlockAt(range.head).to, -1));
function toMatchingBracket(state$1, dispatch, extend) {
    let found = false, selection = updateSel(state$1.selection, range => {
        let matching = matchbrackets.matchBrackets(state$1, range.head, -1)
            || matchbrackets.matchBrackets(state$1, range.head, 1)
            || (range.head > 0 && matchbrackets.matchBrackets(state$1, range.head - 1, 1))
            || (range.head < state$1.doc.length && matchbrackets.matchBrackets(state$1, range.head + 1, -1));
        if (!matching || !matching.end)
            return range;
        found = true;
        let head = matching.start.from == range.head ? matching.end.to : matching.end.from;
        return extend ? state.EditorSelection.range(range.anchor, head) : state.EditorSelection.cursor(head);
    });
    if (!found)
        return false;
    dispatch(setSel(state$1, selection));
    return true;
}
/**
Move the selection to the bracket matching the one it is currently
on, if any.
*/
const cursorMatchingBracket = ({ state, dispatch }) => toMatchingBracket(state, dispatch, false);
/**
Extend the selection to the bracket matching the one the selection
head is currently on, if any.
*/
const selectMatchingBracket = ({ state, dispatch }) => toMatchingBracket(state, dispatch, true);
function extendSel(view, how) {
    let selection = updateSel(view.state.selection, range => {
        let head = how(range);
        return state.EditorSelection.range(range.anchor, head.head, head.goalColumn);
    });
    if (selection.eq(view.state.selection))
        return false;
    view.dispatch(setSel(view.state, selection));
    return true;
}
function selectByChar(view, forward) {
    return extendSel(view, range => view.moveByChar(range, forward));
}
/**
Move the selection head one character to the left, while leaving
the anchor in place.
*/
const selectCharLeft = view$1 => selectByChar(view$1, view$1.textDirection != view.Direction.LTR);
/**
Move the selection head one character to the right.
*/
const selectCharRight = view$1 => selectByChar(view$1, view$1.textDirection == view.Direction.LTR);
/**
Move the selection head one character forward.
*/
const selectCharForward = view => selectByChar(view, true);
/**
Move the selection head one character backward.
*/
const selectCharBackward = view => selectByChar(view, false);
function selectByGroup(view, forward) {
    return extendSel(view, range => view.moveByGroup(range, forward));
}
/**
Move the selection head one [group](https://codemirror.net/6/docs/ref/#commands.cursorGroupLeft) to
the left.
*/
const selectGroupLeft = view$1 => selectByGroup(view$1, view$1.textDirection != view.Direction.LTR);
/**
Move the selection head one group to the right.
*/
const selectGroupRight = view$1 => selectByGroup(view$1, view$1.textDirection == view.Direction.LTR);
/**
Move the selection head one group forward.
*/
const selectGroupForward = view => selectByGroup(view, true);
/**
Move the selection head one group backward.
*/
const selectGroupBackward = view => selectByGroup(view, false);
function selectBySubword(view, forward) {
    return extendSel(view, range => moveBySubword(view, range, forward));
}
/**
Move the selection head one group or camel-case subword forward.
*/
const selectSubwordForward = view => selectBySubword(view, true);
/**
Move the selection head one group or subword backward.
*/
const selectSubwordBackward = view => selectBySubword(view, false);
/**
Move the selection head over the next syntactic element to the left.
*/
const selectSyntaxLeft = view$1 => extendSel(view$1, range => moveBySyntax(view$1.state, range, view$1.textDirection != view.Direction.LTR));
/**
Move the selection head over the next syntactic element to the right.
*/
const selectSyntaxRight = view$1 => extendSel(view$1, range => moveBySyntax(view$1.state, range, view$1.textDirection == view.Direction.LTR));
function selectByLine(view, forward) {
    return extendSel(view, range => view.moveVertically(range, forward));
}
/**
Move the selection head one line up.
*/
const selectLineUp = view => selectByLine(view, false);
/**
Move the selection head one line down.
*/
const selectLineDown = view => selectByLine(view, true);
function selectByPage(view, forward) {
    return extendSel(view, range => view.moveVertically(range, forward, view.dom.clientHeight));
}
/**
Move the selection head one page up.
*/
const selectPageUp = view => selectByPage(view, false);
/**
Move the selection head one page down.
*/
const selectPageDown = view => selectByPage(view, true);
/**
Move the selection head to the next line boundary.
*/
const selectLineBoundaryForward = view => extendSel(view, range => moveByLineBoundary(view, range, true));
/**
Move the selection head to the previous line boundary.
*/
const selectLineBoundaryBackward = view => extendSel(view, range => moveByLineBoundary(view, range, false));
/**
Move the selection head to the start of the line.
*/
const selectLineStart = view => extendSel(view, range => state.EditorSelection.cursor(view.lineBlockAt(range.head).from));
/**
Move the selection head to the end of the line.
*/
const selectLineEnd = view => extendSel(view, range => state.EditorSelection.cursor(view.lineBlockAt(range.head).to));
/**
Move the selection to the start of the document.
*/
const cursorDocStart = ({ state, dispatch }) => {
    dispatch(setSel(state, { anchor: 0 }));
    return true;
};
/**
Move the selection to the end of the document.
*/
const cursorDocEnd = ({ state, dispatch }) => {
    dispatch(setSel(state, { anchor: state.doc.length }));
    return true;
};
/**
Move the selection head to the start of the document.
*/
const selectDocStart = ({ state, dispatch }) => {
    dispatch(setSel(state, { anchor: state.selection.main.anchor, head: 0 }));
    return true;
};
/**
Move the selection head to the end of the document.
*/
const selectDocEnd = ({ state, dispatch }) => {
    dispatch(setSel(state, { anchor: state.selection.main.anchor, head: state.doc.length }));
    return true;
};
/**
Select the entire document.
*/
const selectAll = ({ state, dispatch }) => {
    dispatch(state.update({ selection: { anchor: 0, head: state.doc.length }, userEvent: "select" }));
    return true;
};
/**
Expand the selection to cover entire lines.
*/
const selectLine = ({ state: state$1, dispatch }) => {
    let ranges = selectedLineBlocks(state$1).map(({ from, to }) => state.EditorSelection.range(from, Math.min(to + 1, state$1.doc.length)));
    dispatch(state$1.update({ selection: state.EditorSelection.create(ranges), userEvent: "select" }));
    return true;
};
/**
Select the next syntactic construct that is larger than the
selection. Note that this will only work insofar as the language
[provider](https://codemirror.net/6/docs/ref/#language.language) you use builds up a full
syntax tree.
*/
const selectParentSyntax = ({ state: state$1, dispatch }) => {
    let selection = updateSel(state$1.selection, range => {
        var _a;
        let context = language.syntaxTree(state$1).resolveInner(range.head, 1);
        while (!((context.from < range.from && context.to >= range.to) ||
            (context.to > range.to && context.from <= range.from) ||
            !((_a = context.parent) === null || _a === void 0 ? void 0 : _a.parent)))
            context = context.parent;
        return state.EditorSelection.range(context.to, context.from);
    });
    dispatch(setSel(state$1, selection));
    return true;
};
/**
Simplify the current selection. When multiple ranges are selected,
reduce it to its main range. Otherwise, if the selection is
non-empty, convert it to a cursor selection.
*/
const simplifySelection = ({ state: state$1, dispatch }) => {
    let cur = state$1.selection, selection = null;
    if (cur.ranges.length > 1)
        selection = state.EditorSelection.create([cur.main]);
    else if (!cur.main.empty)
        selection = state.EditorSelection.create([state.EditorSelection.cursor(cur.main.head)]);
    if (!selection)
        return false;
    dispatch(setSel(state$1, selection));
    return true;
};
function deleteBy({ state: state$1, dispatch }, by) {
    if (state$1.readOnly)
        return false;
    let event = "delete.selection";
    let changes = state$1.changeByRange(range => {
        let { from, to } = range;
        if (from == to) {
            let towards = by(from);
            if (towards < from)
                event = "delete.backward";
            else if (towards > from)
                event = "delete.forward";
            from = Math.min(from, towards);
            to = Math.max(to, towards);
        }
        return from == to ? { range } : { changes: { from, to }, range: state.EditorSelection.cursor(from) };
    });
    if (changes.changes.empty)
        return false;
    dispatch(state$1.update(changes, { scrollIntoView: true, userEvent: event }));
    return true;
}
function skipAtomic(target, pos, forward) {
    if (target instanceof view.EditorView)
        for (let ranges of target.pluginField(view.PluginField.atomicRanges))
            ranges.between(pos, pos, (from, to) => {
                if (from < pos && to > pos)
                    pos = forward ? to : from;
            });
    return pos;
}
const deleteByChar = (target, forward) => deleteBy(target, pos => {
    let { state } = target, line = state.doc.lineAt(pos), before, targetPos;
    if (!forward && pos > line.from && pos < line.from + 200 &&
        !/[^ \t]/.test(before = line.text.slice(0, pos - line.from))) {
        if (before[before.length - 1] == "\t")
            return pos - 1;
        let col = text.countColumn(before, state.tabSize), drop = col % language.getIndentUnit(state) || language.getIndentUnit(state);
        for (let i = 0; i < drop && before[before.length - 1 - i] == " "; i++)
            pos--;
        targetPos = pos;
    }
    else {
        targetPos = text.findClusterBreak(line.text, pos - line.from, forward, forward) + line.from;
        if (targetPos == pos && line.number != (forward ? state.doc.lines : 1))
            targetPos += forward ? 1 : -1;
    }
    return skipAtomic(target, targetPos, forward);
});
/**
Delete the selection, or, for cursor selections, the character
before the cursor.
*/
const deleteCharBackward = view => deleteByChar(view, false);
/**
Delete the selection or the character after the cursor.
*/
const deleteCharForward = view => deleteByChar(view, true);
const deleteByGroup = (target, forward) => deleteBy(target, start => {
    let pos = start, { state } = target, line = state.doc.lineAt(pos);
    let categorize = state.charCategorizer(pos);
    for (let cat = null;;) {
        if (pos == (forward ? line.to : line.from)) {
            if (pos == start && line.number != (forward ? state.doc.lines : 1))
                pos += forward ? 1 : -1;
            break;
        }
        let next = text.findClusterBreak(line.text, pos - line.from, forward) + line.from;
        let nextChar = line.text.slice(Math.min(pos, next) - line.from, Math.max(pos, next) - line.from);
        let nextCat = categorize(nextChar);
        if (cat != null && nextCat != cat)
            break;
        if (nextChar != " " || pos != start)
            cat = nextCat;
        pos = next;
    }
    return skipAtomic(target, pos, forward);
});
/**
Delete the selection or backward until the end of the next
[group](https://codemirror.net/6/docs/ref/#view.EditorView.moveByGroup), only skipping groups of
whitespace when they consist of a single space.
*/
const deleteGroupBackward = target => deleteByGroup(target, false);
/**
Delete the selection or forward until the end of the next group.
*/
const deleteGroupForward = target => deleteByGroup(target, true);
/**
Delete the selection, or, if it is a cursor selection, delete to
the end of the line. If the cursor is directly at the end of the
line, delete the line break after it.
*/
const deleteToLineEnd = view => deleteBy(view, pos => {
    let lineEnd = view.lineBlockAt(pos).to;
    return skipAtomic(view, pos < lineEnd ? lineEnd : Math.min(view.state.doc.length, pos + 1), true);
});
/**
Delete the selection, or, if it is a cursor selection, delete to
the start of the line. If the cursor is directly at the start of the
line, delete the line break before it.
*/
const deleteToLineStart = view => deleteBy(view, pos => {
    let lineStart = view.lineBlockAt(pos).from;
    return skipAtomic(view, pos > lineStart ? lineStart : Math.max(0, pos - 1), false);
});
/**
Delete all whitespace directly before a line end from the
document.
*/
const deleteTrailingWhitespace = ({ state, dispatch }) => {
    if (state.readOnly)
        return false;
    let changes = [];
    for (let pos = 0, prev = "", iter = state.doc.iter();;) {
        iter.next();
        if (iter.lineBreak || iter.done) {
            let trailing = prev.search(/\s+$/);
            if (trailing > -1)
                changes.push({ from: pos - (prev.length - trailing), to: pos });
            if (iter.done)
                break;
            prev = "";
        }
        else {
            prev = iter.value;
        }
        pos += iter.value.length;
    }
    if (!changes.length)
        return false;
    dispatch(state.update({ changes, userEvent: "delete" }));
    return true;
};
/**
Replace each selection range with a line break, leaving the cursor
on the line before the break.
*/
const splitLine = ({ state: state$1, dispatch }) => {
    if (state$1.readOnly)
        return false;
    let changes = state$1.changeByRange(range => {
        return { changes: { from: range.from, to: range.to, insert: text.Text.of(["", ""]) },
            range: state.EditorSelection.cursor(range.from) };
    });
    dispatch(state$1.update(changes, { scrollIntoView: true, userEvent: "input" }));
    return true;
};
/**
Flip the characters before and after the cursor(s).
*/
const transposeChars = ({ state: state$1, dispatch }) => {
    if (state$1.readOnly)
        return false;
    let changes = state$1.changeByRange(range => {
        if (!range.empty || range.from == 0 || range.from == state$1.doc.length)
            return { range };
        let pos = range.from, line = state$1.doc.lineAt(pos);
        let from = pos == line.from ? pos - 1 : text.findClusterBreak(line.text, pos - line.from, false) + line.from;
        let to = pos == line.to ? pos + 1 : text.findClusterBreak(line.text, pos - line.from, true) + line.from;
        return { changes: { from, to, insert: state$1.doc.slice(pos, to).append(state$1.doc.slice(from, pos)) },
            range: state.EditorSelection.cursor(to) };
    });
    if (changes.changes.empty)
        return false;
    dispatch(state$1.update(changes, { scrollIntoView: true, userEvent: "move.character" }));
    return true;
};
function selectedLineBlocks(state) {
    let blocks = [], upto = -1;
    for (let range of state.selection.ranges) {
        let startLine = state.doc.lineAt(range.from), endLine = state.doc.lineAt(range.to);
        if (!range.empty && range.to == endLine.from)
            endLine = state.doc.lineAt(range.to - 1);
        if (upto >= startLine.number) {
            let prev = blocks[blocks.length - 1];
            prev.to = endLine.to;
            prev.ranges.push(range);
        }
        else {
            blocks.push({ from: startLine.from, to: endLine.to, ranges: [range] });
        }
        upto = endLine.number + 1;
    }
    return blocks;
}
function moveLine(state$1, dispatch, forward) {
    if (state$1.readOnly)
        return false;
    let changes = [], ranges = [];
    for (let block of selectedLineBlocks(state$1)) {
        if (forward ? block.to == state$1.doc.length : block.from == 0)
            continue;
        let nextLine = state$1.doc.lineAt(forward ? block.to + 1 : block.from - 1);
        let size = nextLine.length + 1;
        if (forward) {
            changes.push({ from: block.to, to: nextLine.to }, { from: block.from, insert: nextLine.text + state$1.lineBreak });
            for (let r of block.ranges)
                ranges.push(state.EditorSelection.range(Math.min(state$1.doc.length, r.anchor + size), Math.min(state$1.doc.length, r.head + size)));
        }
        else {
            changes.push({ from: nextLine.from, to: block.from }, { from: block.to, insert: state$1.lineBreak + nextLine.text });
            for (let r of block.ranges)
                ranges.push(state.EditorSelection.range(r.anchor - size, r.head - size));
        }
    }
    if (!changes.length)
        return false;
    dispatch(state$1.update({
        changes,
        scrollIntoView: true,
        selection: state.EditorSelection.create(ranges, state$1.selection.mainIndex),
        userEvent: "move.line"
    }));
    return true;
}
/**
Move the selected lines up one line.
*/
const moveLineUp = ({ state, dispatch }) => moveLine(state, dispatch, false);
/**
Move the selected lines down one line.
*/
const moveLineDown = ({ state, dispatch }) => moveLine(state, dispatch, true);
function copyLine(state, dispatch, forward) {
    if (state.readOnly)
        return false;
    let changes = [];
    for (let block of selectedLineBlocks(state)) {
        if (forward)
            changes.push({ from: block.from, insert: state.doc.slice(block.from, block.to) + state.lineBreak });
        else
            changes.push({ from: block.to, insert: state.lineBreak + state.doc.slice(block.from, block.to) });
    }
    dispatch(state.update({ changes, scrollIntoView: true, userEvent: "input.copyline" }));
    return true;
}
/**
Create a copy of the selected lines. Keep the selection in the top copy.
*/
const copyLineUp = ({ state, dispatch }) => copyLine(state, dispatch, false);
/**
Create a copy of the selected lines. Keep the selection in the bottom copy.
*/
const copyLineDown = ({ state, dispatch }) => copyLine(state, dispatch, true);
/**
Delete selected lines.
*/
const deleteLine = view => {
    if (view.state.readOnly)
        return false;
    let { state } = view, changes = state.changes(selectedLineBlocks(state).map(({ from, to }) => {
        if (from > 0)
            from--;
        else if (to < state.doc.length)
            to++;
        return { from, to };
    }));
    let selection = updateSel(state.selection, range => view.moveVertically(range, true)).map(changes);
    view.dispatch({ changes, selection, scrollIntoView: true, userEvent: "delete.line" });
    return true;
};
/**
Replace the selection with a newline.
*/
const insertNewline = ({ state, dispatch }) => {
    dispatch(state.update(state.replaceSelection(state.lineBreak), { scrollIntoView: true, userEvent: "input" }));
    return true;
};
function isBetweenBrackets(state, pos) {
    if (/\(\)|\[\]|\{\}/.test(state.sliceDoc(pos - 1, pos + 1)))
        return { from: pos, to: pos };
    let context = language.syntaxTree(state).resolveInner(pos);
    let before = context.childBefore(pos), after = context.childAfter(pos), closedBy;
    if (before && after && before.to <= pos && after.from >= pos &&
        (closedBy = before.type.prop(common.NodeProp.closedBy)) && closedBy.indexOf(after.name) > -1 &&
        state.doc.lineAt(before.to).from == state.doc.lineAt(after.from).from)
        return { from: before.to, to: after.from };
    return null;
}
/**
Replace the selection with a newline and indent the newly created
line(s). If the current line consists only of whitespace, this
will also delete that whitespace. When the cursor is between
matching brackets, an additional newline will be inserted after
the cursor.
*/
const insertNewlineAndIndent = newlineAndIndent(false);
/**
Create a blank, indented line below the current line.
*/
const insertBlankLine = newlineAndIndent(true);
function newlineAndIndent(atEof) {
    return ({ state: state$1, dispatch }) => {
        if (state$1.readOnly)
            return false;
        let changes = state$1.changeByRange(range => {
            let { from, to } = range, line = state$1.doc.lineAt(from);
            let explode = !atEof && from == to && isBetweenBrackets(state$1, from);
            if (atEof)
                from = to = (to <= line.to ? line : state$1.doc.lineAt(to)).to;
            let cx = new language.IndentContext(state$1, { simulateBreak: from, simulateDoubleBreak: !!explode });
            let indent = language.getIndentation(cx, from);
            if (indent == null)
                indent = /^\s*/.exec(state$1.doc.lineAt(from).text)[0].length;
            while (to < line.to && /\s/.test(line.text[to - line.from]))
                to++;
            if (explode)
                ({ from, to } = explode);
            else if (from > line.from && from < line.from + 100 && !/\S/.test(line.text.slice(0, from)))
                from = line.from;
            let insert = ["", language.indentString(state$1, indent)];
            if (explode)
                insert.push(language.indentString(state$1, cx.lineIndent(line.from, -1)));
            return { changes: { from, to, insert: text.Text.of(insert) },
                range: state.EditorSelection.cursor(from + 1 + insert[1].length) };
        });
        dispatch(state$1.update(changes, { scrollIntoView: true, userEvent: "input" }));
        return true;
    };
}
function changeBySelectedLine(state$1, f) {
    let atLine = -1;
    return state$1.changeByRange(range => {
        let changes = [];
        for (let pos = range.from; pos <= range.to;) {
            let line = state$1.doc.lineAt(pos);
            if (line.number > atLine && (range.empty || range.to > line.from)) {
                f(line, changes, range);
                atLine = line.number;
            }
            pos = line.to + 1;
        }
        let changeSet = state$1.changes(changes);
        return { changes,
            range: state.EditorSelection.range(changeSet.mapPos(range.anchor, 1), changeSet.mapPos(range.head, 1)) };
    });
}
/**
Auto-indent the selected lines. This uses the [indentation service
facet](https://codemirror.net/6/docs/ref/#language.indentService) as source for auto-indent
information.
*/
const indentSelection = ({ state, dispatch }) => {
    if (state.readOnly)
        return false;
    let updated = Object.create(null);
    let context = new language.IndentContext(state, { overrideIndentation: start => {
            let found = updated[start];
            return found == null ? -1 : found;
        } });
    let changes = changeBySelectedLine(state, (line, changes, range) => {
        let indent = language.getIndentation(context, line.from);
        if (indent == null)
            return;
        if (!/\S/.test(line.text))
            indent = 0;
        let cur = /^\s*/.exec(line.text)[0];
        let norm = language.indentString(state, indent);
        if (cur != norm || range.from < line.from + cur.length) {
            updated[line.from] = indent;
            changes.push({ from: line.from, to: line.from + cur.length, insert: norm });
        }
    });
    if (!changes.changes.empty)
        dispatch(state.update(changes, { userEvent: "indent" }));
    return true;
};
/**
Add a [unit](https://codemirror.net/6/docs/ref/#language.indentUnit) of indentation to all selected
lines.
*/
const indentMore = ({ state, dispatch }) => {
    if (state.readOnly)
        return false;
    dispatch(state.update(changeBySelectedLine(state, (line, changes) => {
        changes.push({ from: line.from, insert: state.facet(language.indentUnit) });
    }), { userEvent: "input.indent" }));
    return true;
};
/**
Remove a [unit](https://codemirror.net/6/docs/ref/#language.indentUnit) of indentation from all
selected lines.
*/
const indentLess = ({ state, dispatch }) => {
    if (state.readOnly)
        return false;
    dispatch(state.update(changeBySelectedLine(state, (line, changes) => {
        let space = /^\s*/.exec(line.text)[0];
        if (!space)
            return;
        let col = text.countColumn(space, state.tabSize), keep = 0;
        let insert = language.indentString(state, Math.max(0, col - language.getIndentUnit(state)));
        while (keep < space.length && keep < insert.length && space.charCodeAt(keep) == insert.charCodeAt(keep))
            keep++;
        changes.push({ from: line.from + keep, to: line.from + space.length, insert: insert.slice(keep) });
    }), { userEvent: "delete.dedent" }));
    return true;
};
/**
Insert a tab character at the cursor or, if something is selected,
use [`indentMore`](https://codemirror.net/6/docs/ref/#commands.indentMore) to indent the entire
selection.
*/
const insertTab = ({ state, dispatch }) => {
    if (state.selection.ranges.some(r => !r.empty))
        return indentMore({ state, dispatch });
    dispatch(state.update(state.replaceSelection("\t"), { scrollIntoView: true, userEvent: "input" }));
    return true;
};
/**
Array of key bindings containing the Emacs-style bindings that are
available on macOS by default.

 - Ctrl-b: [`cursorCharLeft`](https://codemirror.net/6/docs/ref/#commands.cursorCharLeft) ([`selectCharLeft`](https://codemirror.net/6/docs/ref/#commands.selectCharLeft) with Shift)
 - Ctrl-f: [`cursorCharRight`](https://codemirror.net/6/docs/ref/#commands.cursorCharRight) ([`selectCharRight`](https://codemirror.net/6/docs/ref/#commands.selectCharRight) with Shift)
 - Ctrl-p: [`cursorLineUp`](https://codemirror.net/6/docs/ref/#commands.cursorLineUp) ([`selectLineUp`](https://codemirror.net/6/docs/ref/#commands.selectLineUp) with Shift)
 - Ctrl-n: [`cursorLineDown`](https://codemirror.net/6/docs/ref/#commands.cursorLineDown) ([`selectLineDown`](https://codemirror.net/6/docs/ref/#commands.selectLineDown) with Shift)
 - Ctrl-a: [`cursorLineStart`](https://codemirror.net/6/docs/ref/#commands.cursorLineStart) ([`selectLineStart`](https://codemirror.net/6/docs/ref/#commands.selectLineStart) with Shift)
 - Ctrl-e: [`cursorLineEnd`](https://codemirror.net/6/docs/ref/#commands.cursorLineEnd) ([`selectLineEnd`](https://codemirror.net/6/docs/ref/#commands.selectLineEnd) with Shift)
 - Ctrl-d: [`deleteCharForward`](https://codemirror.net/6/docs/ref/#commands.deleteCharForward)
 - Ctrl-h: [`deleteCharBackward`](https://codemirror.net/6/docs/ref/#commands.deleteCharBackward)
 - Ctrl-k: [`deleteToLineEnd`](https://codemirror.net/6/docs/ref/#commands.deleteToLineEnd)
 - Ctrl-Alt-h: [`deleteGroupBackward`](https://codemirror.net/6/docs/ref/#commands.deleteGroupBackward)
 - Ctrl-o: [`splitLine`](https://codemirror.net/6/docs/ref/#commands.splitLine)
 - Ctrl-t: [`transposeChars`](https://codemirror.net/6/docs/ref/#commands.transposeChars)
 - Ctrl-v: [`cursorPageDown`](https://codemirror.net/6/docs/ref/#commands.cursorPageDown)
 - Alt-v: [`cursorPageUp`](https://codemirror.net/6/docs/ref/#commands.cursorPageUp)
*/
const emacsStyleKeymap = [
    { key: "Ctrl-b", run: cursorCharLeft, shift: selectCharLeft, preventDefault: true },
    { key: "Ctrl-f", run: cursorCharRight, shift: selectCharRight },
    { key: "Ctrl-p", run: cursorLineUp, shift: selectLineUp },
    { key: "Ctrl-n", run: cursorLineDown, shift: selectLineDown },
    { key: "Ctrl-a", run: cursorLineStart, shift: selectLineStart },
    { key: "Ctrl-e", run: cursorLineEnd, shift: selectLineEnd },
    { key: "Ctrl-d", run: deleteCharForward },
    { key: "Ctrl-h", run: deleteCharBackward },
    { key: "Ctrl-k", run: deleteToLineEnd },
    { key: "Ctrl-Alt-h", run: deleteGroupBackward },
    { key: "Ctrl-o", run: splitLine },
    { key: "Ctrl-t", run: transposeChars },
    { key: "Ctrl-v", run: cursorPageDown },
];
/**
An array of key bindings closely sticking to platform-standard or
widely used bindings. (This includes the bindings from
[`emacsStyleKeymap`](https://codemirror.net/6/docs/ref/#commands.emacsStyleKeymap), with their `key`
property changed to `mac`.)

 - ArrowLeft: [`cursorCharLeft`](https://codemirror.net/6/docs/ref/#commands.cursorCharLeft) ([`selectCharLeft`](https://codemirror.net/6/docs/ref/#commands.selectCharLeft) with Shift)
 - ArrowRight: [`cursorCharRight`](https://codemirror.net/6/docs/ref/#commands.cursorCharRight) ([`selectCharRight`](https://codemirror.net/6/docs/ref/#commands.selectCharRight) with Shift)
 - Ctrl-ArrowLeft (Alt-ArrowLeft on macOS): [`cursorGroupLeft`](https://codemirror.net/6/docs/ref/#commands.cursorGroupLeft) ([`selectGroupLeft`](https://codemirror.net/6/docs/ref/#commands.selectGroupLeft) with Shift)
 - Ctrl-ArrowRight (Alt-ArrowRight on macOS): [`cursorGroupRight`](https://codemirror.net/6/docs/ref/#commands.cursorGroupRight) ([`selectGroupRight`](https://codemirror.net/6/docs/ref/#commands.selectGroupRight) with Shift)
 - Cmd-ArrowLeft (on macOS): [`cursorLineStart`](https://codemirror.net/6/docs/ref/#commands.cursorLineStart) ([`selectLineStart`](https://codemirror.net/6/docs/ref/#commands.selectLineStart) with Shift)
 - Cmd-ArrowRight (on macOS): [`cursorLineEnd`](https://codemirror.net/6/docs/ref/#commands.cursorLineEnd) ([`selectLineEnd`](https://codemirror.net/6/docs/ref/#commands.selectLineEnd) with Shift)
 - ArrowUp: [`cursorLineUp`](https://codemirror.net/6/docs/ref/#commands.cursorLineUp) ([`selectLineUp`](https://codemirror.net/6/docs/ref/#commands.selectLineUp) with Shift)
 - ArrowDown: [`cursorLineDown`](https://codemirror.net/6/docs/ref/#commands.cursorLineDown) ([`selectLineDown`](https://codemirror.net/6/docs/ref/#commands.selectLineDown) with Shift)
 - Cmd-ArrowUp (on macOS): [`cursorDocStart`](https://codemirror.net/6/docs/ref/#commands.cursorDocStart) ([`selectDocStart`](https://codemirror.net/6/docs/ref/#commands.selectDocStart) with Shift)
 - Cmd-ArrowDown (on macOS): [`cursorDocEnd`](https://codemirror.net/6/docs/ref/#commands.cursorDocEnd) ([`selectDocEnd`](https://codemirror.net/6/docs/ref/#commands.selectDocEnd) with Shift)
 - Ctrl-ArrowUp (on macOS): [`cursorPageUp`](https://codemirror.net/6/docs/ref/#commands.cursorPageUp) ([`selectPageUp`](https://codemirror.net/6/docs/ref/#commands.selectPageUp) with Shift)
 - Ctrl-ArrowDown (on macOS): [`cursorPageDown`](https://codemirror.net/6/docs/ref/#commands.cursorPageDown) ([`selectPageDown`](https://codemirror.net/6/docs/ref/#commands.selectPageDown) with Shift)
 - PageUp: [`cursorPageUp`](https://codemirror.net/6/docs/ref/#commands.cursorPageUp) ([`selectPageUp`](https://codemirror.net/6/docs/ref/#commands.selectPageUp) with Shift)
 - PageDown: [`cursorPageDown`](https://codemirror.net/6/docs/ref/#commands.cursorPageDown) ([`selectPageDown`](https://codemirror.net/6/docs/ref/#commands.selectPageDown) with Shift)
 - Home: [`cursorLineBoundaryBackward`](https://codemirror.net/6/docs/ref/#commands.cursorLineBoundaryBackward) ([`selectLineBoundaryBackward`](https://codemirror.net/6/docs/ref/#commands.selectLineBoundaryBackward) with Shift)
 - End: [`cursorLineBoundaryForward`](https://codemirror.net/6/docs/ref/#commands.cursorLineBoundaryForward) ([`selectLineBoundaryForward`](https://codemirror.net/6/docs/ref/#commands.selectLineBoundaryForward) with Shift)
 - Ctrl-Home (Cmd-Home on macOS): [`cursorDocStart`](https://codemirror.net/6/docs/ref/#commands.cursorDocStart) ([`selectDocStart`](https://codemirror.net/6/docs/ref/#commands.selectDocStart) with Shift)
 - Ctrl-End (Cmd-Home on macOS): [`cursorDocEnd`](https://codemirror.net/6/docs/ref/#commands.cursorDocEnd) ([`selectDocEnd`](https://codemirror.net/6/docs/ref/#commands.selectDocEnd) with Shift)
 - Enter: [`insertNewlineAndIndent`](https://codemirror.net/6/docs/ref/#commands.insertNewlineAndIndent)
 - Ctrl-a (Cmd-a on macOS): [`selectAll`](https://codemirror.net/6/docs/ref/#commands.selectAll)
 - Backspace: [`deleteCharBackward`](https://codemirror.net/6/docs/ref/#commands.deleteCharBackward)
 - Delete: [`deleteCharForward`](https://codemirror.net/6/docs/ref/#commands.deleteCharForward)
 - Ctrl-Backspace (Alt-Backspace on macOS): [`deleteGroupBackward`](https://codemirror.net/6/docs/ref/#commands.deleteGroupBackward)
 - Ctrl-Delete (Alt-Delete on macOS): [`deleteGroupForward`](https://codemirror.net/6/docs/ref/#commands.deleteGroupForward)
 - Cmd-Backspace (macOS): [`deleteToLineStart`](https://codemirror.net/6/docs/ref/#commands.deleteToLineStart).
 - Cmd-Delete (macOS): [`deleteToLineEnd`](https://codemirror.net/6/docs/ref/#commands.deleteToLineEnd).
*/
const standardKeymap = [
    { key: "ArrowLeft", run: cursorCharLeft, shift: selectCharLeft, preventDefault: true },
    { key: "Mod-ArrowLeft", mac: "Alt-ArrowLeft", run: cursorGroupLeft, shift: selectGroupLeft },
    { mac: "Cmd-ArrowLeft", run: cursorLineBoundaryBackward, shift: selectLineBoundaryBackward },
    { key: "ArrowRight", run: cursorCharRight, shift: selectCharRight, preventDefault: true },
    { key: "Mod-ArrowRight", mac: "Alt-ArrowRight", run: cursorGroupRight, shift: selectGroupRight },
    { mac: "Cmd-ArrowRight", run: cursorLineBoundaryForward, shift: selectLineBoundaryForward },
    { key: "ArrowUp", run: cursorLineUp, shift: selectLineUp, preventDefault: true },
    { mac: "Cmd-ArrowUp", run: cursorDocStart, shift: selectDocStart },
    { mac: "Ctrl-ArrowUp", run: cursorPageUp, shift: selectPageUp },
    { key: "ArrowDown", run: cursorLineDown, shift: selectLineDown, preventDefault: true },
    { mac: "Cmd-ArrowDown", run: cursorDocEnd, shift: selectDocEnd },
    { mac: "Ctrl-ArrowDown", run: cursorPageDown, shift: selectPageDown },
    { key: "PageUp", run: cursorPageUp, shift: selectPageUp },
    { key: "PageDown", run: cursorPageDown, shift: selectPageDown },
    { key: "Home", run: cursorLineBoundaryBackward, shift: selectLineBoundaryBackward },
    { key: "Mod-Home", run: cursorDocStart, shift: selectDocStart },
    { key: "End", run: cursorLineBoundaryForward, shift: selectLineBoundaryForward },
    { key: "Mod-End", run: cursorDocEnd, shift: selectDocEnd },
    { key: "Enter", run: insertNewlineAndIndent },
    { key: "Mod-a", run: selectAll },
    { key: "Backspace", run: deleteCharBackward, shift: deleteCharBackward },
    { key: "Delete", run: deleteCharForward },
    { key: "Mod-Backspace", mac: "Alt-Backspace", run: deleteGroupBackward },
    { key: "Mod-Delete", mac: "Alt-Delete", run: deleteGroupForward },
    { mac: "Mod-Backspace", run: deleteToLineStart },
    { mac: "Mod-Delete", run: deleteToLineEnd }
].concat(emacsStyleKeymap.map(b => ({ mac: b.key, run: b.run, shift: b.shift })));
/**
The default keymap. Includes all bindings from
[`standardKeymap`](https://codemirror.net/6/docs/ref/#commands.standardKeymap) plus the following:

- Alt-ArrowLeft (Ctrl-ArrowLeft on macOS): [`cursorSyntaxLeft`](https://codemirror.net/6/docs/ref/#commands.cursorSyntaxLeft) ([`selectSyntaxLeft`](https://codemirror.net/6/docs/ref/#commands.selectSyntaxLeft) with Shift)
- Alt-ArrowRight (Ctrl-ArrowRight on macOS): [`cursorSyntaxRight`](https://codemirror.net/6/docs/ref/#commands.cursorSyntaxRight) ([`selectSyntaxRight`](https://codemirror.net/6/docs/ref/#commands.selectSyntaxRight) with Shift)
- Alt-ArrowUp: [`moveLineUp`](https://codemirror.net/6/docs/ref/#commands.moveLineUp)
- Alt-ArrowDown: [`moveLineDown`](https://codemirror.net/6/docs/ref/#commands.moveLineDown)
- Shift-Alt-ArrowUp: [`copyLineUp`](https://codemirror.net/6/docs/ref/#commands.copyLineUp)
- Shift-Alt-ArrowDown: [`copyLineDown`](https://codemirror.net/6/docs/ref/#commands.copyLineDown)
- Escape: [`simplifySelection`](https://codemirror.net/6/docs/ref/#commands.simplifySelection)
- Ctrl-Enter (Comd-Enter on macOS): [`insertBlankLine`](https://codemirror.net/6/docs/ref/#commands.insertBlankLine)
- Alt-l (Ctrl-l on macOS): [`selectLine`](https://codemirror.net/6/docs/ref/#commands.selectLine)
- Ctrl-i (Cmd-i on macOS): [`selectParentSyntax`](https://codemirror.net/6/docs/ref/#commands.selectParentSyntax)
- Ctrl-[ (Cmd-[ on macOS): [`indentLess`](https://codemirror.net/6/docs/ref/#commands.indentLess)
- Ctrl-] (Cmd-] on macOS): [`indentMore`](https://codemirror.net/6/docs/ref/#commands.indentMore)
- Ctrl-Alt-\\ (Cmd-Alt-\\ on macOS): [`indentSelection`](https://codemirror.net/6/docs/ref/#commands.indentSelection)
- Shift-Ctrl-k (Shift-Cmd-k on macOS): [`deleteLine`](https://codemirror.net/6/docs/ref/#commands.deleteLine)
- Shift-Ctrl-\\ (Shift-Cmd-\\ on macOS): [`cursorMatchingBracket`](https://codemirror.net/6/docs/ref/#commands.cursorMatchingBracket)
*/
const defaultKeymap = [
    { key: "Alt-ArrowLeft", mac: "Ctrl-ArrowLeft", run: cursorSyntaxLeft, shift: selectSyntaxLeft },
    { key: "Alt-ArrowRight", mac: "Ctrl-ArrowRight", run: cursorSyntaxRight, shift: selectSyntaxRight },
    { key: "Alt-ArrowUp", run: moveLineUp },
    { key: "Shift-Alt-ArrowUp", run: copyLineUp },
    { key: "Alt-ArrowDown", run: moveLineDown },
    { key: "Shift-Alt-ArrowDown", run: copyLineDown },
    { key: "Escape", run: simplifySelection },
    { key: "Mod-Enter", run: insertBlankLine },
    { key: "Alt-l", mac: "Ctrl-l", run: selectLine },
    { key: "Mod-i", run: selectParentSyntax, preventDefault: true },
    { key: "Mod-[", run: indentLess },
    { key: "Mod-]", run: indentMore },
    { key: "Mod-Alt-\\", run: indentSelection },
    { key: "Shift-Mod-k", run: deleteLine },
    { key: "Shift-Mod-\\", run: cursorMatchingBracket }
].concat(standardKeymap);
/**
A binding that binds Tab to [`indentMore`](https://codemirror.net/6/docs/ref/#commands.indentMore) and
Shift-Tab to [`indentLess`](https://codemirror.net/6/docs/ref/#commands.indentLess).
Please see the [Tab example](../../examples/tab/) before using
this.
*/
const indentWithTab = { key: "Tab", run: indentMore, shift: indentLess };

exports.copyLineDown = copyLineDown;
exports.copyLineUp = copyLineUp;
exports.cursorCharBackward = cursorCharBackward;
exports.cursorCharForward = cursorCharForward;
exports.cursorCharLeft = cursorCharLeft;
exports.cursorCharRight = cursorCharRight;
exports.cursorDocEnd = cursorDocEnd;
exports.cursorDocStart = cursorDocStart;
exports.cursorGroupBackward = cursorGroupBackward;
exports.cursorGroupForward = cursorGroupForward;
exports.cursorGroupLeft = cursorGroupLeft;
exports.cursorGroupRight = cursorGroupRight;
exports.cursorLineBoundaryBackward = cursorLineBoundaryBackward;
exports.cursorLineBoundaryForward = cursorLineBoundaryForward;
exports.cursorLineDown = cursorLineDown;
exports.cursorLineEnd = cursorLineEnd;
exports.cursorLineStart = cursorLineStart;
exports.cursorLineUp = cursorLineUp;
exports.cursorMatchingBracket = cursorMatchingBracket;
exports.cursorPageDown = cursorPageDown;
exports.cursorPageUp = cursorPageUp;
exports.cursorSubwordBackward = cursorSubwordBackward;
exports.cursorSubwordForward = cursorSubwordForward;
exports.cursorSyntaxLeft = cursorSyntaxLeft;
exports.cursorSyntaxRight = cursorSyntaxRight;
exports.defaultKeymap = defaultKeymap;
exports.deleteCharBackward = deleteCharBackward;
exports.deleteCharForward = deleteCharForward;
exports.deleteGroupBackward = deleteGroupBackward;
exports.deleteGroupForward = deleteGroupForward;
exports.deleteLine = deleteLine;
exports.deleteToLineEnd = deleteToLineEnd;
exports.deleteToLineStart = deleteToLineStart;
exports.deleteTrailingWhitespace = deleteTrailingWhitespace;
exports.emacsStyleKeymap = emacsStyleKeymap;
exports.indentLess = indentLess;
exports.indentMore = indentMore;
exports.indentSelection = indentSelection;
exports.indentWithTab = indentWithTab;
exports.insertBlankLine = insertBlankLine;
exports.insertNewline = insertNewline;
exports.insertNewlineAndIndent = insertNewlineAndIndent;
exports.insertTab = insertTab;
exports.moveLineDown = moveLineDown;
exports.moveLineUp = moveLineUp;
exports.selectAll = selectAll;
exports.selectCharBackward = selectCharBackward;
exports.selectCharForward = selectCharForward;
exports.selectCharLeft = selectCharLeft;
exports.selectCharRight = selectCharRight;
exports.selectDocEnd = selectDocEnd;
exports.selectDocStart = selectDocStart;
exports.selectGroupBackward = selectGroupBackward;
exports.selectGroupForward = selectGroupForward;
exports.selectGroupLeft = selectGroupLeft;
exports.selectGroupRight = selectGroupRight;
exports.selectLine = selectLine;
exports.selectLineBoundaryBackward = selectLineBoundaryBackward;
exports.selectLineBoundaryForward = selectLineBoundaryForward;
exports.selectLineDown = selectLineDown;
exports.selectLineEnd = selectLineEnd;
exports.selectLineStart = selectLineStart;
exports.selectLineUp = selectLineUp;
exports.selectMatchingBracket = selectMatchingBracket;
exports.selectPageDown = selectPageDown;
exports.selectPageUp = selectPageUp;
exports.selectParentSyntax = selectParentSyntax;
exports.selectSubwordBackward = selectSubwordBackward;
exports.selectSubwordForward = selectSubwordForward;
exports.selectSyntaxLeft = selectSyntaxLeft;
exports.selectSyntaxRight = selectSyntaxRight;
exports.simplifySelection = simplifySelection;
exports.splitLine = splitLine;
exports.standardKeymap = standardKeymap;
exports.transposeChars = transposeChars;
