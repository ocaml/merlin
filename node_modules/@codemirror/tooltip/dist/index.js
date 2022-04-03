import { ViewPlugin, Direction, EditorView, logException } from '@codemirror/view';
import { Facet, StateEffect, StateField, MapMode } from '@codemirror/state';

const ios = typeof navigator != "undefined" &&
    !/*@__PURE__*//Edge\/(\d+)/.exec(navigator.userAgent) && /*@__PURE__*//Apple Computer/.test(navigator.vendor) &&
    (/*@__PURE__*//Mobile\/\w+/.test(navigator.userAgent) || navigator.maxTouchPoints > 2);
const Outside = "-10000px";
class TooltipViewManager {
    constructor(view, facet, createTooltipView) {
        this.facet = facet;
        this.createTooltipView = createTooltipView;
        this.input = view.state.facet(facet);
        this.tooltips = this.input.filter(t => t);
        this.tooltipViews = this.tooltips.map(createTooltipView);
    }
    update(update) {
        let input = update.state.facet(this.facet);
        let tooltips = input.filter(x => x);
        if (input === this.input) {
            for (let t of this.tooltipViews)
                if (t.update)
                    t.update(update);
            return false;
        }
        let tooltipViews = [];
        for (let i = 0; i < tooltips.length; i++) {
            let tip = tooltips[i], known = -1;
            if (!tip)
                continue;
            for (let i = 0; i < this.tooltips.length; i++) {
                let other = this.tooltips[i];
                if (other && other.create == tip.create)
                    known = i;
            }
            if (known < 0) {
                tooltipViews[i] = this.createTooltipView(tip);
            }
            else {
                let tooltipView = tooltipViews[i] = this.tooltipViews[known];
                if (tooltipView.update)
                    tooltipView.update(update);
            }
        }
        for (let t of this.tooltipViews)
            if (tooltipViews.indexOf(t) < 0)
                t.dom.remove();
        this.input = input;
        this.tooltips = tooltips;
        this.tooltipViews = tooltipViews;
        return true;
    }
}
/**
Return an extension that configures tooltip behavior.
*/
function tooltips(config = {}) {
    return tooltipConfig.of(config);
}
function windowSpace() {
    return { top: 0, left: 0, bottom: innerHeight, right: innerWidth };
}
const tooltipConfig = /*@__PURE__*/Facet.define({
    combine: values => {
        var _a, _b, _c;
        return ({
            position: ios ? "absolute" : ((_a = values.find(conf => conf.position)) === null || _a === void 0 ? void 0 : _a.position) || "fixed",
            parent: ((_b = values.find(conf => conf.parent)) === null || _b === void 0 ? void 0 : _b.parent) || null,
            tooltipSpace: ((_c = values.find(conf => conf.tooltipSpace)) === null || _c === void 0 ? void 0 : _c.tooltipSpace) || windowSpace,
        });
    }
});
const tooltipPlugin = /*@__PURE__*/ViewPlugin.fromClass(class {
    constructor(view) {
        var _a;
        this.view = view;
        this.inView = true;
        this.lastTransaction = 0;
        this.measureTimeout = -1;
        let config = view.state.facet(tooltipConfig);
        this.position = config.position;
        this.parent = config.parent;
        this.classes = view.themeClasses;
        this.createContainer();
        this.measureReq = { read: this.readMeasure.bind(this), write: this.writeMeasure.bind(this), key: this };
        this.manager = new TooltipViewManager(view, showTooltip, t => this.createTooltip(t));
        this.intersectionObserver = typeof IntersectionObserver == "function" ? new IntersectionObserver(entries => {
            if (Date.now() > this.lastTransaction - 50 &&
                entries.length > 0 && entries[entries.length - 1].intersectionRatio < 1)
                this.measureSoon();
        }, { threshold: [1] }) : null;
        this.observeIntersection();
        (_a = view.dom.ownerDocument.defaultView) === null || _a === void 0 ? void 0 : _a.addEventListener("resize", this.measureSoon = this.measureSoon.bind(this));
        this.maybeMeasure();
    }
    createContainer() {
        if (this.parent) {
            this.container = document.createElement("div");
            this.container.style.position = "relative";
            this.container.className = this.view.themeClasses;
            this.parent.appendChild(this.container);
        }
        else {
            this.container = this.view.dom;
        }
    }
    observeIntersection() {
        if (this.intersectionObserver) {
            this.intersectionObserver.disconnect();
            for (let tooltip of this.manager.tooltipViews)
                this.intersectionObserver.observe(tooltip.dom);
        }
    }
    measureSoon() {
        if (this.measureTimeout < 0)
            this.measureTimeout = setTimeout(() => {
                this.measureTimeout = -1;
                this.maybeMeasure();
            }, 50);
    }
    update(update) {
        if (update.transactions.length)
            this.lastTransaction = Date.now();
        let updated = this.manager.update(update);
        if (updated)
            this.observeIntersection();
        let shouldMeasure = updated || update.geometryChanged;
        let newConfig = update.state.facet(tooltipConfig);
        if (newConfig.position != this.position) {
            this.position = newConfig.position;
            for (let t of this.manager.tooltipViews)
                t.dom.style.position = this.position;
            shouldMeasure = true;
        }
        if (newConfig.parent != this.parent) {
            if (this.parent)
                this.container.remove();
            this.parent = newConfig.parent;
            this.createContainer();
            for (let t of this.manager.tooltipViews)
                this.container.appendChild(t.dom);
            shouldMeasure = true;
        }
        else if (this.parent && this.view.themeClasses != this.classes) {
            this.classes = this.container.className = this.view.themeClasses;
        }
        if (shouldMeasure)
            this.maybeMeasure();
    }
    createTooltip(tooltip) {
        let tooltipView = tooltip.create(this.view);
        tooltipView.dom.classList.add("cm-tooltip");
        if (tooltip.arrow && !tooltipView.dom.querySelector(".cm-tooltip > .cm-tooltip-arrow")) {
            let arrow = document.createElement("div");
            arrow.className = "cm-tooltip-arrow";
            tooltipView.dom.appendChild(arrow);
        }
        tooltipView.dom.style.position = this.position;
        tooltipView.dom.style.top = Outside;
        this.container.appendChild(tooltipView.dom);
        if (tooltipView.mount)
            tooltipView.mount(this.view);
        return tooltipView;
    }
    destroy() {
        var _a, _b;
        (_a = this.view.dom.ownerDocument.defaultView) === null || _a === void 0 ? void 0 : _a.removeEventListener("resize", this.measureSoon);
        for (let { dom } of this.manager.tooltipViews)
            dom.remove();
        (_b = this.intersectionObserver) === null || _b === void 0 ? void 0 : _b.disconnect();
        clearTimeout(this.measureTimeout);
    }
    readMeasure() {
        let editor = this.view.dom.getBoundingClientRect();
        return {
            editor,
            parent: this.parent ? this.container.getBoundingClientRect() : editor,
            pos: this.manager.tooltips.map((t, i) => {
                let tv = this.manager.tooltipViews[i];
                return tv.getCoords ? tv.getCoords(t.pos) : this.view.coordsAtPos(t.pos);
            }),
            size: this.manager.tooltipViews.map(({ dom }) => dom.getBoundingClientRect()),
            space: this.view.state.facet(tooltipConfig).tooltipSpace(this.view),
        };
    }
    writeMeasure(measured) {
        let { editor, space } = measured;
        let others = [];
        for (let i = 0; i < this.manager.tooltips.length; i++) {
            let tooltip = this.manager.tooltips[i], tView = this.manager.tooltipViews[i], { dom } = tView;
            let pos = measured.pos[i], size = measured.size[i];
            // Hide tooltips that are outside of the editor.
            if (!pos || pos.bottom <= Math.max(editor.top, space.top) ||
                pos.top >= Math.min(editor.bottom, space.bottom) ||
                pos.right < Math.max(editor.left, space.left) - .1 ||
                pos.left > Math.min(editor.right, space.right) + .1) {
                dom.style.top = Outside;
                continue;
            }
            let arrow = tooltip.arrow ? tView.dom.querySelector(".cm-tooltip-arrow") : null;
            let arrowHeight = arrow ? 7 /* Size */ : 0;
            let width = size.right - size.left, height = size.bottom - size.top;
            let offset = tView.offset || noOffset, ltr = this.view.textDirection == Direction.LTR;
            let left = size.width > space.right - space.left ? (ltr ? space.left : space.right - size.width)
                : ltr ? Math.min(pos.left - (arrow ? 14 /* Offset */ : 0) + offset.x, space.right - width)
                    : Math.max(space.left, pos.left - width + (arrow ? 14 /* Offset */ : 0) - offset.x);
            let above = !!tooltip.above;
            if (!tooltip.strictSide && (above
                ? pos.top - (size.bottom - size.top) - offset.y < space.top
                : pos.bottom + (size.bottom - size.top) + offset.y > space.bottom) &&
                above == (space.bottom - pos.bottom > pos.top - space.top))
                above = !above;
            let top = above ? pos.top - height - arrowHeight - offset.y : pos.bottom + arrowHeight + offset.y;
            let right = left + width;
            if (tView.overlap !== true)
                for (let r of others)
                    if (r.left < right && r.right > left && r.top < top + height && r.bottom > top)
                        top = above ? r.top - height - 2 - arrowHeight : r.bottom + arrowHeight + 2;
            if (this.position == "absolute") {
                dom.style.top = (top - measured.parent.top) + "px";
                dom.style.left = (left - measured.parent.left) + "px";
            }
            else {
                dom.style.top = top + "px";
                dom.style.left = left + "px";
            }
            if (arrow)
                arrow.style.left = `${pos.left + (ltr ? offset.x : -offset.x) - (left + 14 /* Offset */ - 7 /* Size */)}px`;
            if (tView.overlap !== true)
                others.push({ left, top, right, bottom: top + height });
            dom.classList.toggle("cm-tooltip-above", above);
            dom.classList.toggle("cm-tooltip-below", !above);
            if (tView.positioned)
                tView.positioned();
        }
    }
    maybeMeasure() {
        if (this.manager.tooltips.length) {
            if (this.view.inView)
                this.view.requestMeasure(this.measureReq);
            if (this.inView != this.view.inView) {
                this.inView = this.view.inView;
                if (!this.inView)
                    for (let tv of this.manager.tooltipViews)
                        tv.dom.style.top = Outside;
            }
        }
    }
}, {
    eventHandlers: {
        scroll() { this.maybeMeasure(); }
    }
});
const baseTheme = /*@__PURE__*/EditorView.baseTheme({
    ".cm-tooltip": {
        zIndex: 100
    },
    "&light .cm-tooltip": {
        border: "1px solid #bbb",
        backgroundColor: "#f5f5f5"
    },
    "&light .cm-tooltip-section:not(:first-child)": {
        borderTop: "1px solid #bbb",
    },
    "&dark .cm-tooltip": {
        backgroundColor: "#333338",
        color: "white"
    },
    ".cm-tooltip-arrow": {
        height: `${7 /* Size */}px`,
        width: `${7 /* Size */ * 2}px`,
        position: "absolute",
        zIndex: -1,
        overflow: "hidden",
        "&:before, &:after": {
            content: "''",
            position: "absolute",
            width: 0,
            height: 0,
            borderLeft: `${7 /* Size */}px solid transparent`,
            borderRight: `${7 /* Size */}px solid transparent`,
        },
        ".cm-tooltip-above &": {
            bottom: `-${7 /* Size */}px`,
            "&:before": {
                borderTop: `${7 /* Size */}px solid #bbb`,
            },
            "&:after": {
                borderTop: `${7 /* Size */}px solid #f5f5f5`,
                bottom: "1px"
            }
        },
        ".cm-tooltip-below &": {
            top: `-${7 /* Size */}px`,
            "&:before": {
                borderBottom: `${7 /* Size */}px solid #bbb`,
            },
            "&:after": {
                borderBottom: `${7 /* Size */}px solid #f5f5f5`,
                top: "1px"
            }
        },
    },
    "&dark .cm-tooltip .cm-tooltip-arrow": {
        "&:before": {
            borderTopColor: "#333338",
            borderBottomColor: "#333338"
        },
        "&:after": {
            borderTopColor: "transparent",
            borderBottomColor: "transparent"
        }
    }
});
const noOffset = { x: 0, y: 0 };
/**
Behavior by which an extension can provide a tooltip to be shown.
*/
const showTooltip = /*@__PURE__*/Facet.define({
    enables: [tooltipPlugin, baseTheme]
});
const showHoverTooltip = /*@__PURE__*/Facet.define();
class HoverTooltipHost {
    constructor(view) {
        this.view = view;
        this.mounted = false;
        this.dom = document.createElement("div");
        this.dom.classList.add("cm-tooltip-hover");
        this.manager = new TooltipViewManager(view, showHoverTooltip, t => this.createHostedView(t));
    }
    // Needs to be static so that host tooltip instances always match
    static create(view) {
        return new HoverTooltipHost(view);
    }
    createHostedView(tooltip) {
        let hostedView = tooltip.create(this.view);
        hostedView.dom.classList.add("cm-tooltip-section");
        this.dom.appendChild(hostedView.dom);
        if (this.mounted && hostedView.mount)
            hostedView.mount(this.view);
        return hostedView;
    }
    mount(view) {
        for (let hostedView of this.manager.tooltipViews) {
            if (hostedView.mount)
                hostedView.mount(view);
        }
        this.mounted = true;
    }
    positioned() {
        for (let hostedView of this.manager.tooltipViews) {
            if (hostedView.positioned)
                hostedView.positioned();
        }
    }
    update(update) {
        this.manager.update(update);
    }
}
const showHoverTooltipHost = /*@__PURE__*/showTooltip.compute([showHoverTooltip], state => {
    let tooltips = state.facet(showHoverTooltip).filter(t => t);
    if (tooltips.length === 0)
        return null;
    return {
        pos: Math.min(...tooltips.map(t => t.pos)),
        end: Math.max(...tooltips.filter(t => t.end != null).map(t => t.end)),
        create: HoverTooltipHost.create,
        above: tooltips[0].above,
        arrow: tooltips.some(t => t.arrow),
    };
});
class HoverPlugin {
    constructor(view, source, field, setHover, hoverTime) {
        this.view = view;
        this.source = source;
        this.field = field;
        this.setHover = setHover;
        this.hoverTime = hoverTime;
        this.hoverTimeout = -1;
        this.restartTimeout = -1;
        this.pending = null;
        this.lastMove = { x: 0, y: 0, target: view.dom, time: 0 };
        this.checkHover = this.checkHover.bind(this);
        view.dom.addEventListener("mouseleave", this.mouseleave = this.mouseleave.bind(this));
        view.dom.addEventListener("mousemove", this.mousemove = this.mousemove.bind(this));
    }
    update() {
        if (this.pending) {
            this.pending = null;
            clearTimeout(this.restartTimeout);
            this.restartTimeout = setTimeout(() => this.startHover(), 20);
        }
    }
    get active() {
        return this.view.state.field(this.field);
    }
    checkHover() {
        this.hoverTimeout = -1;
        if (this.active)
            return;
        let hovered = Date.now() - this.lastMove.time;
        if (hovered < this.hoverTime)
            this.hoverTimeout = setTimeout(this.checkHover, this.hoverTime - hovered);
        else
            this.startHover();
    }
    startHover() {
        var _a;
        clearTimeout(this.restartTimeout);
        let { lastMove } = this;
        let pos = this.view.contentDOM.contains(lastMove.target) ? this.view.posAtCoords(lastMove) : null;
        if (pos == null)
            return;
        let posCoords = this.view.coordsAtPos(pos);
        if (posCoords == null || lastMove.y < posCoords.top || lastMove.y > posCoords.bottom ||
            lastMove.x < posCoords.left - this.view.defaultCharacterWidth ||
            lastMove.x > posCoords.right + this.view.defaultCharacterWidth)
            return;
        let bidi = this.view.bidiSpans(this.view.state.doc.lineAt(pos)).find(s => s.from <= pos && s.to >= pos);
        let rtl = bidi && bidi.dir == Direction.RTL ? -1 : 1;
        let open = this.source(this.view, pos, (lastMove.x < posCoords.left ? -rtl : rtl));
        if ((_a = open) === null || _a === void 0 ? void 0 : _a.then) {
            let pending = this.pending = { pos };
            open.then(result => {
                if (this.pending == pending) {
                    this.pending = null;
                    if (result)
                        this.view.dispatch({ effects: this.setHover.of(result) });
                }
            }, e => logException(this.view.state, e, "hover tooltip"));
        }
        else if (open) {
            this.view.dispatch({ effects: this.setHover.of(open) });
        }
    }
    mousemove(event) {
        var _a;
        this.lastMove = { x: event.clientX, y: event.clientY, target: event.target, time: Date.now() };
        if (this.hoverTimeout < 0)
            this.hoverTimeout = setTimeout(this.checkHover, this.hoverTime);
        let tooltip = this.active;
        if (tooltip && !isInTooltip(this.lastMove.target) || this.pending) {
            let { pos } = tooltip || this.pending, end = (_a = tooltip === null || tooltip === void 0 ? void 0 : tooltip.end) !== null && _a !== void 0 ? _a : pos;
            if ((pos == end ? this.view.posAtCoords(this.lastMove) != pos
                : !isOverRange(this.view, pos, end, event.clientX, event.clientY, 6 /* MaxDist */))) {
                this.view.dispatch({ effects: this.setHover.of(null) });
                this.pending = null;
            }
        }
    }
    mouseleave() {
        clearTimeout(this.hoverTimeout);
        this.hoverTimeout = -1;
        if (this.active)
            this.view.dispatch({ effects: this.setHover.of(null) });
    }
    destroy() {
        clearTimeout(this.hoverTimeout);
        this.view.dom.removeEventListener("mouseleave", this.mouseleave);
        this.view.dom.removeEventListener("mousemove", this.mousemove);
    }
}
function isInTooltip(elt) {
    for (let cur = elt; cur; cur = cur.parentNode)
        if (cur.nodeType == 1 && cur.classList.contains("cm-tooltip"))
            return true;
    return false;
}
function isOverRange(view, from, to, x, y, margin) {
    let range = document.createRange();
    let fromDOM = view.domAtPos(from), toDOM = view.domAtPos(to);
    range.setEnd(toDOM.node, toDOM.offset);
    range.setStart(fromDOM.node, fromDOM.offset);
    let rects = range.getClientRects();
    range.detach();
    for (let i = 0; i < rects.length; i++) {
        let rect = rects[i];
        let dist = Math.max(rect.top - y, y - rect.bottom, rect.left - x, x - rect.right);
        if (dist <= margin)
            return true;
    }
    return false;
}
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
function hoverTooltip(source, options = {}) {
    let setHover = StateEffect.define();
    let hoverState = StateField.define({
        create() { return null; },
        update(value, tr) {
            if (value && (options.hideOnChange && (tr.docChanged || tr.selection)))
                return null;
            for (let effect of tr.effects) {
                if (effect.is(setHover))
                    return effect.value;
                if (effect.is(closeHoverTooltipEffect))
                    return null;
            }
            if (value && tr.docChanged) {
                let newPos = tr.changes.mapPos(value.pos, -1, MapMode.TrackDel);
                if (newPos == null)
                    return null;
                let copy = Object.assign(Object.create(null), value);
                copy.pos = newPos;
                if (value.end != null)
                    copy.end = tr.changes.mapPos(value.end);
                return copy;
            }
            return value;
        },
        provide: f => showHoverTooltip.from(f)
    });
    return [
        hoverState,
        ViewPlugin.define(view => new HoverPlugin(view, source, hoverState, setHover, options.hoverTime || 300 /* Time */)),
        showHoverTooltipHost
    ];
}
/**
Get the active tooltip view for a given tooltip, if available.
*/
function getTooltip(view, tooltip) {
    let plugin = view.plugin(tooltipPlugin);
    if (!plugin)
        return null;
    let found = plugin.manager.tooltips.indexOf(tooltip);
    return found < 0 ? null : plugin.manager.tooltipViews[found];
}
/**
Returns true if any hover tooltips are currently active.
*/
function hasHoverTooltips(state) {
    return state.facet(showHoverTooltip).some(x => x);
}
const closeHoverTooltipEffect = /*@__PURE__*/StateEffect.define();
/**
Transaction effect that closes all hover tooltips.
*/
const closeHoverTooltips = /*@__PURE__*/closeHoverTooltipEffect.of(null);
/**
Tell the tooltip extension to recompute the position of the active
tooltips. This can be useful when something happens (such as a
re-positioning or CSS change affecting the editor) that could
invalidate the existing tooltip positions.
*/
function repositionTooltips(view) {
    var _a;
    (_a = view.plugin(tooltipPlugin)) === null || _a === void 0 ? void 0 : _a.maybeMeasure();
}

export { closeHoverTooltips, getTooltip, hasHoverTooltips, hoverTooltip, repositionTooltips, showTooltip, tooltips };
