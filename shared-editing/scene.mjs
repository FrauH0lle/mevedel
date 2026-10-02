/* The displayed scene derived from stored elements: Excalidraw defaults,
   bound arrow endpoints, bound text layout and bounds. Browser, host and
   exports share it, so every view resolves the same geometry. */
import { compareOrder } from './model.mjs';
import { layoutText, lineHeightOf } from './text.mjs';

export const LINEAR = ['arrow', 'line'];
export const STROKED = ['arrow', 'line', 'freedraw'];
export const CONTAINERS = ['rectangle', 'diamond', 'ellipse', 'stickynote', 'arrow'];
const BINDABLE = ['rectangle', 'diamond', 'ellipse', 'stickynote', 'text', 'image', 'frame',
  'magicframe', 'embeddable', 'iframe'];
const BASE = { strokeColor: '#1e1e1e', backgroundColor: 'transparent', fillStyle: 'solid',
  strokeWidth: 2, strokeStyle: 'solid', roughness: 1, opacity: 100, angle: 0, roundness: null,
  groupIds: [], frameId: null, link: null, locked: false };
/* A stable roughjs seed for elements stored without one. */
export function seedOf(id) {
  let hash = 0x811c9dc5;
  for (let i = 0; i < id.length; i++) hash = Math.imul(hash ^ id.charCodeAt(i), 0x01000193);
  return (hash >>> 0) % 2 ** 31;
}
/* An element with every absent field at its Excalidraw default. */
export function complete(element, bound = false) {
  const typed = {
    text: () => ({ fontSize: 20, fontFamily: 5, textAlign: bound ? 'center' : 'left',
      verticalAlign: bound ? 'middle' : 'top', containerId: null, autoResize: true,
      lineHeight: lineHeightOf(element.fontFamily ?? 5), originalText: element.text,
      baseFontSize: null }),
    arrow: () => ({ startArrowhead: null, endArrowhead: 'arrow', startBinding: null,
      endBinding: null, elbowed: false }),
    line: () => ({ startArrowhead: null, endArrowhead: null, polygon: false }),
    freedraw: () => ({ pressures: [], simulatePressure: true }),
    image: () => ({ fileId: null, status: 'saved', scale: [1, 1], crop: null }),
    stickynote: () => ({ backgroundColor: '#ffdf6b', baseHeight: element.height }),
    frame: () => ({ name: null }), magicframe: () => ({ name: null }),
  }[element.type]?.() || {};
  const full = { ...BASE, seed: seedOf(element.id), ...typed, ...element };
  if (full.type === 'stickynote') {
    full.fillStyle = 'solid';
    if (full.backgroundColor === 'transparent') full.backgroundColor = '#ffdf6b';
  }
  return full;
}

const rotate = ([x, y], [cx, cy], angle) => {
  const c = Math.cos(angle), s = Math.sin(angle);
  return [cx + (x - cx) * c - (y - cy) * s, cy + (x - cx) * s + (y - cy) * c];
};
export const center = (e) => [e.x + e.width / 2, e.y + e.height / 2];
/* Bounds of local points, as [minX, minY, maxX, maxY]. */
export function pointBounds(points) {
  const xs = points.map((p) => p[0]), ys = points.map((p) => p[1]);
  return [Math.min(...xs), Math.min(...ys), Math.max(...xs), Math.max(...ys)];
}
/* Absolute points of a stroke, rotated about its points' centre. */
function absolutePoints(e) {
  const [x1, y1, x2, y2] = pointBounds(e.points);
  const c = [e.x + (x1 + x2) / 2, e.y + (y1 + y2) / 2];
  return e.points.map(([x, y]) => (e.angle ? rotate([e.x + x, e.y + y], c, e.angle) : [e.x + x, e.y + y]));
}

// Hand-drawn wobble is decorative; bindings use the nominal silhouette.
function inside(target, point, gap) {
  const [cx, cy] = center(target);
  const [px, py] = rotate(point, [cx, cy], -target.angle).map((v, i) => Math.abs(v - [cx, cy][i]));
  const rx = target.width / 2 + gap, ry = target.height / 2 + gap;
  if (target.type === 'ellipse') return (px / rx) ** 2 + (py / ry) ** 2 <= 1;
  if (target.type === 'diamond') return px / rx + py / ry <= 1;
  return px <= rx && py <= ry;
}
function fixedPointOf(target, binding) {
  const [fx, fy] = binding.fixedPoint;
  return rotate([target.x + target.width * fx, target.y + target.height * fy], center(target), target.angle);
}
/* An arrow end bound in orbit sits on the target outline, offset by the
   binding gap, on the segment from its fixed point toward FROM. */
function orbitPoint(target, focus, from) {
  const gap = 5 + target.strokeWidth / 2;
  if (inside(target, from, gap)) return focus;
  let low = 0, high = 1; // fraction from focus (inside) toward from (outside)
  for (let i = 0; i < 40; i++) {
    const mid = (low + high) / 2;
    if (inside(target, focus.map((v, axis) => v + (from[axis] - v) * mid), gap)) low = mid;
    else high = mid;
  }
  return focus.map((v, axis) => v + (from[axis] - v) * high);
}
/* Displayed absolute points of an arrow or line, following bound shapes. */
export function linearPath(e, byId) {
  const points = absolutePoints(e);
  if (e.type !== 'arrow' || points.length < 2) return points;
  const ends = [e.startBinding, e.endBinding].map((binding) => {
    const target = binding && byId.get(binding.elementId);
    return target && BINDABLE.includes(target.type) && !(target.type === 'text' && target.containerId)
      ? { target, binding, focus: fixedPointOf(target, binding) } : null;
  });
  const last = points.length - 1;
  if (e.elbowed && !e.fixedSegments?.length) return elbowPath(points[0], points[last], ends);
  const reference = (i) => {
    const other = i ? 0 : last;
    if (points.length === 2) return ends[i ? 0 : 1]?.focus || points[other];
    return points[i ? last - 1 : 1];
  };
  const resolved = ends.map((end, i) => end && (end.binding.mode === 'inside'
    ? end.focus : orbitPoint(end.target, end.focus, reference(i))));
  if (resolved[0]) points[0] = resolved[0];
  if (resolved[1]) points[last] = resolved[1];
  return points;
}
// Elbow arrows (Excalidraw's elbowArrow.ts routes with A* around obstacles).
// ponytail: a fixed orthogonal route between side anchors that does not avoid
// other shapes; port Excalidraw's router when boards need obstacle avoidance.
const STUB = 20;
const dominant = ([dx, dy]) => (Math.abs(dx) >= Math.abs(dy) ? [Math.sign(dx) || 1, 0] : [0, Math.sign(dy) || 1]);
/* The midpoint of TARGET's side facing TOWARD, offset by the binding gap,
   and the outward heading there. */
function sideAnchor(target, toward) {
  const c = center(target), gap = 5 + target.strokeWidth / 2;
  const [lx, ly] = rotate(toward, c, -target.angle);
  const horizontal = Math.abs(lx - c[0]) / (target.width || 1) >= Math.abs(ly - c[1]) / (target.height || 1);
  const heading = horizontal ? [Math.sign(lx - c[0]) || 1, 0] : [0, Math.sign(ly - c[1]) || 1];
  const side = [c[0] + heading[0] * (target.width / 2 + gap), c[1] + heading[1] * (target.height / 2 + gap)];
  return { point: rotate(side, c, target.angle), heading: rotate(heading, [0, 0], target.angle) };
}
/* An orthogonal route from START to END; ENDS are the bound targets. */
function elbowPath(start, end, ends) {
  const anchors = [0, 1].map((i) => {
    const target = ends[i]?.target, other = ends[1 - i]?.target;
    const toward = other ? center(other) : i ? start : end;
    return target ? { ...sideAnchor(target, toward), stub: STUB } : null;
  });
  const [s, t] = [anchors[0]?.point || start, anchors[1]?.point || end];
  const hs = anchors[0]?.heading || dominant([t[0] - s[0], t[1] - s[1]]);
  const he = anchors[1]?.heading || dominant([s[0] - t[0], s[1] - t[1]]);
  const s1 = [s[0] + hs[0] * (anchors[0]?.stub || 0), s[1] + hs[1] * (anchors[0]?.stub || 0)];
  const e1 = [t[0] + he[0] * (anchors[1]?.stub || 0), t[1] + he[1] * (anchors[1]?.stub || 0)];
  const across = Math.abs(hs[0]) >= Math.abs(hs[1]), arrive = Math.abs(he[0]) >= Math.abs(he[1]);
  const middle = across && arrive ? [[(s1[0] + e1[0]) / 2, s1[1]], [(s1[0] + e1[0]) / 2, e1[1]]]
    : !across && !arrive ? [[s1[0], (s1[1] + e1[1]) / 2], [e1[0], (s1[1] + e1[1]) / 2]]
    : across ? [[e1[0], s1[1]]] : [[s1[0], e1[1]]];
  const route = [];
  for (const p of [s, s1, ...middle, e1, t]) {
    const prev = route.at(-1);
    if (prev && Math.hypot(p[0] - prev[0], p[1] - prev[1]) < 0.5) continue;
    // Drop a middle point that continues a straight run.
    const before = route.at(-2);
    if (before && prev && ((Math.abs(before[0] - prev[0]) < 0.5 && Math.abs(prev[0] - p[0]) < 0.5) ||
        (Math.abs(before[1] - prev[1]) < 0.5 && Math.abs(prev[1] - p[1]) < 0.5))) route.pop();
    route.push(p);
  }
  return route.length >= 2 ? route : [s, t];
}
/* Axis-aligned [x, y, w, h] of a resolved element. */
export function elementBox(e) {
  if (e.path) {
    const [x1, y1, x2, y2] = pointBounds(e.path);
    return [x1, y1, x2 - x1, y2 - y1];
  }
  if (e.type === 'freedraw') {
    const [x1, y1, x2, y2] = pointBounds(absolutePoints(e));
    return [x1, y1, x2 - x1, y2 - y1];
  }
  if (!e.angle) return [e.x, e.y, e.width, e.height];
  const c = center(e);
  const corners = [[e.x, e.y], [e.x + e.width, e.y], [e.x + e.width, e.y + e.height], [e.x, e.y + e.height]]
    .map((p) => rotate(p, c, e.angle));
  const [x1, y1, x2, y2] = pointBounds(corners);
  return [x1, y1, x2 - x1, y2 - y1];
}

/* Resolve stored ELEMENTS for display. Returns `order`, the drawing order
   with bound text right after its container, `byId`, and `labels` by
   container id. Dangling references are drawn unbound. */
export function resolveScene(elements) {
  const sorted = [...elements].sort(compareOrder);
  const stored = new Map(sorted.map((e) => [e.id, e]));
  const byId = new Map();
  for (const e of sorted) {
    const container = e.type === 'text' && e.containerId && stored.get(e.containerId);
    byId.set(e.id, complete(e, Boolean(container && CONTAINERS.includes(container.type))));
  }
  for (const e of byId.values()) if (LINEAR.includes(e.type)) e.path = linearPath(e, byId);
  const labels = new Map();
  for (const e of byId.values()) {
    const container = e.type === 'text' && e.containerId && byId.get(e.containerId);
    if (!container || !CONTAINERS.includes(container.type)) continue;
    const laid = layoutText(e, container, container.path);
    Object.assign(e, laid, { bound: true });
    labels.set(container.id, [...(labels.get(container.id) || []), e]);
  }
  const order = [];
  for (const e of sorted) {
    const item = byId.get(e.id);
    if (item.bound) continue;
    if (item.type === 'text') Object.assign(item, layoutText(item));
    order.push(item, ...(labels.get(item.id) || []));
  }
  for (const item of order) item.box = elementBox(item);
  return { order, byId, labels };
}

/* Tight [x, y, w, h] around elements, as displayed in SCENE. */
export function extent(elements, scene = resolveScene(elements)) {
  let left = Infinity, top = Infinity, right = -Infinity, bottom = -Infinity;
  for (const e of elements) {
    const item = scene.byId.get(e.id);
    if (!item?.box) continue;
    const [x, y, w, h] = item.box;
    left = Math.min(left, x); top = Math.min(top, y);
    right = Math.max(right, x + w); bottom = Math.max(bottom, y + h);
  }
  return left === Infinity ? [0, 0, 0, 0] : [left, top, right - left, bottom - top];
}
export function bounds(elements, scene) {
  if (!elements.length) return [-40, -40, 800, 500];
  const [left, top, width, height] = extent(elements, scene);
  return [left - 30, top - 30, Math.max(100, width + 60), Math.max(100, height + 60)];
}
/* Selectable elements a region [x, y, w, h] contains entirely, or touches
   when TOUCHING. Bound text is selected through its container. */
export function shapesInRegion(elements, region, touching = false, scene = resolveScene(elements)) {
  const [rx, ry, rw, rh] = region;
  return elements.filter((e) => {
    const item = scene.byId.get(e.id);
    if (!item?.box || item.bound) return false;
    const [x, y, w, h] = item.box;
    return touching
      ? x <= rx + rw && x + w >= rx && y <= ry + rh && y + h >= ry
      : x >= rx && y >= ry && x + w <= rx + rw && y + h <= ry + rh;
  });
}
