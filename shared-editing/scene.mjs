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
