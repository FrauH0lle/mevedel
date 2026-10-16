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
export function linearPath(e, byId, obstacles = []) {
  const points = absolutePoints(e);
  if (e.type !== 'arrow' || points.length < 2) return points;
  const ends = [e.startBinding, e.endBinding].map((binding) => {
    const target = binding && byId.get(binding.elementId);
    return target && BINDABLE.includes(target.type) && !(target.type === 'text' && target.containerId)
      ? { target, binding, focus: fixedPointOf(target, binding) } : null;
  });
  const last = points.length - 1;
  if (e.elbowed && !e.fixedSegments?.length) return elbowPath(points[0], points[last], ends, obstacles);
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
// Elbow arrows leave and enter the facing sides of their shapes. The direct
// orthogonal route is kept while it is clear; otherwise an A* search over the
// lines along obstacle edges detours around every shape in the way, by
// whichever sides of its shapes suit. A bend costs as much as the distance
// between the ends, so a detour takes few bends over a shorter zigzag.
// Excalidraw's elbowArrow.ts avoids only the arrow's own two shapes.
const STUB = 20, CLEARANCE = 15; // CLEARANCE < STUB + gap: an arrow's own shapes stay obstacles
const OBSTACLES = ['rectangle', 'diamond', 'ellipse', 'stickynote', 'image', 'embeddable', 'iframe'];
/* Padded [x1, y1, x2, y2] boxes of the shapes elbow arrows route around,
   each with a `key`; the array's `present` holds the keys and `byId` maps
   element ids to boxes. */
function obstacleBoxes(elements) {
  const boxes = [];
  boxes.byId = new Map();
  for (const e of elements) {
    if (!OBSTACLES.includes(e.type)) continue;
    const [x, y, w, h] = elementBox(e);
    const box = [x - CLEARANCE, y - CLEARANCE, x + w + CLEARANCE, y + h + CLEARANCE];
    box.key = box.join();
    boxes.push(box);
    boxes.byId.set(e.id, box);
  }
  boxes.present = new Set(boxes.map((b) => b.key));
  return boxes;
}
const EPS = 0.5;
const inBox = (b, [x, y]) => x > b[0] + EPS && x < b[2] - EPS && y > b[1] + EPS && y < b[3] - EPS;
/* Whether the axis-aligned segment A-B passes through BOX's interior. */
const crosses = (box, a, b) => Math.min(a[0], b[0]) < box[2] - EPS && Math.max(a[0], b[0]) > box[0] + EPS
  && Math.min(a[1], b[1]) < box[3] - EPS && Math.max(a[1], b[1]) > box[1] + EPS;
/* Whether the polyline PATH passes through box B. */
function blocks(b, path) {
  for (let i = 1; i < path.length; i++) if (crosses(b, path[i - 1], path[i])) return true;
  return false;
}
const DIRS = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const dirOf = (h) => DIRS.findIndex((d) => d[0] === h[0] && d[1] === h[1]);
/* Cheapest orthogonal path, by length plus BEND per turn, on the grid
   through BOXES' edges from one of STARTS to one of GOALS, each {p, d}: a
   point and the direction of travel leaving or arriving there. Returns
   {path, start, goal} or null. */
function search(starts, goals, boxes, bend) {
  const ports = [...starts, ...goals];
  const sorted = (axis) => [...new Set([...ports.map((q) => q.p[axis]), ...boxes.flatMap((b) => [b[axis], b[axis + 2]])])]
    .sort((a, b) => a - b);
  const xs = sorted(0), ys = sorted(1), nx = xs.length, ny = ys.length;
  const xi = new Map(xs.map((v, i) => [v, i])), yi = new Map(ys.map((v, i) => [v, i]));
  // Grid lines run along every box edge, so a grid edge lies wholly inside
  // or outside each box: mark those inside. hBlock[i * ny + j] is the edge
  // from (i, j) to (i + 1, j); vBlock the one to (i, j + 1).
  const hBlock = new Uint8Array(nx * ny), vBlock = new Uint8Array(nx * ny);
  for (const b of boxes) {
    const [i1, j1, i2, j2] = [xi.get(b[0]), yi.get(b[1]), xi.get(b[2]), yi.get(b[3])];
    for (let i = i1; i < i2; i++) for (let j = j1 + 1; j < j2; j++) hBlock[i * ny + j] = 1;
    for (let i = i1 + 1; i < i2; i++) for (let j = j1; j < j2; j++) vBlock[i * ny + j] = 1;
  }
  const cellOf = (p) => xi.get(p[0]) * ny + yi.get(p[1]);
  const goalAt = new Map(goals.map((q, n) => [cellOf(q.p), n]));
  // Admissible estimate: distance to the nearest goal plus the bends it must
  // still make.
  const h = (i, j, d) => {
    let best = Infinity;
    for (const { p: e, d: arrive } of goals) {
      const dx = e[0] - xs[i], dy = e[1] - ys[j], [ux, uy] = DIRS[d];
      if (!dx && !dy) return 0;
      const along = Math.sign(ux * dx + uy * dy); // 1 ahead, 0 sideways, -1 behind
      const turns = dx && dy ? (ux * dx < 0 || uy * dy < 0 ? 2 : 1) : along > 0 ? 0 : along < 0 ? 2 : 1;
      best = Math.min(best, Math.abs(dx) + Math.abs(dy) + bend * (turns || (d === arrive ? 0 : 1)));
    }
    return best;
  };
  const g = new Float64Array(nx * ny * 4).fill(Infinity), from = new Int32Array(nx * ny * 4).fill(-1);
  const closed = new Uint8Array(nx * ny * 4);
  const heapF = [], heapK = [];
  const push = (f, k) => {
    let i = heapF.length;
    heapF.push(f); heapK.push(k);
    while (i) {
      const up = (i - 1) >> 1;
      if (heapF[up] <= f) break;
      heapF[i] = heapF[up]; heapK[i] = heapK[up];
      i = up;
    }
    heapF[i] = f; heapK[i] = k;
  };
  const pop = () => {
    const top = heapK[0], f = heapF.pop(), k = heapK.pop(), n = heapF.length;
    if (n) {
      let i = 0;
      for (;;) {
        let c = 2 * i + 1;
        if (c >= n) break;
        if (c + 1 < n && heapF[c + 1] < heapF[c]) c++;
        if (heapF[c] >= f) break;
        heapF[i] = heapF[c]; heapK[i] = heapK[c];
        i = c;
      }
      heapF[i] = f; heapK[i] = k;
    }
    return top;
  };
  for (const { p, d } of starts) {
    const k = cellOf(p) * 4 + d;
    g[k] = 0;
    push(h(xi.get(p[0]), yi.get(p[1]), d), k);
  }
  while (heapF.length) {
    const k = pop();
    if (closed[k]) continue;
    closed[k] = 1;
    const d = k & 3, cell = k >> 2, i = (cell / ny) | 0, j = cell - i * ny;
    if (goalAt.has(cell) && from[k] >= 0) {
      const path = [];
      for (let at = k; at >= 0; at = from[at]) {
        const c = at >> 2, ci = (c / ny) | 0;
        path.push([xs[ci], ys[c - ci * ny]]);
      }
      path.reverse();
      const start = starts.findIndex((q) => q.p[0] === path[0][0] && q.p[1] === path[0][1]);
      return { path, start, goal: goalAt.get(cell) };
    }
    for (let nd = 0; nd < 4; nd++) {
      if ((nd ^ 1) === d) continue; // no reversal
      const [dx, dy] = DIRS[nd], ni = i + dx, nj = j + dy;
      if (ni < 0 || nj < 0 || ni >= nx || nj >= ny) continue;
      if (dx ? hBlock[Math.min(i, ni) * ny + j] : vBlock[i * ny + Math.min(j, nj)]) continue;
      const goal = goalAt.get(ni * ny + nj), arrive = goal === undefined ? nd : goals[goal].d;
      if ((nd ^ 1) === arrive) continue; // would leave the goal backwards
      const ng = g[k] + Math.abs(xs[ni] - xs[i]) + Math.abs(ys[nj] - ys[j]) + (nd === d ? 0 : bend)
        + (nd !== arrive ? bend : 0);
      const nk = (ni * ny + nj) * 4 + nd;
      if (ng >= g[nk]) continue;
      g[nk] = ng;
      from[nk] = k;
      push(ng + h(ni, nj, nd), nk);
    }
  }
  return null;
}
/* A search from STARTS to GOALS around BOXES, first among those near the
   direct route from S1 to E1, adding any others the path runs into. */
function detour(starts, goals, s1, e1, boxes) {
  const span = [Math.min(s1[0], e1[0]), Math.min(s1[1], e1[1]), Math.max(s1[0], e1[0]), Math.max(s1[1], e1[1])];
  const used = new Set(boxes.filter((b) => b[0] <= span[2] && b[2] >= span[0] && b[1] <= span[3] && b[3] >= span[1]));
  const bend = Math.abs(s1[0] - e1[0]) + Math.abs(s1[1] - e1[1]) + 1;
  for (;;) {
    const found = search(starts, goals, [...used], bend);
    if (!found) return null;
    const hit = boxes.filter((b) => !used.has(b) && blocks(b, found.path));
    if (!hit.length) return { ...found, used: [...used] };
    for (const b of hit) used.add(b);
  }
}
// Detours by the geometry of their ports. A detour stays the best route while
// every box it depended on (searched around, or closing a port) is unchanged
// and no box blocks it: a new box off its path cannot open a better one. A
// drag frame thus searches again only for arrows near what moved.
const detours = new Map();
const DETOURS = 1024;
const dominant = ([dx, dy]) => (Math.abs(dx) >= Math.abs(dy) ? [Math.sign(dx) || 1, 0] : [0, Math.sign(dy) || 1]);
/* The midpoint of TARGET's side with outward HEADING (in its unrotated
   frame), offset by the binding gap, and the rotated heading there. */
function side(target, heading) {
  const c = center(target), gap = 5 + target.strokeWidth / 2;
  const point = [c[0] + heading[0] * (target.width / 2 + gap), c[1] + heading[1] * (target.height / 2 + gap)];
  return { point: rotate(point, c, target.angle), heading: rotate(heading, [0, 0], target.angle) };
}
/* TARGET's side facing TOWARD. */
function sideAnchor(target, toward) {
  const c = center(target);
  const [lx, ly] = rotate(toward, c, -target.angle);
  const horizontal = Math.abs(lx - c[0]) / (target.width || 1) >= Math.abs(ly - c[1]) / (target.height || 1);
  return side(target, horizontal ? [Math.sign(lx - c[0]) || 1, 0] : [0, Math.sign(ly - c[1]) || 1]);
}
/* Where a route leaves the side {POINT, HEADING} of a shape with obstacle box
   OWN: STUB out, or just past OWN when the shape is rotated and its box is
   wider than the shape, so the shape itself stays an obstacle. */
function stubOf({ point, heading }, own) {
  if (!own) return [point[0] + heading[0] * STUB, point[1] + heading[1] * STUB];
  let exit = Infinity;
  for (const axis of [0, 1])
    if (Math.abs(heading[axis]) > 1e-9)
      exit = Math.min(exit, ((heading[axis] > 0 ? own[axis + 2] : own[axis]) - point[axis]) / heading[axis]);
  const length = Math.max(STUB, exit + 1);
  return [point[0] + heading[0] * length, point[1] + heading[1] * length];
}
/* An orthogonal route from START to END around OBSTACLES; ENDS are the bound
   targets. A detour may leave or enter a bound shape by any side. */
function elbowPath(start, end, ends, obstacles) {
  const own = ends.map((end) => end && obstacles.byId?.get(end.target.id));
  const anchors = [0, 1].map((i) => {
    const target = ends[i]?.target, other = ends[1 - i]?.target;
    const toward = other ? center(other) : i ? start : end;
    return target ? sideAnchor(target, toward) : null;
  });
  const [s, t] = [anchors[0]?.point || start, anchors[1]?.point || end];
  const hs = anchors[0]?.heading || dominant([t[0] - s[0], t[1] - s[1]]);
  const he = anchors[1]?.heading || dominant([s[0] - t[0], s[1] - t[1]]);
  const s1 = anchors[0] ? stubOf(anchors[0], own[0]) : s, e1 = anchors[1] ? stubOf(anchors[1], own[1]) : t;
  const across = Math.abs(hs[0]) >= Math.abs(hs[1]), arrive = Math.abs(he[0]) >= Math.abs(he[1]);
  const middle = across && arrive ? [[(s1[0] + e1[0]) / 2, s1[1]], [(s1[0] + e1[0]) / 2, e1[1]]]
    : !across && !arrive ? [[s1[0], (s1[1] + e1[1]) / 2], [e1[0], (s1[1] + e1[1]) / 2]]
    : across ? [[e1[0], s1[1]]] : [[s1[0], e1[1]]];
  // Shapes enclosing an end's shape, such as a lane behind it, are not in the
  // way; a shape that merely crowds a stub closes that side instead.
  const cores = [0, 1].map((i) => (ends[i] ? center(ends[i].target) : i ? t : s));
  const lane = (b) => [0, 1].some((i) => b.key !== own[i]?.key && inBox(b, cores[i]));
  // The short run from a shape to its stub may pass anything but that shape.
  const stubCrosses = (b, i, anchor, stub) => b.key !== own[i]?.key && crosses(b, anchor, stub);
  let points = [s, s1, ...middle, e1, t];
  const direct = points.slice(1, -1), boxes = [];
  let blocked = false;
  for (const b of obstacles) {
    if (lane(b)) continue;
    boxes.push(b);
    blocked ||= blocks(b, direct) || stubCrosses(b, 0, s, s1) || stubCrosses(b, 1, t, e1);
  }
  const stubBlocker = (i, anchor, stub) => boxes.find((b) => stubCrosses(b, i, anchor, stub));
  if (blocked) {
    // A bound end offers each side whose stub is clear; a loose end its point.
    const ports = (i, point, heading) => (ends[i] ? DIRS.map((d) => side(ends[i].target, d)) : [{ point, heading }])
      .map((q) => ({ anchor: q.point, p: ends[i] ? stubOf(q, own[i]) : point, out: dominant(q.heading) }));
    // The ports follow from the ends' geometry alone.
    const key = [0, 1].map((i) => {
      const e = ends[i]?.target;
      return e ? `${e.x},${e.y},${e.width},${e.height},${e.angle},${e.strokeWidth}` : `${i ? t : s};${i ? he : hs}`;
    }).join('|');
    const known = detours.get(key);
    if (known && known.deps.every((b) => obstacles.present.has(b.key) && !lane(b))
        && !stubBlocker(0, known.points[0], known.path[0]) && !stubBlocker(1, known.points.at(-1), known.path.at(-1))
        && !boxes.some((b) => blocks(b, known.path))) {
      points = known.points;
    } else {
      const all = [ports(0, s, hs), ports(1, t, he)];
      const closing = [];
      const clear = (i) => (q) => {
        const b = boxes.find((o) => inBox(o, q.p)) || stubBlocker(i, q.anchor, q.p);
        if (b) closing.push(b);
        return !b;
      };
      const [from, to] = all.map((list, i) => list.filter(clear(i)));
      const found = detour(from.map((q) => ({ p: q.p, d: dirOf(q.out) })),
        to.map((q) => ({ p: q.p, d: dirOf([-q.out[0], -q.out[1]]) })), s1, e1, boxes);
      if (found) {
        points = [from[found.start].anchor, ...found.path, to[found.goal].anchor];
        detours.delete(key);
        if (detours.size >= DETOURS) detours.delete(detours.keys().next().value);
        detours.set(key, { points, path: found.path, deps: [...found.used, ...closing] });
      }
    }
  }
  const route = [];
  for (const p of points) {
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
  let obstacles = null;
  for (const e of byId.values()) {
    if (!LINEAR.includes(e.type)) continue;
    if (e.elbowed) obstacles ||= obstacleBoxes(byId.values());
    e.path = linearPath(e, byId, obstacles || []);
  }
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
