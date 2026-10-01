/* Recognise a hand-drawn stroke as a rectangle, diamond, ellipse, arrow or
   line. A moment-based classifier ported from Excalidraw's convertToShape.ts
   (MIT, https://github.com/excalidraw/excalidraw); rectangle vs. diamond is
   deliberately not rotation invariant. */

const RESAMPLE_N = 64;
const MIN_SCREEN_SIZE = 25;
const CLOSED_GAP_MAX_RATIO = 0.15;
const LINEAR_MAX_ELONGATION = 0.25;
const ARROWHEAD_ZONE_RATIO = 0.5;
const LINEAR_MAX_SHAFT_DEVIATION = 0.15;
const ARROW_MIN_SKEW = 0.3;
const CLOSED_SHAPE_MAX_DISTANCE = 1.5;
const TURN_WINDOW = 3;
const PROTOTYPES = [
  { type: 'rectangle', hullFillRatio: 1, cornerTurnShare: 0.95, kurtosisProduct: 1.83 },
  { type: 'diamond', hullFillRatio: 0.5, cornerTurnShare: 0.95, kurtosisProduct: 3.24 },
  { type: 'ellipse', hullFillRatio: Math.PI / 4, cornerTurnShare: 0.55, kurtosisProduct: 2.25 },
];
const TOLERANCE = { hullFillRatio: 0.2, cornerTurnShare: 0.2, kurtosisProduct: 0.7 };

const distance = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]);
export function pointsBox(points) {
  const xs = points.map((p) => p[0]), ys = points.map((p) => p[1]);
  return [Math.min(...xs), Math.min(...ys), Math.max(...xs), Math.max(...ys)];
}
/* POINTS resampled to N evenly spaced points along the stroke. */
function resample(points, n) {
  let total = 0;
  for (let i = 1; i < points.length; i++) total += distance(points[i], points[i - 1]);
  const interval = total / (n - 1), result = [points[0]];
  let accumulated = 0, prev = points[0];
  for (let i = 1; i < points.length; i++) {
    const curr = points[i], segment = distance(curr, prev);
    if (accumulated + segment >= interval) {
      let remaining = interval - accumulated;
      while (remaining <= segment + 1e-10) {
        const t = remaining / segment;
        const next = [prev[0] + t * (curr[0] - prev[0]), prev[1] + t * (curr[1] - prev[1])];
        result.push(next);
        if (result.length === n) return result;
        prev = next;
        accumulated = 0;
        remaining += interval;
      }
      accumulated = segment - (remaining - interval);
    } else accumulated += segment;
    prev = curr;
  }
  while (result.length < n) result.push(points.at(-1));
  return result;
}
function moment(values, order) {
  const mean = values.reduce((a, b) => a + b, 0) / values.length;
  let variance = 0, m = 0;
  for (const v of values) { variance += (v - mean) ** 2; m += (v - mean) ** order; }
  variance /= values.length; m /= values.length;
  const sigma = Math.sqrt(variance);
  return sigma > Number.EPSILON ? m / sigma ** order : 0;
}
function principalAxes(points) {
  const c = [0, 1].map((i) => points.reduce((sum, p) => sum + p[i], 0) / points.length);
  let m20 = 0, m02 = 0, m11 = 0;
  for (const [x, y] of points) {
    m20 += (x - c[0]) ** 2; m02 += (y - c[1]) ** 2; m11 += (x - c[0]) * (y - c[1]);
  }
  [m20, m02, m11] = [m20, m02, m11].map((v) => v / points.length);
  const trace = m20 + m02, diff = Math.hypot(m20 - m02, 2 * m11);
  const majorVariance = (trace + diff) / 2, minorVariance = (trace - diff) / 2;
  let major = Math.abs(m11) > Number.EPSILON ? [majorVariance - m02, m11] : m20 >= m02 ? [1, 0] : [0, 1];
  const length = Math.hypot(...major);
  major = major.map((v) => v / length);
  return { c, major, majorVariance, minorVariance };
}
const along = (points, { c, major }) => points.map(([x, y]) => (x - c[0]) * major[0] + (y - c[1]) * major[1]);
function convexHull(points) {
  if (points.length < 3) return [...points];
  const sorted = [...points].sort((a, b) => (a[0] === b[0] ? a[1] - b[1] : a[0] - b[0]));
  const cross = (o, a, b) => (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0]);
  const half = (list) => {
    const chain = [];
    for (const p of list) {
      while (chain.length >= 2 && cross(chain.at(-2), chain.at(-1), p) <= 0) chain.pop();
      chain.push(p);
    }
    chain.pop();
    return chain;
  };
  const hull = [...half(sorted), ...half([...sorted].reverse())];
  return hull.length >= 3 ? hull : [...points];
}
const area = (polygon) => Math.abs(polygon.reduce((sum, p, i) => {
  const q = polygon[(i + 1) % polygon.length];
  return sum + p[0] * q[1] - q[0] * p[1];
}, 0)) / 2;
function segmentDistance(p, a, b) {
  const [dx, dy] = [b[0] - a[0], b[1] - a[1]], length = dx * dx + dy * dy;
  const t = length ? Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / length)) : 0;
  return distance(p, [a[0] + t * dx, a[1] + t * dy]);
}
function shaftDeviationRatio(points) {
  const start = points[0];
  let tip = start, tipDistance = 0;
  for (const p of points) if (distance(p, start) > tipDistance) { tipDistance = distance(p, start); tip = p; }
  if (!tipDistance) return 0;
  let max = 0;
  for (const p of points)
    if (distance(p, tip) > ARROWHEAD_ZONE_RATIO * tipDistance) max = Math.max(max, segmentDistance(p, start, tip));
  return max / tipDistance;
}
function cornerTurnShare(points) {
  const turns = [];
  for (let i = TURN_WINDOW; i < points.length - TURN_WINDOW; i++) {
    const [a, b, c] = [points[i - TURN_WINDOW], points[i], points[i + TURN_WINDOW]];
    const v1 = [b[0] - a[0], b[1] - a[1]], v2 = [c[0] - b[0], c[1] - b[1]];
    turns.push(Math.abs(Math.atan2(v1[0] * v2[1] - v1[1] * v2[0], v1[0] * v2[0] + v1[1] * v2[1])));
  }
  const total = turns.reduce((a, b) => a + b, 0);
  if (!total) return 0;
  const taken = turns.map(() => false);
  let top = 0;
  for (let corner = 0; corner < 4; corner++) {
    let peak = -1, peakTurn = 0;
    turns.forEach((t, i) => { if (!taken[i] && t > peakTurn) { peakTurn = t; peak = i; } });
    if (peak < 0) break;
    for (let i = peak - TURN_WINDOW; i <= peak + TURN_WINDOW; i++)
      if (i >= 0 && i < turns.length && !taken[i]) { top += turns[i]; taken[i] = true; }
  }
  return top / total;
}
function features(input) {
  const points = resample(input, RESAMPLE_N);
  let length = 0;
  for (let i = 1; i < points.length; i++) length += distance(points[i], points[i - 1]);
  let axes = principalAxes(points);
  if (moment(along(points, axes), 3) > 0) axes = { ...axes, major: axes.major.map((v) => -v) };
  const [x1, y1, x2, y2] = pointsBox(points), box = (x2 - x1) * (y2 - y1);
  return {
    gapRatio: length > 0 ? distance(points.at(-1), points[0]) / length : 0,
    elongation: axes.majorVariance > 0 ? axes.minorVariance / axes.majorVariance : 1,
    majorSkew: moment(along(points, axes), 3),
    hullFillRatio: box > 0 ? area(convexHull(points)) / box : 0,
    cornerTurnShare: cornerTurnShare(points),
    kurtosisProduct: moment(points.map((p) => p[0]), 4) * moment(points.map((p) => p[1]), 4),
    shaftDeviationRatio: shaftDeviationRatio(points),
  };
}
function classify(f) {
  if (f.gapRatio > CLOSED_GAP_MAX_RATIO) {
    if (f.elongation > LINEAR_MAX_ELONGATION || f.shaftDeviationRatio > LINEAR_MAX_SHAFT_DEVIATION) return 'freedraw';
    return Math.abs(f.majorSkew) >= ARROW_MIN_SKEW ? 'arrow' : 'line';
  }
  let best = 'freedraw', bestDistance = CLOSED_SHAPE_MAX_DISTANCE;
  for (const p of PROTOTYPES) {
    const d = Math.hypot(...Object.keys(TOLERANCE).map((k) => (f[k] - p[k]) / TOLERANCE[k]));
    if (d < bestDistance) { bestDistance = d; best = p.type; }
  }
  return best;
}
/* The arrow tip: the input point nearest the bounding-box perimeter point
   farthest from the start. */
function arrowTip(points, [x1, y1, x2, y2]) {
  if (x1 === x2 && y1 === y2) return points.at(-1);
  const perimeter = [[x1, y1], [(x1 + x2) / 2, y1], [x2, y1], [x2, (y1 + y2) / 2], [x2, y2],
    [(x1 + x2) / 2, y2], [x1, y2], [x1, (y1 + y2) / 2]];
  const ideal = perimeter.reduce((best, p) => (distance(p, points[0]) > distance(best, points[0]) ? p : best));
  return points.reduce((best, p) => (distance(p, ideal) < distance(best, ideal) ? p : best));
}
/* The shape POINTS (board coordinates) were drawn as, at ZOOM screen pixels
   per board unit: {type, box: [x, y, w, h], from?, to?}, or {type: 'freedraw'}. */
export function recognize(points, zoom = 1) {
  const bounds = pointsBox(points);
  if (points.length < 3 || Math.max(bounds[2] - bounds[0], bounds[3] - bounds[1]) * zoom < MIN_SCREEN_SIZE)
    return { type: 'freedraw' };
  let type = classify(features(points));
  const box = [bounds[0], bounds[1], bounds[2] - bounds[0], bounds[3] - bounds[1]];
  if (type === 'arrow') {
    const to = arrowTip(points, bounds);
    // A short stroke with a hook reads better as a line than a tiny arrow.
    return { type: distance(points[0], to) < 60 ? 'line' : 'arrow', box, from: points[0], to };
  }
  if (type === 'line') return { type, box, from: points[0], to: points.at(-1) };
  return { type, box };
}
