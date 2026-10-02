/* Selection transforms: resize and rotation handles and the geometry they
   produce, following Excalidraw's transform handles. Pure, so browser tests
   and Node tests share them. */

export const MIN_SIZE = 10;
export const rotate = ([x, y], [cx, cy], angle) => {
  const c = Math.cos(angle), s = Math.sin(angle);
  return [cx + (x - cx) * c - (y - cy) * s, cy + (x - cx) * s + (y - cy) * c];
};
export const HANDLES = ['nw', 'n', 'ne', 'e', 'se', 's', 'sw', 'w'];
const CURSORS = { nw: 'nwse', se: 'nwse', ne: 'nesw', sw: 'nesw', n: 'ns', s: 'ns', e: 'ew', w: 'ew' };

/* Handle positions of FRAME [x, y, w, h] turned by ANGLE, for a view
   SCALE (screen pixels per board unit). Side handles hide on small frames,
   as in Excalidraw; the rotation handle sits above the top edge. */
export function handlePositions([x, y, w, h], angle, scale) {
  const c = [x + w / 2, y + h / 2], small = Math.min(w, h) * scale < 5 * 10;
  const at = { nw: [x, y], n: [x + w / 2, y], ne: [x + w, y], e: [x + w, y + h / 2], se: [x + w, y + h],
    s: [x + w / 2, y + h], sw: [x, y + h], w: [x, y + h / 2], rotation: [x + w / 2, y - 24 / scale] };
  return Object.entries(at)
    .filter(([name]) => !(small && name.length === 1))
    .map(([name, point]) => ({ name, point: rotate(point, c, angle),
      cursor: name === 'rotation' ? 'grab' : `${CURSORS[name]}-resize` }));
}

/* BOX [x, y, w, h], turned by ANGLE about its centre, resized by dragging
   HANDLE to POINT. The opposite edge or corner stays in place; KEEPASPECT
   keeps the proportions on corner handles. */
export function resizeBox([x, y, w, h], angle, handle, point, keepAspect = false) {
  const c = [x + w / 2, y + h / 2], [px, py] = rotate(point, c, -angle);
  let [l, t, r, b] = [x, y, x + w, y + h];
  if (handle.includes('w')) l = Math.min(px, r - MIN_SIZE);
  if (handle.includes('e')) r = Math.max(px, l + MIN_SIZE);
  if (handle.includes('n')) t = Math.min(py, b - MIN_SIZE);
  if (handle.includes('s')) b = Math.max(py, t + MIN_SIZE);
  if (keepAspect && handle.length === 2 && w && h) {
    const k = Math.max((r - l) / w, (b - t) / h);
    if (handle.includes('w')) l = r - w * k; else r = l + w * k;
    if (handle.includes('n')) t = b - h * k; else b = t + h * k;
  }
  const [cx, cy] = rotate([(l + r) / 2, (t + b) / 2], c, angle);
  return [cx - (r - l) / 2, cy - (b - t) / 2, r - l, b - t];
}

/* New boxes for BOXES (id to [x, y, w, h]) when their common FRAME is
   resized by HANDLE to POINT, with the factors [sx, sy]. */
export function scaleBoxes(boxes, frame, handle, point, uniform = false) {
  const next = resizeBox(frame, 0, handle, point, uniform);
  const sx = next[2] / frame[2], sy = next[3] / frame[3];
  const moved = new Map([...boxes].map(([id, [x, y, w, h]]) =>
    [id, [next[0] + (x - frame[0]) * sx, next[1] + (y - frame[1]) * sy, w * sx, h * sy]]));
  return { boxes: moved, scale: [sx, sy] };
}

/* The turn of a rotation drag about CENTER from START to POINT, snapped to
   15 degrees when SNAP. */
export function rotationDelta(center, start, point, snap = false) {
  const angle = (p) => Math.atan2(p[1] - center[1], p[0] - center[0]);
  const delta = angle(point) - angle(start);
  const step = Math.PI / 12;
  return snap ? Math.round(delta / step) * step : delta;
}
export const normalizeAngle = (angle) => ((angle % (2 * Math.PI)) + 2 * Math.PI) % (2 * Math.PI);
