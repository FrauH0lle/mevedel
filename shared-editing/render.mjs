/* Deterministic SVG for whiteboards and HTML for documents, used by both
   browser and host. Board rendering follows Excalidraw's renderer (MIT,
   https://github.com/excalidraw/excalidraw): roughjs shapes with the
   element's seed, perfect-freehand strokes, and its text metrics. */
import rough from 'roughjs';
import { getStroke } from 'perfect-freehand';
import { imageSource } from './image.mjs';
import { fontStack, verticalOffset, BOUND_TEXT_PADDING } from './text.mjs';
import { resolveScene, bounds, extent, shapesInRegion, LINEAR, pointBounds } from './scene.mjs';

export { resolveScene, bounds, extent, shapesInRegion };
export const escape = (value) =>
  String(value ?? '').replace(
    /[&<>"']/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c],
  );
const generator = rough.generator();
const n = (v) => String(+(+v).toFixed(2));
const transparent = (color) => !color || color === 'transparent' || /^#[0-9a-f]{6}00$/i.test(color);

function adjustRoughness(e) {
  const max = Math.max(e.width, e.height), min = Math.min(e.width, e.height);
  if ((min >= 20 && max >= 50) ||
      (min >= 15 && e.roundness && ['rectangle', 'diamond', 'line', 'image', 'stickynote',
        'iframe', 'embeddable'].includes(e.type)) ||
      (LINEAR.includes(e.type) && max >= 50))
    return e.roughness;
  return Math.min(e.roughness / (max < 10 ? 3 : 2), 2.5);
}
const isLoop = (points) => points.length >= 3 &&
  Math.hypot(points[0][0] - points.at(-1)[0], points[0][1] - points.at(-1)[1]) <= 8;
function roughOptions(e, continuous = false) {
  const sw = e.strokeWidth;
  const options = {
    seed: e.seed || 1,
    strokeLineDash: e.strokeStyle === 'dashed' ? [8, 8 + sw] : e.strokeStyle === 'dotted' ? [1.5, 6 + sw] : undefined,
    disableMultiStroke: e.strokeStyle !== 'solid',
    strokeWidth: e.strokeStyle !== 'solid' ? sw + 0.5 : sw,
    fillWeight: sw / 2,
    // ponytail: at most ~800 hatch lines per direction, so a board-sized fill stays cheap.
    hachureGap: Math.max(sw * 4, Math.hypot(e.width, e.height) / 800),
    roughness: adjustRoughness(e),
    stroke: e.strokeColor,
    preserveVertices: continuous || e.roughness < 2,
  };
  if (['rectangle', 'diamond', 'ellipse', 'iframe', 'embeddable'].includes(e.type)) {
    options.fillStyle = e.fillStyle;
    options.fill = transparent(e.backgroundColor) ? undefined : e.backgroundColor;
    if (e.type === 'ellipse') options.curveFitting = 1;
  } else if ((e.type === 'line' || e.type === 'freedraw') && isLoop(e.points)) {
    options.fillStyle = e.fillStyle;
    options.fill = e.backgroundColor === 'transparent' ? undefined : e.backgroundColor;
  }
  return options;
}
/* SVG for a roughjs drawable, as RoughSVG.draw writes it. */
function draw(drawable, extra = '') {
  const o = drawable.options;
  return drawable.sets.map((set) => {
    const d = generator.opsToPath(set, 2);
    if (set.type === 'path')
      return `<path d="${d}" stroke="${escape(o.stroke)}" stroke-width="${o.strokeWidth}" fill="none"${o.strokeLineDash ? ` stroke-dasharray="${o.strokeLineDash.join(' ')}"` : ''}${extra}/>`;
    if (set.type === 'fillPath')
      return `<path d="${d}" stroke="none" fill="${escape(o.fill)}"${['curve', 'polygon'].includes(drawable.shape) ? ' fill-rule="evenodd"' : ''}/>`;
    return `<path d="${d}" stroke="${escape(o.fill)}" stroke-width="${o.fillWeight < 0 ? o.strokeWidth / 2 : o.fillWeight}" fill="none"/>`;
  }).join('');
}
export function cornerRadius(x, e) {
  if (e.roundness?.type === 1 || e.roundness?.type === 2) return x * 0.25;
  if (e.roundness?.type === 3) {
    const fixed = e.roundness.value ?? 32;
    return x <= fixed / 0.25 ? x * 0.25 : fixed;
  }
  return 0;
}
function diamondPoints(e) {
  const top = Math.floor(e.width / 2) + 1, right = Math.floor(e.height / 2) + 1;
  return [[top, 0], [e.width, right], [top, e.height], [0, right]];
}
function shapeDrawable(e) {
  const { width: w, height: h } = e;
  if (['rectangle', 'iframe', 'embeddable'].includes(e.type)) {
    if (!e.roundness) return generator.rectangle(0, 0, w, h, roughOptions(e));
    const r = cornerRadius(Math.min(w, h), e);
    return generator.path(`M ${r} 0 L ${w - r} 0 Q ${w} 0, ${w} ${r} L ${w} ${h - r} Q ${w} ${h}, ${w - r} ${h} L ${r} ${h} Q 0 ${h}, 0 ${h - r} L 0 ${r} Q 0 0, ${r} 0`, roughOptions(e, true));
  }
  if (e.type === 'diamond') {
    const [[tx, ty], [rx, ry], [bx, by], [lx, ly]] = diamondPoints(e);
    if (!e.roundness) return generator.polygon([[tx, ty], [rx, ry], [bx, by], [lx, ly]], roughOptions(e));
    const vr = cornerRadius(Math.abs(tx - lx), e), hr = cornerRadius(Math.abs(ry - ty), e);
    return generator.path(`M ${tx + vr} ${ty + hr} L ${rx - vr} ${ry - hr} C ${rx} ${ry}, ${rx} ${ry}, ${rx - vr} ${ry + hr} L ${bx + vr} ${by - hr} C ${bx} ${by}, ${bx} ${by}, ${bx - vr} ${by - hr} L ${lx + vr} ${ly + hr} C ${lx} ${ly}, ${lx} ${ly}, ${lx + vr} ${ly - hr} L ${tx - vr} ${ty + hr} C ${tx} ${ty}, ${tx} ${ty}, ${tx + vr} ${ty + hr}`, roughOptions(e, true));
  }
  return generator.ellipse(w / 2, h / 2, w, h, roughOptions(e));
}

// Arrowheads (Excalidraw's bounds.ts getArrowheadPoints and shape.ts).
const HEAD_SIZE = { arrow: 25, diamond: 12, diamond_outline: 12, cardinality_many: 15,
  cardinality_one_or_many: 15, cardinality_zero_or_many: 15, cardinality_one: 20,
  cardinality_exactly_one: 20, cardinality_zero_or_one: 20 };
const turn = ([x, y], [cx, cy], a) => [cx + (x - cx) * Math.cos(a) - (y - cy) * Math.sin(a),
  cy + (x - cx) * Math.sin(a) + (y - cy) * Math.cos(a)];
function headPoints(e, points, curve, position, head, offset = 0) {
  const ops = (curve.sets.find((s) => s.type === 'path') || curve.sets[0])?.ops || [];
  if (ops.length < 2) return null;
  const index = position === 'start' ? 1 : ops.length - 1, data = ops[index].data;
  if (data.length !== 6) return null;
  const p3 = [data[4], data[5]], p2 = [data[2], data[3]], p1 = [data[0], data[1]];
  const prev = ops[index - 1];
  const p0 = prev.op === 'move' ? prev.data : prev.op === 'bcurveTo' ? [prev.data[4], prev.data[5]] : [0, 0];
  const at = (t, i) => (1 - t) ** 3 * p3[i] + 3 * t * (1 - t) ** 2 * p2[i] + 3 * t ** 2 * (1 - t) * p1[i] + p0[i] * t ** 3;
  const [x2, y2] = position === 'start' ? p0 : p3, [x1, y1] = [at(0.3, 0), at(0.3, 1)];
  const distance = Math.hypot(x2 - x1, y2 - y1) || 1, nx = (x2 - x1) / distance, ny = (y2 - y1) / distance;
  const [c, p] = position === 'end' ? [points.at(-1), points.at(-2) || [0, 0]] : [points[0], points[1] || [0, 0]];
  const size = Math.min(HEAD_SIZE[head] ?? 15, Math.hypot(c[0] - p[0], c[1] - p[1]) * (head.startsWith('diamond') ? 0.25 : 0.5));
  const tx = x2 - nx * size * offset, ty = y2 - ny * size * offset, xs = tx - nx * size, ys = ty - ny * size;
  if (head.startsWith('circle')) return [tx, ty, Math.hypot(ys - ty, xs - tx) + e.strokeWidth - 2];
  const angle = ((head === 'bar' ? 90 : head === 'arrow' ? 20 : 25) * Math.PI) / 180;
  if (head === 'cardinality_many' || head === 'cardinality_one_or_many')
    return [xs, ys, ...turn([tx, ty], [xs, ys], -angle), ...turn([tx, ty], [xs, ys], angle)];
  const a = turn([xs, ys], [tx, ty], -angle), b = turn([xs, ys], [tx, ty], angle);
  if (head.startsWith('diamond')) {
    const [px, py] = position === 'start' ? points[1] || [0, 0] : points.at(-2) || [0, 0];
    const o = position === 'start'
      ? turn([tx + size * 2, ty], [tx, ty], Math.atan2(py - ty, px - tx))
      : turn([tx - size * 2, ty], [tx, ty], Math.atan2(ty - py, tx - px));
    return [tx, ty, ...a, ...o, ...b];
  }
  return [tx, ty, ...a, ...b];
}
function arrowheads(e, points, curve, options, position, head, paper) {
  const line = { ...options, roughness: Math.min(1, options.roughness || 0) };
  if (e.strokeStyle === 'dotted') line.strokeLineDash = [1.5, 6 + e.strokeWidth - 1 - 1];
  else delete line.strokeLineDash;
  const solid = (fill) => {
    const o = { ...options, fill, fillStyle: 'solid', roughness: Math.min(1, options.roughness || 0) };
    delete o.strokeLineDash;
    return o;
  };
  const toTip = (p) => p ? [generator.line(p[2], p[3], p[0], p[1], line), generator.line(p[4], p[5], p[0], p[1], line)] : [];
  const bar = (p) => p ? [generator.line(p[2], p[3], p[4], p[5], line)] : [];
  const circle = (p, fill, scale = 1) => {
    if (!p) return [];
    const o = { ...options, fill, fillStyle: 'solid', stroke: e.strokeColor, roughness: Math.min(0.5, options.roughness || 0) };
    delete o.strokeLineDash;
    return [generator.circle(p[0], p[1], p[2] * scale, o)];
  };
  const pts = (h, offset) => headPoints(e, points, curve, position, h, offset);
  switch (head) {
    case 'circle': case 'circle_outline':
      return circle(pts(head), head === 'circle' ? e.strokeColor : paper);
    case 'triangle': case 'triangle_outline': case 'diamond': case 'diamond_outline': {
      const p = pts(head);
      if (!p) return [];
      const corners = [];
      for (let i = 0; i < p.length; i += 2) corners.push([p[i], p[i + 1]]);
      return [generator.polygon([...corners, corners[0]], solid(head.endsWith('_outline') ? paper : e.strokeColor))];
    }
    case 'cardinality_one': return bar(pts(head));
    case 'cardinality_many': return toTip(pts(head));
    case 'cardinality_one_or_many': return [...toTip(pts('cardinality_many')), ...bar(pts('cardinality_one', -0.25))];
    case 'cardinality_exactly_one': return [...bar(pts('cardinality_one', -0.5)), ...bar(pts('cardinality_one'))];
    case 'cardinality_zero_or_one': return [...circle(pts('circle_outline', 1.5), paper, 0.8), ...bar(pts('cardinality_one', -0.5))];
    case 'cardinality_zero_or_many': return [...toTip(pts('cardinality_many')), ...circle(pts('circle_outline', 1.5), paper, 0.8)];
    default: return toTip(pts(head));
  }
}
function elbowPath(points, radius = 16) {
  let d = `M ${points[0][0]} ${points[0][1]}`;
  for (let i = 1; i < points.length - 1; i++) {
    const [prev, point, next] = [points[i - 1], points[i], points[i + 1]];
    const corner = Math.min(radius, Math.hypot(next[0] - point[0], next[1] - point[1]) / 2,
      Math.hypot(prev[0] - point[0], prev[1] - point[1]) / 2);
    const toward = (other) => {
      const horizontal = Math.abs(other[0] - point[0]) >= Math.abs(other[1] - point[1]);
      return horizontal ? [point[0] + Math.sign(other[0] - point[0]) * corner, point[1]]
        : [point[0], point[1] + Math.sign(other[1] - point[1]) * corner];
    };
    const [a, b] = [toward(prev), toward(next)];
    d += ` L ${a[0]} ${a[1]} Q ${point[0]} ${point[1]}, ${b[0]} ${b[1]}`;
  }
  return `${d} L ${points.at(-1)[0]} ${points.at(-1)[1]}`;
}
/* PAPER is the canvas colour an outlined arrowhead is filled with. */
function linearSVG(e, label, paper) {
  const points = e.path, options = roughOptions(e);
  const curve = e.elbowed ? generator.path(elbowPath(points), roughOptions(e, true))
    : !e.roundness ? (options.fill ? generator.polygon(points, options) : generator.linearPath(points, options))
    : generator.curve(points, options);
  const shapes = [curve];
  if (e.type === 'arrow') {
    if (e.startArrowhead) shapes.push(...arrowheads(e, points, curve, options, 'start', e.startArrowhead, paper));
    if (e.endArrowhead) shapes.push(...arrowheads(e, points, curve, options, 'end', e.endArrowhead, paper));
  }
  let body = shapes.map((s) => draw(s)).join(''), defs = '';
  if (label) {
    // Cut a padded hole for the label, as Excalidraw does.
    const [x1, y1, x2, y2] = pointBounds(points), P = BOUND_TEXT_PADDING, id = `mask-${escape(e.id)}`;
    const area = `x="${n(x1 - 100)}" y="${n(y1 - 100)}" width="${n(x2 - x1 + 200)}" height="${n(y2 - y1 + 200)}"`;
    defs = `<mask id="${id}" maskUnits="userSpaceOnUse" ${area}><rect ${area} fill="#fff"/><rect x="${n(label.x - P)}" y="${n(label.y - P)}" width="${n(label.width + 2 * P)}" height="${n(label.height + 2 * P)}" fill="#000"/></mask>`;
    body = `<g mask="url(#${id})">${body}</g>`;
  }
  return defs + `<g stroke-linecap="round">${body}</g>`;
}

// Freedraw (Excalidraw's shape.ts freedraw helpers).
function simplify(points, tolerance) {
  if (points.length < 3) return points;
  const [a, b] = [points[0], points.at(-1)];
  let index = 0, max = 0;
  for (let i = 1; i < points.length - 1; i++) {
    const [px, py] = points[i], [dx, dy] = [b[0] - a[0], b[1] - a[1]], length = Math.hypot(dx, dy);
    const distance = length ? Math.abs(dy * px - dx * py + b[0] * a[1] - b[1] * a[0]) / length
      : Math.hypot(px - a[0], py - a[1]);
    if (distance > max) { max = distance; index = i; }
  }
  if (max <= tolerance) return [a, b];
  return [...simplify(points.slice(0, index + 1), tolerance).slice(0, -1), ...simplify(points.slice(index), tolerance)];
}
function strokePath(outline) {
  if (!outline.length) return '';
  const mid = (a, b) => [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2];
  const parts = ['M', outline[0], 'Q'];
  outline.forEach((p, i) => parts.push(...(i === outline.length - 1 ? [p, mid(p, outline[0]), 'L', outline[0], 'Z'] : [p, mid(p, outline[i + 1])])));
  return parts.map((p) => typeof p === 'string' ? p : `${n(p[0])},${n(p[1])}`).join(' ');
}
function freedrawSVG(e) {
  const constant = e.strokeOptions?.variability === 'constant';
  const input = !e.points.length ? [[0, 0, 0.5]]
    : e.simulatePressure || constant ? e.points.map(([x, y]) => [x, y, constant ? 1 : 0.5])
    : e.points.map(([x, y], i) => [x, y, e.pressures[i] ?? 0.5]);
  // ponytail: constant-width strokes approximate @excalidraw/laser-pointer with an unthinned stroke.
  const outline = getStroke(input, {
    simulatePressure: e.simulatePressure && !constant,
    size: e.strokeWidth * (constant ? 2.8 : 4.25),
    thinning: constant ? 0 : 0.6, smoothing: 0.5,
    streamline: e.strokeOptions?.streamline ?? 0.5,
    easing: (t) => Math.sin((t * Math.PI) / 2), last: true,
  });
  let body = '';
  if (isLoop(e.points) && !transparent(e.backgroundColor))
    body += draw(generator.curve(simplify(e.points, 0.75), { ...roughOptions(e), stroke: 'none' }));
  return body + `<path d="${strokePath(outline)}" fill="${escape(e.strokeColor)}" stroke="none"/>`;
}

function stickyPath(e, random, shadow = 0) {
  const amount = Math.min([0, 1.5, 8][Math.max(0, Math.min(2, Math.round(e.roughness)))], Math.min(e.width, e.height) * 0.012);
  const corners = [[0, 0], [e.width, 0], [e.width, e.height], [0, e.height]]
    .map(([x, y]) => [x + shadow + (random() * 2 - 1) * amount, y + shadow + (random() * 2 - 1) * amount]);
  const radius = e.roundness ? Math.min(Math.min(e.width, e.height) * 0.04, 16) : 0;
  let d = '';
  corners.forEach((c, i) => {
    const prev = corners[(i + 3) % 4], next = corners[(i + 1) % 4];
    const r = Math.min(radius, Math.hypot(c[0] - prev[0], c[1] - prev[1]) / 2, Math.hypot(c[0] - next[0], c[1] - next[1]) / 2);
    const toward = (o) => { const l = Math.hypot(o[0] - c[0], o[1] - c[1]) || 1; return [c[0] + (o[0] - c[0]) / l * r, c[1] + (o[1] - c[1]) / l * r]; };
    const [a, b] = [toward(prev), toward(next)];
    d += `${i ? 'L' : 'M'} ${n(a[0])} ${n(a[1])} Q ${n(c[0])} ${n(c[1])} ${n(b[0])} ${n(b[1])} `;
  });
  return d + 'Z';
}
function seeded(seed) {
  let a = seed >>> 0 || 1;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
// ponytail: sticky notes omit Excalidraw's lifted corner and date footer.
function stickySVG(e) {
  const body = stickyPath(e, seeded(e.seed)), id = `sticky-${escape(e.id)}`;
  return `<path d="${stickyPath(e, seeded(e.seed + 1), 3)}" fill="#000" fill-opacity="0.16" stroke="none"/><path d="${body}" fill="${escape(e.backgroundColor)}" stroke="none"/><clipPath id="${id}"><path d="${body}"/></clipPath><path d="${body}" fill="none" stroke="#000" stroke-opacity="0.08" stroke-width="1" clip-path="url(#${id})"/>`;
}

function imageSVG(e, files) {
  const file = e.fileId && files?.[e.fileId];
  const w = e.width, h = e.height;
  if (!file) return `<rect width="${n(w)}" height="${n(h)}" fill="#e7e7e7"/><path transform="translate(${n(w / 2 - 12)} ${n(h / 2 - 12)})" d="M3 3h18v18H3Zm4 12 4-5 3 4 2-2 4 5H7Z" fill="#888"/>`;
  let uw = w, uh = h, cx = 0, cy = 0;
  if (e.crop) {
    uw = w / (e.crop.width / e.crop.naturalWidth);
    uh = h / (e.crop.height / e.crop.naturalHeight);
    cx = e.crop.x / (e.crop.naturalWidth / uw);
    cy = e.crop.y / (e.crop.naturalHeight / uh);
  }
  const [sx, sy] = e.scale, id = `image-${escape(e.id)}`;
  const flip = sx !== 1 || sy !== 1 ? ` transform="translate(${n(w / 2)} ${n(h / 2)}) scale(${sx} ${sy}) translate(${n(-w / 2)} ${n(-h / 2)})"` : '';
  const radius = e.roundness ? cornerRadius(Math.min(w, h), e) : 0;
  return `<clipPath id="${id}"><rect width="${n(w)}" height="${n(h)}"${radius ? ` rx="${n(radius)}"` : ''}/></clipPath><g clip-path="url(#${id})"><g${flip}><image href="${escape(file.dataURL)}" x="${n(-cx)}" y="${n(-cy)}" width="${n(uw)}" height="${n(uh)}" preserveAspectRatio="none"/></g></g>`;
}

export function textSVG(e) {
  const lines = e.text.split('\n'), lineHeight = e.fontSize * e.lineHeight;
  const x = e.textAlign === 'center' ? e.width / 2 : e.textAlign === 'right' ? e.width : 0;
  const anchor = e.textAlign === 'center' ? 'middle' : e.textAlign === 'right' ? 'end' : 'start';
  const top = verticalOffset(e.fontFamily, e.fontSize, lineHeight);
  // One positioned text per line keeps blank lines and works in viewers that ignore tspans.
  return lines.map((line, i) => `<text x="${n(x)}" y="${n(i * lineHeight + top)}" font-family="${escape(fontStack(e.fontFamily))}" font-size="${n(e.fontSize)}px" fill="${escape(e.strokeColor)}" text-anchor="${anchor}" xml:space="preserve" style="white-space:pre">${escape(line)}</text>`).join('');
}

function frameSVG(e) {
  const name = e.name ?? 'Frame';
  return `<rect class="frame-outline" width="${n(e.width)}" height="${n(e.height)}" rx="8" ry="8" fill="none" stroke="#bbb" stroke-width="2"/><text x="0" y="-6" font-family="Noto Sans, sans-serif" font-size="14px" fill="#999999">${escape(name)}</text>`;
}
function placeholderSVG(e) {
  const filled = { ...e, roughness: 0, backgroundColor: transparent(e.backgroundColor) ? '#d3d3d3' : e.backgroundColor, fillStyle: 'solid' };
  return draw(shapeDrawable(filled)) + `<text x="${n(e.width / 2)}" y="${n(e.height / 2)}" text-anchor="middle" font-family="Noto Sans, sans-serif" font-size="14px" fill="#1e1e1e">${escape((e.link || 'Embedded content').slice(0, 80))}</text>`;
}

/* An unfilled shape is picked by its interior too; roughjs strokes are open lines. */
function silhouette(e) {
  const { width: w, height: h } = e;
  if (e.type === 'ellipse') return `<ellipse class="hit" cx="${n(w / 2)}" cy="${n(h / 2)}" rx="${n(w / 2)}" ry="${n(h / 2)}" fill="transparent"/>`;
  if (e.type === 'diamond')
    return `<path class="hit" d="M${diamondPoints(e).map((p) => p.map(n).join(' ')).join('L')}Z" fill="transparent"/>`;
  return `<rect class="hit" width="${n(w)}" height="${n(h)}" fill="transparent"/>`;
}
const cache = new Map();
/* SVG group for one resolved element. INTERACTIVE adds editor hit areas. */
/* SCENE.paper, white by default, is the canvas colour under the elements. */
export function elementSVG(e, scene, files, interactive = false) {
  const label = e.type === 'arrow' && scene.labels.get(e.id)?.[0];
  const paper = scene.paper || '#ffffff';
  const key = JSON.stringify([e, interactive, e.fileId ? Boolean(files?.[e.fileId]) : 0, LINEAR.includes(e.type) && paper,
    e.frameId && scene.byId.get(e.frameId)?.opacity, label && [label.x, label.y, label.width, label.height]]);
  const hit = cache.get(key);
  if (hit) return hit;
  let svg;
  const deg = n((e.angle * 180) / Math.PI);
  const local = (body) => `<g transform="translate(${n(e.x)} ${n(e.y)})${e.angle ? ` rotate(${deg} ${n(e.width / 2)} ${n(e.height / 2)})` : ''}">${body}</g>`;
  if (LINEAR.includes(e.type)) {
    svg = linearSVG(e, label, paper);
    // A closed line is picked by its interior too, like the shapes' silhouettes.
    const closed = e.type === 'line' && isLoop(e.points);
    if (interactive) svg = `<path class="hit" d="M${e.path.map((p) => `${n(p[0])} ${n(p[1])}`).join('L')}${closed ? 'Z' : ''}" stroke="transparent" stroke-width="14" vector-effect="non-scaling-stroke" fill="${closed ? 'transparent' : 'none'}"/>` + svg;
  } else if (e.type === 'freedraw') {
    const [x1, y1, x2, y2] = pointBounds(e.points);
    const hitArea = interactive && isLoop(e.points)
      ? `<path class="hit" d="M${e.points.map((p) => `${n(p[0])} ${n(p[1])}`).join('L')}Z" fill="transparent"/>` : '';
    svg = `<g transform="translate(${n(e.x)} ${n(e.y)})${e.angle ? ` rotate(${deg} ${n((x1 + x2) / 2)} ${n((y1 + y2) / 2)})` : ''}">${hitArea}${freedrawSVG(e)}</g>`;
  } else if (e.type === 'text')
    svg = local((interactive ? `<rect class="hit" width="${n(e.width)}" height="${n(e.height)}" fill="transparent"/>` : '') + textSVG(e));
  else if (e.type === 'image') svg = local(imageSVG(e, files));
  else if (e.type === 'stickynote') svg = local(stickySVG(e));
  else if (e.type === 'frame' || e.type === 'magicframe') svg = local(frameSVG(e));
  else if (e.type === 'embeddable' || e.type === 'iframe') svg = local(placeholderSVG(e));
  else svg = local((interactive ? silhouette(e) : '') + `<g stroke-linecap="round">${draw(shapeDrawable(e))}</g>`);
  const frame = e.frameId && scene.byId.get(e.frameId);
  const opacity = ((frame && ['frame', 'magicframe'].includes(frame.type) ? frame.opacity : 100) * e.opacity) / 10000;
  svg = `<g data-shape="${escape(e.id)}"${LINEAR.includes(e.type) ? ' data-linear="true"' : ''}${opacity < 1 ? ` opacity="${n(opacity)}"` : ''}>${svg}</g>`;
  if (cache.size > 4000) cache.clear();
  cache.set(key, svg);
  return svg;
}
/* Clip paths for frames, so elements inside a frame stay inside it. */
function frameClips(scene) {
  return scene.order.filter((e) => e.type === 'frame' || e.type === 'magicframe').map((f) =>
    `<clipPath id="frame-${escape(f.id)}"><rect width="${n(f.width)}" height="${n(f.height)}" rx="8" ry="8" transform="translate(${n(f.x)} ${n(f.y)})${f.angle ? ` rotate(${n((f.angle * 180) / Math.PI)} ${n(f.width / 2)} ${n(f.height / 2)})` : ''}"/></clipPath>`).join('');
}
export function sceneSVG(scene, files, interactive = false) {
  return frameClips(scene) + scene.order.map((e) => {
    const svg = elementSVG(e, scene, files, interactive);
    const frame = e.frameId && scene.byId.get(e.frameId);
    return frame && ['frame', 'magicframe'].includes(frame.type) ? `<g clip-path="url(#frame-${escape(frame.id)})">${svg}</g>` : svg;
  }).join('');
}
/* A standalone board SVG on an opaque BACKGROUND, white by default. FONTS
   maps a font family to an embeddable data URL for viewers without the fonts. */
export function boardSVG(elements, { files = {}, maxEdge = 2048, box, maxScale = 1, context = elements, fonts, background = '#ffffff' } = {}) {
  const scene = resolveScene(context);
  const shown = new Set(elements.flatMap((e) => [e.id, ...(scene.labels.get(e.id) || []).map((t) => t.id)]));
  const subset = { ...scene, order: scene.order.filter((e) => shown.has(e.id)), paper: background };
  box ||= bounds(elements, scene);
  const scale = Math.min(maxScale, maxEdge / Math.max(box[2], box[3]));
  const faces = fonts ? Object.entries(fonts).map(([family, url]) => `@font-face{font-family:"${family}";src:url(${url})}`).join('') : '';
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${Math.ceil(box[2] * scale)}" height="${Math.ceil(box[3] * scale)}" viewBox="${box.map(n).join(' ')}">${faces ? `<style>${faces}</style>` : ''}<rect x="${n(box[0])}" y="${n(box[1])}" width="${n(box[2])}" height="${n(box[3])}" fill="${escape(background)}"/>${sceneSVG(subset, files)}</svg>`;
}
/* Families a scene's text uses, for embedding fonts in exports. */
export function usedFonts(elements) {
  return [...new Set(resolveScene(elements).order.filter((e) => e.type === 'text').map((e) => fontStack(e.fontFamily).split(',')[0]))];
}
export function documentHTML(json) {
  const render = (node) => {
    if (node.type === 'text') {
      let value = escape(node.text);
      for (const mark of node.marks || []) {
        const tag = { bold: 'strong', italic: 'em', strike: 's', underline: 'u', code: 'code' }[
          mark.type
        ];
        if (tag) value = `<${tag}>${value}</${tag}>`;
        else if (mark.type === 'link')
          value = `<a href="${escape(mark.attrs.href)}" rel="noreferrer">${value}</a>`;
      }
      return value;
    }
    const inner = (node.content || []).map(render).join('');
    if (node.type === 'doc') return inner;
    if (node.type === 'hardBreak') return '<br>';
    if (node.type === 'horizontalRule') return '<hr>';
    if (node.type === 'image') {
      const a = node.attrs;
      return `<img src="${escape(imageSource(a))}" alt="${escape(a.alt || '')}"${a.title ? ` title="${escape(a.title)}"` : ''}${a.width ? ` width="${a.width}"` : ''}${a.height ? ` height="${a.height}"` : ''}>`;
    }
    if (node.type === 'codeBlock') return `<pre><code>${inner}</code></pre>`;
    const tag =
      node.type === 'heading'
        ? `h${node.attrs?.level || 1}`
        : {
            paragraph: 'p',
            blockquote: 'blockquote',
            bulletList: 'ul',
            orderedList: 'ol',
            listItem: 'li',
            table: 'table',
            tableRow: 'tr',
            tableCell: 'td',
            tableHeader: 'th',
          }[node.type];
    return `<${tag}>${inner}</${tag}>`;
  };
  return `<!doctype html><meta charset="utf-8"><style>body{max-width:55em;margin:3em auto;font:18px/1.6 system-ui}table{border-collapse:collapse}td,th{border:1px solid #888;padding:.4em}pre{white-space:pre-wrap}img{max-width:100%;height:auto}</style><article>${render(json)}</article>`;
}
