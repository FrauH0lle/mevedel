/* Deterministic, data-only exports used by both browser and host. */
import { imageSource } from './image.mjs';
export const escape = (value) =>
  String(value ?? '').replace(
    /[&<>"']/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c],
  );
export const FILLABLE = ['rect', 'ellipse', 'diamond', 'cylinder', 'sticky'];
export const LINEAR = ['arrow', 'line', 'pen'];
/* Resolved style of a shape: every absent property has one fixed meaning. */
export function styleOf(s) {
  return {
    stroke: s.stroke || '#242424',
    fill: s.fill || (s.type === 'sticky' ? '#fff1a8' : 'none'),
    pattern: s.pattern || 'solid',
    width: s.width || 2,
    dash: s.dash || 'solid',
    rough: s.rough || 0,
    edges: s.edges || 'round',
    opacity: s.opacity ?? 100,
    fontSize: s.fontSize || 16,
    layer: s.layer || 0,
  };
}
export function pathPoints(shape, context) {
  const [x, y, w, h] = shape.box;
  const points = shape.points || [
    [x, y],
    [x + w, y + h],
  ];
  if (shape.type === 'pen') return points;
  // ponytail: linear lookup, at most 2,000 shapes; index if measured render cost warrants it.
  const targets = [shape.from, shape.to].map(id => context.find(s => s.id === id));
  const centers = targets.map((target, i) => target
    ? [target.box[0] + target.box[2] / 2, target.box[1] + target.box[3] / 2]
    : i ? points.at(-1) : points[0]);
  return targets.map((target, i) => target
    ? borderPoint(target, centers[1 - i]) : centers[i]);
}

// Intersect a center-to-center ray with the nominal silhouette. Hand-drawn
// wobble is decorative; it must not make a connector's attachment wander.
function borderPoint(shape, toward) {
  const [x, y, w, h] = shape.box, rx = w / 2, ry = h / 2;
  const center = [x + rx, y + ry], dx = toward[0] - center[0], dy = toward[1] - center[1];
  if (!w || !h || (!dx && !dy)) return center;
  const scale = Math.min(rx / Math.abs(dx), ry / Math.abs(dy));
  const vx = dx * scale, vy = dy * scale;
  const radius = shape.type === 'rect' && styleOf(shape).edges === 'round' && Math.min(w,h) > 6
    ? Math.min(32, Math.min(w,h) * .25) : 0;
  const vertices = shape.type === 'sticky' ? sticky(shape.box) : null;
  const inside = t => {
    const px = Math.abs(vx * t), py = Math.abs(vy * t);
    if (shape.type === 'ellipse') return (px / rx) ** 2 + (py / ry) ** 2 <= 1;
    if (shape.type === 'diamond') return px / rx + py / ry <= 1;
    if (shape.type === 'cylinder') {
      const rim = rimRy(h);
      return (px / rx) ** 2 + (Math.max(0, py - ry + rim) / rim) ** 2 <= 1;
    }
    if (vertices) return vertices.every((a, i) => {
      const b = vertices[(i + 1) % vertices.length];
      return (b[0]-a[0]) * (center[1]+vy*t-a[1]) - (b[1]-a[1]) * (center[0]+vx*t-a[0]) >= 0;
    });
    return !radius || Math.max(0, px-rx+radius) ** 2 + Math.max(0, py-ry+radius) ** 2 <= radius ** 2;
  };
  let low = 0, high = 1;
  // Most rays meet a straight side. Curved corners need a bounded search.
  if (inside(high)) low = high;
  else for (let i = 0; i < 32; i++) {
    const mid = (low + high) / 2;
    if (inside(mid)) low = mid;
    else high = mid;
  }
  return [center[0] + vx * low, center[1] + vy * low];
}

/* Tight [x, y, w, h] around shapes. Connectors use their drawn path: a bound
   arrow's stored box goes stale when its endpoints move. */
export function extent(shapes, context = shapes) {
  let left = Infinity,
    top = Infinity,
    right = -Infinity,
    bottom = -Infinity;
  for (const s of shapes)
    for (const [x, y] of LINEAR.includes(s.type)
      ? pathPoints(s, context)
      : [[s.box[0], s.box[1]], [s.box[0] + s.box[2], s.box[1] + s.box[3]]]) {
      left = Math.min(left, x);
      top = Math.min(top, y);
      right = Math.max(right, x);
      bottom = Math.max(bottom, y);
    }
  return [left, top, right - left, bottom - top];
}

export function bounds(shapes, context = shapes) {
  if (!shapes.length) return [-40, -40, 800, 500];
  const [left, top, width, height] = extent(shapes, context);
  return [left - 30, top - 30, Math.max(100, width + 60), Math.max(100, height + 60)];
}

/* Shapes a region [x, y, w, h] contains entirely, or touches when TOUCHING. */
export function shapesInRegion(shapes, region, touching = false) {
  const [rx, ry, rw, rh] = region;
  return shapes.filter((s) => {
    const [x, y, w, h] = extent([s], shapes);
    return touching
      ? x <= rx + rw && x + w >= rx && y <= ry + rh && y + h >= ry
      : x >= rx && y >= ry && x + w <= rx + rw && y + h <= ry + rh;
  });
}


/* Sloppy geometry. The generator is seeded from the shape id, so every
   browser and the host PNG draw the same wobble for the same shape. */
const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
const n = (v) => String(+v.toFixed(2));
function random(seed) {
  let a = 0;
  for (const c of seed) a = (a * 31 + c.charCodeAt(0)) | 0;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const offset = (length, r) => (r <= 0 ? 0 : (r + 0.18 * r * r) * clamp(length / 100, 0.35, 1.5));
function line(x1, y1, x2, y2, r, rng, cont) {
  const dx = x2 - x1,
    dy = y2 - y1,
    length = Math.hypot(dx, dy) || 0.001,
    o = offset(length, r),
    j = () => (rng() * 2 - 1) * o,
    nx = -dy / length,
    ny = dx / length,
    bow = (rng() * 2 - 1) * o * 0.9;
  return (
    (cont ? '' : `M${n(x1 + j() * 0.5)} ${n(y1 + j() * 0.5)}`) +
    `C${n(x1 + dx * 0.3 + j() * 0.7 + nx * bow)} ${n(y1 + dy * 0.3 + j() * 0.7 + ny * bow)} ${n(x1 + dx * 0.7 + j() * 0.7 + nx * bow)} ${n(y1 + dy * 0.7 + j() * 0.7 + ny * bow)} ${n(x2 + j() * 0.5)} ${n(y2 + j() * 0.5)}`
  );
}
function polygon(pts, r, rng) {
  return (
    pts.map((a, i) => line(...a, ...pts[(i + 1) % pts.length], r, rng, i > 0)).join('') + 'Z'
  );
}
function through(pts) {
  let d = `M${n(pts[0][0])} ${n(pts[0][1])}`;
  for (let i = 1; i < pts.length - 1; i++)
    d += `Q${n(pts[i][0])} ${n(pts[i][1])} ${n((pts[i][0] + pts[i + 1][0]) / 2)} ${n((pts[i][1] + pts[i + 1][1]) / 2)}`;
  return d + `L${n(pts.at(-1)[0])} ${n(pts.at(-1)[1])}`;
}
function ellipse(cx, cy, rx, ry, r, rng) {
  if (r <= 0 || rx < 1 || ry < 1)
    return `M${n(cx + rx)} ${n(cy)}A${n(rx)} ${n(ry)} 0 1 0 ${n(cx - rx)} ${n(cy)}A${n(rx)} ${n(ry)} 0 1 0 ${n(cx + rx)} ${n(cy)}Z`;
  const steps = clamp(Math.round((Math.PI * (rx + ry)) / 10), 16, 90) + 3,
    o = offset(Math.min(rx, ry) * 2, r),
    ph1 = rng() * Math.PI * 2,
    ph2 = rng() * Math.PI * 2,
    k1 = 2 + Math.floor(rng() * 2),
    k2 = 4 + Math.floor(rng() * 3),
    a1 = o * (0.55 + rng() * 0.45),
    a2 = o * 0.35 * rng(),
    start = rng() * Math.PI * 2,
    total = Math.PI * 2 + 0.14 + rng() * 0.16,
    pts = [];
  for (let i = 0; i <= steps; i++) {
    const a = start + (total * i) / steps,
      d = a1 * Math.sin(k1 * a + ph1) + a2 * Math.sin(k2 * a + ph2);
    pts.push([cx + (rx + d) * Math.cos(a), cy + (ry + d * 0.85) * Math.sin(a)]);
  }
  return through(pts);
}
function arc(cx, cy, rx, ry, a0, a1, r, rng) {
  const steps = clamp(Math.round((Math.PI * (rx + ry)) / 12), 8, 60),
    o = offset(Math.min(rx, ry) * 2, r) * 0.6,
    pts = [];
  for (let i = 0; i <= steps; i++) {
    const a = a0 + ((a1 - a0) * i) / steps,
      d = (rng() * 2 - 1) * o;
    pts.push([cx + (rx + d) * Math.cos(a), cy + (ry + d * 0.85) * Math.sin(a)]);
  }
  return through(pts);
}
function roundRect(x, y, w, h, rad, r, rng) {
  const o = offset(Math.min(w, h), r) * 0.5,
    j = () => (rng() * 2 - 1) * o,
    q = (cx, cy, ex, ey) => `Q${n(cx)} ${n(cy)} ${n(ex)} ${n(ey)}`;
  return (
    line(x + rad, y, x + w - rad, y, r, rng, false) +
    q(x + w + j(), y + j(), x + w + j() * 0.3, y + rad) +
    line(x + w, y + rad, x + w, y + h - rad, r, rng, true) +
    q(x + w + j(), y + h + j(), x + w - rad, y + h + j() * 0.3) +
    line(x + w - rad, y + h, x + rad, y + h, r, rng, true) +
    q(x + j(), y + h + j(), x + j() * 0.3, y + h - rad) +
    line(x, y + h - rad, x, y + rad, r, rng, true) +
    q(x + j(), y + j(), x + rad + j() * 0.3, y + j() * 0.3)
  );
}
const rimRy = (h) => Math.min(16, h * 0.18);
const sticky = ([x, y, w, h]) => {
  const lift = Math.min(3, h * 0.06);
  return [
    [x + 1, y + lift],
    [x + w - 2, y],
    [x + w, y + h - 1],
    [x, y + h],
  ];
};
/* Outline paths of one shape: the strokes it is drawn with, and its
   smooth silhouette used for fills. */
function outline(s, r, rng) {
  const [x, y, w, h] = s.box;
  if (s.type === 'ellipse') return [ellipse(x + w / 2, y + h / 2, w / 2, h / 2, r, rng)];
  if (s.type === 'diamond')
    return [
      polygon(
        [
          [x + w / 2, y],
          [x + w, y + h / 2],
          [x + w / 2, y + h],
          [x, y + h / 2],
        ],
        r,
        rng,
      ),
    ];
  if (s.type === 'sticky') return [polygon(sticky(s.box), r, rng)];
  if (s.type === 'cylinder') {
    const ry = rimRy(h),
      cx = x + w / 2;
    if (r <= 0)
      return [
        ellipse(cx, y + ry, w / 2, ry, 0, rng),
        `M${n(x)} ${n(y + ry)}V${n(y + h - ry)}A${n(w / 2)} ${n(ry)} 0 0 0 ${n(x + w)} ${n(y + h - ry)}V${n(y + ry)}`,
      ];
    return [
      ellipse(cx, y + ry, w / 2, ry, r, rng),
      line(x, y + ry, x, y + h - ry, r, rng, false) +
        line(x + w, y + ry, x + w, y + h - ry, r, rng, false),
      arc(cx, y + h - ry, w / 2, ry, 0, Math.PI, r, rng),
    ];
  }
  const rad = Math.min(32, Math.min(w, h) * 0.25);
  if (styleOf(s).edges === 'round' && Math.min(w, h) > 6)
    return [
      r <= 0
        ? `M${n(x + rad)} ${n(y)}H${n(x + w - rad)}A${n(rad)} ${n(rad)} 0 0 1 ${n(x + w)} ${n(y + rad)}V${n(y + h - rad)}A${n(rad)} ${n(rad)} 0 0 1 ${n(x + w - rad)} ${n(y + h)}H${n(x + rad)}A${n(rad)} ${n(rad)} 0 0 1 ${n(x)} ${n(y + h - rad)}V${n(y + rad)}A${n(rad)} ${n(rad)} 0 0 1 ${n(x + rad)} ${n(y)}Z`
        : roundRect(x, y, w, h, rad, r, rng),
    ];
  return [
    polygon(
      [
        [x, y],
        [x + w, y],
        [x + w, y + h],
        [x, y + h],
      ],
      r,
      rng,
    ),
  ];
}
function hatch(s, style, rng) {
  const [x, y, w, h] = s.box,
    cx = x + w / 2,
    cy = y + h / 2,
    R = Math.hypot(w, h) / 2;
  // ponytail: at most 400 lines per direction, so a board-sized shape stays cheap to draw.
  const gap = Math.max(6, style.width * 3.4, (2 * R) / 400);
  let d = '';
  for (const angle of style.pattern === 'cross' ? [-Math.PI / 4, Math.PI / 4] : [-Math.PI / 4]) {
    const dx = Math.cos(angle),
      dy = Math.sin(angle);
    for (let t = -R - gap + gap * rng(); t <= R + gap; t += gap) {
      const px = cx - dy * t,
        py = cy + dx * t;
      d += line(px - dx * R, py - dy * R, px + dx * R, py + dy * R, style.rough * 0.35, rng, false);
    }
  }
  return d;
}
export function shapeSVG(s, shapes) {
  const [x, y, w, h] = s.box,
    style = styleOf(s),
    ink = escape(style.stroke),
    rng = random(s.id),
    dash =
      style.dash === 'dashed'
        ? ` stroke-dasharray="10 ${n(7 + style.width)}"`
        : style.dash === 'dotted'
          ? ` stroke-dasharray="1.2 ${n(4.5 + style.width * 1.4)}"`
          : '';
  const attrs = `stroke="${ink}" stroke-width="${style.width}" stroke-linecap="round" stroke-linejoin="round"${dash}`;
  const passes = style.rough > 0 && style.dash === 'solid' ? 2 : 1;
  let body = '';
  if (s.type === 'image')
    body = `<image href="${escape(imageSource(s))}" x="${x}" y="${y}" width="${w}" height="${h}"/>`;
  else if (LINEAR.includes(s.type)) {
    const pts = pathPoints(s, shapes);
    if (s.type === 'pen') body = `<path d="${through(pts)}" fill="none" ${attrs}/>`;
    else
      for (let p = 0; p < passes; p++)
        body += `<path d="${pts.map((a, i) => (i ? line(...pts[i - 1], ...a, style.rough, rng, i > 1) : '')).join('')}" fill="none" ${attrs}/>`;
    if (s.type === 'arrow') {
      const [ax, ay] = pts.at(-2), [bx, by] = pts.at(-1);
      const length = Math.hypot(bx - ax, by - ay);
      if (length) {
        const ux = (bx - ax) / length, uy = (by - ay) / length, size = style.width * 7;
        // Draw the head directly so SVG readers need no marker/context paint support.
        body += `<path d="M${n(bx)} ${n(by)}L${n(bx - size * ux - size / 2 * uy)} ${n(by - size * uy + size / 2 * ux)}L${n(bx - size * ux + size / 2 * uy)} ${n(by - size * uy - size / 2 * ux)}Z" fill="${ink}"/>`;
      }
    }
  } else if (s.type !== 'text') {
    const fill = escape(style.fill);
    if (fill !== 'none') {
      const silhouette = outline(s, 0, rng).join('');
      if (style.pattern === 'solid') body += `<path d="${silhouette}" fill="${fill}" stroke="none"/>`;
      else
        body += `<clipPath id="clip-${escape(s.id)}"><path d="${silhouette}"/></clipPath><g clip-path="url(#clip-${escape(s.id)})"><path d="${hatch(s, style, rng)}" fill="none" stroke="${fill}" stroke-width="${n(Math.max(1, style.width * 0.6))}" stroke-linecap="round"/></g>`;
    }
    for (let p = 0; p < passes; p++)
      for (const d of outline(s, style.rough, rng)) body += `<path d="${d}" fill="none" ${attrs}/>`;
  }
  if (s.text) {
    const centered = !['text', ...LINEAR].includes(s.type),
      size = style.fontSize,
      lead = size * 1.375;
    const lines = s.text.split('\n'),
      tx = centered ? x + w / 2 : x + 10;
    const ty = centered ? y + h / 2 - ((lines.length - 1) * lead) / 2 + size * 0.375 : y + size * 1.5;
    // Explicit baselines also work in SVG viewers that ignore tspan positions.
    // Empty lines still consume a full line of vertical space.
    body += lines.map((line, i) => `<text x="${tx}" y="${n(ty + i * lead)}" text-anchor="${centered ? 'middle' : 'start'}" font-family="Noto Sans, sans-serif" font-size="${size}" fill="${ink}">${escape(line)}</text>`).join('');
  }
  return `<g data-shape="${escape(s.id)}"${style.opacity < 100 ? ` opacity="${style.opacity / 100}"` : ''}>${body}</g>`;
}
export function boardSVG(shapes, maxEdge = 2048, context = shapes, box = bounds(shapes, context), maxScale = 1) {
  const scale = Math.min(maxScale, maxEdge / Math.max(box[2], box[3]));
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${Math.ceil(box[2] * scale)}" height="${Math.ceil(box[3] * scale)}" viewBox="${box.join(' ')}"><rect x="${box[0]}" y="${box[1]}" width="${box[2]}" height="${box[3]}" fill="#ffffff"/>${shapes.map((s) => shapeSVG(s, context)).join('')}</svg>`;
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
