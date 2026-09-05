/* Deterministic, data-only exports used by both browser and host. */
export const escape = (value) =>
  String(value ?? '').replace(
    /[&<>"']/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c],
  );
export function pathPoints(shape, context) {
  const [x, y, w, h] = shape.box;
  const points = shape.points || [
    [x, y],
    [x + w, y + h],
  ];
  if (shape.type === 'pen') return points;
  // ponytail: linear lookup, at most 2,000 shapes; index if measured render cost warrants it.
  const center = (id) => {
    const target = context.find((s) => s.id === id);
    return target ? [target.box[0] + target.box[2] / 2, target.box[1] + target.box[3] / 2] : null;
  };
  return [center(shape.from) || points[0], center(shape.to) || points.at(-1)];
}
export function bounds(shapes, context = shapes) {
  if (!shapes.length) return [-40, -40, 800, 500];
  let left = Infinity,
    top = Infinity,
    right = -Infinity,
    bottom = -Infinity;
  for (const s of shapes)
    for (const [x, y] of [
      [s.box[0], s.box[1]],
      [s.box[0] + s.box[2], s.box[1] + s.box[3]],
      ...(['arrow', 'line', 'pen'].includes(s.type) ? pathPoints(s, context) : []),
    ]) {
      left = Math.min(left, x);
      top = Math.min(top, y);
      right = Math.max(right, x);
      bottom = Math.max(bottom, y);
    }
  return [left - 30, top - 30, Math.max(100, right - left + 60), Math.max(100, bottom - top + 60)];
}
export function shapeSVG(s, shapes) {
  const [x, y, w, h] = s.box,
    ink = escape(s.stroke || '#242424'),
    fill = escape(s.fill || (s.type === 'sticky' ? '#fff1a8' : 'none'));
  const attrs = `stroke="${ink}" fill="${fill}" stroke-width="${s.width || 2}"`;
  let body = '';
  if (s.type === 'ellipse')
    body = `<ellipse cx="${x + w / 2}" cy="${y + h / 2}" rx="${w / 2}" ry="${h / 2}" ${attrs}/>`;
  else if (s.type === 'diamond')
    body = `<polygon points="${x + w / 2},${y} ${x + w},${y + h / 2} ${x + w / 2},${y + h} ${x},${y + h / 2}" ${attrs}/>`;
  else if (s.type === 'cylinder') {
    const r = Math.min(h / 4, 20);
    body = `<path d="M${x},${y + r}a${w / 2},${r} 0 0 1 ${w},0v${h - 2 * r}a${w / 2},${r} 0 0 1 ${-w},0Z" ${attrs}/><ellipse cx="${x + w / 2}" cy="${y + r}" rx="${w / 2}" ry="${r}" ${attrs}/>`;
  } else if (s.type === 'image')
    body = `<image href="${escape(s.src)}" x="${x}" y="${y}" width="${w}" height="${h}"/>`;
  else if (s.type === 'arrow' || s.type === 'line' || s.type === 'pen') {
    const pts = pathPoints(s, shapes);
    body = `<polyline points="${pts.map((p) => p.join(',')).join(' ')}" fill="none" stroke="${ink}" stroke-width="${s.width || 2}" stroke-linecap="round" stroke-linejoin="round"${s.type === 'arrow' ? ' marker-end="url(#arrowhead)"' : ''}/>`;
  } else if (s.type !== 'text')
    body = `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="4" ${attrs}/>`;
  if (s.text) {
    const centered = !['text', 'arrow', 'line', 'pen'].includes(s.type);
    const lines = s.text.split('\n'),
      tx = centered ? x + w / 2 : x + 10;
    const ty = centered ? y + h / 2 - (lines.length - 1) * 11 + 6 : y + 24;
    body += `<text x="${tx}" y="${ty}" text-anchor="${centered ? 'middle' : 'start'}" font-family="Noto Sans, sans-serif" font-size="16" fill="${ink}">${lines
      .map((line, i) => `<tspan x="${tx}" dy="${i ? 22 : 0}">${escape(line)}</tspan>`)
      .join('')}</text>`;
  }
  return `<g data-shape="${escape(s.id)}">${body}</g>`;
}
export const definitions =
  '<defs><marker id="arrowhead" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="context-stroke"/></marker></defs>';
export function boardSVG(shapes, maxEdge = 2048, context = shapes) {
  const box = bounds(shapes, context),
    scale = Math.min(1, maxEdge / Math.max(box[2], box[3]));
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${Math.ceil(box[2] * scale)}" height="${Math.ceil(box[3] * scale)}" viewBox="${box.join(' ')}">${definitions}<rect x="${box[0]}" y="${box[1]}" width="${box[2]}" height="${box[3]}" fill="#faf9f5"/>${shapes.map((s) => shapeSVG(s, context)).join('')}</svg>`;
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
  return `<!doctype html><meta charset="utf-8"><style>body{max-width:55em;margin:3em auto;font:18px/1.6 system-ui}table{border-collapse:collapse}td,th{border:1px solid #888;padding:.4em}pre{white-space:pre-wrap}</style><article>${render(json)}</article>`;
}
