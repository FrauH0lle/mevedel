/* Text measurement and layout shared by the browser editor and host renderer.
   Advance widths come from the bundled fonts, so every client wraps the
   same text at the same places. Constants follow Excalidraw's text model. */
import metrics from './font-metrics.json' with { type: 'json' };

export const BOUND_TEXT_PADDING = 5;
const STICKY_PADDING = 16, STICKY_INSET_Y = 52;
/* Excalidraw family ids: [rendering font, lineHeight, ascender, descender, unitsPerEm]. */
const FAMILIES = {
  1: ['Excalifont', 1.25, 886, -374, 1000],
  2: ['Noto Sans', 1.15, 1577, -471, 2048],
  3: ['Comic Shanns', 1.2, 1900, -480, 2048],
  5: ['Excalifont', 1.25, 886, -374, 1000],
  6: ['Nunito', 1.25, 1011, -353, 1000],
  7: ['Nunito', 1.15, 923, -220, 1000],
  8: ['Comic Shanns', 1.25, 750, -250, 1000],
  9: ['Noto Sans', 1.15, 1854, -434, 2048],
  10: ['Noto Sans', 1.25, 1021, -287, 2048],
};
const family = (id) => FAMILIES[id] || FAMILIES[5];
export const lineHeightOf = (id) => family(id)[1];
/* CSS/SVG font-family list for an Excalidraw family id. */
export const fontStack = (id) =>
  `${family(id)[0]}, Noto Sans, ${[3, 8].includes(id) ? 'monospace' : 'sans-serif'}`;
export const FONT_FILES = { Excalifont: 'Excalifont', Nunito: 'Nunito', 'Comic Shanns': 'ComicShanns' };

const widths = new Map();
export function lineWidth(text, fontFamily, fontSize) {
  const table = metrics[family(fontFamily)[0]];
  let units = 0;
  for (const char of text) {
    const code = char.codePointAt(0);
    let advance = widths.get(table.name + code);
    if (advance === undefined) {
      advance = table.advances[code] ?? metrics['Noto Sans'].advances[code] ?? table.fallback;
      widths.set(table.name + code, advance);
    }
    units += advance;
  }
  return (units / table.unitsPerEm) * fontSize;
}
export const normalizeText = (text) => String(text ?? '').replace(/\r\n?/g, '\n').replace(/\t/g, '    ');
export function measure(text, fontFamily, fontSize, lineHeight = lineHeightOf(fontFamily)) {
  const lines = normalizeText(text).split('\n');
  return {
    width: Math.max(0, ...lines.map((line) => lineWidth(line || ' ', fontFamily, fontSize))),
    height: lines.length * fontSize * lineHeight,
  };
}
/* Baseline of the first line from the top of the text box. */
export function verticalOffset(fontFamily, fontSize, lineHeightPx) {
  const [, , ascender, descender, unitsPerEm] = family(fontFamily);
  const em = fontSize / unitsPerEm;
  return em * ascender + (lineHeightPx - em * ascender + em * descender) / 2;
}

// Break before and after whitespace, after hyphens, and around CJK characters.
const CJK = '\\p{Script=Han}\\p{Script=Hiragana}\\p{Script=Katakana}\\p{Script=Hangul}';
const TOKENS = new RegExp(`\\s+|[${CJK}]|[^\\s${CJK}]+?-(?=[^\\s])|[^\\s${CJK}]+`, 'gu');
export function wrap(text, fontFamily, fontSize, maxWidth) {
  if (!Number.isFinite(maxWidth) || maxWidth < 0) return normalizeText(text);
  const fits = (value) => lineWidth(value, fontFamily, fontSize) <= maxWidth;
  const out = [];
  for (const hard of normalizeText(text).split('\n')) {
    if (fits(hard)) { out.push(hard); continue; }
    let line = '';
    const push = () => { out.push(line.replace(/\s+$/u, '')); line = ''; };
    for (const token of hard.match(TOKENS) || []) {
      if (fits(line + token)) { line += token; continue; }
      if (/^\s+$/u.test(token)) { push(); continue; }
      if (line.trim()) push();
      line = '';
      // A word wider than the box breaks between characters.
      for (const char of token) {
        if (line && !fits(line + char)) push();
        line += char;
      }
    }
    if (line || !out.length) push();
  }
  return out.join('\n');
}

/* The text box available inside a container, from Excalidraw's textElement.ts. */
export function containerTextBox(container, fontSize = 20) {
  const { width: w, height: h, type } = container, P = BOUND_TEXT_PADDING;
  if (type === 'ellipse') {
    const k = 1 - Math.SQRT2 / 2;
    return { x: (w / 2) * k + P, y: (h / 2) * k + P,
      width: Math.round((w / 2) * Math.SQRT2) - 2 * P, height: Math.round((h / 2) * Math.SQRT2) - 2 * P };
  }
  if (type === 'diamond')
    return { x: w / 4 + P, y: h / 4 + P, width: Math.round(w / 2) - 2 * P, height: Math.round(h / 2) - 2 * P };
  if (type === 'stickynote')
    return { x: STICKY_PADDING, y: STICKY_PADDING, width: w - 2 * STICKY_PADDING, height: Math.max(0, h - STICKY_INSET_Y) };
  if (type === 'arrow')
    return { x: 0, y: 0, width: Math.max(0.7 * w, fontSize * 11), height: Infinity };
  return { x: P, y: P, width: w - 2 * P, height: h - 2 * P };
}

/* The point at FRACTION of a polyline's arc length. */
export function pointAlong(points, fraction) {
  const lengths = points.slice(1).map((p, i) => Math.hypot(p[0] - points[i][0], p[1] - points[i][1]));
  let remaining = Math.max(0, Math.min(1, fraction)) * lengths.reduce((a, b) => a + b, 0);
  for (let i = 0; i < lengths.length; i++) {
    if (remaining <= lengths[i] || i === lengths.length - 1) {
      const t = lengths[i] ? Math.min(1, remaining / lengths[i]) : 0;
      return points[i].map((v, axis) => v + (points[i + 1][axis] - v) * t);
    }
    remaining -= lengths[i];
  }
  return points[0];
}
function arrowLabelCenter(points, labelPosition) {
  if (labelPosition != null) return pointAlong(points, labelPosition);
  const n = points.length;
  if (n % 2) return points[Math.floor(n / 2)];
  const [a, b] = [points[n / 2 - 1], points[n / 2]];
  return [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2];
}

/* Displayed geometry of a text element: free text measures itself; bound
   text wraps to and aligns inside its container. CONTAINER is resolved, with
   absolute PATH points for an arrow. */
export function layoutText(text, container, path) {
  const fontSize = text.fontSize ?? 20, fontFamily = text.fontFamily ?? 5;
  const lineHeight = text.lineHeight ?? lineHeightOf(fontFamily);
  const original = text.originalText ?? text.text ?? '';
  if (!container) {
    const autoResize = text.autoResize ?? true;
    const shown = autoResize ? normalizeText(original) : wrap(original, fontFamily, fontSize, text.width);
    const size = measure(shown, fontFamily, fontSize, lineHeight);
    return { ...text, text: shown, width: autoResize ? size.width : text.width, height: size.height };
  }
  const box = containerTextBox(container, fontSize);
  const shown = wrap(original, fontFamily, fontSize, Math.max(0, box.width));
  const { width, height } = measure(shown, fontFamily, fontSize, lineHeight);
  if (container.type === 'arrow') {
    const [cx, cy] = arrowLabelCenter(path, text.labelPosition);
    return { ...text, text: shown, width, height, x: cx - width / 2, y: cy - height / 2, angle: 0 };
  }
  const align = text.textAlign ?? 'center', vertical = text.verticalAlign ?? 'middle';
  let x = box.x + (align === 'left' ? 0 : align === 'right' ? box.width - width : box.width / 2 - width / 2);
  let y = vertical === 'top' ? box.y : vertical === 'bottom' ? box.y + box.height - height
    : container.type === 'stickynote'
      ? box.y + Math.min((container.height - 2 * STICKY_PADDING - height) / 2, box.height - height)
      : box.y + box.height / 2 - height / 2;
  const angle = container.angle || 0;
  if (angle) {
    // Rotate the text centre about the container centre.
    const [cx, cy] = [container.width / 2, container.height / 2];
    const [dx, dy] = [x + width / 2 - cx, y + height / 2 - cy];
    x = cx + dx * Math.cos(angle) - dy * Math.sin(angle) - width / 2;
    y = cy + dx * Math.sin(angle) + dy * Math.cos(angle) - height / 2;
  }
  return { ...text, text: shown, width, height, x: container.x + x, y: container.y + y, angle };
}

/* Container size needed for a label, from Excalidraw's computeContainerDimensionForBoundText. */
export function containerSizeFor(dimension, type) {
  dimension = Math.ceil(dimension);
  const pad = BOUND_TEXT_PADDING * 2;
  if (type === 'ellipse') return Math.round(((dimension + pad) / Math.SQRT2) * 2);
  if (type === 'arrow') return dimension + 80;
  if (type === 'diamond') return 2 * (dimension + pad);
  return dimension + pad;
}
