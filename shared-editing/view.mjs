/* The model's view of shared items: text addressed as shared://ITEM, one
   line per element or top-level block with the hash SharedEdit names it by.
   Long values point to their own address instead of filling the line, and
   embedded image data stays out of text, so a read costs what it shows. */
import { check } from './model.mjs';

/* JSON with sorted keys, so equal content hashes equally whatever order its
   writers produced. */
export function canonical(value) {
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  if (value && typeof value === 'object')
    return `{${Object.keys(value).sort().filter((k) => value[k] !== undefined)
      .map((k) => `${JSON.stringify(k)}:${canonical(value[k])}`).join(',')}}`;
  return JSON.stringify(value);
}
/* A short content hash: SharedEdit's precondition for one element or block. */
export function contentHash(value) {
  const text = canonical(value);
  let hash = 0xcbf29ce484222325n;
  for (let i = 0; i < text.length; i++)
    hash = BigInt.asUintN(64, (hash ^ BigInt(text.charCodeAt(i))) * 0x100000001b3n);
  return hash.toString(16).padStart(16, '0').slice(0, 12);
}

export const address = (id, ...parts) => [`shared://${id}`, ...parts].join('/');
export const nodesOf = (kind, content) => (kind === 'whiteboard' ? content : content.content || []);
export const nodeId = (kind, node) => (kind === 'whiteboard' ? node.id : node.attrs?.id);

/* Document images are named by their data, never spelled out in text. */
const IMAGE = /^shared:\/\/[a-zA-Z0-9_-]{1,80}\/images\/(img-[0-9a-f]{12})$/;
const imageKey = (src) => `img-${contentHash(src)}`;
function mapImages(node, replace) {
  if (Array.isArray(node)) return node.map((n) => mapImages(n, replace));
  if (!node || typeof node !== 'object') return node;
  const out = {};
  for (const [key, value] of Object.entries(node)) out[key] = mapImages(value, replace);
  if (node.type === 'image' && node.attrs) {
    out.attrs = { ...out.attrs };
    if (typeof node.attrs.src === 'string') out.attrs.src = replace(node.attrs.src);
    if (node.attrs.imageEdit?.src) out.attrs.imageEdit = { ...node.attrs.imageEdit, src: replace(node.attrs.imageEdit.src) };
  }
  return out;
}
/* Data URLs of a document's images by key. */
export function documentImages(content) {
  const images = new Map();
  mapImages(nodesOf('document', content), (src) => (images.set(imageKey(src), src), src));
  return images;
}
/* Keys in one stable order, identity and geometry first: stored key order
   varies with how a record was written and merged. */
const FIRST = ['id', 'type', 'attrs', 'x', 'y', 'width', 'height', 'angle', 'text', 'content', 'marks'];
const rank = (key) => (FIRST.includes(key) ? FIRST.indexOf(key) : FIRST.length);
function ordered(value) {
  if (Array.isArray(value)) return value.map(ordered);
  if (!value || typeof value !== 'object') return value;
  return Object.fromEntries(Object.keys(value).sort((a, b) => rank(a) - rank(b) || (a < b ? -1 : a > b ? 1 : 0))
    .map((key) => [key, ordered(value[key])]));
}
/* NODE as the model sees it: stable key order, and document images as their
   addresses. */
export const shown = (id, kind, node) => ordered(
  kind === 'document' ? mapImages(node, (src) => (src.startsWith('data:') ? address(id, 'images', imageKey(src)) : src)) : node);
/* NODE as the model wrote it, with image addresses restored to their data. */
export function restoreImages(node, images) {
  return mapImages(node, (src) => {
    const match = IMAGE.exec(src);
    if (!match) return src;
    check(images.has(match[1]), `No image ${match[1]} in this document`);
    return images.get(match[1]);
  });
}

/* A line stays readable at Read's 2000-character cap; past that, a stroke's
   samples move to the element's own address. */
const LINE = 1800;
export function line(id, kind, node) {
  const value = shown(id, kind, node);
  let json = JSON.stringify(value);
  if (json.length > LINE && Array.isArray(value.points)) {
    const elsewhere = `${value.points.length} points; Read ${address(id, 'elements', node.id)}`;
    const { pressures, ...rest } = value;
    json = JSON.stringify({ ...rest, points: elsewhere, ...(pressures ? { pressures: `${pressures.length} pressures` } : {}) });
  }
  return `${contentHash(node)} ${json}`;
}

/* One element or block in full: its hash, then JSON with each point on a
   line, so even a long stroke pages. */
export function element(id, kind, node) {
  const value = shown(id, kind, node);
  const fields = Object.entries(value).map(([key, v]) => {
    const text = key === 'points' || key === 'pressures'
      ? `[\n${v.map((p) => `    ${JSON.stringify(p)}`).join(',\n')}\n  ]`
      : JSON.stringify(v);
    return `  ${JSON.stringify(key)}: ${text}`;
  });
  return `hash ${contentHash(node)}\n{\n${fields.join(',\n')}\n}\n`;
}

export function overview(id, state, inspected, comments) {
  const { kind, title, background, content } = inspected;
  const nodes = nodesOf(kind, content);
  const noun = kind === 'whiteboard' ? 'element' : 'block';
  const head = [
    `${kind} ${JSON.stringify(title)} · revision ${state.revision} · ${nodes.length} ${noun}${nodes.length === 1 ? '' : 's'}`
      + (background ? ` · background ${background}` : ''),
    `Lines are HASH ${noun.toUpperCase()}-JSON in ${kind === 'whiteboard' ? 'drawing' : 'document'} order. ` +
      `${noun[0].toUpperCase() + noun.slice(1)}: ${address(id, 'elements', 'ID')}` +
      (kind === 'whiteboard' ? ` · Rendering: ${address(id, 'view.png')}` : '') +
      ` · Comments (${comments.length}): ${address(id, 'comments')} · History: ${address(id, 'history')}`,
  ];
  return `${[...head, ...nodes.map((node) => line(id, kind, node))].join('\n')}\n`;
}

export function commentsView(id, comments) {
  if (!comments.length) return 'No comments.\n';
  return `${comments.map((c) => {
    const anchor = c.liveSelection ? `on ${c.liveSelection.join(', ') || 'removed objects'}` : '';
    const area = c.region ? ` · area ${c.region.join(', ')}` : '';
    const lines = [`comment ${c.id} · ${c.resolved ? 'resolved' : 'open'} · ${c.anchorStatus}${anchor ? ` · ${anchor}` : ''}${area}`,
      `  quote: ${JSON.stringify(c.liveQuote ?? c.quote ?? '')}`,
      `  ${c.actor}: ${JSON.stringify(c.text)}`,
      ...(c.replies || []).map((r) => `  ${r.actor}: ${JSON.stringify(r.text)}`)];
    return lines.join('\n');
  }).join('\n')}\n`;
}

/* Contributions newest first; Revert takes a contribution id. */
export function historyView(state) {
  if (!state.transactions.length) return `No retained contributions at revision ${state.revision}.\n`;
  const describe = (c) => `${c.id} (${!c.before ? 'added' : !c.after ? 'deleted' : 'changed'})`;
  return `${state.transactions.map((tx) => [
    `${tx.id} · revision ${tx.revision} · ${tx.actor} · ${new Date(tx.time).toISOString()}`,
    tx.changes.length ? ` · ${tx.changes.map(describe).join(', ')}` : '',
    tx.title ? ` · title ${JSON.stringify(tx.title.before)} → ${JSON.stringify(tx.title.after)}` : '',
    tx.background ? ` · background ${tx.background.before ?? 'theme'} → ${tx.background.after ?? 'theme'}` : '',
  ].join('')).join('\n')}\n`;
}
