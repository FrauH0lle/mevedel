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
/* The identity of every item's discussion about the whole item. */
export const GENERAL_DISCUSSION = 'general';
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

/* Keep physical lines below Read's cap, including JSON escape expansion. */
const LINE = 1800;
const STRING = 800;

/* Structural JSON with an explicit output-only notation for long strings.
   Stored content and hashes never use this notation. */
export function readable(value) {
  let chunked = false;
  const indent = depth => '  '.repeat(Math.min(depth, 20));
  function string(value, depth) {
    const encoded = JSON.stringify(value);
    if (encoded.length <= STRING) return encoded;
    chunked = true;
    const chunks = [];
    let part = '', size = 2;
    for (const char of value) {
      const cost = JSON.stringify(char).length - 2;
      if (size + cost > STRING) { chunks.push(JSON.stringify(part)); part = ''; size = 2; }
      part += char;
      size += cost;
    }
    if (part) chunks.push(JSON.stringify(part));
    return `string-chunks(\n${chunks.map(chunk => indent(depth + 1) + chunk).join('\n')}\n${indent(depth)})`;
  }
  function format(value, depth) {
    if (typeof value === 'string') return string(value, depth);
    if (!value || typeof value !== 'object') return JSON.stringify(value ?? null);
    if (Array.isArray(value)) {
      if (!value.length) return '[]';
      if (value.every(v => !v || typeof v !== 'object') && JSON.stringify(value).length <= STRING)
        return JSON.stringify(value);
      return `[\n${value.map(v => indent(depth + 1) + format(v, depth + 1)).join(',\n')}\n${indent(depth)}]`;
    }
    const fields = Object.entries(value).filter(([, v]) => v !== undefined);
    if (!fields.length) return '{}';
    return `{\n${fields.map(([key, v]) => `${indent(depth + 1)}${string(key, depth + 1)}: ${format(v, depth + 1)}`).join(',\n')}\n${indent(depth)}}`;
  }
  const text = format(value, 0);
  return (chunked ? 'Output notation: string-chunks(...) is one string. Decode each ordered JSON string line and join without separators before SharedEdit.\n' : '') + text;
}

/* Short values stay on one line; longer ones advertise their lossless view. */
export function line(id, kind, node, summarize = true) {
  const value = shown(id, kind, node);
  let json = JSON.stringify(value);
  if (json.length > LINE && Array.isArray(value.points)) {
    const elsewhere = `${value.points.length} points; Read ${address(id, 'elements', node.id)}`;
    const { pressures, ...rest } = value;
    json = JSON.stringify({ ...rest, points: elsewhere, ...(pressures ? { pressures: `${pressures.length} pressures` } : {}) });
  }
  if (summarize && json.length > LINE) json = JSON.stringify({
    id: nodeId(kind, node), type: value.type,
    abbreviated: `Read ${address(id, 'elements', nodeId(kind, node))}`,
    preview: json.slice(0, 320),
  });
  return `${contentHash(node)} ${json}`;
}

/* One element or block in full, structured and lossless even for long text. */
export function element(id, kind, node, search = false) {
  const value = shown(id, kind, node);
  return `hash ${contentHash(node)}\n${search ? JSON.stringify(value) : readable(value)}\n`;
}

export function overview(id, state, inspected, discussions, search = false) {
  const { kind, title, background, content } = inspected;
  const nodes = nodesOf(kind, content);
  const noun = kind === 'whiteboard' ? 'element' : 'block';
  const head = [
    `${kind} ${JSON.stringify(title)} · revision ${state.revision} · ${nodes.length} ${noun}${nodes.length === 1 ? '' : 's'}`
      + (background ? ` · background ${background}` : ''),
    `Lines are HASH ${noun.toUpperCase()}-JSON in ${kind === 'whiteboard' ? 'drawing' : 'document'} order; abbreviated entries are summaries, not replacement JSON. ` +
      `${noun[0].toUpperCase() + noun.slice(1)}: ${address(id, 'elements', 'ID')}` +
      (kind === 'whiteboard' ? ` · Rendering: ${address(id, 'view.png')}` : '') +
      ` · Discussions (${discussions.length}): ${address(id, 'discussions')} · History: ${address(id, 'history')}`,
  ];
  // Search receives original strings, never preview or artificial chunk boundaries.
  const lines = nodes.map(node => search
    ? `${contentHash(node)} ${JSON.stringify(shown(id, kind, node))}` : line(id, kind, node));
  return `${[...head, ...lines].join('\n')}\n`;
}

/* Discussions, the one about the whole item first: each with its target's
   live state, then its messages in order. */
export function discussionsView(discussions, search = false) {
  if (!discussions.length) return 'No discussions.\n';
  const ordered = [...discussions].sort((a, b) =>
    (b.id === GENERAL_DISCUSSION) - (a.id === GENERAL_DISCUSSION));
  return `${ordered.map((d) => {
    const target = d.target.kind === 'whole' ? 'whole item'
      : d.liveSelection ? `on ${d.liveSelection.join(', ') || 'removed objects'}` : 'passage';
    const area = d.target.region ? ` · area ${d.target.region.join(', ')}` : '';
    const status = d.target.kind === 'whole' ? '' : ` · ${d.anchorStatus}`;
    const lines = [`discussion ${d.id} · ${d.resolved ? 'resolved' : 'open'}${status} · ${target}${area}`,
      ...(d.target.kind === 'whole' ? [] : [`  quote: ${JSON.stringify(d.liveQuote ?? d.quote ?? '')}`]),
      ...d.messages.map((m) => `  ${m.actor}: ${m.alone ? '(sent the selection as the request)' : JSON.stringify(m.text)}`)];
    return lines.map(line => !search && line.length > LINE ? readable(line) : line).join('\n');
  }).join('\n')}\n`;
}

/* Contributions newest first; Revert takes a contribution id. */
export function historyView(state, search = false) {
  if (!state.transactions.length) return `No retained contributions at revision ${state.revision}.\n`;
  const describe = (c) => `${c.id} (${!c.before ? 'added' : !c.after ? 'deleted' : 'changed'})`;
  return `${state.transactions.map((tx) => [
    `${tx.id} · revision ${tx.revision} · ${tx.actor} · ${new Date(tx.time).toISOString()}`,
    tx.changes.length ? ` · ${tx.changes.map(describe).join(', ')}` : '',
    tx.title ? ` · title ${JSON.stringify(tx.title.before)} → ${JSON.stringify(tx.title.after)}` : '',
    tx.background ? ` · background ${tx.background.before ?? 'theme'} → ${tx.background.after ?? 'theme'}` : '',
  ].join('')).map(line => !search && line.length > LINE ? readable(line) : line).join('\n')}\n`;
}
