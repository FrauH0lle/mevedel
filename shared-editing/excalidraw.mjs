/* .excalidraw scenes and .excalidrawlib libraries. Import keeps the fields
   mevedel stores (see model.mjs) after Excalidraw's own restore migrations;
   export completes every element and derives Excalidraw's reconciliation
   fields, so excalidraw.com and other editors open the result directly. */
import { generateNKeysBetween } from 'fractional-indexing';
import { check, identifier, validateElement, validateFile, fieldValid, TYPES, ARROWHEADS } from './model.mjs';
import { complete, resolveScene, seedOf, pointBounds, LINEAR } from './scene.mjs';
import { lineHeightOf } from './text.mjs';

export const SOURCE = 'https://github.com/FrauH0lle/mevedel';
const COMMON = ['id', 'type', 'x', 'y', 'width', 'height', 'angle', 'strokeColor', 'backgroundColor',
  'fillStyle', 'strokeWidth', 'strokeStyle', 'roughness', 'opacity', 'roundness', 'seed', 'index',
  'groupIds', 'frameId', 'link', 'locked', 'customData', 'created'];
const LEGACY_HEADS = { dot: 'circle', crowfoot_one: 'cardinality_one', crowfoot_many: 'cardinality_many',
  crowfoot_one_or_many: 'cardinality_one_or_many' };
const FONTS = { Virgil: 1, Helvetica: 2, Cascadia: 3, Excalifont: 5, Nunito: 6, 'Lilita One': 7,
  'Comic Shanns': 8, 'Liberation Sans': 9, Assistant: 10 };

function restoreBinding(binding) {
  if (!binding || typeof binding !== 'object' || !binding.elementId) return null;
  // Legacy focus/gap bindings re-attach at the centre; Excalidraw recomputes
  // them from geometry too, and differs only until the shape first moves.
  const point = Array.isArray(binding.fixedPoint) && binding.fixedPoint.length === 2 &&
    binding.fixedPoint.every(Number.isFinite)
    ? binding.fixedPoint.map((v) => Math.max(-10, Math.min(10, v))) : [0.5001, 0.5001];
  return { elementId: String(binding.elementId), fixedPoint: point,
    mode: ['inside', 'orbit', 'skip'].includes(binding.mode) ? binding.mode : 'orbit' };
}
/* One Excalidraw element as stored here, or null for elements Excalidraw
   itself would drop. Unknown and derived keys are removed. */
export function restoreElement(source) {
  if (!source || typeof source !== 'object' || source.isDeleted || !Object.hasOwn(TYPES, source.type)) return null;
  const e = {};
  for (const key of [...COMMON, ...TYPES[source.type]])
    if (source[key] !== undefined) e[key] = source[key];
  if (e.roundness === undefined && source.strokeSharpness === 'round')
    e.roundness = { type: ['rectangle', 'image', 'iframe', 'embeddable'].includes(e.type) ? 1 : 2 };
  for (const key of ['x', 'y', 'width', 'height']) e[key] = Number.isFinite(e[key]) ? e[key] : 0;
  if (e.width < 0) { e.x += e.width; e.width = -e.width; }
  if (e.height < 0) { e.y += e.height; e.height = -e.height; }
  if (e.type === 'text') {
    if (typeof source.font === 'string' && e.fontSize === undefined) {
      e.fontSize = parseFloat(source.font) || 20;
      e.fontFamily = FONTS[source.font.replace(/^[\d.]+px\s*/, '')] || 5;
    }
    e.text = typeof e.text === 'string' ? e.text : '';
    if (!e.text) return null;
    if (e.lineHeight === undefined && e.height && e.fontSize)
      e.lineHeight = e.height / e.text.split('\n').length / e.fontSize;
    e.lineHeight ??= lineHeightOf(e.fontFamily ?? 5);
  }
  for (const key of ['startArrowhead', 'endArrowhead'])
    if (Object.hasOwn(e, key)) e[key] = LEGACY_HEADS[e[key]] || (ARROWHEADS.includes(e[key]) ? e[key] : null);
  if (e.type === 'arrow') {
    if (source.endArrowhead === undefined) e.endArrowhead = 'arrow';
    e.startBinding = restoreBinding(source.startBinding);
    e.endBinding = restoreBinding(source.endBinding);
  }
  if (['line', 'arrow', 'freedraw'].includes(e.type)) {
    const points = (Array.isArray(e.points) ? e.points : []).filter((p) =>
      Array.isArray(p) && p.length >= 2 && Number.isFinite(p[0]) && Number.isFinite(p[1])).map((p) => [p[0], p[1]]);
    e.points = points.length >= (e.type === 'freedraw' ? 1 : 2) ? points : [[0, 0], [e.width, e.height]];
    if (e.type === 'freedraw' && Array.isArray(e.pressures))
      e.pressures = e.pressures.slice(0, e.points.length).map((p) => Number.isFinite(p) ? Math.max(0, Math.min(1, p)) : 0.5);
  }
  if (e.type === 'image') e.scale = Array.isArray(e.scale) && e.scale.length === 2 ? e.scale.map((v) => (v < 0 ? -1 : 1)) : [1, 1];
  // Like Excalidraw's restore, an unusable optional value falls back to its default.
  for (const key of Object.keys(e)) if (!fieldValid(key, e[key])) delete e[key];
  for (const key of ['x', 'y']) e[key] = Math.max(-1e6, Math.min(1e6, e[key] ?? 0));
  for (const key of ['width', 'height']) e[key] = Math.min(2e6, e[key] ?? 0);
  if (e.containerId === e.id) delete e.containerId;
  if (e.type === 'text' && !e.text) return null;
  if (['line', 'arrow'].includes(e.type) && !(e.points?.length >= 2)) return null;
  if (e.type === 'freedraw' && !e.points?.length) return null;
  return e;
}

/* Restore a list of elements: drop invisible ones, give invalid or
   duplicate ids fresh ones (and follow references), and validate. */
function restoreElements(list) {
  check(Array.isArray(list), 'Expected an elements array');
  const elements = list.map(restoreElement).filter(Boolean);
  const ids = new Map(), seen = new Set();
  for (const e of elements) {
    const old = e.id;
    if (!identifier(old) || seen.has(old)) e.id = crypto.randomUUID();
    seen.add(e.id);
    if (identifier(old) && !ids.has(old)) ids.set(old, e.id);
  }
  for (const e of elements) {
    if (e.containerId) e.containerId = ids.get(e.containerId) ?? null;
    if (e.frameId) e.frameId = ids.get(e.frameId) ?? null;
    for (const key of ['startBinding', 'endBinding'])
      if (e[key]) e[key] = ids.has(e[key].elementId) ? { ...e[key], elementId: ids.get(e[key].elementId) } : null;
    if (e.groupIds) e.groupIds = e.groupIds.filter(identifier);
    try {
      validateElement(e);
    } catch (error) {
      throw new Error(`Element ${String(e.id).slice(0, 40)}: ${error.message}`);
    }
  }
  return elements;
}

/* Parse .excalidraw JSON TEXT into stored elements and image files. Notes
   explain anything that could not be kept. */
export function parseScene(text) {
  const data = typeof text === 'string' ? JSON.parse(text) : text;
  check(data && data.type === 'excalidraw' && (!data.elements || Array.isArray(data.elements)),
    'Not an Excalidraw scene');
  const elements = restoreElements(data.elements || []);
  check(elements.length <= 4000, 'Too many elements');
  const files = {}, notes = [];
  for (const e of elements) {
    if (e.type !== 'image' || !e.fileId || files[e.fileId]) continue;
    const file = data.files?.[e.fileId];
    if (!file) continue;
    const entry = { mimeType: file.mimeType, dataURL: file.dataURL, ...(Number.isFinite(file.created) ? { created: file.created } : {}) };
    try {
      validateFile(e.fileId, entry);
      files[e.fileId] = entry;
    } catch (error) {
      notes.push(`Image ${e.fileId.slice(0, 12)}: ${error.message}; shown as a placeholder`);
    }
  }
  return { elements, files, notes };
}

/* Excalidraw library items, from version 1 or 2 .excalidrawlib JSON. */
export function parseLibrary(text) {
  const data = typeof text === 'string' ? JSON.parse(text) : text;
  check(data && data.type === 'excalidrawlib' && [1, 2].includes(data.version), 'Not an Excalidraw library');
  const items = data.libraryItems || data.library;
  check(Array.isArray(items) && items.length <= 1000, 'Invalid library items');
  return items.map((item) => {
    const raw = Array.isArray(item) ? { elements: item } : item;
    const elements = restoreElements(raw?.elements || []);
    return elements.length ? {
      id: typeof raw.id === 'string' && raw.id.length <= 80 ? raw.id : crypto.randomUUID(),
      status: raw.status === 'published' ? 'published' : 'unpublished',
      created: Number.isFinite(raw.created) ? raw.created : Date.now(),
      ...(typeof raw.name === 'string' ? { name: raw.name.slice(0, 200) } : {}),
      elements,
    } : null;
  }).filter(Boolean);
}

/* Complete Excalidraw elements for ELEMENTS in displayed order, with bound
   geometry resolved, references repaired and reconciliation fields derived. */
export function exportElements(elements, now = Date.now()) {
  const scene = resolveScene(elements);
  const present = new Set(scene.order.map((e) => e.id));
  const bound = new Map();
  const attach = (target, id, type) => {
    if (present.has(target)) bound.set(target, [...(bound.get(target) || []), { id, type }]);
  };
  for (const e of scene.order) {
    if (e.bound) attach(e.containerId, e.id, 'text');
    for (const key of ['startBinding', 'endBinding'])
      if (e[key] && present.has(e[key].elementId)) attach(e[key].elementId, e.id, 'arrow');
  }
  const keys = generateNKeysBetween(null, null, scene.order.length);
  return scene.order.map((item, i) => {
    const { path, box, bound: isBound, ...e } = item;
    if (LINEAR.includes(e.type) && !e.angle) {
      const [x1, y1, x2, y2] = pointBounds(path);
      Object.assign(e, { x: path[0][0], y: path[0][1], width: x2 - x1, height: y2 - y1,
        points: path.map(([x, y]) => [x - path[0][0], y - path[0][1]]) });
    }
    for (const key of ['startBinding', 'endBinding'])
      if (e[key] && !present.has(e[key].elementId)) e[key] = null;
    if (e.type === 'text') {
      if (!isBound) e.containerId = null;
      e.autoResize ??= true;
    }
    if (e.frameId && !present.has(e.frameId)) e.frameId = null;
    return { ...complete(e, isBound), index: keys[i], version: 1, versionNonce: seedOf(`${e.id}:${now}`),
      isDeleted: false, boundElements: bound.get(e.id) || null, updated: now, created: e.created ?? null };
  });
}
export function serializeScene(elements, files, now = Date.now()) {
  const exported = exportElements(elements, now);
  const used = new Set(exported.map((e) => e.fileId).filter(Boolean));
  return JSON.stringify({
    type: 'excalidraw', version: 2, source: SOURCE, elements: exported,
    appState: { gridSize: 20, gridStep: 5, gridModeEnabled: false, viewBackgroundColor: '#ffffff', lockedMultiSelections: {} },
    files: Object.fromEntries(Object.entries(files).filter(([id]) => used.has(id))
      .map(([id, file]) => [id, { mimeType: file.mimeType, id, dataURL: file.dataURL, created: file.created ?? now }])),
  }, null, 2);
}
export function serializeLibrary(items, now = Date.now()) {
  return JSON.stringify({
    type: 'excalidrawlib', version: 2, source: SOURCE,
    libraryItems: items.map((item) => ({ id: item.id, status: item.status || 'unpublished',
      created: item.created ?? now, ...(item.name ? { name: item.name } : {}),
      elements: exportElements(item.elements, now) })),
  }, null, 2);
}

/* Copies of library ELEMENTS centred on CENTER with fresh ids, seeds and
   groups; references among the copies follow, others are dropped. */
export function placeElements(elements, center, indices, seed) {
  const boxes = elements.map((e) => e.points
    ? (([x1, y1, x2, y2]) => [e.x + x1, e.y + y1, e.x + x2, e.y + y2])(pointBounds(e.points))
    : [e.x, e.y, e.x + e.width, e.y + e.height]);
  const left = Math.min(...boxes.map((b) => b[0])), top = Math.min(...boxes.map((b) => b[1]));
  const width = Math.max(...boxes.map((b) => b[2])) - left, height = Math.max(...boxes.map((b) => b[3])) - top;
  const fresh = new Map(elements.map((e) => [e.id, crypto.randomUUID()]));
  const groups = new Map();
  const group = (g) => (groups.has(g) || groups.set(g, crypto.randomUUID().replace(/-/g, '')), groups.get(g));
  return elements.map((e, i) => {
    const copy = { ...e, id: fresh.get(e.id), seed: seed(), index: indices[i],
      x: e.x - left + center[0] - width / 2, y: e.y - top + center[1] - height / 2 };
    if (copy.groupIds) copy.groupIds = copy.groupIds.map(group);
    if (copy.containerId) copy.containerId = fresh.get(copy.containerId) ?? null;
    if (copy.containerId === null) delete copy.containerId;
    if (copy.frameId) copy.frameId = fresh.get(copy.frameId) ?? null;
    for (const key of ['startBinding', 'endBinding']) {
      if (!copy[key]) continue;
      if (fresh.has(copy[key].elementId)) copy[key] = { ...copy[key], elementId: fresh.get(copy[key].elementId) };
      else delete copy[key];
    }
    return copy;
  });
}
