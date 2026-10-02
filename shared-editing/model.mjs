/* Canonical shared content. Browser and host use the same validated Yjs model.
   Whiteboards hold Excalidraw elements and their image files. */
import * as Y from 'yjs';
import { equalityDeep } from 'lib0/function';
import { documentJSON, validateDocument } from './document.mjs';
import { validateImage } from './image.mjs';

export const LIMIT = 16 * 1024 * 1024;
export const identifier = (value) =>
  typeof value === 'string' && /^[a-zA-Z0-9_-]{1,80}$/.test(value);
export const same = equalityDeep;
export const clone = (value) => JSON.parse(JSON.stringify(value));
export function check(ok, message) {
  if (!ok) throw new Error(message);
}
export function create(kind, title) {
  check(['whiteboard', 'document'].includes(kind), 'Unknown editor kind');
  const doc = new Y.Doc();
  doc.getMap('meta').set('kind', kind);
  doc.getMap('meta').set('title', title);
  doc.getMap('elements');
  doc.getMap('files');
  doc.getXmlFragment('document');
  validate(doc);
  return doc;
}
export const encode = (doc) => Y.encodeStateAsUpdate(doc);

/* Geometry is one atomic Yjs property, so concurrent moves and resizes never
   combine one writer's points with another writer's size. */
export const GEOMETRY = ['x', 'y', 'width', 'height', 'angle', 'points', 'pressures'];
export function elementJSON(value) {
  const { geometry, ...properties } = value.toJSON();
  check(
    geometry && Object.keys(geometry).every((k) => GEOMETRY.includes(k)),
    'Invalid element geometry record',
  );
  check(GEOMETRY.every((k) => !Object.hasOwn(properties, k)), 'Geometry must stay together');
  return { ...properties, ...geometry };
}

/* Z-order: fractional `index` keys, then id. Elements without an index draw
   above indexed ones, as when the model adds them without choosing a place. */
export function compareOrder(a, b) {
  if (a.index && b.index && a.index !== b.index) return a.index < b.index ? -1 : 1;
  if (!a.index !== !b.index) return a.index ? -1 : 1;
  return a.id < b.id ? -1 : a.id > b.id ? 1 : 0;
}
export function inspect(doc) {
  const background = doc.getMap('meta').get('background');
  return {
    kind: doc.getMap('meta').get('kind'),
    title: doc.getMap('meta').get('title'),
    // A whiteboard's canvas colour, shared like its title; absent is the theme's.
    ...(background ? { background } : {}),
    content:
      doc.getMap('meta').get('kind') === 'document'
        ? documentJSON(doc)
        : [...doc.getMap('elements').values()].map(elementJSON).sort(compareOrder),
  };
}
export const filesOf = (doc) => doc.getMap('files').toJSON();
export function restore(bytes) {
  check(bytes.length <= LIMIT, 'Shared content is too large');
  const doc = new Y.Doc();
  try {
    Y.applyUpdate(doc, bytes);
    validate(doc);
    return doc;
  } catch (error) {
    doc.destroy();
    throw error;
  }
}

// Validation of Excalidraw elements. Absent optional fields take Excalidraw's
// defaults; Excalidraw's own reconciliation fields are derived on export.
const finite = (low, high) => (v) => Number.isFinite(v) && v >= low && v <= high;
const oneOf = (...values) => (v) => values.includes(v);
const nullable = (test) => (v) => v === null || test(v);
const string = (max) => (v) => typeof v === 'string' && v.length <= max;
const point = (v) => Array.isArray(v) && v.length === 2 && v.every(finite(-1e6, 1e6));
/* An opaque canvas colour, as Excalidraw's viewBackgroundColor. */
export const validBackground = (v) => typeof v === 'string' && /^#[0-9a-fA-F]{6}$/.test(v);
const COLOR = /^(transparent|#[0-9a-fA-F]{3,4}|#[0-9a-fA-F]{6}|#[0-9a-fA-F]{8}|[a-zA-Z]{3,20}|(rgb|rgba|hsl|hsla)\([0-9.,%\s/+-]{1,60}\))$/;
export const ARROWHEADS = ['arrow', 'bar', 'circle', 'circle_outline', 'triangle', 'triangle_outline',
  'diamond', 'diamond_outline', 'cardinality_one', 'cardinality_many', 'cardinality_one_or_many',
  'cardinality_exactly_one', 'cardinality_zero_or_one', 'cardinality_zero_or_many'];
const binding = nullable((v) => v && typeof v === 'object' &&
  Object.keys(v).every((k) => ['elementId', 'fixedPoint', 'mode'].includes(k)) &&
  identifier(v.elementId) && Array.isArray(v.fixedPoint) && v.fixedPoint.length === 2 &&
  v.fixedPoint.every(finite(-10, 10)) && ['inside', 'orbit', 'skip'].includes(v.mode));
const FIELDS = {
  id: identifier,
  type: string(20),
  x: finite(-1e6, 1e6),
  y: finite(-1e6, 1e6),
  width: finite(0, 2e6),
  height: finite(0, 2e6),
  angle: finite(-1e3, 1e3),
  strokeColor: (v) => typeof v === 'string' && COLOR.test(v),
  backgroundColor: (v) => typeof v === 'string' && COLOR.test(v),
  fillStyle: oneOf('hachure', 'cross-hatch', 'solid', 'zigzag'),
  strokeWidth: finite(0, 64),
  strokeStyle: oneOf('solid', 'dashed', 'dotted'),
  roughness: finite(0, 5),
  opacity: finite(0, 100),
  roundness: nullable((v) => v && typeof v === 'object' &&
    Object.keys(v).every((k) => ['type', 'value'].includes(k)) && [1, 2, 3].includes(v.type) &&
    (v.value === undefined || finite(0, 1e4)(v.value))),
  seed: (v) => Number.isSafeInteger(v) && v >= 0 && v <= 2 ** 31,
  index: nullable((v) => typeof v === 'string' && /^[0-9A-Za-z]{1,128}$/.test(v)),
  groupIds: (v) => Array.isArray(v) && v.length <= 32 && v.every(identifier),
  frameId: nullable(identifier),
  link: nullable(string(2048)),
  locked: (v) => typeof v === 'boolean',
  customData: (v) => v && typeof v === 'object' && !Array.isArray(v) && JSON.stringify(v).length <= 16384,
  created: nullable(finite(0, 1e15)),
  baseHeight: finite(0, 2e6),
  text: string(10000),
  originalText: string(10000),
  fontSize: finite(1, 2000),
  fontFamily: (v) => Number.isSafeInteger(v) && v >= 1 && v <= 1000,
  textAlign: oneOf('left', 'center', 'right'),
  verticalAlign: oneOf('top', 'middle', 'bottom'),
  containerId: nullable(identifier),
  autoResize: (v) => typeof v === 'boolean',
  lineHeight: finite(0.1, 10),
  baseFontSize: nullable(finite(1, 2000)),
  labelPosition: nullable(finite(0, 1)),
  points: (v) => Array.isArray(v) && v.length >= 1 && v.length <= 4000 && v.every(point),
  pressures: (v) => Array.isArray(v) && v.length <= 4000 && v.every(finite(0, 1)),
  simulatePressure: (v) => typeof v === 'boolean',
  strokeOptions: (v) => v && typeof v === 'object' &&
    Object.keys(v).every((k) => ['variability', 'streamline'].includes(k)) &&
    ['constant', 'variable'].includes(v.variability) && finite(0, 1)(v.streamline),
  startArrowhead: nullable(oneOf(...ARROWHEADS)),
  endArrowhead: nullable(oneOf(...ARROWHEADS)),
  polygon: (v) => typeof v === 'boolean',
  startBinding: binding,
  endBinding: binding,
  elbowed: (v) => typeof v === 'boolean',
  fixedSegments: nullable((v) => Array.isArray(v) && v.length <= 1000 && v.every((s) =>
    s && point(s.start) && point(s.end) && Number.isSafeInteger(s.index) &&
    Object.keys(s).every((k) => ['start', 'end', 'index'].includes(k)))),
  startIsSpecial: nullable((v) => typeof v === 'boolean'),
  endIsSpecial: nullable((v) => typeof v === 'boolean'),
  fileId: nullable(identifier),
  status: oneOf('pending', 'saved', 'error'),
  scale: (v) => Array.isArray(v) && v.length === 2 && v.every((n) => n === 1 || n === -1),
  crop: nullable((v) => v && typeof v === 'object' &&
    Object.keys(v).every((k) => ['x', 'y', 'width', 'height', 'naturalWidth', 'naturalHeight'].includes(k)) &&
    ['x', 'y', 'width', 'height', 'naturalWidth', 'naturalHeight'].every((k) => finite(0, 1e5)(v[k]))),
  name: nullable(string(200)),
};
const COMMON = ['id', 'type', 'x', 'y', 'width', 'height', 'angle', 'strokeColor', 'backgroundColor',
  'fillStyle', 'strokeWidth', 'strokeStyle', 'roughness', 'opacity', 'roundness', 'seed', 'index',
  'groupIds', 'frameId', 'link', 'locked', 'customData', 'created'];
const LINEAR = ['points', 'startArrowhead', 'endArrowhead'];
export const TYPES = {
  rectangle: [], diamond: [], ellipse: [], embeddable: [], iframe: [],
  stickynote: ['baseHeight'],
  text: ['text', 'originalText', 'fontSize', 'fontFamily', 'textAlign', 'verticalAlign', 'containerId',
    'autoResize', 'lineHeight', 'baseFontSize', 'labelPosition'],
  line: [...LINEAR, 'polygon'],
  arrow: [...LINEAR, 'startBinding', 'endBinding', 'elbowed', 'fixedSegments', 'startIsSpecial', 'endIsSpecial'],
  freedraw: ['points', 'pressures', 'simulatePressure', 'strokeOptions'],
  image: ['fileId', 'status', 'scale', 'crop'],
  frame: ['name'], magicframe: ['name'],
};
export const DERIVED = ['version', 'versionNonce', 'updated', 'isDeleted', 'boundElements'];
/* Whether VALUE is acceptable for element field KEY. */
export const fieldValid = (key, value) => Object.hasOwn(FIELDS, key) && value !== undefined && FIELDS[key](value);
export function validateElement(element) {
  check(element && typeof element === 'object' && identifier(element.id), 'Invalid element identity');
  check(Object.hasOwn(TYPES, element.type), `Unknown element type ${String(element.type).slice(0, 20)}`);
  const allowed = [...COMMON, ...TYPES[element.type]];
  for (const key of Object.keys(element)) {
    check(!DERIVED.includes(key), `${key} is derived on export; omit it`);
    check(allowed.includes(key), `Unknown ${element.type} property ${key.slice(0, 40)}`);
    check(element[key] !== undefined && FIELDS[key](element[key]), `Invalid ${element.type} ${key}`);
  }
  for (const key of ['x', 'y', 'width', 'height'])
    check(Object.hasOwn(element, key), `An element needs ${key}`);
  if (element.type === 'text') check(Object.hasOwn(element, 'text'), 'A text element needs text');
  if (['line', 'arrow', 'freedraw'].includes(element.type)) {
    check(element.points, `A ${element.type} needs points`);
    check(element.type === 'freedraw' || element.points.length >= 2, 'A line needs two points');
  }
  check(element.containerId !== element.id, 'Text cannot contain itself');
}
/* An embedded image file; returns its pixel count. */
export function validateFile(id, file) {
  check(identifier(id) && file && typeof file === 'object' &&
    Object.keys(file).every((k) => ['mimeType', 'dataURL', 'created'].includes(k)), 'Invalid image file');
  check(['image/png', 'image/jpeg', 'image/webp'].includes(file.mimeType) &&
    file.dataURL?.startsWith?.(`data:${file.mimeType};base64,`), 'Images must be PNG, JPEG, or WebP');
  check(file.created === undefined || Number.isFinite(file.created), 'Invalid image file');
  return validateImage(file.dataURL);
}
export function validate(doc) {
  check(
    [...doc.share.keys()].every((key) => ['meta', 'elements', 'files', 'document'].includes(key)),
    'Unknown shared content',
  );
  const meta = doc.getMap('meta');
  check(
    [...meta.keys()].every((key) => ['kind', 'title', 'background'].includes(key)),
    'Unknown editor property',
  );
  check(['whiteboard', 'document'].includes(meta.get('kind')), 'Unknown editor kind');
  check(!meta.has('background') || (meta.get('kind') === 'whiteboard' && validBackground(meta.get('background'))),
    'Invalid canvas background');
  check(
    typeof meta.get('title') === 'string' &&
      meta.get('title').trim().length > 0 &&
      meta.get('title').length <= 200,
    'Invalid title',
  );
  const elements = doc.getMap('elements'), files = doc.getMap('files');
  check(elements.size <= 4000, 'Too many elements');
  for (const [id, value] of elements) {
    check(value instanceof Y.Map && value.get('id') === id, 'Invalid element record');
    validateElement(elementJSON(value));
  }
  check(files.size <= 200, 'Too many image files');
  let pixels = 0;
  for (const [id, file] of files) pixels += validateFile(id, file);
  check(pixels <= 32000000, 'Board images exceed 32 megapixels in total');
  if (meta.get('kind') === 'document') {
    check(elements.size === 0 && files.size === 0, 'Elements cannot enter a document');
    if (doc.getXmlFragment('document').length) validateDocument(documentJSON(doc));
  } else check(doc.getXmlFragment('document').length === 0, 'Text document cannot enter a board');
  check(encode(doc).length <= LIMIT, 'Shared content is too large');
}
export function putElement(doc, element) {
  validateElement(element);
  const geometry = {}, properties = {};
  for (const [key, value] of Object.entries(element))
    (GEOMETRY.includes(key) ? geometry : properties)[key] = value;
  properties.geometry = geometry;
  const elements = doc.getMap('elements');
  doc.transact(() => {
    let target = elements.get(element.id);
    if (!target) {
      target = new Y.Map();
      elements.set(element.id, target);
    }
    for (const key of [...target.keys()]) if (!(key in properties)) target.delete(key);
    for (const [key, value] of Object.entries(properties))
      if (!same(target.get(key), value)) target.set(key, clone(value));
  });
}
export function putFile(doc, id, file) {
  validateFile(id, file);
  if (!doc.getMap('files').has(id)) doc.getMap('files').set(id, clone(file));
}
/* Remove image files that neither an element nor KEEP references. */
export function pruneFiles(doc, keep = new Set()) {
  const used = new Set([...keep, ...[...doc.getMap('elements').values()].map((e) => e.get('fileId'))]);
  doc.transact(() => {
    for (const id of [...doc.getMap('files').keys()]) if (!used.has(id)) doc.getMap('files').delete(id);
  });
}
export function applyUpdate(doc, bytes) {
  // Validate a disposable replica before altering the accepted state.
  const candidate = restore(encode(doc));
  try {
    Y.applyUpdate(candidate, bytes);
    validate(candidate);
    check(
      candidate.getMap('meta').get('kind') === doc.getMap('meta').get('kind'),
      'Cannot change editor kind',
    );
    Y.applyUpdate(doc, bytes);
  } finally {
    candidate.destroy();
  }
}
export function patch(doc, changes) {
  check(
    Array.isArray(changes) && changes.length > 0 && changes.length <= 200,
    'Expected 1 to 200 changes',
  );
  const elements = doc.getMap('elements'),
    ids = new Set();
  for (const change of changes) {
    check(identifier(change.id) && !ids.has(change.id), 'Invalid or duplicate target');
    ids.add(change.id);
    const current = elements.has(change.id) ? elementJSON(elements.get(change.id)) : null;
    if (!same(current, change.before))
      throw Object.assign(new Error(`Stale target: ${change.id}`), {
        code: 'stale',
        targets: [{ id: change.id, current }],
      });
    if (change.after !== null) {
      validateElement(change.after);
      check(change.after.id === change.id, 'Cannot change element identity');
      check(change.after.type !== 'image' || !change.after.fileId ||
        doc.getMap('files').has(change.after.fileId), 'Images must reference an existing file');
    }
  }
  doc.transact(() => {
    for (const change of changes) {
      if (change.after === null) elements.delete(change.id);
      else putElement(doc, change.after);
    }
  });
  validate(doc);
}
