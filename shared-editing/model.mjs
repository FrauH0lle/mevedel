/* Canonical shared content. Browser and host use the same validated Yjs model. */
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
  doc.getMap('shapes');
  doc.getXmlFragment('document');
  validate(doc);
  return doc;
}
export const encode = (doc) => Y.encodeStateAsUpdate(doc);
export function shapeJSON(value) {
  const { geometry, ...properties } = value.toJSON();
  check(
    geometry && Object.keys(geometry).every((k) => ['box', 'points'].includes(k)),
    'Invalid shape geometry record',
  );
  check(
    !Object.hasOwn(properties, 'box') && !Object.hasOwn(properties, 'points'),
    'Geometry must stay together',
  );
  return { ...properties, ...geometry };
}
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
export function inspect(doc) {
  return {
    kind: doc.getMap('meta').get('kind'),
    title: doc.getMap('meta').get('title'),
    content:
      doc.getMap('meta').get('kind') === 'document'
        ? documentJSON(doc)
        : [...doc.getMap('shapes').values()]
            .map(shapeJSON)
            .sort((a, b) => (a.layer || 0) - (b.layer || 0) || a.id.localeCompare(b.id)),
  };
}
export function validateShape(shape) {
  check(shape && identifier(shape.id), 'Invalid shape identity');
  check(
    [
      'rect',
      'ellipse',
      'diamond',
      'cylinder',
      'sticky',
      'text',
      'arrow',
      'line',
      'pen',
      'image',
    ].includes(shape.type),
    'Unknown shape type',
  );
  const allowed = [
    'id',
    'type',
    'box',
    'text',
    'stroke',
    'fill',
    'width',
    'points',
    'from',
    'to',
    'src',
    'dash',
    'rough',
    'pattern',
    'edges',
    'opacity',
    'fontSize',
    'layer',
  ];
  check(
    Object.keys(shape).every((key) => allowed.includes(key)),
    'Unknown shape property',
  );
  check(
    Array.isArray(shape.box) &&
      shape.box.length === 4 &&
      shape.box.every((v) => Number.isFinite(v) && Math.abs(v) <= 1e6) &&
      shape.box[2] >= 0 &&
      shape.box[3] >= 0,
    'Invalid shape geometry',
  );
  for (const key of ['text', 'stroke', 'fill', 'src', 'from', 'to'])
    check(shape[key] === undefined || typeof shape[key] === 'string', 'Invalid shape text');
  check((shape.text?.length || 0) <= 10000, 'Shape text is too long');
  for (const key of ['stroke', 'fill'])
    check(!shape[key] || /^(none|#[0-9a-fA-F]{6})$/.test(shape[key]), 'Invalid shape colour');
  check(
    shape.width === undefined ||
      (Number.isFinite(shape.width) && shape.width >= 1 && shape.width <= 20),
    'Invalid stroke width',
  );
  const option = (key, values) =>
    check(shape[key] === undefined || values.includes(shape[key]), `Invalid shape ${key}`);
  option('dash', ['solid', 'dashed', 'dotted']);
  option('rough', [0, 1, 2]);
  option('pattern', ['solid', 'hachure', 'cross']);
  option('edges', ['sharp', 'round']);
  const range = (key, low, high) =>
    check(
      shape[key] === undefined ||
        (Number.isFinite(shape[key]) && shape[key] >= low && shape[key] <= high),
      `Invalid shape ${key}`,
    );
  range('opacity', 0, 100);
  range('fontSize', 4, 400);
  range('layer', -1e6, 1e6);
  check(
    shape.points === undefined ||
      (Array.isArray(shape.points) &&
        shape.points.length <= 4000 &&
        shape.points.every(
          (p) =>
            Array.isArray(p) &&
            p.length === 2 &&
            p.every((n) => Number.isFinite(n) && Math.abs(n) <= 1e6),
        )),
    'Invalid drawing points',
  );
  if (['pen', 'line', 'arrow'].includes(shape.type) && shape.points)
    check(shape.points.length >= 2, 'A stroke needs two points');
  if (shape.points) {
    check(['pen', 'line', 'arrow'].includes(shape.type), 'Only strokes carry points');
    const [x, y, w, h] = shape.box;
    check(
      shape.points.every(
        ([px, py]) => px >= x - 1e-8 && py >= y - 1e-8 && px <= x + w + 1e-8 && py <= y + h + 1e-8,
      ),
      'Drawing points exceed their geometry',
    );
  }
  check(
    !shape.src ||
      (shape.src.length <= 6 * 1024 * 1024 &&
        /^data:image\/(png|jpeg|webp);base64,[A-Za-z0-9+/]*={0,2}$/.test(shape.src)),
    'Invalid embedded image',
  );
  check(
    shape.type === 'image' ? Boolean(shape.src) : shape.src === undefined,
    'Images require embedded data on an image shape',
  );
  const pixels = shape.src ? validateImage(shape.src) : 0;
  for (const key of ['from', 'to'])
    check(!shape[key] || identifier(shape[key]), 'Invalid connector target');
  check(shape.type === 'arrow' || (!shape.from && !shape.to), 'Only arrows bind to shapes');
  return pixels;
}
export function validate(doc) {
  check(
    [...doc.share.keys()].every((key) => ['meta', 'shapes', 'document'].includes(key)),
    'Unknown shared content',
  );
  const meta = doc.getMap('meta');
  check(
    [...meta.keys()].every((key) => ['kind', 'title'].includes(key)),
    'Unknown editor property',
  );
  check(['whiteboard', 'document'].includes(meta.get('kind')), 'Unknown editor kind');
  check(
    typeof meta.get('title') === 'string' &&
      meta.get('title').trim().length > 0 &&
      meta.get('title').length <= 200,
    'Invalid title',
  );
  const shapes = doc.getMap('shapes');
  check(shapes.size <= 2000, 'Too many shapes');
  let pixels = 0;
  for (const [id, value] of shapes) {
    check(value instanceof Y.Map && value.get('id') === id, 'Invalid shape record');
    pixels += validateShape(shapeJSON(value));
  }
  check(pixels <= 32000000, 'Board images exceed 32 megapixels in total');
  if (meta.get('kind') === 'document') {
    check(shapes.size === 0, 'Shapes cannot enter a document');
    if (doc.getXmlFragment('document').length) validateDocument(documentJSON(doc));
  } else check(doc.getXmlFragment('document').length === 0, 'Text document cannot enter a board');
  check(encode(doc).length <= LIMIT, 'Shared content is too large');
}
export function putShape(doc, shape) {
  validateShape(shape);
  const { box, points, ...properties } = shape;
  shape = { ...properties, geometry: points ? { box, points } : { box } };
  const shapes = doc.getMap('shapes');
  doc.transact(() => {
    let target = shapes.get(shape.id);
    if (!target) {
      target = new Y.Map();
      shapes.set(shape.id, target);
    }
    for (const key of [...target.keys()]) if (!(key in shape)) target.delete(key);
    for (const [key, value] of Object.entries(shape))
      if (!same(target.get(key), value)) target.set(key, clone(value));
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
  const shapes = doc.getMap('shapes'),
    ids = new Set();
  for (const change of changes) {
    check(identifier(change.id) && !ids.has(change.id), 'Invalid or duplicate target');
    ids.add(change.id);
    const current = shapes.has(change.id) ? shapeJSON(shapes.get(change.id)) : null;
    if (!same(current, change.before))
      throw Object.assign(new Error(`Stale target: ${change.id}`), {
        code: 'stale',
        targets: [{ id: change.id, current }],
      });
    if (change.after !== null) {
      validateShape(change.after);
      check(change.after.id === change.id, 'Cannot change shape identity');
    }
  }
  doc.transact(() => {
    for (const change of changes) {
      if (change.after === null) shapes.delete(change.id);
      else putShape(doc, change.after);
    }
  });
  validate(doc);
}
