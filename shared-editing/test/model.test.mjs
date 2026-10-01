import assert from 'node:assert/strict';
import test from 'node:test';
import { create, restore, encode, inspect, applyUpdate, patch, putElement, putFile, pruneFiles, filesOf } from '../model.mjs';
import * as Y from 'yjs';
const rect = (id, x, extra = {}) => ({ id, type: 'rectangle', x, y: 0, width: 100, height: 60, ...extra });

test('two writers and an agent preserve independent edits and reject a stale target', () => {
  const host = create('whiteboard', 'Architecture');
  patch(host, [
    { id: 'client', before: null, after: rect('client', 0, { strokeColor: '#1971c2' }) },
  ]);
  const left = restore(encode(host)),
    right = restore(encode(host));
  const base = Y.encodeStateVector(host);
  putElement(left, rect('api', 200));
  putElement(right, rect('db', 400));
  applyUpdate(host, Y.encodeStateAsUpdate(right, base));
  applyUpdate(host, Y.encodeStateAsUpdate(left, base));
  applyUpdate(host, Y.encodeStateAsUpdate(right, base));
  assert.deepEqual(
    inspect(host)
      .content.map((s) => s.id)
      .sort(),
    ['api', 'client', 'db'],
  );
  const old = inspect(host).content.find((s) => s.id === 'client');
  const moved = { ...old, x: 50, y: 50 };
  patch(host, [{ id: 'client', before: old, after: moved }]);
  assert.throws(
    () => patch(host, [{ id: 'client', before: old, after: { ...old, strokeColor: '#e03131' } }]),
    /stale/i,
  );
  assert.equal(inspect(host).content.find((s) => s.id === 'client').strokeColor, '#1971c2');
  const restored = restore(encode(host));
  assert.deepEqual(inspect(restored), inspect(host));
  restored.destroy();
  [host, left, right].forEach((doc) => doc.destroy());
});

test('simultaneous first typing shares one text identity and preserves selective undo', async () => {
  const { initializeDocument } = await import('../document.mjs');
  const host = create('document', 'Empty');
  initializeDocument(host);
  const a = restore(encode(host)),
    b = restore(encode(host));
  const root = (d) => d.getXmlFragment('document'),
    text = (d) => root(d).get(0).get(0);
  const ua = new Y.UndoManager(root(a)),
    ub = new Y.UndoManager(root(b));
  text(a).insert(0, 'Alice');
  text(b).insert(0, 'Bob');
  Y.applyUpdate(a, encode(b), 'remote');
  Y.applyUpdate(b, encode(a), 'remote');
  ua.undo();
  Y.applyUpdate(b, encode(a), 'remote');
  assert.equal(text(b).toString(), 'Bob');
  ua.redo();
  Y.applyUpdate(b, encode(a), 'remote');
  assert.ok(text(b).toString().includes('Alice'));
  ub.undo();
  Y.applyUpdate(a, encode(b), 'remote');
  assert.equal(text(a).toString(), 'Alice');
  [ua, ub, a, b, host].forEach((v) => v.destroy());
});

test('concurrent document typing converges and agent block edits preserve other blocks', async () => {
  const { initializeDocument, documentJSON, patchDocument } = await import('../document.mjs');
  const host = create('document', 'Notes');
  initializeDocument(host, {
    type: 'doc',
    content: [
      { type: 'paragraph', attrs: { id: 'first' }, content: [{ type: 'text', text: 'Hello' }] },
      { type: 'paragraph', attrs: { id: 'second' }, content: [{ type: 'text', text: 'World' }] },
    ],
  });
  const a = restore(encode(host)),
    b = restore(encode(host)),
    vector = Y.encodeStateVector(host);
  a.getXmlFragment('document').get(0).get(0).insert(5, ' Alice');
  b.getXmlFragment('document').get(0).get(0).insert(0, 'Dear ');
  applyUpdate(host, Y.encodeStateAsUpdate(a, vector));
  applyUpdate(host, Y.encodeStateAsUpdate(b, vector));
  assert.equal(documentJSON(host).content[0].content[0].text, 'Dear Hello Alice');
  const before = documentJSON(host).content[1];
  patchDocument(host, [
    { id: 'second', before, after: { ...before, content: [{ type: 'text', text: 'Everyone' }] } },
  ]);
  assert.equal(documentJSON(host).content[0].content[0].text, 'Dear Hello Alice');
  assert.equal(documentJSON(host).content[1].content[0].text, 'Everyone');
  assert.throws(() => patchDocument(host, [{ id: 'second', before, after: null }]), /stale/i);
  [host, a, b].forEach((d) => d.destroy());
});

test('document insertion beside a replaced block preserves both and ignores JSON key order', async () => {
  const { initializeDocument, documentJSON, patchDocument } = await import('../document.mjs');
  const doc = create('document', 'Notes');
  const p = (id, text) => ({ type: 'paragraph', attrs: { id }, content: [{ type: 'text', text }] });
  initializeDocument(doc, { type: 'doc', content: [p('first', 'One'), p('second', 'Two')] });
  const before = documentJSON(doc).content[1];
  patchDocument(doc, [
    { id: 'inserted', before: null, after: p('inserted', 'Between'), afterId: 'first' },
    {
      id: 'second',
      before: Object.fromEntries(Object.entries(before).reverse()),
      after: p('second', 'Revised'),
    },
  ]);
  assert.deepEqual(
    documentJSON(doc).content.map((n) => n.attrs.id),
    ['first', 'inserted', 'second'],
  );
  assert.deepEqual(
    documentJSON(doc).content.map((n) => n.content[0].text),
    ['One', 'Between', 'Revised'],
  );
  doc.destroy();
});

test('same-property writes converge and deletion defeats an in-flight property edit', () => {
  const host = create('whiteboard', 'Conflicts');
  putElement(host, rect('box', 0));
  const a = restore(encode(host)),
    b = restore(encode(host)),
    vector = Y.encodeStateVector(host);
  a.getMap('elements').get('box').set('geometry', { x: 10, y: 10, width: 100, height: 50 });
  b.getMap('elements').get('box').set('geometry', { x: 20, y: 20, width: 300, height: 50 });
  const ua = Y.encodeStateAsUpdate(a, vector),
    ub = Y.encodeStateAsUpdate(b, vector);
  applyUpdate(a, ub);
  applyUpdate(b, ua);
  assert.deepEqual(inspect(a), inspect(b));
  const { x, width } = inspect(a).content[0];
  assert.ok((x === 10 && width === 100) || (x === 20 && width === 300), 'geometry never mixes writers');
  b.getMap('elements').delete('box');
  applyUpdate(a, Y.encodeStateAsUpdate(b, vector));
  assert.equal(inspect(a).content.length, 0);
  [a, b, host].forEach((d) => d.destroy());
});

test('Excalidraw fields validate by value and fractional indices order the scene', () => {
  const doc = create('whiteboard', 'Styles');
  putElement(doc, rect('front', 0, { strokeStyle: 'dashed', roughness: 2, fillStyle: 'cross-hatch',
    backgroundColor: '#ffc9c9', roundness: { type: 3 }, opacity: 40, index: 'a2' }));
  putElement(doc, { id: 'back', type: 'ellipse', x: 0, y: 0, width: 100, height: 50, index: 'a0' });
  putElement(doc, { id: 'middle', type: 'diamond', x: 0, y: 0, width: 100, height: 50, index: 'a1' });
  putElement(doc, { id: 'unplaced', type: 'diamond', x: 0, y: 0, width: 100, height: 50 });
  assert.deepEqual(inspect(doc).content.map((s) => s.id), ['back', 'middle', 'front', 'unplaced'],
    'elements without an index draw above indexed ones');
  for (const [bad, message] of [
    [{ strokeStyle: 'wavy' }, /Invalid rectangle strokeStyle/],
    [{ fillStyle: 'dots' }, /fillStyle/],
    [{ roundness: { type: 4 } }, /roundness/],
    [{ opacity: 101 }, /opacity/],
    [{ strokeColor: 'url(#x)' }, /strokeColor/],
    [{ index: 'a 0' }, /index/],
    [{ fontSize: 20 }, /Unknown rectangle property fontSize/],
    [{ version: 3 }, /derived on export/],
    [{ boundElements: [] }, /derived on export/],
  ])
    assert.throws(() => putElement(doc, rect('bad', 0, bad)), message);
  const { width: _, ...narrow } = rect('bad', 0);
  assert.throws(() => putElement(doc, narrow), /needs width/);
  assert.throws(() => putElement(doc, { id: 'bad', type: 'cylinder', x: 0, y: 0, width: 1, height: 1 }), /Unknown element type/);
  assert.throws(() => putElement(doc, { id: 't', type: 'text', x: 0, y: 0, width: 0, height: 0 }), /needs text/);
  assert.throws(() => putElement(doc, { id: 'l', type: 'line', x: 0, y: 0, width: 0, height: 0, points: [[0, 0]] }), /two points/);
  assert.throws(() => putElement(doc, { id: 't', type: 'text', x: 0, y: 0, width: 0, height: 0, text: 'x', containerId: 't' }), /contain itself/);
  assert.equal(doc.getMap('elements').has('bad'), false);
  doc.destroy();
});

test('image files are validated, shared by reference and pruned when unreferenced', async () => {
  const { handle } = await import('../host.mjs');
  const { state } = await handle({ action: 'create', id: 'png', kind: 'whiteboard', opId: 'p', actor: 'Alice' });
  const png = (await handle({ action: 'export', state, format: 'png' })).result.data;
  const doc = create('whiteboard', 'Images');
  const file = { mimeType: 'image/png', dataURL: `data:image/png;base64,${png}` };
  putFile(doc, 'f1', file);
  putElement(doc, { id: 'i1', type: 'image', x: 0, y: 0, width: 10, height: 10, fileId: 'f1' });
  putElement(doc, { id: 'i2', type: 'image', x: 20, y: 0, width: 10, height: 10, fileId: 'f1' });
  assert.throws(() => putFile(doc, 'f2', { mimeType: 'image/svg+xml', dataURL: 'data:image/svg+xml;base64,PHN2Zz4=' }), /PNG, JPEG, or WebP/);
  assert.throws(() => putFile(doc, 'f2', { ...file, dataURL: file.dataURL.slice(0, -10) }), /PNG|image/i);
  assert.throws(() => patch(doc, [{ id: 'i3', before: null, after: { id: 'i3', type: 'image', x: 0, y: 0, width: 1, height: 1, fileId: 'missing' } }]), /existing file/);
  doc.getMap('elements').delete('i1');
  pruneFiles(doc);
  assert.deepEqual(Object.keys(filesOf(doc)), ['f1'], 'a file stays while any element uses it');
  doc.getMap('elements').delete('i2');
  pruneFiles(doc, new Set(['f1']));
  assert.deepEqual(Object.keys(filesOf(doc)), ['f1'], 'retained history can keep a file');
  pruneFiles(doc);
  assert.deepEqual(filesOf(doc), {});
  doc.destroy();
});
