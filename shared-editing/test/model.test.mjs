import assert from 'node:assert/strict';
import test from 'node:test';
import { create, restore, encode, inspect, applyUpdate, patch, putShape } from '../model.mjs';
import * as Y from 'yjs';

test('two writers and an agent preserve independent edits and reject a stale target', () => {
  const host = create('whiteboard', 'Architecture');
  patch(host, [
    {
      id: 'client',
      before: null,
      after: { id: 'client', type: 'rect', box: [0, 0, 100, 60], text: 'Client' },
    },
  ]);
  const left = restore(encode(host)),
    right = restore(encode(host));
  const base = Y.encodeStateVector(host);
  putShape(left, { id: 'api', type: 'rect', box: [200, 0, 100, 60], text: 'API' });
  putShape(right, { id: 'db', type: 'rect', box: [400, 0, 100, 60], text: 'Database' });
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
  const moved = { ...old, box: [50, 50, 100, 60] };
  patch(host, [{ id: 'client', before: old, after: moved }]);
  assert.throws(
    () => patch(host, [{ id: 'client', before: old, after: { ...old, text: 'Browser' } }]),
    /stale/i,
  );
  assert.equal(inspect(host).content.find((s) => s.id === 'client').text, 'Client');
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
  putShape(host, { id: 'box', type: 'rect', box: [0, 0, 100, 50] });
  const a = restore(encode(host)),
    b = restore(encode(host)),
    vector = Y.encodeStateVector(host);
  a.getMap('shapes')
    .get('box')
    .set('geometry', { box: [10, 10, 100, 50] });
  b.getMap('shapes')
    .get('box')
    .set('geometry', { box: [20, 20, 100, 50] });
  const ua = Y.encodeStateAsUpdate(a, vector),
    ub = Y.encodeStateAsUpdate(b, vector);
  applyUpdate(a, ub);
  applyUpdate(b, ua);
  assert.deepEqual(inspect(a), inspect(b));
  b.getMap('shapes').delete('box');
  applyUpdate(a, Y.encodeStateAsUpdate(b, vector));
  assert.equal(inspect(a).content.length, 0);
  [a, b, host].forEach((d) => d.destroy());
});

test('style properties validate by value and layers order the scene', () => {
  const doc = create('whiteboard', 'Styles');
  putShape(doc, {
    id: 'front',
    type: 'rect',
    box: [0, 0, 100, 50],
    dash: 'dashed',
    rough: 2,
    pattern: 'cross',
    edges: 'sharp',
    opacity: 40,
    fontSize: 44,
    layer: 3,
  });
  putShape(doc, { id: 'back', type: 'ellipse', box: [0, 0, 100, 50], layer: -1 });
  putShape(doc, { id: 'middle', type: 'diamond', box: [0, 0, 100, 50] });
  assert.deepEqual(
    inspect(doc).content.map((s) => s.id),
    ['back', 'middle', 'front'],
  );
  for (const bad of [
    { dash: 'wavy' },
    { rough: 3 },
    { pattern: 'dots' },
    { edges: 'bevel' },
    { opacity: 101 },
    { fontSize: 2 },
    { layer: Infinity },
    { fontSize: '24' },
  ])
    assert.throws(
      () => putShape(doc, { id: 'bad', type: 'rect', box: [0, 0, 10, 10], ...bad }),
      /Invalid shape/,
    );
  assert.equal(doc.getMap('shapes').has('bad'), false);
  doc.destroy();
});
