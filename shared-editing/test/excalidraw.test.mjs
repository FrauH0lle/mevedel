import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { parseScene, parseLibrary, serializeScene, serializeLibrary, exportElements, placeElements } from '../excalidraw.mjs';
const BUILTIN = parseLibrary(readFileSync(new URL('../builtin.excalidrawlib', import.meta.url), 'utf8'));
import { validateElement } from '../model.mjs';

// A scene as older Excalidraw versions and other editors wrote it.
const legacy = {
  type: 'excalidraw', version: 2, source: 'https://excalidraw.com',
  elements: [
    { id: 'box', type: 'rectangle', x: 10, y: 10, width: -100, height: 50, strokeSharpness: 'round',
      boundElementIds: ['arrow'], version: 7, versionNonce: 3, isDeleted: false, seed: 42, unknownKey: true },
    { id: 'label', type: 'text', x: 0, y: 0, width: 40, height: 25, text: 'Old', font: '20px Virgil',
      containerId: 'box', verticalAlign: '', baseline: 18 },
    { id: 'arrow', type: 'arrow', x: 0, y: 100, width: 100, height: 0, points: [[0, 0], [100, 0]],
      startBinding: { elementId: 'box', focus: 0.2, gap: 4 }, endBinding: { elementId: 'missing', focus: 0, gap: 1 },
      startArrowhead: 'dot', lastCommittedPoint: null },
    { id: 'gone', type: 'rectangle', x: 0, y: 0, width: 1, height: 1, isDeleted: true },
    { id: 'sel', type: 'selection', x: 0, y: 0, width: 1, height: 1 },
    { id: 'box', type: 'ellipse', x: 400, y: 0, width: 50, height: 50 },
    { id: 'blank', type: 'text', x: 0, y: 0, width: 0, height: 0, text: '' },
  ],
  appState: { viewBackgroundColor: '#ffffff' }, files: {},
};

test('Excalidraw restore migrations apply, unknown and derived fields drop, invisible elements vanish', () => {
  const { elements, notes } = parseScene(JSON.stringify(legacy));
  assert.deepEqual(notes, []);
  assert.deepEqual(elements.map((e) => e.type), ['rectangle', 'text', 'arrow', 'ellipse']);
  const [box, label, arrow, duplicate] = elements;
  assert.deepEqual([box.x, box.width, box.roundness, box.seed], [-90, 100, { type: 1 }, 42], 'negative sizes and legacy sharpness normalise');
  for (const key of ['version', 'versionNonce', 'isDeleted', 'boundElementIds', 'unknownKey'])
    assert.equal(Object.hasOwn(box, key), false, key);
  assert.deepEqual([label.fontSize, label.fontFamily, label.containerId, label.verticalAlign], [20, 1, 'box', undefined]);
  assert.equal(label.lineHeight, 1.25, 'legacy line height comes from the stored height');
  assert.deepEqual(arrow.startBinding, { elementId: 'box', fixedPoint: [0.5001, 0.5001], mode: 'orbit' });
  assert.equal(arrow.endBinding, null, 'bindings to missing elements are dropped');
  assert.deepEqual([arrow.startArrowhead, arrow.endArrowhead], ['circle', 'arrow']);
  assert.notEqual(duplicate.id, 'box', 'a duplicate id gets a fresh one');
  elements.forEach(validateElement);
  assert.throws(() => parseScene('{"type":"excalidrawlib"}'), /Not an Excalidraw scene/);
});

test('exported scenes are complete Excalidraw files that import back to the same elements', () => {
  const { elements } = parseScene(JSON.stringify(legacy));
  const file = JSON.parse(serializeScene(elements, {}, { now: 1000 }));
  assert.deepEqual(Object.keys(file), ['type', 'version', 'source', 'elements', 'appState', 'files']);
  const byId = (id) => file.elements.find((e) => e.id === id);
  const [box, label, arrow] = ['box', 'label', 'arrow'].map(byId);
  const order = file.elements.map((e) => e.id);
  assert.deepEqual(file.elements.map((e) => e.index), ['a0', 'a1', 'a2', 'a3']);
  assert.equal(order.indexOf('label'), order.indexOf('box') + 1, 'labels follow their containers');
  assert.deepEqual(box.boundElements, [{ id: 'arrow', type: 'arrow' }, { id: 'label', type: 'text' }]);
  assert.deepEqual([box.version, box.isDeleted, box.updated, box.strokeColor, box.fillStyle], [1, false, 1000, '#1e1e1e', 'solid']);
  assert.deepEqual([label.textAlign, label.verticalAlign, label.originalText, label.autoResize], ['center', 'middle', 'Old', true]);
  assert.ok(label.x > box.x && label.x + label.width < box.x + box.width, 'bound text carries its laid-out place');
  assert.deepEqual(arrow.points[0], [0, 0], 'linear points start at the element origin');
  assert.ok(Math.abs(arrow.x - (box.x + 100 + 6)) < 1e-6, 'a bound end is exported where it is drawn');
  const again = parseScene(JSON.stringify(file)).elements;
  assert.deepEqual(exportElements(again, 1000), file.elements, 'a second round trip changes nothing');
});

test('libraries read versions 1 and 2 and place copies with fresh identities', () => {
  const v1 = parseLibrary({ type: 'excalidrawlib', version: 1, library: [[legacy.elements[0]], []] });
  assert.equal(v1.length, 1, 'empty items are dropped');
  assert.equal(v1[0].status, 'unpublished');
  const again = parseLibrary({ type: 'excalidrawlib', version: 1, library: [[legacy.elements[0]]] });
  assert.equal(again[0].id, v1[0].id, 'id-less items keep one reference across reads');
  const v2 = parseLibrary(serializeLibrary([...BUILTIN, { id: 'mine', name: 'Mine', elements: v1[0].elements }], 5));
  assert.deepEqual(v2.map((item) => item.name), ['Database', 'Mine']);
  assert.throws(() => parseLibrary({ type: 'excalidrawlib', version: 3, libraryItems: [] }), /Not an Excalidraw library/);
  let seed = 0;
  const placed = placeElements(BUILTIN[0].elements, [500, 300], ['b0', 'b1', 'b2', 'b3'], () => ++seed);
  const groups = new Set(placed.flatMap((e) => e.groupIds));
  assert.equal(groups.size, 1, 'an item keeps one fresh group');
  assert.ok(!groups.has('builtin-cylinder'));
  assert.ok(placed.every((e) => !BUILTIN[0].elements.some((o) => o.id === e.id)));
  const xs = placed.map((e) => e.x);
  assert.equal(Math.min(...xs), 500 - 60, 'the item is centred on the target point');
  placed.forEach(validateElement);
});
