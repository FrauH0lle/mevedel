import test from 'node:test';
import assert from 'node:assert/strict';
import { handle } from '../host.mjs';
import { canonical, contentHash } from '../view.mjs';
import { captureContext } from '../context.mjs';
import { restore } from '../model.mjs';

const rect = (id, x, extra = {}) => ({ id, type: 'rectangle', x, y: 0, width: 100, height: 50, ...extra });
const view = async (state, part, extra = {}) => (await handle({ action: 'view', state, part, ...extra })).result;
/* The overview's line for ID, split into its hash and element. */
const lineOf = (text, id) => {
  const line = text.split('\n').find((l) => l.includes(`"id":"${id}"`) || l.includes(`"id":${JSON.stringify(id)}}`));
  const space = line.indexOf(' ');
  return { hash: line.slice(0, space), node: JSON.parse(line.slice(space + 1)) };
};

test('content hashes ignore key order and change with content', () => {
  assert.equal(canonical({ b: 1, a: [{ d: 2, c: 3 }] }), '{"a":[{"c":3,"d":2}],"b":1}');
  assert.equal(contentHash({ a: 1, b: 2 }), contentHash({ b: 2, a: 1 }));
  assert.notEqual(contentHash({ a: 1 }), contentHash({ a: 2 }));
  assert.match(contentHash({}), /^[0-9a-f]{12}$/);
});

test('a board reads as hashed lines; a long stroke pages at its own address', async () => {
  const points = Array.from({ length: 600 }, (_, i) => [i * 2, 0]);
  const pen = { id: 'pen', type: 'freedraw', x: 0, y: 0, width: 1198, height: 0, points };
  const { state } = await handle({ action: 'create', id: 'board', kind: 'whiteboard', title: 'Plan',
    content: [rect('box', 0), pen], actor: 'Guest', opId: 'create' });
  const { text } = await view(state, 'overview');
  const lines = text.trimEnd().split('\n');
  assert.match(lines[0], /^whiteboard "Plan" · revision 1 · 2 elements$/);
  assert.match(lines[1], /shared:\/\/board\/elements\/ID · Rendering: shared:\/\/board\/view\.png/);
  assert.equal(lines.length, 4);
  const box = lineOf(text, 'box');
  assert.equal(box.hash, contentHash(rect('box', 0)));
  assert.deepEqual(box.node, rect('box', 0));
  assert.ok(lines.every((l) => l.length <= 2000), 'every line fits Read');
  assert.equal(lineOf(text, 'pen').node.points, '600 points; Read shared://board/elements/pen');
  const full = (await view(state, 'element', { element: 'pen' })).text.split('\n');
  assert.equal(full[0], `hash ${lineOf(text, 'pen').hash}`);
  assert.ok(full.includes('    [1198,0]'), 'one point per line');
  assert.ok(full.length > 600);
  await assert.rejects(view(state, 'element', { element: 'gone' }), /No element gone; Read shared:\/\/board/);
  const { png, mime } = await view(state, 'png');
  assert.equal(mime, 'image/png');
  assert.equal(Buffer.from(png, 'base64').subarray(0, 4).toString('hex'), '89504e47');
});

test('edits name targets by hash, merge fields, and report stored lines', async () => {
  let { state } = await handle({ action: 'create', id: 'edits', kind: 'whiteboard', title: 'Edits',
    content: [rect('a', 0), rect('b', 200)], actor: 'Guest', opId: 'create' });
  const before = (await view(state, 'overview')).text;
  const a = lineOf(before, 'a'), b = lineOf(before, 'b');
  await assert.rejects(handle({ action: 'patch', state, actor: 'Agent: /root', opId: 'one', changes: [
    { id: 'a', hash: a.hash, unset: ['height'] }] }), /needs height/, 'the merged element still validates');
  let result;
  ({ state, result } = await handle({ action: 'patch', state, actor: 'Agent: /root', opId: 'two', changes: [
      { id: 'a', hash: a.hash, set: { strokeColor: '#e03131', strokeStyle: 'dashed' }, unset: ['strokeStyle'] },
      { id: 'b', hash: b.hash, after: null },
      { id: 'c', after: rect('c', 400) },
    ] }));
  const model = result.model;
  assert.equal(model.address, 'shared://edits');
  assert.equal(model.revision, 2);
  assert.equal(model.contribution, 'two');
  assert.deepEqual(model.deleted, ['b']);
  const stored = Object.fromEntries(model.changed.map((l) => [JSON.parse(l.slice(13)).id, l]));
  assert.deepEqual(JSON.parse(stored.a.slice(13)), rect('a', 0, { strokeColor: '#e03131' }));
  assert.match(stored.a, /^[0-9a-f]{12} \{"id":"a","type":"rectangle","x":0,"y":0,"width":100,"height":50,"strokeColor"/,
    'lines keep one key order');
  assert.equal(stored.a.slice(0, 12), contentHash(rect('a', 0, { strokeColor: '#e03131' })));
  assert.ok(stored.c, 'an element added without a hash');
  // The old hash is stale now; every stale target is reported with its line.
  const stale = await handle({ action: 'patch', state, actor: 'Agent: /root', opId: 'three', changes: [
    { id: 'a', hash: a.hash, set: { x: 5 } }, { id: 'c', after: rect('c', 0) }] }).catch((e) => e);
  assert.equal(stale.code, 'stale');
  assert.deepEqual(stale.targets.map((t) => t.id), ['a', 'c']);
  assert.equal(stale.targets[0].line, stored.a);
  assert.match(stale.message, /Stale a, c/);
  for (const [change, message] of [
    [{ id: 'a', hash: stored.a.slice(0, 12) }, /give after, or set and unset/],
    [{ id: 'a', hash: stored.a.slice(0, 12), set: [1] }, /set is an object/],
    [{ id: 'a', hash: stored.a.slice(0, 12), before: {} }, /Unknown change field before/],
    [{ id: 'a', hash: stored.a.slice(0, 12), after: { ...rect('a', 0), id: 'z' } }, /identity/],
  ])
    await assert.rejects(handle({ action: 'patch', state, actor: 'Agent', opId: 'bad', changes: [change] }), message);
});

test('document images are addresses in text and survive edits that name them', async () => {
  const board = await handle({ action: 'create', id: 'pixels', kind: 'whiteboard', actor: 'Guest', opId: 'pixels' });
  const data = (await handle({ action: 'export', state: board.state, format: 'png' })).result.data;
  const src = `data:image/png;base64,${data}`;
  const image = { type: 'image', attrs: { id: 'picture', src, alt: 'Diagram', width: 32, height: 16 } };
  const para = { type: 'paragraph', attrs: { id: 'p' }, content: [{ type: 'text', text: 'Hello' }] };
  let { state } = await handle({ action: 'create', id: 'doc', kind: 'document', title: 'Notes', actor: 'Guest',
    opId: 'create', content: { type: 'doc', content: [para, image] } });
  const { text } = await view(state, 'overview');
  assert.ok(!text.includes('base64'), 'no image data in text');
  const shown = lineOf(text, 'picture');
  const [, key] = /shared:\/\/doc\/images\/(img-[0-9a-f]{12})/.exec(shown.node.attrs.src);
  assert.match(text.split('\n')[1], /Block: shared:\/\/doc\/elements\/ID · Comments \(0\)/);
  const fetched = await view(state, 'image', { image: key });
  assert.deepEqual([fetched.mime, fetched.png], ['image/png', data]);
  let result;
  ({ state, result } = await handle({ action: 'patch', state, actor: 'Agent', opId: 'alt', changes: [
    { id: 'picture', hash: shown.hash, after: { ...shown.node, attrs: { ...shown.node.attrs, alt: 'Chart' } } }] }));
  assert.equal(result.content.content[1].attrs.src, src, 'the image address restores its data');
  assert.equal(result.content.content[1].attrs.alt, 'Chart');
  await assert.rejects(handle({ action: 'patch', state, actor: 'Agent', opId: 'bad', changes: [
    { id: 'p', hash: lineOf((await view(state, 'overview')).text, 'p').hash,
      after: { type: 'image', attrs: { id: 'p', src: 'shared://doc/images/img-000000000000' } } }] }), /No image img-000000000000/);
  await assert.rejects(view(state, 'png'), /A document has no rendering/);
});

test('comments, history and libraries read as text', async () => {
  let { state } = await handle({ action: 'create', id: 'talk', kind: 'whiteboard', title: 'Talk',
    content: [rect('a', 0)], actor: 'Guest: Ann', opId: 'create' });
  assert.equal((await view(state, 'comments')).text, 'No comments.\n');
  const doc = restore(Buffer.from(state.crdt, 'base64'));
  const expected = contentHash(captureContext(doc, { selection: ['a'] }).snapshot);
  doc.destroy();
  ({ state } = await handle({ action: 'comment', state, actor: 'Guest: Ann', opId: 'note', text: 'Is this the API?',
    selection: ['a'], expected }));
  const comments = (await view(state, 'comments')).text;
  assert.match(comments, /^comment note · open · current · on a\n {2}quote: "1 object\\nrectangle"\n {2}Guest: Ann: "Is this the API\?"\n$/);
  ({ state } = await handle({ action: 'rename', state, actor: 'Agent: /root', opId: 'title', title: 'Talks' }));
  const history = (await view(state, 'history')).text.split('\n');
  assert.match(history[0], /^title · revision 3 · Agent: \/root · \d{4}-.*Z · title "Talk" → "Talks"$/);
  const library = JSON.stringify({ type: 'excalidrawlib', version: 2, libraryItems: [
    { id: 'cloud', name: 'Cloud', status: 'published', elements: [{ id: 'c', type: 'ellipse', x: 0, y: 0, width: 80, height: 40 }] }] });
  const listing = (await handle({ action: 'library-view', part: 'list', at: 'shared://library',
    libraries: [{ name: 'Weather', text: library }] })).result.text;
  assert.match(listing, /^1 library item; insert one with SharedEdit insert and its ref\. The sheet shared:\/\/library\/sheet\.png numbers items 1-1\.\n1\. Weather\/cloud · "Cloud" · Weather · 1 element · 80×40\n$/);
  const sheet = (await handle({ action: 'library-view', part: 'sheet', libraries: [{ name: 'Weather', text: library }] })).result;
  assert.equal(sheet.mime, 'image/png');
});
