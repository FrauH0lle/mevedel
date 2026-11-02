import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { main } from '../src/cli.mjs';
import { callTool, tools } from '../src/tools.mjs';
import { bind, fixture, startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

async function room(t, options) {
  const fake = await startFakeRoom(options);
  t.after(() => fake.close());
  return fake;
}

/* A config with ORIGIN attached at TIER as snt-pipeline. */
async function attached(t, origin, tier = 'edit') {
  const env = tempEnv(t);
  const err = sink();
  assert.equal(await main(['attach', origin.link(tier)], { env, stdout: sink(), stderr: err }), 0, err.text);
  return env;
}

/* A fresh directory for image files. */
function scratch(t) {
  const dir = mkdtempSync(path.join(tmpdir(), 'mevedel-remote-artifact-images-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  return dir;
}

/* The consumer body fixture NAME sends, with its example bindings. */
function request(name) {
  const exchange = fixture(name);
  return bind(exchange.frames.find((f) => f.from === 'consumer').frame.body, exchange.bindings);
}

const PNG = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 1, 2, 3]);
const JPEG = Buffer.from([0xff, 0xd8, 0xff, 0xe0, 1, 2, 3]);
const WEBP = Buffer.concat([Buffer.from('RIFF'), Buffer.from([4, 0, 0, 0]), Buffer.from('WEBPVP8 ')]);
const SVG = Buffer.from('<?xml version="1.0"?>\n<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"/>');

/* An origin answering image, rename, background and view as SharedEdit would. */
function shared(body) {
  if (body.action === 'view') {
    return { result: { png: Buffer.from(body.image ? `image ${body.image}` : 'board').toString('base64'),
      mime: body.image ? 'image/webp' : 'image/png' } };
  }
  return { result: { text: JSON.stringify({ id: body.id, changed: [] }) } };
}

test('the registry offers edit_artifact and view_artifact', () => {
  const edit = tools.find((tool) => tool.name === 'edit_artifact');
  const view = tools.find((tool) => tool.name === 'view_artifact');
  // Exactly SharedEdit's content actions: an action added here reaches the origin.
  assert.deepEqual(edit.inputSchema.properties.action.enum,
    ['patch', 'image', 'rename', 'background', 'insert', 'revert']);
  for (const name of ['changes', 'title', 'background', 'source', 'x', 'y', 'width', 'alt', 'afterId',
    'item', 'transaction']) {
    assert.ok(edit.inputSchema.properties[name], name);
  }
  assert.deepEqual(view.inputSchema.required, ['name']);
  assert.deepEqual(Object.keys(view.inputSchema.properties), ['name', 'image', 'item']);
});

test('edit_artifact reverts a contribution from the history with SharedEdit\'s argument', async (t) => {
  const origin = await room(t);
  const env = await attached(t, origin);
  const { transaction } = request('revert');
  const result = await callTool('edit_artifact', { name: 'snt-pipeline', action: 'revert', transaction },
    { env });
  assert.ok(!result.isError, result.content[0].text);
  const { $TEXT, $PNG } = fixture('revert').bindings;
  assert.deepEqual(result.content, [
    { type: 'text', text: $TEXT },
    { type: 'image', data: $PNG, mimeType: 'image/png' },
  ]);
  assert.deepEqual(origin.received.at(-1).body, request('revert'));
});

test('a refused revert reports the origin\'s reason', async (t) => {
  const origin = await room(t, {
    onEditing: (body) => (body.action === 'revert'
      ? { error: 'Contribution is no longer available to revert' } : undefined),
  });
  const env = await attached(t, origin);
  const result = await callTool('edit_artifact',
    { name: 'snt-pipeline', action: 'revert', transaction: 'gone' }, { env });
  assert.ok(result.isError);
  assert.equal(result.content[0].text, 'Contribution is no longer available to revert');
});

test('edit_artifact patches with SharedEdit\'s arguments and returns its lines and picture', async (t) => {
  const origin = await room(t);
  const env = await attached(t, origin);
  const { changes } = request('patch');
  const result = await callTool('edit_artifact', { name: 'snt-pipeline', action: 'patch', changes }, { env });
  assert.ok(!result.isError, result.content[0].text);
  const { $TEXT, $PNG } = fixture('patch').bindings;
  assert.deepEqual(result.content, [
    { type: 'text', text: $TEXT },
    { type: 'image', data: $PNG, mimeType: 'image/png' },
  ]);
  assert.deepEqual(origin.received.at(-1).body, request('patch'));
});

test('a stale patch is rejected with its targets\' current lines, as SharedEdit does', async (t) => {
  const origin = await room(t);
  const env = await attached(t, origin);
  const { changes } = request('stale-patch');
  const result = await callTool('edit_artifact', { name: 'snt-pipeline', action: 'patch', changes }, { env });
  assert.ok(result.isError);
  const { $MESSAGE, $HASH, $LINE } = fixture('stale-patch').bindings;
  assert.deepEqual(JSON.parse(result.content[0].text), {
    error: $MESSAGE, code: 'stale', targets: [{ id: 'box', hash: $HASH, line: $LINE }], revision: 2,
  });
});

test('a view link\'s edit fails with a plain view-only error', async (t) => {
  const origin = await room(t);
  const env = await attached(t, origin, 'view');
  const { changes } = request('view-only');
  const result = await callTool('edit_artifact', { name: 'snt-pipeline', action: 'patch', changes }, { env });
  assert.ok(result.isError);
  assert.equal(result.content[0].text, 'This artifact link is view-only');
});

test('rename and background send SharedEdit\'s argument names', async (t) => {
  const origin = await room(t, { onEditing: shared });
  const env = await attached(t, origin);
  const { $ID } = fixture('welcome').bindings;
  assert.ok(!(await callTool('edit_artifact', { name: 'snt-pipeline', action: 'rename', title: 'v2' }, { env })).isError);
  assert.deepEqual(origin.received.at(-1).body, { action: 'rename', id: $ID, title: 'v2' });
  assert.ok(!(await callTool('edit_artifact',
    { name: 'snt-pipeline', action: 'background', background: '#ffeedd' }, { env })).isError);
  assert.deepEqual(origin.received.at(-1).body, { action: 'background', id: $ID, background: '#ffeedd' });
});

test('image uploads a local PNG, JPEG, WebP or SVG with its placement', async (t) => {
  const origin = await room(t, { onEditing: shared });
  const env = await attached(t, origin);
  const dir = scratch(t);
  const { $ID } = fixture('welcome').bindings;
  for (const [file, bytes] of [['a.png', PNG], ['b.jpg', JPEG], ['c.webp', WEBP], ['d.svg', SVG]]) {
    writeFileSync(path.join(dir, file), bytes);
    const result = await callTool('edit_artifact', { name: 'snt-pipeline', action: 'image',
      source: path.join(dir, file), x: 10, y: 20, width: 40, alt: 'chart', afterId: 'p1' }, { env });
    assert.ok(!result.isError, `${file}: ${result.content[0].text}`);
    assert.deepEqual(origin.received.at(-1).body, { action: 'image', id: $ID,
      data: bytes.toString('base64'), x: 10, y: 20, width: 40, alt: 'chart', afterId: 'p1' });
  }
});

test('image refuses anything but a PNG, JPEG, WebP or SVG of at most 4.7 MB, before connecting', async (t) => {
  const origin = await room(t, { onEditing: shared });
  const env = await attached(t, origin);
  const before = origin.connections;
  const dir = scratch(t);
  writeFileSync(path.join(dir, 'notes.txt'), 'just text');
  writeFileSync(path.join(dir, 'fake.png'), 'GIF89a not a png');
  writeFileSync(path.join(dir, 'big.png'), Buffer.concat([PNG, Buffer.alloc(4_700_000 - PNG.length + 1)]));
  writeFileSync(path.join(dir, 'limit.png'), Buffer.concat([PNG, Buffer.alloc(4_700_000 - PNG.length)]));
  const edit = (source) => callTool('edit_artifact', { name: 'snt-pipeline', action: 'image', source }, { env });
  for (const [source, message] of [
    [path.join(dir, 'notes.txt'), /notes\.txt is not a PNG, JPEG, WebP or SVG image/],
    [path.join(dir, 'fake.png'), /fake\.png is not a PNG, JPEG, WebP or SVG image/],
    [path.join(dir, 'big.png'), /big\.png is 4700001 bytes; shared images are limited to 4700000 bytes/],
    [path.join(dir, 'missing.png'), /File not found: .*missing\.png/],
    [dir, /is a directory, not an image file/],
    [undefined, /The image action needs source/],
  ]) {
    const result = await edit(source);
    assert.ok(result.isError, String(source));
    assert.match(result.content[0].text, message);
  }
  assert.equal(origin.connections, before);
  assert.ok(!(await edit(path.join(dir, 'limit.png'))).isError);
});

test('edit_artifact refuses every other SharedEdit action without contacting the origin', async (t) => {
  // The fake origin would answer these (shared answers anything), so a
  // refusal here comes from the package's own allowlist.
  const origin = await room(t, { onEditing: shared });
  const env = await attached(t, origin);
  const before = origin.connections;
  for (const action of ['delete', 'version', 'versions', 'restore', 'library-add', 'library-remove',
    'list', 'create', 'duplicate', 'import', 'view', 'read', 'library', 'discussion-post', 'write-file',
    'toString', 'constructor', '__proto__', '']) {
    const result = await callTool('edit_artifact', { name: 'snt-pipeline', action }, { env });
    assert.ok(result.isError, action);
    assert.equal(result.content[0].text,
      'action must be one of patch, image, rename, background, insert, revert', action);
  }
  assert.equal(origin.connections, before);
  assert.deepEqual(origin.received.filter((frame) => frame.t === 'editing'), []);
});

test('view_artifact returns the board picture, or an embedded image by its key', async (t) => {
  const origin = await room(t, { onEditing: shared });
  const env = await attached(t, origin, 'view');
  const { $ID } = fixture('welcome').bindings;
  const board = await callTool('view_artifact', { name: 'snt-pipeline' }, { env });
  assert.ok(!board.isError, board.content[0].text);
  assert.deepEqual(board.content, [{ type: 'image', data: Buffer.from('board').toString('base64'),
    mimeType: 'image/png' }]);
  assert.deepEqual(origin.received.at(-1).body, { action: 'view', id: $ID });
  const image = await callTool('view_artifact', { name: 'snt-pipeline', image: 'file-1' }, { env });
  assert.deepEqual(image.content, [{ type: 'image', data: Buffer.from('image file-1').toString('base64'),
    mimeType: 'image/webp' }]);
  assert.deepEqual(origin.received.at(-1).body, { action: 'view', id: $ID, image: 'file-1' });
});

test('view_artifact previews one library item of a whiteboard', async (t) => {
  const origin = await room(t, {
    onEditing: (body) => (body.action === 'library' && body.part === 'item'
      ? { result: { png: Buffer.from(`item ${body.item}`).toString('base64'), mime: 'image/png' } }
      : undefined),
  });
  const env = await attached(t, origin, 'view');
  const { $ID } = fixture('welcome').bindings;
  const result = await callTool('view_artifact', { name: 'snt-pipeline', item: 'Flowchart/diamond' }, { env });
  assert.ok(!result.isError, result.content[0]?.text);
  assert.deepEqual(result.content, [{ type: 'image', data: Buffer.from('item Flowchart/diamond').toString('base64'),
    mimeType: 'image/png' }]);
  assert.deepEqual(origin.received.at(-1).body,
    { action: 'library', id: $ID, part: 'item', item: 'Flowchart/diamond' });
});

test('view_artifact refuses an item with an image or without a library, before connecting', async (t) => {
  const origin = await room(t, { onEditing: shared });
  const env = await attached(t, origin);
  const before = origin.connections;
  const both = await callTool('view_artifact', { name: 'snt-pipeline', image: 'file-1', item: 'Flowchart/diamond' },
    { env });
  assert.ok(both.isError);
  assert.equal(both.content[0].text,
    'Give image or item, not both: image views an embedded image, item a library item');
  const bare = await callTool('view_artifact', { name: 'snt-pipeline', item: 'diamond' }, { env });
  assert.ok(bare.isError);
  assert.equal(bare.content[0].text, 'item needs LIBRARY/ITEM-ID from whiteboard_libraries');
  assert.equal(origin.connections, before);
});

test('the origin\'s refusal of an item preview reaches the agent as is', async (t) => {
  const origin = await room(t, { kind: 'document',
    onEditing: (body) => (body.action === 'library' ? { error: 'An artifact link does not permit library' }
      : undefined) });
  const env = await attached(t, origin);
  const result = await callTool('view_artifact', { name: 'snt-pipeline', item: 'Flowchart/diamond' }, { env });
  assert.ok(result.isError);
  assert.equal(result.content[0].text, 'An artifact link does not permit library');
});
