import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { deflateSync } from 'node:zlib';
import { handle } from '../host.mjs';
import { restore, filesOf } from '../model.mjs';

/* A real PNG, rendered by the host itself from a one-rectangle board. */
async function samplePng() {
  const made = await handle({ action: 'create', id: 'sample', kind: 'whiteboard', title: 'Sample',
    content: [{ id: 'r', type: 'rectangle', x: 0, y: 0, width: 120, height: 60 }], actor: 'Agent: t', opId: 'c' });
  return (await handle({ action: 'export', state: made.state, format: 'png' })).result.data;
}
const board = async (id = 'b1') =>
  (await handle({ action: 'create', id, kind: 'whiteboard', title: 'Board', actor: 'Agent: t', opId: `create-${id}` })).state;
const documentWith = async (blocks) =>
  (await handle({ action: 'create', id: 'd1', kind: 'document', title: 'Doc', actor: 'Agent: t', opId: 'create-d1',
    content: { type: 'doc', content: blocks } })).state;
const para = (id, text) => ({ type: 'paragraph', attrs: { id }, content: [{ type: 'text', text }] });
const files = (state) => filesOf(restore(Buffer.from(state.crdt, 'base64')));
const sha1 = (b64) => createHash('sha1').update(Buffer.from(b64, 'base64')).digest('hex');
const JPEG = '/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDABALDA4MChAODQ4SERATGCgaGBYWGDEjJR0oOjM9PDkzODdASFxOQERXRTc4UG1RV19iZ2hnPk1xeXBkeFxlZ2P/2wBDARESEhgVGC8aGi9jQjhCY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2P/wAARCAADAAQDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDnaKKK88+vP//Z'; // 4x3
const WEBP = 'UklGRiIAAABXRUJQVlA4IBYAAAAwAQCdASoBAAEADsD+JaQAA3AAAAAA'; // 1x1
/* The pixel size an embedded PNG file declares. */
const storedSize = (state, fileId) => {
  const raw = Buffer.from(files(state)[fileId].dataURL.split(',')[1], 'base64');
  return [raw.readUInt32BE(16), raw.readUInt32BE(20)];
};
const crc = () => Buffer.alloc(4);
const chunk = (kind, body) => Buffer.concat([Buffer.alloc(4), Buffer.from(kind), body, crc()]);
/* A structurally valid PNG whose header claims WxH; pixels are never decoded. */
function claimedPng(width, height) {
  const header = Buffer.alloc(13);
  header.writeUInt32BE(width, 0);
  header.writeUInt32BE(height, 4);
  const sized = (kind, body) => { const c = chunk(kind, body); c.writeUInt32BE(body.length, 0); return c; };
  return Buffer.concat([Buffer.from('89504e470d0a1a0a', 'hex'), sized('IHDR', header),
    sized('IDAT', deflateSync(Buffer.from([0]))), sized('IEND', Buffer.alloc(0))]).toString('base64');
}

test('image action puts a PNG on a board as one attributed, content-addressed contribution', async () => {
  const data = await samplePng();
  const state = await board();
  const { state: next, result } = await handle({ action: 'image', state, actor: 'Agent: t', opId: 'img1',
    data, x: 40, y: 50, image: true });
  const image = result.content.find((e) => e.type === 'image');
  assert.ok(image, 'an image element exists');
  assert.equal(image.fileId, sha1(data), 'file id is the SHA-1 of the bytes, as the browser names it');
  assert.deepEqual([image.x, image.y, image.status], [40, 50, 'saved']);
  assert.ok(image.width > 0 && image.height > 0 && image.width <= 640);
  assert.equal(files(next)[image.fileId].mimeType, 'image/png');
  assert.ok(result.png, 'the board rendering returns to the caller');
  assert.equal(result.transaction.changes.length, 1);
  assert.equal(result.transaction.actor, 'Agent: t');
  assert.match(result.model.changed[0], new RegExp(image.id));
});

test('image action stores identical bytes once and honours width', async () => {
  const data = await samplePng();
  let state = await board();
  for (const opId of ['a', 'b'])
    ({ state } = await handle({ action: 'image', state, actor: 'Agent: t', opId, data, x: 0, y: 0, width: 200 }));
  const read = await handle({ action: 'read', state });
  const images = read.result.content.filter((e) => e.type === 'image');
  assert.equal(images.length, 2);
  assert.equal(Object.keys(files(state)).length, 1, 'one stored file backs both elements');
  assert.equal(images[0].width, 200);
  assert.ok(Math.abs(images[0].height - images[0].width * (60 / 120)) < 40, 'height follows the aspect ratio');
});

test('image action places an image below existing content when no position is given', async () => {
  const data = await samplePng();
  const made = await handle({ action: 'create', id: 'b2', kind: 'whiteboard', title: 'B', actor: 'Agent: t', opId: 'c2',
    content: [{ id: 'r', type: 'rectangle', x: 10, y: 20, width: 100, height: 50 }] });
  const { result } = await handle({ action: 'image', state: made.state, actor: 'Agent: t', opId: 'i', data });
  const image = result.content.find((e) => e.type === 'image');
  assert.equal(image.x, 10);
  assert.ok(image.y >= 70, 'the image starts below the existing extent');
});

test('image action rasterizes SVG to a PNG file, shown at its intrinsic size', async () => {
  const svg = '<svg xmlns="http://www.w3.org/2000/svg" width="300" height="100"><rect width="300" height="100" fill="#c00"/></svg>';
  const state = await board();
  const { state: next, result } = await handle({ action: 'image', state, actor: 'Agent: t', opId: 's',
    data: Buffer.from(svg).toString('base64'), x: 0, y: 0 });
  const image = result.content.find((e) => e.type === 'image');
  const file = files(next)[image.fileId];
  assert.equal(file.mimeType, 'image/png');
  assert.equal(Buffer.from(file.dataURL.split(',')[1], 'base64').subarray(0, 4).toString('hex'), '89504e47');
  assert.deepEqual(storedSize(next, image.fileId), [600, 200], 'drawn at twice its size, for sharp zooming');
  assert.deepEqual([image.width, image.height], [300, 100], 'the extra resolution does not enlarge it');
});

test('image action caps a large SVG at 2048 px and still shows it at 640', async () => {
  const svg = '<svg xmlns="http://www.w3.org/2000/svg" width="4000" height="1000"><rect width="4000" height="1000" fill="#06c"/></svg>';
  const { state, result } = await handle({ action: 'image', state: await board(), actor: 'Agent: t', opId: 'big',
    data: Buffer.from(svg).toString('base64'), x: 0, y: 0 });
  const image = result.content.find((e) => e.type === 'image');
  assert.deepEqual(storedSize(state, image.fileId), [2048, 512]);
  assert.deepEqual([image.width, image.height], [640, 160]);
});

test('image action stores JPEG and WebP bytes as they are', async () => {
  for (const [data, mime, width] of [[JPEG, 'image/jpeg', 4], [WEBP, 'image/webp', 1]]) {
    const { state, result } = await handle({ action: 'image', state: await board(), actor: 'Agent: t', opId: mime.replace('/', '-'),
      data, x: 0, y: 0, image: true });
    const image = result.content.find((e) => e.type === 'image');
    assert.equal(image.fileId, sha1(data));
    assert.equal(files(state)[image.fileId].mimeType, mime);
    assert.equal(files(state)[image.fileId].dataURL, `data:${mime};base64,${data}`);
    assert.equal(image.width, width);
    assert.ok(result.png, 'the board renders with the image on it');
  }
});

test('image action inserts a document image block at the end or after an anchor', async () => {
  const data = await samplePng();
  const state = await documentWith([para('one', 'First'), para('two', 'Second')]);
  const tail = await handle({ action: 'image', state, actor: 'Agent: t', opId: 't', data, alt: 'Plot' });
  const blocks = tail.result.content.content;
  assert.deepEqual(blocks.map((b) => b.type), ['paragraph', 'paragraph', 'image']);
  assert.equal(blocks[2].attrs.alt, 'Plot');
  assert.match(blocks[2].attrs.src, /^data:image\/png;base64,/);
  assert.ok(blocks[2].attrs.id, 'the block has a stable identity for later patches');
  const anchored = await handle({ action: 'image', state, actor: 'Agent: t', opId: 'm', data, afterId: 'one' });
  assert.deepEqual(anchored.result.content.content.map((b) => b.type), ['paragraph', 'image', 'paragraph']);
  assert.match(anchored.result.model.changed[0], /shared:\/\//, 'the model sees an address, not image data');
  assert.doesNotMatch(anchored.result.model.changed[0], /base64/);
});

test('image action refuses unsupported, malformed and oversized images without changing state', async () => {
  const state = await board();
  const attempt = (data) => handle({ action: 'image', state, actor: 'Agent: t', opId: 'x', data, x: 0, y: 0 });
  await assert.rejects(attempt(Buffer.from('plain text').toString('base64')), /PNG, JPEG, WebP or SVG/);
  await assert.rejects(attempt(Buffer.from('GIF89a\u0001\u0000\u0001\u0000', 'latin1').toString('base64')), /PNG, JPEG, WebP or SVG/);
  await assert.rejects(attempt(claimedPng(9000, 9000).slice(0, 40)), /truncated or damaged|Truncated PNG/);
  await assert.rejects(attempt(claimedPng(9000, 9000)), /9000×9000.*re-render smaller/);
  await assert.rejects(attempt('not base64!'), /Invalid state encoding|Image data/);
  await assert.rejects(handle({ action: 'image', state, actor: 'Agent: t', opId: 'y', x: 0, y: 0 }), /Image data/);
  await assert.rejects(handle({ action: 'image', state, actor: 'Agent: t', opId: 'z', data: WEBP, x: 5 }), /both x and y, or neither/);
  assert.equal((await handle({ action: 'read', state })).result.content.length, 0, 'the persisted board is untouched');
});

test('image action is idempotent per operation and revertable with its file', async () => {
  const data = await samplePng();
  const { state } = await handle({ action: 'image', state: await board(), actor: 'Agent: t', opId: 'once', data, x: 0, y: 0 });
  const again = await handle({ action: 'image', state, actor: 'Agent: t', opId: 'once', data, x: 0, y: 0 });
  assert.equal(again.state.revision, state.revision, 'a retried operation adds nothing');
  const reverted = await handle({ action: 'revert', state, actor: 'Agent: t', opId: 'undo', transaction: 'once' });
  assert.equal(reverted.result.content.length, 0, 'revert removes the image');
  const restored = await handle({ action: 'revert', state: reverted.state, actor: 'Agent: t', opId: 'redo', transaction: 'undo' });
  const image = restored.result.content.find((e) => e.type === 'image');
  assert.ok(image && files(restored.state)[image.fileId], 'reverting the revert restores the image and its file');
});
