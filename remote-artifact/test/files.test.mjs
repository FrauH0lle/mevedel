import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { main } from '../src/cli.mjs';
import { callTool, tools } from '../src/tools.mjs';
import { bind, fixture, startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

const PAGE = '<!doctype html>\n<h1>Hello</h1>\n';

const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

/*
 * An origin's HTML files, answering the requests no fixture covers as the
 * origin does: FILES maps names to their content.  Requests a fixture
 * covers (index.html as the fixtures have it) are answered from it.
 */
function fileOrigin(files) {
  const fields = (file) => ({ file, hash: sha256(files[file]), bytes: Buffer.byteLength(files[file]),
    text: files[file] });
  const stale = (file, hash) => ({
    error: hash ? `${file} changed since it was read; redo the change on its current content`
      : `${file} already exists; redo the change on its current content`,
    code: 'stale', ...fields(file),
  });
  return (body, { writable }) => {
    const file = body.file ?? 'index.html';
    if (body.action === 'files') {
      return { result: { primary: 'index.html',
        files: Object.keys(files).sort().map((f) => ({ file: f, bytes: Buffer.byteLength(files[f]) })) } };
    }
    if (body.action === 'read') return files[file] === undefined ? { error: `No file ${file} in this artifact` }
      : { result: fields(file) };
    if (!writable) return { error: 'This artifact link is view-only' };
    if (body.action === 'write-file') {
      const current = files[file] === undefined ? null : sha256(files[file]);
      if (current !== (body.hash ?? null)) return stale(file, body.hash);
      files[file] = body.text;
      return { result: { file, hash: sha256(body.text), bytes: Buffer.byteLength(body.text),
        created: current === null } };
    }
    if (body.action === 'delete-file') {
      if (files[file] === undefined || sha256(files[file]) !== body.hash) return stale(file, body.hash);
      delete files[file];
      return { result: { file, deleted: true } };
    }
    return undefined;
  };
}

/* A fake HTML artifact room, attached at TIER as landing, with FILES behind it. */
async function site(t, files = { 'index.html': PAGE }, tier = 'edit') {
  const origin = await startFakeRoom({ kind: 'html', bindings: { $ID: 'landing', $TITLE: 'Landing' },
    onEditing: fileOrigin(files) });
  t.after(() => origin.close());
  const env = tempEnv(t);
  const err = sink();
  assert.equal(await main(['attach', origin.link(tier)], { env, stdout: sink(), stderr: err }), 0, err.text);
  return { origin, ctx: { env } };
}

/* The consumer body fixture NAME sends, with its example bindings. */
function request(name) {
  const exchange = fixture(name);
  return bind(exchange.frames.find((f) => f.from === 'consumer').frame.body, exchange.bindings);
}

const text = (result) => result.content.map((c) => c.text).join('\n');

test('the registry offers the HTML file tools', () => {
  const names = tools.map((tool) => tool.name);
  for (const name of ['list_artifact_files', 'edit_artifact_file', 'write_artifact_file', 'delete_artifact_file']) {
    assert.ok(names.includes(name), name);
  }
  const read = tools.find((tool) => tool.name === 'read_artifact');
  assert.ok(read.inputSchema.properties.file);
  assert.deepEqual(tools.find((tool) => tool.name === 'edit_artifact_file').inputSchema.required,
    ['name', 'file', 'old_string', 'new_string']);
});

test('read_artifact returns an HTML file\'s text, sending the fixture\'s request', async (t) => {
  const { origin, ctx } = await site(t);
  const result = await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  assert.ok(!result.isError, text(result));
  assert.equal(text(result), PAGE);
  assert.deepEqual(origin.received.at(-1).body, request('read-file'));
});

test('list_artifact_files lists every file and marks the primary one', async (t) => {
  const { ctx } = await site(t, { 'index.html': PAGE, 'assets/site.css': 'p {}' });
  const result = await callTool('list_artifact_files', { name: 'landing' }, ctx);
  assert.ok(!result.isError, text(result));
  assert.equal(text(result), 'assets/site.css (4 bytes)\nindex.html (31 bytes, primary)');
});

test('edit_artifact_file replaces one exact string, writing with the hash it read', async (t) => {
  const { origin, ctx } = await site(t);
  await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  const result = await callTool('edit_artifact_file', { name: 'landing', file: 'index.html',
    old_string: '<h1>Hello</h1>', new_string: '<h1>Hello, world</h1>' }, ctx);
  assert.ok(!result.isError, text(result));
  assert.match(text(result), /^Edited index\.html/);
  assert.deepEqual(origin.received.at(-1).body, request('write-file'));
});

test('edit_artifact_file refuses a missing or ambiguous string and an unread file', async (t) => {
  const { origin, ctx } = await site(t, { 'index.html': PAGE, 'about.html': '<p>a</p><p>a</p>' });
  const unread = await callTool('edit_artifact_file', { name: 'landing', file: 'about.html',
    old_string: 'a', new_string: 'b' }, ctx);
  assert.ok(unread.isError);
  assert.match(text(unread), /Read about\.html with read_artifact first/);
  await callTool('read_artifact', { name: 'landing', file: 'about.html' }, ctx);
  const missing = await callTool('edit_artifact_file', { name: 'landing', file: 'about.html',
    old_string: 'zzz', new_string: 'b' }, ctx);
  assert.ok(missing.isError);
  assert.match(text(missing), /not found in about\.html/);
  const ambiguous = await callTool('edit_artifact_file', { name: 'landing', file: 'about.html',
    old_string: '<p>a</p>', new_string: 'b' }, ctx);
  assert.ok(ambiguous.isError);
  assert.match(text(ambiguous), /occurs 2 times in about\.html/);
  assert.ok(!origin.received.some((frame) => frame.body?.action === 'write-file'));
});

test('edit_artifact_file refuses a file changed since it was read, with its current content', async (t) => {
  const files = { 'index.html': PAGE, 'about.html': '<h1>Hello</h1>\n' };
  const { origin, ctx } = await site(t, files);
  await callTool('read_artifact', { name: 'landing', file: 'about.html' }, ctx);
  files['about.html'] = '<h1>Changed</h1>\n';
  const result = await callTool('edit_artifact_file', { name: 'landing', file: 'about.html',
    old_string: '<h1>Hello</h1>', new_string: '<h1>Hi</h1>' }, ctx);
  assert.ok(result.isError);
  assert.match(text(result), /about\.html changed since it was read/);
  assert.match(text(result), /<h1>Changed<\/h1>/);
  assert.ok(!origin.received.some((frame) => frame.body?.action === 'write-file'));
  // The content came with the refusal, so the edit can be redone at once.
  const again = await callTool('edit_artifact_file', { name: 'landing', file: 'about.html',
    old_string: '<h1>Changed</h1>', new_string: '<h1>Hi</h1>' }, ctx);
  assert.ok(!again.isError, text(again));
  assert.equal(files['about.html'], '<h1>Hi</h1>\n');
});

test('write_artifact_file reports the origin\'s conflict with the current content', async (t) => {
  const files = { 'index.html': PAGE };
  const { origin, ctx } = await site(t, files);
  await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  const { text: content } = request('write-conflict');
  const result = await callTool('write_artifact_file', { name: 'landing', file: 'index.html', content }, ctx);
  assert.deepEqual(origin.received.at(-1).body, request('write-conflict'));
  assert.ok(result.isError);
  const reply = fixture('write-conflict').frames.find((f) => f.from === 'origin').frame.body;
  assert.match(text(result), new RegExp(reply.error));
  assert.ok(text(result).includes(reply.text));
  // Redoing it names the hash the refusal returned.
  files['index.html'] = reply.text;
  const again = await callTool('write_artifact_file', { name: 'landing', file: 'index.html', content }, ctx);
  assert.ok(!again.isError, text(again));
  assert.equal(origin.received.at(-1).body.hash, reply.hash);
  assert.equal(files['index.html'], content);
});

test('write_artifact_file creates a new file and then writes it with its new hash', async (t) => {
  const files = { 'index.html': PAGE };
  const { origin, ctx } = await site(t, files);
  const created = await callTool('write_artifact_file',
    { name: 'landing', file: 'assets/site.css', content: 'p {}' }, ctx);
  assert.ok(!created.isError, text(created));
  assert.match(text(created), /^Created assets\/site\.css/);
  assert.equal(origin.received.at(-1).body.hash, null);
  const rewritten = await callTool('write_artifact_file',
    { name: 'landing', file: 'assets/site.css', content: 'p { margin: 0 }' }, ctx);
  assert.ok(!rewritten.isError, text(rewritten));
  assert.equal(origin.received.at(-1).body.hash, sha256('p {}'));
  assert.equal(files['assets/site.css'], 'p { margin: 0 }');
});

test('delete_artifact_file deletes a file it read, and only one it read', async (t) => {
  const files = { 'index.html': PAGE, 'old.html': '<p>old</p>' };
  const { ctx } = await site(t, files);
  const unread = await callTool('delete_artifact_file', { name: 'landing', file: 'old.html' }, ctx);
  assert.ok(unread.isError);
  assert.match(text(unread), /Read old\.html with read_artifact first/);
  await callTool('read_artifact', { name: 'landing', file: 'old.html' }, ctx);
  const deleted = await callTool('delete_artifact_file', { name: 'landing', file: 'old.html' }, ctx);
  assert.ok(!deleted.isError, text(deleted));
  assert.equal(files['old.html'], undefined);
});

test('a view link\'s file write is refused as view-only', async (t) => {
  const files = { 'index.html': PAGE };
  const { ctx } = await site(t, files, 'view');
  await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  const result = await callTool('write_artifact_file', { name: 'landing', file: 'index.html',
    content: 'x' }, ctx);
  assert.ok(result.isError);
  assert.match(text(result), /view-only/);
  assert.equal(files['index.html'], PAGE);
});

test('read_artifact shows an image file as an image and other bytes as a note', async (t) => {
  const png = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0, 1]);
  const origin = await startFakeRoom({ kind: 'html', bindings: { $ID: 'landing', $TITLE: 'Landing' },
    onEditing: (body) => ({ result: { file: body.file, hash: sha256(png), bytes: png.length,
      data: png.toString('base64') } }) });
  t.after(() => origin.close());
  const env = tempEnv(t);
  assert.equal(await main(['attach', origin.link('edit')], { env, stdout: sink(), stderr: sink() }), 0);
  const image = await callTool('read_artifact', { name: 'landing', file: 'logo.png' }, { env });
  assert.deepEqual(image.content, [{ type: 'image', data: png.toString('base64'), mimeType: 'image/png' }]);
  const other = await callTool('read_artifact', { name: 'landing', file: 'font.woff2' }, { env });
  assert.match(text(other), /font\.woff2 is binary \(10 bytes\)/);
});
