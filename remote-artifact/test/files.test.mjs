import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import path from 'node:path';
import { main } from '../src/cli.mjs';
import { callTool, tools } from '../src/tools.mjs';
import { bind, fixture, startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

const PAGE = '<!doctype html>\n<h1>Hello</h1>\n';

const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

/*
 * An origin's HTML files, answering the requests no fixture covers as the
 * origin does (mevedel-collaboration-artifact-room.el and the store's file
 * operations), with its messages: FILES maps names to their content, and
 * index.html is the primary file.  Requests a fixture covers (index.html
 * as the fixtures have it) are answered from it.
 */
function fileOrigin(files) {
  const fields = (file) => ({ file, hash: sha256(files[file]), bytes: Buffer.byteLength(files[file]),
    text: files[file] });
  const guard = (file) => {
    if (typeof file !== 'string' || !file || path.isAbsolute(file)
      || file.split('/').some((part) => part === '' || part === '.' || part === '..')) {
      throw new Error(`Refused path ${JSON.stringify(file)}: name a file relative to the artifact, `
        + 'without `..\' or symbolic links');
    }
  };
  // The current file comes with a refusal only under 256 KiB.
  const stale = (file, expected) => ({
    error: files[file] === undefined ? `${file} does not exist; write it without a hash to create it`
      : expected ? `${file} changed since it was read; redo the change on its current content`
        : `${file} already exists; redo the change on its current content`,
    code: 'stale',
    ...(files[file] === undefined ? { file, hash: null }
      : Buffer.byteLength(files[file]) < 256 * 1024 ? fields(file)
        : { file, hash: sha256(files[file]), bytes: Buffer.byteLength(files[file]) }),
  });
  const answer = (body, { writable }) => {
    const file = body.file ?? 'index.html';
    if (body.action === 'files') {
      return { result: { primary: 'index.html',
        files: Object.keys(files).sort().map((f) => ({ file: f, bytes: Buffer.byteLength(files[f]) })) } };
    }
    if (body.action === 'read') {
      guard(file);
      return files[file] === undefined ? { error: `No file ${file} in this artifact` } : { result: fields(file) };
    }
    if (!['write-file', 'delete-file'].includes(body.action)) return undefined;
    if (!writable) return { error: 'This artifact link is view-only' };
    guard(file);
    if (body.action === 'write-file') {
      const current = files[file] === undefined ? null : sha256(files[file]);
      if (current !== (body.hash ?? null)) return stale(file, body.hash);
      files[file] = body.text;
      return { result: { file, hash: sha256(body.text), bytes: Buffer.byteLength(body.text),
        created: current === null } };
    }
    if (file === 'index.html') {
      return { error: 'index.html is the artifact\'s primary file; an artifact link cannot delete it' };
    }
    if (files[file] === undefined || sha256(files[file]) !== body.hash) return stale(file, body.hash);
    delete files[file];
    return { result: { file, deleted: true } };
  };
  return (body, options) => {
    try { return answer(body, options); } catch (error) { return { error: error.message }; }
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

// Every error message of the file tools, exactly as the agent sees it.

/* Call file tool NAME with ARGS for artifact landing; return its error text, failing if it succeeded. */
async function refused(ctx, name, args) {
  const result = await callTool(name, { name: 'landing', ...args }, ctx);
  assert.ok(result.isError, `${name} succeeded: ${text(result)}`);
  return text(result);
}

const REFUSED = (file) => `Refused path ${JSON.stringify(file)}: name a file relative to the artifact, `
  + 'without `..\' or symbolic links';

test('read_artifact names a missing file and a refused path', async (t) => {
  const { ctx } = await site(t);
  assert.equal(await refused(ctx, 'read_artifact', { file: 'missing.html' }),
    'No file missing.html in this artifact');
  assert.equal(await refused(ctx, 'read_artifact', { file: '../secret.txt' }), REFUSED('../secret.txt'));
});

test('write_artifact_file refuses a path outside the artifact', async (t) => {
  const files = { 'index.html': PAGE };
  const { ctx } = await site(t, files);
  for (const file of ['../escape.html', '/etc/passwd', 'assets/./x.css']) {
    assert.equal(await refused(ctx, 'write_artifact_file', { file, content: 'x' }), REFUSED(file));
  }
  assert.deepEqual(Object.keys(files), ['index.html']);
});

test('the file tools on a whiteboard report the origin\'s refusal', async (t) => {
  const origin = await startFakeRoom({
    onEditing: (body) => (body.action === 'read' && body.file ? { error: 'Only HTML artifacts have files to read' }
      : undefined),
  });
  t.after(() => origin.close());
  const env = tempEnv(t);
  assert.equal(await main(['attach', origin.link('edit'), '--name', 'landing'],
    { env, stdout: sink(), stderr: sink() }), 0);
  assert.equal(await refused({ env }, 'read_artifact', { file: 'index.html' }),
    'Only HTML artifacts have files to read');
  assert.equal(await refused({ env }, 'list_artifact_files', {}), 'An artifact link does not permit files');
  assert.equal(await refused({ env }, 'write_artifact_file', { file: 'index.html', content: 'x' }),
    'An artifact link does not permit write-file');
});

test('every file tool reports an unreachable origin plainly', async (t) => {
  const { origin, ctx } = await site(t, { 'index.html': PAGE, 'old.html': '<p>old</p>' });
  await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  await callTool('read_artifact', { name: 'landing', file: 'old.html' }, ctx);
  await origin.close();
  const message = 'Origin unavailable (revoked, deleted, or offline)';
  assert.equal(await refused(ctx, 'read_artifact', {}), message);
  assert.equal(await refused(ctx, 'list_artifact_files', {}), message);
  assert.equal(await refused(ctx, 'edit_artifact_file',
    { file: 'index.html', old_string: 'Hello', new_string: 'Hi' }), message);
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'index.html', content: 'x' }), message);
  assert.equal(await refused(ctx, 'delete_artifact_file', { file: 'old.html' }), message);
});

test('a view link may read and list files, but every change is refused as view-only', async (t) => {
  const files = { 'index.html': PAGE, 'old.html': '<p>old</p>' };
  const { ctx } = await site(t, files, 'view');
  assert.equal(text(await callTool('read_artifact', { name: 'landing', file: 'old.html' }, ctx)), '<p>old</p>');
  assert.ok(!(await callTool('list_artifact_files', { name: 'landing' }, ctx)).isError);
  const message = 'This artifact link is view-only';
  assert.equal(await refused(ctx, 'edit_artifact_file',
    { file: 'old.html', old_string: 'old', new_string: 'new' }), message);
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'old.html', content: 'x' }), message);
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'new.html', content: 'x' }), message);
  assert.equal(await refused(ctx, 'delete_artifact_file', { file: 'old.html' }), message);
  assert.deepEqual(files, { 'index.html': PAGE, 'old.html': '<p>old</p>' });
});

test('edit_artifact_file\'s own refusals, each before anything is written', async (t) => {
  const files = { 'index.html': PAGE, 'about.html': '<p>a</p><p>a</p>' };
  const { origin, ctx } = await site(t, files);
  assert.equal(await refused(ctx, 'edit_artifact_file', { file: 'about.html', old_string: 'a', new_string: 'b' }),
    'Read about.html with read_artifact first');
  await callTool('read_artifact', { name: 'landing', file: 'about.html' }, ctx);
  assert.equal(await refused(ctx, 'edit_artifact_file', { file: 'about.html', old_string: '', new_string: 'b' }),
    'old_string must not be empty; use write_artifact_file to rewrite a file');
  assert.equal(await refused(ctx, 'edit_artifact_file', { file: 'about.html', old_string: 'zzz', new_string: 'b' }),
    'old_string not found in about.html');
  assert.equal(await refused(ctx, 'edit_artifact_file',
    { file: 'about.html', old_string: '<p>a</p>', new_string: 'b' }),
  'old_string occurs 2 times in about.html; include more context to make it unique');
  assert.ok(!origin.received.some((frame) => frame.body?.action === 'write-file'));
  assert.equal(files['about.html'], '<p>a</p><p>a</p>');
});

test('edit_artifact_file reports a stale file with its content, and a file deleted since', async (t) => {
  const files = { 'index.html': PAGE, 'about.html': '<h1>Hello</h1>\n', 'gone.html': '<p>gone</p>' };
  const { ctx } = await site(t, files);
  await callTool('read_artifact', { name: 'landing', file: 'about.html' }, ctx);
  await callTool('read_artifact', { name: 'landing', file: 'gone.html' }, ctx);
  files['about.html'] = '<h1>Changed</h1>\n';
  delete files['gone.html'];
  assert.equal(await refused(ctx, 'edit_artifact_file',
    { file: 'about.html', old_string: 'Hello', new_string: 'Hi' }),
  'about.html changed since it was read; redo the change on its current content.\n'
    + 'Current content of about.html:\n<h1>Changed</h1>\n');
  assert.equal(await refused(ctx, 'edit_artifact_file', { file: 'gone.html', old_string: 'gone', new_string: 'x' }),
    'No file gone.html in this artifact');
  assert.equal(text(await callTool('edit_artifact_file', { name: 'landing', file: 'about.html',
    old_string: 'Changed', new_string: 'Hi' }, ctx)), 'Edited about.html (12 bytes)');
});

test('edit_artifact_file refuses a binary file', async (t) => {
  const png = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0, 1]);
  const origin = await startFakeRoom({ kind: 'html', bindings: { $ID: 'landing', $TITLE: 'Landing' },
    onEditing: (body) => ({ result: { file: body.file, hash: sha256(png), bytes: png.length,
      data: png.toString('base64') } }) });
  t.after(() => origin.close());
  const env = tempEnv(t);
  assert.equal(await main(['attach', origin.link('edit')], { env, stdout: sink(), stderr: sink() }), 0);
  const ctx = { env };
  await callTool('read_artifact', { name: 'landing', file: 'logo.png' }, ctx);
  assert.equal(await refused(ctx, 'edit_artifact_file', { file: 'logo.png', old_string: 'P', new_string: 'Q' }),
    'logo.png is not a text file');
});

test('write_artifact_file\'s stale refusals say what the file is now', async (t) => {
  const big = 'x'.repeat(256 * 1024);
  const files = { 'index.html': PAGE, 'about.html': '<p>about</p>', 'big.html': big, 'gone.html': '<p>gone</p>' };
  const { ctx } = await site(t, files);

  // Exact replay of the fixture's conflict.
  await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  const reply = fixture('write-conflict').frames.find((f) => f.from === 'origin').frame.body;
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'index.html', content: request('write-conflict').text }),
    `${reply.error}.\nCurrent content of index.html:\n${reply.text}`);

  // Creating a file that exists: its content counts as read.
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'about.html', content: '<p>new</p>' }),
    'about.html already exists; redo the change on its current content.\n'
      + 'Current content of about.html:\n<p>about</p>');
  assert.equal(text(await callTool('write_artifact_file', { name: 'landing', file: 'about.html',
    content: '<p>new</p>' }, ctx)), 'Wrote about.html (10 bytes)');

  // A large file comes back without its content: it must be read again.
  await callTool('read_artifact', { name: 'landing', file: 'big.html' }, ctx);
  files['big.html'] = `${big}y`;
  const largeMessage = 'big.html changed since it was read; redo the change on its current content.\n'
    + `big.html has ${big.length + 1} bytes now; read it again with read_artifact.`;
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'big.html', content: 'small' }), largeMessage);
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'big.html', content: 'small' }), largeMessage);
  await callTool('read_artifact', { name: 'landing', file: 'big.html' }, ctx);
  assert.equal(text(await callTool('write_artifact_file', { name: 'landing', file: 'big.html', content: 'small' },
    ctx)), 'Wrote big.html (5 bytes)');

  // A file deleted since it was read: the next write creates it.
  await callTool('read_artifact', { name: 'landing', file: 'gone.html' }, ctx);
  delete files['gone.html'];
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'gone.html', content: '<p>back</p>' }),
    'gone.html does not exist; write it without a hash to create it.');
  assert.equal(text(await callTool('write_artifact_file', { name: 'landing', file: 'gone.html',
    content: '<p>back</p>' }, ctx)), 'Created gone.html (11 bytes)');
});

test('write_artifact_file refuses content over 16 MiB before connecting', async (t) => {
  const { origin, ctx } = await site(t);
  const before = origin.connections;
  assert.equal(await refused(ctx, 'write_artifact_file', { file: 'huge.html', content: 'x'.repeat(16 * 1024 * 1024 + 1) }),
    'huge.html would be larger than 16777216 bytes');
  assert.equal(origin.connections, before);
});

test('delete_artifact_file\'s refusals: unread, primary, stale and deleted since', async (t) => {
  const files = { 'index.html': PAGE, 'old.html': '<p>old</p>', 'gone.html': '<p>gone</p>' };
  const { ctx } = await site(t, files);
  assert.equal(await refused(ctx, 'delete_artifact_file', { file: 'old.html' }),
    'Read old.html with read_artifact first');
  await callTool('read_artifact', { name: 'landing', file: 'index.html' }, ctx);
  assert.equal(await refused(ctx, 'delete_artifact_file', { file: 'index.html' }),
    'index.html is the artifact\'s primary file; an artifact link cannot delete it');
  await callTool('read_artifact', { name: 'landing', file: 'old.html' }, ctx);
  files['old.html'] = '<p>newer</p>';
  assert.equal(await refused(ctx, 'delete_artifact_file', { file: 'old.html' }),
    'old.html changed since it was read; redo the change on its current content.\n'
      + 'Current content of old.html:\n<p>newer</p>');
  // The refusal carried the content, so the delete can be redone at once.
  assert.equal(text(await callTool('delete_artifact_file', { name: 'landing', file: 'old.html' }, ctx)),
    'Deleted old.html');
  await callTool('read_artifact', { name: 'landing', file: 'gone.html' }, ctx);
  delete files['gone.html'];
  assert.equal(await refused(ctx, 'delete_artifact_file', { file: 'gone.html' }),
    'gone.html does not exist; write it without a hash to create it.');
  assert.deepEqual(Object.keys(files), ['index.html']);
});
