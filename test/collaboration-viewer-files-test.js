/* Focused project files controller and uploader assertions.
 * Run: node test/collaboration-viewer-files-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const plain = value => JSON.parse(JSON.stringify(value));
const tick = () => new Promise(resolve => setImmediate(resolve));

const ids = ['lobby-files', 'files-path', 'files-list', 'files-status',
             'files-upload', 'files-input'];

function fakeFile(name, size) {
  const bytes = Uint8Array.from({length: size}, (_, index) => index % 251);
  return {
    name, size, bytes,
    slice: (start, end) => ({arrayBuffer: async () => bytes.slice(start, end).buffer}),
  };
}

function build({confirmed = true, sendOk = true} = {}) {
  const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
  const document = {getElementById: id => nodes[id], createElement: tag => new Element(tag)};
  let dropTray = null;
  const window = {
    confirm: () => confirmed,
    mevedelTranscriptRenderer: {formatBytes: bytes => `${bytes} B`},
    mevedelAttachments: {bind: (tray, {target}) => { dropTray = {tray, target}; }},
  };
  const timers = [];
  const context = {window, document, console, btoa, Uint8Array, String, Promise,
                   setTimeout: callback => { timers.push(callback); return timers.length; },
                   clearTimeout: () => {}};
  load('relay/viewer/viewer-files.js', context);
  const sent = [];
  const notices = [];
  const opened = [];
  const send = async frame => { sent.push(frame); return sendOk; };
  const uploads = window.mevedelFilesView.uploader({
    send, setTimer: context.setTimeout, clearTimer: context.clearTimeout});
  const files = window.mevedelFilesView.create({
    send, el: (tag, className, text) => element(document, tag, className, text),
    notice: text => notices.push(text), uploads, openFile: path => opened.push(path),
  });
  return {files, uploads, nodes, sent, notices, opened, timers, window,
          drop: () => dropTray, api: window.mevedelFilesView};
}

const rootListing = reqId => ({
  t: 'files', reqId, dir: '', omitted: 0,
  entries: [{name: 'docs', kind: 'dir'}, {name: 'notes.md', kind: 'file', size: 12}],
});

// Showing the tree lists the root; rows name folders and files.
{
  const {files, nodes, sent, opened} = build();
  files.show('proj');
  assert.deepEqual(plain(sent), [{t: 'files', reqId: 1, dir: ''}]);
  assert.equal(textOf(nodes['files-status']), 'Loading…');
  files.listed(rootListing(1));
  assert.equal(textOf(nodes['files-path']), 'proj');
  const rows = nodes['files-list'].children;
  assert.equal(rows.length, 2);
  assert.match(textOf(rows[0]), /docs\//);
  // A folder row has no remove control; a file row does.
  assert.equal(rows[0].children.length, 1);
  assert.match(textOf(rows[1]), /notes\.md12 BRemove/);
  assert.equal(nodes['files-status'].hidden, true);
  // Opening a folder asks for it; opening a file hands its path on.
  rows[0].children[0].dispatch('click');
  assert.deepEqual(plain(sent.at(-1)), {t: 'files', reqId: 2, dir: 'docs'});
  rows[1].children[0].dispatch('click');
  assert.deepEqual(opened, ['notes.md']);
  // A stale answer does not replace the newer request's view.
  files.listed(rootListing(1));
  files.listed({t: 'files', reqId: 2, dir: 'docs', omitted: 3,
                entries: [{name: 'a.md', kind: 'file', size: 1}]});
  assert.equal(textOf(nodes['files-path']), 'proj/docs');
  assert.equal(textOf(nodes['files-status']), '3 more entries not shown.');
  // Breadcrumbs go back up.
  nodes['files-path'].children[0].dispatch('click');
  assert.deepEqual(plain(sent.at(-1)), {t: 'files', reqId: 3, dir: ''});
  files.listed({t: 'files', reqId: 3, dir: '', entries: [], omitted: 0});
  assert.equal(textOf(nodes['files-status']), 'No files here yet.');
}

// A folder that disappeared falls back to the root; a root refusal shows.
{
  const {files, nodes, sent} = build();
  files.show('proj');
  files.listed({t: 'files', reqId: 1, dir: 'gone', entries: [], omitted: 0});
  files.refresh();
  files.listed({t: 'files', reqId: 2, error: 'This folder is not in the project'});
  assert.deepEqual(plain(sent.at(-1)), {t: 'files', reqId: 3, dir: ''});
  files.listed({t: 'files', reqId: 3, error: 'A view link cannot use the project files'});
  assert.equal(textOf(nodes['files-status']), 'A view link cannot use the project files');
}

// Removing asks first, then reports and reloads.
{
  const {files, nodes, sent, notices} = build();
  files.show('proj');
  files.listed(rootListing(1));
  nodes['files-list'].children[1].children.at(-1).dispatch('click');
  assert.deepEqual(plain(sent.at(-1)), {t: 'file-remove', reqId: 2, path: 'notes.md'});
  files.removed({t: 'file-remove', reqId: 2, ok: true, path: 'notes.md'});
  assert.deepEqual(notices, ['notes.md moved to the trash.']);
  assert.equal(sent.at(-1).t, 'files');
  files.removed({t: 'file-remove', reqId: 99, ok: true});
  assert.equal(notices.length, 1);
  const declined = build({confirmed: false});
  declined.files.show('proj');
  declined.files.listed(rootListing(1));
  declined.nodes['files-list'].children[1].children.at(-1).dispatch('click');
  assert.equal(declined.sent.length, 1);
}

// Another guest's change reloads only the folder on screen.
{
  const {files, nodes, sent} = build();
  files.show('proj');
  files.listed(rootListing(1));
  files.changed({t: 'files-changed', dir: 'docs'});
  assert.equal(sent.length, 1);
  files.changed({t: 'files-changed', dir: ''});
  assert.equal(sent.length, 2);
  nodes['lobby-files'].hidden = true;
  files.changed({t: 'files-changed', dir: ''});
  assert.equal(sent.length, 2);
}

(async () => {
  // Uploads go in acknowledged chunks: the next is sent only after the
  // host took the previous one, and the first announces the file.
  {
    const {uploads, sent, api} = build();
    const file = fakeFile('big.bin', api.CHUNK_BYTES + 10);
    const progress = [];
    const done = uploads.upload(file, 'docs', {progress: share => progress.push(share)});
    await tick();
    assert.equal(sent.length, 1);
    const {data: _data, ...announced} = plain(sent[0]);
    assert.deepEqual(announced,
                     {t: 'file-upload', reqId: 1, dir: 'docs', name: 'big.bin',
                      size: api.CHUNK_BYTES + 10, rename: false});
    assert.equal(sent[0].final, undefined);
    uploads.handle({t: 'file-upload', reqId: 1, ok: true, received: api.CHUNK_BYTES});
    await tick();
    assert.equal(sent.length, 2);
    assert.equal(sent[1].final, true);
    assert.equal(sent[1].name, undefined);
    const decoded = Buffer.concat(sent.map(frame => Buffer.from(frame.data, 'base64')));
    assert.deepEqual(new Uint8Array(decoded), file.bytes);
    uploads.handle({t: 'file-upload', reqId: 1, ok: true, path: 'docs/big.bin'});
    assert.equal(await done, 'docs/big.bin');
    assert.equal(progress.at(-1), 1);
  }

  // A refusal, a dead socket, a silent host and an oversized file all
  // reject with a reason.
  {
    const {uploads, timers} = build();
    const refused = uploads.upload(fakeFile('a.txt', 3), '', {rename: true});
    await tick();
    uploads.handle({t: 'file-upload', reqId: 1, error: 'a.txt already exists'});
    await assert.rejects(refused, /already exists/);
    const silent = uploads.upload(fakeFile('b.txt', 3), '');
    await tick();
    timers.at(-1)();
    await assert.rejects(silent, /did not answer/);
    await assert.rejects(uploads.upload({name: 'huge', size: 17 * 1048576}, ''), /upload limit/);
    const offline = build({sendOk: false});
    await assert.rejects(offline.uploads.upload(fakeFile('c.txt', 1), ''), /Connection lost/);
  }

  // The tree uploads picked or dropped files into the folder it shows.
  {
    const {files, nodes, sent, notices, uploads, drop} = build();
    files.show('proj');
    files.listed({t: 'files', reqId: 1, dir: 'docs', entries: [], omitted: 0});
    assert.equal(drop().target, nodes['lobby-files']);
    nodes['files-input'].files = [fakeFile('one.txt', 2)];
    nodes['files-input'].dispatch('change');
    await tick();
    const frame = sent.at(-1);
    assert.equal(frame.t, 'file-upload');
    assert.equal(frame.dir, 'docs');
    assert.equal(nodes['files-upload'].disabled, true);
    uploads.handle({t: 'file-upload', reqId: frame.reqId, ok: true, path: 'docs/one.txt'});
    await tick();
    assert.deepEqual(notices, ['docs/one.txt added.']);
    assert.equal(nodes['files-upload'].disabled, false);
    assert.deepEqual(plain(sent.at(-1)), {t: 'files', reqId: 2, dir: 'docs'});
    drop().tray.add([fakeFile('two.txt', 1)]);
    await tick();
    uploads.handle({t: 'file-upload', reqId: sent.at(-1).reqId, error: 'Nope'});
    await tick();
    assert.deepEqual(notices.at(-1), 'Nope');
  }

  console.log('viewer files controller passed');
})().catch(error => {
  console.error(error);
  process.exit(1);
});
