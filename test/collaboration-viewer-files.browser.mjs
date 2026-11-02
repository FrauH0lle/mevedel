/* The lobby's project files in a real browser: per-row downloads, dropped
 * uploads, and rows on a phone.
 *
 * The real viewer runs with every script. The lobby's relay socket is
 * answered by a stand-in host that seals frames with the room's key and
 * keeps the uploaded bytes, so downloads and drops travel the same frames
 * they do against Emacs.
 *
 * Run: node --test test/collaboration-viewer-files.browser.mjs
 * Requires `npm ci --prefix shared-editing` and Playwright Chromium.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';
import {webcrypto as crypto} from 'node:crypto';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');

const TYPES = {html: 'text/html', css: 'text/css', js: 'text/javascript'};

function base64url(bytes) {
  return Buffer.from(bytes).toString('base64')
    .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

// The lobby's owner secret; the full link is needed to use project files.
const secret = new Uint8Array(64).map((_, index) => (7 * 31 + index) % 256);
const ROOM = 'filesfilesfiles1';
const LINK = `http://localhost:8765/#${ROOM}.${base64url(secret)}`;

async function seal(key, frame) {
  const nonce = crypto.getRandomValues(new Uint8Array(12));
  const sealed = new Uint8Array(await crypto.subtle.encrypt(
    {name: 'AES-GCM', iv: nonce}, key, new TextEncoder().encode(JSON.stringify(frame))));
  const envelope = new Uint8Array(16 + sealed.length);
  envelope.set(nonce, 4);
  envelope.set(sealed, 16);
  return Buffer.from(envelope);
}

async function unseal(key, bytes) {
  const plain = await crypto.subtle.decrypt(
    {name: 'AES-GCM', iv: bytes.subarray(4, 16)}, key, bytes.subarray(16));
  return JSON.parse(new TextDecoder().decode(plain));
}

// Every byte value, so a text decode or lossy base64 step shows.
const binary = length => Buffer.from(Array.from({length}, (_, index) => (index * 7 + 3) % 256));

const LONG_NAME = 'quarterly-report-final-version-with-a-deliberately-unbroken-name-2026.bin';
const TOO_LARGE = 'recording.mov';
const TAKEN = 'taken.txt';

/* A stand-in host for the lobby's project files. FILES maps project paths
   to their bytes; folders are the paths' parents plus EMPTY_DIRS. While
   HOLD names a frame type, replies to it wait in HELD until released. */
function filesHost(files) {
  const state = {files: new Map(Object.entries(files)), emptyDirs: new Set(['empty']),
                 hold: new Set(), held: [], uploads: new Map()};
  const listing = dir => {
    const prefix = dir ? `${dir}/` : '';
    const dirs = new Set([...state.emptyDirs].filter(name => !dir && !name.includes('/')));
    const entries = [];
    for (const [path, bytes] of state.files) {
      if (!path.startsWith(prefix)) continue;
      const rest = path.slice(prefix.length);
      if (rest.includes('/')) dirs.add(rest.split('/')[0]);
      else entries.push({name: rest, kind: 'file', size: bytes.length});
    }
    return [...[...dirs].sort().map(name => ({name, kind: 'dir'})),
            ...entries.sort((a, b) => a.name.localeCompare(b.name))];
  };
  function answer(frame, reply, push) {
    if (frame.t === 'hello' || frame.t === 'lobby-refresh') {
      reply({t: 'lobby', project: 'proj', workspace: 'ws', omitted: 0, sessions: [
        {id: 's1', name: 'Feature work', updated: 1, preview: 'Hello', live: true, shared: true}]});
    } else if (frame.t === 'store-list') {
      reply({t: 'store-artifacts', artifacts: []});
    } else if (frame.t === 'files') {
      reply({t: 'files', reqId: frame.reqId, dir: frame.dir, omitted: 0, entries: listing(frame.dir)});
    } else if (frame.t === 'file-get') {
      const bytes = state.files.get(frame.path);
      if (!bytes || frame.path.endsWith(TOO_LARGE)) {
        reply({t: 'file', reqId: frame.reqId,
               error: bytes ? 'File too large to send (over 16 MB); open it on the host'
                 : 'This file is not in the project'});
        return;
      }
      // Two chunks, as the host's chunked sender splits larger files.
      const data = bytes.toString('base64');
      const cut = Math.floor(data.length / 8) * 4;
      // Like the host, untyped text previews as plain text.
      const text = /\.(txt|md)$/.test(frame.path);
      const meta = {t: 'file', reqId: frame.reqId, path: frame.path,
                    name: frame.path.split('/').pop(),
                    mime: text ? 'text/plain' : 'application/octet-stream', size: bytes.length};
      reply({...meta, data: data.slice(0, cut), final: false});
      reply({...meta, data: data.slice(cut), final: true});
    } else if (frame.t === 'file-upload') {
      let upload = state.uploads.get(frame.reqId);
      if (!upload) {
        const path = frame.dir ? `${frame.dir}/${frame.name}` : frame.name;
        if (frame.name === TAKEN || state.files.has(path)) {
          reply({t: 'file-upload', reqId: frame.reqId, error: `${path} already exists`});
          return;
        }
        upload = {path, size: frame.size, parts: []};
        state.uploads.set(frame.reqId, upload);
      }
      upload.parts.push(Buffer.from(frame.data, 'base64'));
      if (frame.final !== true) {
        reply({t: 'file-upload', reqId: frame.reqId, ok: true,
               received: upload.parts.reduce((sum, part) => sum + part.length, 0)});
        return;
      }
      state.uploads.delete(frame.reqId);
      const bytes = Buffer.concat(upload.parts);
      assert.equal(bytes.length, upload.size, `${upload.path} arrived whole`);
      state.files.set(upload.path, bytes);
      reply({t: 'file-upload', reqId: frame.reqId, ok: true, path: upload.path});
      const slash = upload.path.lastIndexOf('/');
      push({t: 'files-changed', dir: slash < 0 ? '' : upload.path.slice(0, slash)});
    }
  }
  state.answer = (frame, reply, push) => {
    if (state.hold.has(frame.t)) state.held.push(() => answer(frame, reply, push));
    else answer(frame, reply, push);
  };
  // Release the held replies, in ORDER of their indexes when given.
  state.release = (order = state.held.map((_, index) => index)) => {
    const held = state.held.splice(0);
    order.forEach(index => held[index]());
  };
  return state;
}

async function open(browser, {viewport = {width: 1200, height: 800}, files = {}} = {}) {
  const context = await browser.newContext({viewport, acceptDownloads: true});
  const page = await context.newPage();
  const key = await crypto.subtle.importKey(
    'raw', secret.slice(0, 32), 'AES-GCM', false, ['encrypt', 'decrypt']);
  const host = filesHost(files);
  page.host = host;
  page.received = [];
  page.navigations = [];
  await context.routeWebSocket(/\/r\//, socket => {
    // Sealing is asynchronous; chain it so frames leave in order, as on the
    // relay's socket. Unchained, a file's final chunk could overtake its first.
    let sending = Promise.resolve();
    const send = frame => {
      sending = sending.then(async () => socket.send(await seal(key, frame)));
      return sending;
    };
    socket.onMessage(async message => {
      if (typeof message === 'string') return;
      const frame = await unseal(key, message);
      page.received.push(frame);
      host.answer(frame, send, send);
    });
  });
  await context.route('http://localhost:8765/**', async route => {
    let path = new URL(route.request().url()).pathname;
    if (path === '/') path = '/index.html';
    const type = TYPES[path.split('.').pop()];
    await route.fulfill(type
      ? {body: await readFile(resolve(root, 'relay/viewer', `.${path}`)), contentType: type}
      : {status: 204, body: ''});
  });
  await page.goto(LINK);
  page.on('framenavigated', frame => {
    if (frame === page.mainFrame()) page.navigations.push(frame.url());
  });
  await page.locator('#lobby-list').getByText('Feature work', {exact: true}).waitFor();
  await page.locator('#lobby-tab-files').click();
  await page.locator('#files-list li, #files-status:not([hidden])').first().waitFor();
  // Only what the test itself causes counts as leaving the page.
  page.navigations.splice(0);
  return page;
}

// A pending download wait must not mask the assertion that ends a test early.
function nextDownload(page) {
  const download = page.waitForEvent('download');
  download.catch(() => {});
  return download;
}

const sent = (page, type) => page.received.filter(frame => frame.t === type);

async function until(predicate, what) {
  const deadline = Date.now() + 5000;
  while (!predicate()) {
    if (Date.now() > deadline) assert.fail(`timed out waiting for ${what}`);
    await new Promise(resolve => setTimeout(resolve, 10));
  }
}

const notice = (page, pattern) => page.waitForFunction(source => {
  const node = document.getElementById('notice');
  return node && !node.hidden && new RegExp(source).test(node.textContent);
}, pattern.source);

const row = (page, name) => page.locator('#files-list li').filter({
  has: page.getByRole('link', {name: `View ${name}`, exact: true})});

/* Drag FILES over SELECTOR and drop them, with the events a browser fires
   for an operating-system drag. Each file is {name, data} (base64) or
   {name, size} (zeros, made in the page). Returns whether the page took
   over dragover, whether it marked a drop zone, and whether it took over
   the drop. A drop the page does not take is the browser's: it opens the
   file and leaves the viewer. Synthetic events never run that default,
   so defaultPrevented is the evidence. */
function drop(page, selector, files) {
  return page.evaluate(({selector, files}) => {
    const transfer = new DataTransfer();
    for (const file of files) {
      const bytes = file.data !== undefined
        ? Uint8Array.from(atob(file.data), char => char.charCodeAt(0))
        : new Uint8Array(file.size);
      transfer.items.add(new File([bytes], file.name));
    }
    const target = document.querySelector(selector);
    const fire = type => {
      const event = new DragEvent(type, {bubbles: true, cancelable: true, dataTransfer: transfer});
      target.dispatchEvent(event);
      return event.defaultPrevented;
    };
    fire('dragenter');
    const accepted = fire('dragover');
    const marked = document.querySelector('[data-dropping]') !== null;
    const taken = fire('drop');
    const cleared = document.querySelector('[data-dropping]') === null;
    return {accepted, marked, taken, cleared};
  }, {selector, files: files.map(file => file.bytes
    ? {name: file.name, data: file.bytes.toString('base64')} : file)});
}

test('a row downloads its file at once, leaving the page, panel and history alone', async () => {
  const browser = await chromium.launch();
  try {
    const bin = binary(3000);
    const inner = binary(70);
    const page = await open(browser, {files: {
      [LONG_NAME]: bin, 'notes.txt': Buffer.from('Plain project notes.'),
      [TOO_LARGE]: binary(10), 'docs/inner.bin': inner}});
    await row(page, LONG_NAME).waitFor();
    const before = await page.evaluate(() => ({href: location.href, length: history.length}));
    const unchanged = async label => {
      assert.deepEqual(await page.evaluate(() => ({href: location.href, length: history.length})),
                       before, `${label}: address and history`);
      assert.deepEqual(page.navigations, [], `${label}: no navigation`);
      assert.equal(await page.locator('#artifact-panel').isVisible(), false, `${label}: panel stays shut`);
      assert.equal(await page.locator('#lobby-tab-files').getAttribute('aria-selected'), 'true');
    };

    // A download and a preview in flight together each get their own
    // bytes, whichever the host answers first. This runs on a fresh page,
    // where separate request counters would hand out the same ids.
    for (const order of [[1, 0], [0, 1]]) {
      page.host.hold.add('file-get');
      const both = nextDownload(page);
      await page.getByRole('button', {name: `Download ${LONG_NAME}`, exact: true}).click();
      await row(page, 'notes.txt').getByRole('link', {name: 'View notes.txt'}).click();
      await until(() => page.host.held.length === 2, 'both requests');
      page.host.hold.clear();
      page.host.release(order);
      const file = await both;
      assert.equal(file.suggestedFilename(), LONG_NAME, `order ${order}`);
      assert.deepEqual(await readFile(await file.path()), bin, `order ${order}`);
      await file.delete();
      await page.locator('#artifact-body').getByText('Plain project notes.', {exact: true}).waitFor();
      assert.equal(await page.locator('#artifact-title').textContent(), 'notes.txt');
      await page.locator('#artifact-close').click();
      await row(page, LONG_NAME).waitFor();
    }
    const reopened = await page.evaluate(() => ({href: location.href, length: history.length}));
    page.navigations.splice(0);

    // The bytes arrive whole and the browser saves them under the file's name.
    const gets = sent(page, 'file-get').length;
    page.host.hold.add('file-get');
    const button = page.getByRole('button', {name: `Download ${LONG_NAME}`, exact: true});
    const downloaded = nextDownload(page);
    await button.click();
    await until(() => page.host.held.length === 1, 'the download request');
    // A second press while it travels starts no second transfer.
    await button.evaluate(node => node.click());
    await page.waitForTimeout(100);
    assert.equal(sent(page, 'file-get').length, gets + 1);
    page.host.hold.clear();
    page.host.release();
    const download = await downloaded;
    assert.equal(download.suggestedFilename(), LONG_NAME);
    assert.deepEqual(await readFile(await download.path()), bin);
    await download.delete();
    Object.assign(before, reopened);
    await unchanged('download');
    // The browser saves before the row hears the transfer finished.
    await page.waitForFunction(node => !node.disabled && node.textContent === 'Download',
                               await button.elementHandle());

    // A file in a folder saves under its own name, not its project path.
    await page.getByRole('button', {name: 'Open folder docs'}).click();
    await row(page, 'inner.bin').waitFor();
    const innerDownload = nextDownload(page);
    await page.getByRole('button', {name: 'Download inner.bin', exact: true}).click();
    const saved = await innerDownload;
    assert.equal(saved.suggestedFilename(), 'inner.bin');
    assert.deepEqual(await readFile(await saved.path()), inner);
    await saved.delete();
    assert.equal(sent(page, 'file-get').at(-1).path, 'docs/inner.bin');
    await page.locator('#files-path').getByRole('button', {name: 'All files'}).click();
    await row(page, TOO_LARGE).waitFor();

    // A refusal is reported and opens nothing.
    await page.getByRole('button', {name: `Download ${TOO_LARGE}`, exact: true}).click();
    await notice(page, /too large to send/);
    assert.equal(await page.locator('#artifact-panel').isVisible(), false);
    await page.context().close();
  } finally {
    await browser.close();
  }
});

test('dropped files upload whole into the folder on screen, with progress and refusals', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser, {files: {'docs/readme.md': Buffer.from('# Docs')}});
    await page.getByRole('button', {name: 'Open folder empty'}).click();
    await page.locator('#files-status').getByText(/No files here yet/).waitFor();
    // An empty folder still offers a surface to drop on, and says so.
    assert.equal(await page.locator('#files-drop-hint').isVisible(), true);
    const height = await page.locator('#lobby-files').evaluate(node => node.getBoundingClientRect().height);
    assert.ok(height >= 96, `empty files panel is ${height}px tall`);

    // Two files, one over the upload chunk size, go in acknowledged chunks.
    const big = {name: 'capture.bin', bytes: binary(512 * 1024 + 4099)};
    const small = {name: 'tiny.dat', bytes: binary(17)};
    page.host.hold.add('file-upload');
    const result = await drop(page, '#files-drop-hint', [big, small]);
    assert.deepEqual(result, {accepted: true, marked: true, taken: true, cleared: true});
    await until(() => page.host.held.length === 1, 'the first chunk');
    assert.equal(sent(page, 'file-upload')[0].dir, 'empty');
    page.host.release();
    await page.locator('#files-status').getByText(/Uploading capture\.bin… \d+%/).waitFor();
    page.host.hold.clear();
    page.host.release();
    await notice(page, /^2 files added\.$/);
    assert.deepEqual(page.host.files.get('empty/capture.bin'), big.bytes);
    assert.deepEqual(page.host.files.get('empty/tiny.dat'), small.bytes);
    assert.equal(sent(page, 'file-upload').filter(frame => frame.name).length, 2);
    await row(page, 'capture.bin').waitFor();
    await row(page, 'tiny.dat').waitFor();

    // The whole lobby takes a drop while Files shows, the heading included,
    // and it lands in the folder on screen.
    await page.locator('#files-path').getByRole('button', {name: 'All files'}).click();
    await page.getByRole('button', {name: 'Open folder docs'}).click();
    await row(page, 'readme.md').waitFor();
    const header = await drop(page, '#lobby-title', [{name: 'header.bin', bytes: binary(9)}]);
    assert.deepEqual(header, {accepted: true, marked: true, taken: true, cleared: true});
    await notice(page, /^docs\/header\.bin added\.$/);
    assert.deepEqual(page.host.files.get('docs/header.bin'), binary(9));
    await row(page, 'header.bin').waitFor();

    // A drop that misses the lobby uploads nothing, and the page still
    // takes it, so the browser does not open the file in place of the viewer.
    const missedBefore = sent(page, 'file-upload').length;
    const missed = await drop(page, 'body > header.topbar', [{name: 'missed.bin', bytes: binary(5)}]);
    assert.deepEqual(missed, {accepted: true, marked: false, taken: true, cleared: true});
    await page.waitForTimeout(100);
    assert.equal(sent(page, 'file-upload').length, missedBefore);

    // A host refusal and the browser's own size bound are both reported;
    // nothing is written.
    const uploads = sent(page, 'file-upload').length;
    await drop(page, '#files-list', [{name: TAKEN, bytes: Buffer.from('x')}]);
    await notice(page, /taken\.txt already exists/);
    await drop(page, '#files-list', [{name: 'huge.iso', size: 17 * 1024 * 1024}]);
    await notice(page, /huge\.iso is over the 16 MB upload limit/);
    assert.equal(sent(page, 'file-upload').length, uploads + 1);
    assert.equal(page.host.files.has('docs/huge.iso'), false);
    assert.equal(await page.locator('#files-status').isVisible(), false);

    // Another tab's drop is not an upload.
    await page.locator('#lobby-tab-sessions').click();
    await page.locator('#lobby-files').waitFor({state: 'hidden'});
    const before = sent(page, 'file-upload').length;
    const elsewhere = await drop(page, '#lobby-list', [{name: 'stray.txt', bytes: Buffer.from('x')}]);
    assert.equal(elsewhere.marked, false);
    await page.waitForTimeout(100);
    assert.equal(sent(page, 'file-upload').length, before);
    await page.context().close();
  } finally {
    await browser.close();
  }
});

test('file rows keep their name and both actions within a 375px phone', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser, {viewport: {width: 375, height: 667}, files: {
      [LONG_NAME]: binary(123456), 'a.md': Buffer.from('a'), 'src/main.el': Buffer.from('x')}});
    await row(page, LONG_NAME).waitFor();
    const width = await page.evaluate(() => document.documentElement.clientWidth);
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true,
                 'the page does not scroll sideways');
    for (const name of [LONG_NAME, 'a.md']) {
      const item = row(page, name);
      const fits = await item.evaluate(node => node.scrollWidth <= node.clientWidth + 1);
      assert.equal(fits, true, `${name} row does not overflow`);
      // The name keeps most of the row rather than a word-per-line column
      // beside the actions.
      const share = await item.evaluate(node => node.querySelector('.files-name')
        .getBoundingClientRect().width / node.getBoundingClientRect().width);
      assert.ok(share >= 0.6, `${name} gets ${Math.round(share * 100)}% of its row`);
      const boxes = {};
      for (const label of ['View', 'Download', 'Remove']) {
        const control = label === 'View'
          ? item.getByRole('link', {name: `View ${name}`, exact: true})
          : item.getByRole('button', {name: `${label} ${name}`, exact: true});
        assert.equal(await control.isVisible(), true, `${label} ${name} shows`);
        boxes[label] = await control.boundingBox();
        assert.ok(boxes[label].x >= 0 && boxes[label].x + boxes[label].width <= width + 1,
                  `${label} ${name} is within the screen`);
      }
      for (const label of ['Download', 'Remove']) {
        assert.ok(boxes[label].height >= 24, `${label} ${name} is a usable target`);
      }
      const apart = (a, b) => a.x + a.width <= b.x + 1 || b.x + b.width <= a.x + 1
        || a.y + a.height <= b.y + 1 || b.y + b.height <= a.y + 1;
      assert.ok(apart(boxes.Download, boxes.Remove), `${name}: Download and Remove do not overlap`);
      assert.ok(apart(boxes.View, boxes.Download), `${name}: the name does not run under Download`);
    }
    // A folder row stays a single open control.
    const folder = page.locator('#files-list li').filter({
      has: page.getByRole('button', {name: 'Open folder src'})});
    assert.equal(await folder.getByRole('button', {name: /^Download/}).count(), 0);
    const hint = page.locator('#files-drop-hint');
    assert.equal(await hint.isVisible(), true);
    assert.equal(await hint.evaluate(node => node.scrollWidth <= node.clientWidth + 1), true);
    // The hint is no instruction to a phone that cannot drop: the Upload
    // button still adds files.
    assert.equal(await page.locator('#files-upload').isVisible(), true);
    await page.context().close();
  } finally {
    await browser.close();
  }
});
