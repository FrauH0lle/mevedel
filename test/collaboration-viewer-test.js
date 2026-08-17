/* collaboration-viewer-test.js -- deterministic sealed viewer assertions
 *
 * Drives relay/viewer/viewer.js through a fake DOM and WebSocket with
 * node's WebCrypto, which is the same API a browser uses, so host<->guest
 * sealing interop is asserted here without a browser.
 *
 * Run: node test/collaboration-viewer-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

class Element {
  constructor(tag) {
    this.tagName = tag;
    this.children = [];
    this.listeners = {};
    this.textContent = '';
    this.className = '';
    this.dataset = {};
    this.hidden = false;
    this.value = '';
    this.scrollHeight = 100;
  }
  append(...children) {
    for (const child of children) child.parent = this;
    this.children.push(...children);
  }
  replaceChildren(...children) { this.children = children; }
  remove() {
    if (this.parent) this.parent.children = this.parent.children.filter(x => x !== this);
  }
  addEventListener(type, callback) {
    (this.listeners[type] ||= []).push(callback);
  }
  dispatchEvent(event) { this.dispatch(event.type, event); }
  dispatch(type, event = {}) {
    if (!event.preventDefault) event.preventDefault = () => {};
    if (!event.type) event.type = type;
    for (const callback of this.listeners[type] || []) callback(event);
  }
}

class Socket {
  static OPEN = 1;
  constructor() { this.listeners = {}; this.readyState = Socket.OPEN; this.sent = []; }
  addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); }
  send(value) { this.sent.push(value); }
  close() { this.readyState = 3; this.dispatch('close', {code: 1006}); }
  dispatch(type, event = {}) {
    for (const callback of this.listeners[type] || []) callback(event);
  }
}

class FakeEvent {
  constructor(type, options = {}) { this.type = type; this.cancelable = !!options.cancelable; }
  preventDefault() {}
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function base64url(bytes) {
  return Buffer.from(bytes).toString('base64url');
}

// Host-side sealing, mirroring mevedel-collaboration--seal: envelope is
// [4-byte peerId][12-byte nonce][ciphertext||tag].
async function seal(key, peer, frame) {
  const nonce = crypto.getRandomValues(new Uint8Array(12));
  const sealed = new Uint8Array(await crypto.subtle.encrypt(
    {name: 'AES-GCM', iv: nonce}, key, encoder.encode(JSON.stringify(frame))));
  const envelope = new Uint8Array(4 + 12 + sealed.length);
  new DataView(envelope.buffer).setUint32(0, peer);
  envelope.set(nonce, 4);
  envelope.set(sealed, 16);
  return envelope.buffer;
}

async function unseal(key, data) {
  const bytes = new Uint8Array(data);
  const plaintext = await crypto.subtle.decrypt(
    {name: 'AES-GCM', iv: bytes.slice(4, 16)}, key, bytes.slice(16));
  return JSON.parse(decoder.decode(plaintext));
}

const tick = () => new Promise(resolve => setImmediate(resolve));

// WebCrypto may resolve off the immediate queue; poll briefly.
async function waitFor(predicate, what) {
  for (let i = 0; i < 500; i++) {
    if (predicate()) return;
    await tick();
  }
  throw new Error(`timed out waiting for ${what}`);
}

async function main() {
  const keyBytes = crypto.getRandomValues(new Uint8Array(32));
  const writeToken = crypto.getRandomValues(new Uint8Array(16));
  const key = await crypto.subtle.importKey('raw', keyBytes, 'AES-GCM', false,
                                            ['encrypt', 'decrypt']);
  const roomId = 'roomroomroomroom';
  const fullSecret = base64url(Buffer.concat([Buffer.from(keyBytes),
                                              Buffer.from(writeToken)]));

  const ids = ['transcript', 'connection', 'notice', 'live-button',
               'composer', 'composer-input', 'composer-name',
               'send-button', 'stop-button', 'filter', 'requests'];
  const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
  nodes.composer.hidden = true;
  const sockets = [];
  let timer;
  const storage = new Map();
  const window = {
    location: {
      href: `http://127.0.0.1:1/index.html#${roomId}.${fullSecret}`,
      hash: `#${roomId}.${fullSecret}`,
      pathname: '/index.html', search: '', protocol: 'http:',
    },
    history: {replaceState(_state, _title, path) { window.replaced = path; }},
    innerHeight: 20,
    scrollY: 0,
    listeners: {},
    addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); },
    setTimeout(callback) { timer = callback; return 1; },
    scrollTo(options) { this.scrollY = options.top; },
  };
  const document = {
    getElementById(id) { return nodes[id]; },
    createElement(tag) { return new Element(tag); },
    documentElement: {scrollHeight: 100},
  };
  class TestWebSocket extends Socket {
    constructor(url) { super(); this.url = url; sockets.push(this); }
  }
  const context = {
    document, window, WebSocket: TestWebSocket, URL, console,
    crypto, TextEncoder, TextDecoder, atob, btoa, Date,
    Event: FakeEvent,
    localStorage: {
      getItem: k => (storage.has(k) ? storage.get(k) : null),
      setItem: (k, v) => storage.set(k, v),
    },
    setTimeout: window.setTimeout,
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/viewer.js', 'utf8'), context);

  // Link grammar: view links carry the bare key, full links append the
  // write token, anything else is rejected.
  const api = context.window.mevedelViewer;
  const view = api.parseFragment(`#${roomId}.${base64url(keyBytes)}`);
  assert.equal(view.roomId, roomId);
  assert.equal(view.writeToken, null);
  const full = api.parseFragment(`#${roomId}.${fullSecret}`);
  assert.equal(full.keyBytes.length, 32);
  assert.equal(full.writeToken.length, 16);
  assert.equal(api.parseFragment('#short.abc'), null);
  assert.equal(api.parseFragment(`#${roomId}.${base64url(new Uint8Array(31))}`), null);
  assert.equal(api.parseFragment('#nodotsecret'), null);

  // The key never survives in the URL, and the guest dials the room.
  assert.equal(window.replaced, '/index.html');
  await tick();
  assert.equal(sockets.length, 1);
  const first = sockets[0];
  assert.match(first.url, new RegExp(`/r/${roomId}\\?role=guest$`));

  // Hello is sealed and carries the write token.
  first.dispatch('open');
  await waitFor(() => first.sent.length === 1, 'sealed hello');
  const hello = await unseal(key, first.sent[0]);
  assert.equal(hello.t, 'hello');
  assert.equal(hello.proto, 2);
  assert.equal(hello.writeToken, base64url(writeToken));
  assert.equal(typeof hello.name, 'string');

  let delivered = 0;
  let applied = 0;
  const deliver = async frame => {
    first.dispatch('message', {data: await seal(key, 1, frame)});
    delivered++;
    // Wait until the viewer's serialized inbound chain has consumed it.
    await waitFor(() => context.window.mevedelViewerApplied === delivered,
                  'frame application');
  };

  // Welcome for a writable guest reveals the composer; the snapshot loads
  // through final-flagged chunks with live updates queued behind it.
  await deliver({t: 'welcome', proto: 2, readOnly: false, recordCount: 3});
  assert.equal(nodes.composer.hidden, false);
  await deliver({t: 'snapshot-chunk', final: false, records: [
    {id: 'assistant', kind: 'assistant', revision: 0, text: '<b>inert</b>'},
    {id: 'tool', kind: 'tool', revision: 0, name: 'Bash', status: 'completed',
     summary: 'Bash', result: 'large', truncated: true},
  ]});
  await deliver({t: 'record', record: {id: 'assistant', kind: 'assistant',
                                       revision: 1, text: 'stream replacement'}});
  await deliver({t: 'snapshot-chunk', final: true, records: [
    {id: 'guest-user', kind: 'user', revision: 0, text: 'from the phone',
     guest: 'roland'},
  ]});
  assert.equal(nodes.transcript.children.length, 3);
  assert.equal(nodes.transcript.children[0].children[1].textContent,
               'stream replacement');
  assert.equal(nodes.transcript.children[0].children[1].tagName, 'pre');
  assert.match(nodes.transcript.children[1].children[1].children[0].textContent,
               /truncated/);
  // Guest badge renders on the attributed prompt only.
  assert.equal(nodes.transcript.children[2].children[0].children[0].textContent,
               'Guest roland');
  assert.equal(nodes.transcript.children[2].children[0].children[0].className,
               'guest-badge');

  // Live record update and removal.
  await deliver({t: 'record', record: {id: 'assistant', kind: 'assistant',
                                       revision: 2, text: 'newest'}});
  assert.equal(nodes.transcript.children[0].children[1].textContent, 'newest');
  await deliver({t: 'remove', ids: ['tool']});
  assert.equal(nodes.transcript.children.length, 2);

  // Composer sends a sealed prompt; interrupt sends a sealed abort.
  nodes['composer-input'].value = 'check the tests';
  nodes['composer-name'].value = 'roland';
  nodes.composer.dispatch('submit');
  await waitFor(() => first.sent.length === 2, 'sealed prompt');
  const prompt = await unseal(key, first.sent[1]);
  assert.deepEqual(prompt, {t: 'prompt', text: 'check the tests',
                            name: 'roland'});
  assert.equal(nodes['composer-input'].value, '');
  // The host's queued acknowledgement surfaces as a notice.
  await deliver({t: 'queued'});
  assert.match(nodes.notice.textContent, /queued/i);
  nodes['stop-button'].dispatch('click');
  await waitFor(() => first.sent.length === 3, 'sealed abort');
  assert.deepEqual(await unseal(key, first.sent[2]), {t: 'abort'});

  // Directive-tagged records grow a client-side filter menu; selecting a
  // directive hides everything outside it, per guest, no round-trips.
  assert.equal(nodes.filter.hidden, true);
  await deliver({t: 'record', record: {
    id: 'directive-user', kind: 'user', revision: 0,
    text: 'Refactor the parser\nwith details', directive: 'dir-1',
  }});
  assert.equal(nodes.filter.hidden, false);
  assert.deepEqual(nodes.filter.children.map(o => o.value),
                   ['all', 'main', 'dir-1']);
  assert.equal(nodes.filter.children[2].textContent, '◆ Refactor the parser');
  nodes.filter.value = 'dir-1';
  nodes.filter.dispatch('change');
  assert.equal(nodes.transcript.children.find(
    c => c.dataset.recordId === 'assistant').hidden, true);
  assert.equal(nodes.transcript.children.find(
    c => c.dataset.recordId === 'directive-user').hidden, false);
  nodes.filter.value = 'main';
  nodes.filter.dispatch('change');
  assert.equal(nodes.transcript.children.find(
    c => c.dataset.recordId === 'assistant').hidden, false);
  assert.equal(nodes.transcript.children.find(
    c => c.dataset.recordId === 'directive-user').hidden, true);
  nodes.filter.value = 'all';
  nodes.filter.dispatch('change');

  // A ui-request renders a card whose buttons and feedback field answer
  // through sealed ui-response frames; ui-request-end dismisses it.
  await deliver({t: 'ui-request', reqId: 41, body: 'Run rm -rf /tmp/x?',
                 options: [{id: 0, label: 'Allow once'}, {id: 1, label: 'Deny'}],
                 allowFeedback: true});
  assert.equal(nodes.requests.children.length, 1);
  const card = nodes.requests.children[0];
  assert.equal(card.children[0].textContent, 'Run rm -rf /tmp/x?');
  const sentBefore = first.sent.length;
  card.children[1].children[1].dispatch('click'); // Deny
  await waitFor(() => first.sent.length === sentBefore + 1, 'ui-response');
  assert.deepEqual(await unseal(key, first.sent[sentBefore]),
                   {t: 'ui-response', reqId: 41, option: 1});
  const feedbackRow = card.children[2];
  feedbackRow.children[0].value = 'do a dry run first';
  feedbackRow.children[1].dispatch('click');
  await waitFor(() => first.sent.length === sentBefore + 2, 'feedback');
  assert.deepEqual(await unseal(key, first.sent[sentBefore + 1]),
                   {t: 'ui-response', reqId: 41,
                    feedback: 'do a dry run first'});
  await deliver({t: 'ui-request-end', reqId: 41});
  assert.equal(nodes.requests.children.length, 0);

  // Unknown future frames are tolerated.
  await deliver({t: 'future-frame', payload: 1});

  // A drop schedules a retry and the fresh socket re-hellos.
  first.dispatch('close', {code: 1006});
  assert.equal(typeof timer, 'function');
  timer();
  await tick();
  assert.equal(sockets.length, 2);
  sockets[1].dispatch('open');
  await waitFor(() => sockets[1].sent.length === 1, 'second hello');
  assert.equal((await unseal(key, sockets[1].sent[0])).t, 'hello');

  // Bye ends the session: no reconnect, composer gone.
  timer = null;
  sockets[1].dispatch('message', {data: await seal(key, 1, {t: 'bye', reason: 'user-stop'})});
  await waitFor(() => nodes.connection.textContent === 'Session ended', 'bye');
  assert.equal(nodes.connection.textContent, 'Session ended');
  assert.equal(nodes.composer.hidden, true);
  sockets[1].dispatch('close', {code: 4001});
  assert.equal(timer, null);

  console.log('viewer protocol passed');
}

main().catch(error => {
  console.error(error);
  process.exit(1);
});
