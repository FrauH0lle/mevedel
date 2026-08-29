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
    this.open = false;
    this.attributes = {};
    this.scrollHeight = 100;
  }
  append(...children) {
    for (const child of children) {
      if (typeof child !== 'string') child.parent = this;
    }
    this.children.push(...children);
  }
  replaceChildren(...children) { this.children = children; }
  remove() {
    if (this.parent) this.parent.children = this.parent.children.filter(x => x !== this);
  }
  replaceWith(other) {
    if (!this.parent) return;
    const index = this.parent.children.indexOf(this);
    other.parent = this.parent;
    if (index >= 0) this.parent.children[index] = other;
  }
  setAttribute(name, value) { this.attributes[name] = value; }
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

// A File stand-in: the viewer only reads name, type, size, and bytes.
function fakeFile(name, type, text) {
  const bytes = Buffer.from(text);
  return {
    name, type, size: bytes.length,
    arrayBuffer: async () => bytes,
  };
}

// textContent in the real DOM concatenates descendants; the fake keeps
// strings and elements as children, so flatten recursively.
function textOf(node) {
  if (typeof node === 'string') return node;
  return (node.textContent || '') + node.children.map(textOf).join('');
}

function findByRecordId(root, id) {
  return root.children.find(c => typeof c !== 'string'
                            && c.dataset && c.dataset.recordId === id);
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
  // Shared known-answer vector, mirrored by the ERT crypto suite: 32
  // zero-byte key, 12 zero-byte nonce, 16 zero-byte plaintext must seal to
  // this exact ciphertext||tag, pinning WebCrypto and gnutls to each other.
  const katKey = await crypto.subtle.importKey(
    'raw', new Uint8Array(32), 'AES-GCM', false, ['encrypt']);
  const katOut = new Uint8Array(await crypto.subtle.encrypt(
    {name: 'AES-GCM', iv: new Uint8Array(12)}, katKey, new Uint8Array(16)));
  assert.equal(Buffer.from(katOut).toString('hex'),
               'cea7403d4d606b6e074ec5d3baf39d18'
               + 'd0d1c8a799996bf0265b98b5d48ab919');

  const keyBytes = crypto.getRandomValues(new Uint8Array(32));
  const writeToken = crypto.getRandomValues(new Uint8Array(16));
  const key = await crypto.subtle.importKey('raw', keyBytes, 'AES-GCM', false,
                                            ['encrypt', 'decrypt']);
  const roomId = 'roomroomroomroom';
  const fullSecret = base64url(Buffer.concat([Buffer.from(keyBytes),
                                              Buffer.from(writeToken)]));

  const ids = ['transcript', 'connection', 'notice', 'live-button',
               'composer', 'composer-input', 'composer-name',
               'send-button', 'stop-button', 'filter', 'requests',
               'session-label', 'queue-state', 'attachments',
               'attach-button', 'image-input', 'notify-button',
               'composer-scope', 'own-queue', 'skill-chips'];
  const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
  nodes.composer.hidden = true;
  nodes.filter.hidden = true;
  nodes['notify-button'].hidden = true;
  nodes['own-queue'].hidden = true;
  const sockets = [];
  let timer;
  const storage = new Map();
  // Web Notification stand-in: permission flows and constructed
  // notifications are observable, nothing is displayed.
  const shownNotifications = [];
  class FakeNotification {
    constructor(title, options = {}) {
      this.title = title;
      this.body = options.body || '';
      shownNotifications.push(this);
    }
    static permission = 'default';
    static async requestPermission() {
      FakeNotification.permission = 'granted';
      return 'granted';
    }
  }
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
    hidden: false,
  };
  class TestWebSocket extends Socket {
    constructor(url) { super(); this.url = url; sockets.push(this); }
  }
  const context = {
    document, window, WebSocket: TestWebSocket, URL, console,
    crypto, TextEncoder, TextDecoder, atob, btoa, Date,
    Event: FakeEvent, Notification: FakeNotification,
    localStorage: {
      getItem: k => (storage.has(k) ? storage.get(k) : null),
      setItem: (k, v) => storage.set(k, v),
      removeItem: k => storage.delete(k),
    },
    setTimeout: window.setTimeout,
    clearTimeout: () => {},
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
  // The stable per-browser guest id rides every hello, so the host can
  // match this guest's own queued entries across reconnects.
  assert.match(hello.guestId, /^[A-Za-z0-9_-]{8,64}$/);
  assert.equal(storage.get('mevedel-guest-id'), hello.guestId);

  let delivered = 0;
  const deliver = async frame => {
    first.dispatch('message', {data: await seal(key, 1, frame)});
    delivered++;
    // Wait until the viewer's serialized inbound chain has consumed it.
    await waitFor(() => context.window.mevedelViewerApplied === delivered,
                  'frame application');
  };

  // Welcome for a writable guest reveals the composer; the snapshot loads
  // through final-flagged chunks with live updates queued behind it.
  await deliver({t: 'welcome', proto: 2, readOnly: false, recordCount: 3,
                 skills: ['plan', 'review']});
  assert.equal(nodes.composer.hidden, false);
  // The host-curated skill roster renders as tappable chips.
  assert.equal(nodes['skill-chips'].hidden, false);
  assert.equal(nodes['skill-chips'].children.length, 2);
  assert.match(textOf(nodes['skill-chips'].children[0]), /plan/);
  await deliver({t: 'snapshot-chunk', final: false, records: [
    {id: 'assistant', kind: 'assistant', revision: 0,
     text: 'Some **bold** and `inline` text.\n\n```elisp\n(defun demo ()\n  "doc")\n```'},
    {id: 'tool', kind: 'tool', revision: 0, name: 'Bash', status: 'completed',
     summary: 'Bash', detail: 'head -5 notes.txt', result: 'large',
     truncated: true},
    {id: 'patch', kind: 'tool', revision: 0, name: 'ApplyPatch',
     status: 'completed', summary: 'ApplyPatch', detail: 'parser.el',
     result: 'Applied patch: 1 changes',
     diff: '@@ -1 +1 @@\n-old\n+new'},
  ]});
  await deliver({t: 'record', record: {id: 'assistant', kind: 'assistant',
                                       revision: 1, text: 'stream replacement'}});
  await deliver({t: 'snapshot-chunk', final: true, records: [
    {id: 'guest-user', kind: 'user', revision: 0, text: 'from the phone',
     guest: 'roland'},
  ]});
  assert.equal(nodes.transcript.children.length, 4);

  // The filter strip is visible from connection on, even before any
  // directive record exists.
  assert.equal(nodes.filter.hidden, false);
  assert.match(textOf(nodes.filter.children[0]), /^All/);

  // ApplyPatch tool row: the authored patch renders as a diff pane and the
  // result summary stays a plain line.
  const patchTurn = findByRecordId(nodes.transcript, 'patch');
  const patchFlat = JSON.stringify(patchTurn,
                                   (k, v) => (k === 'parent' ? undefined : v));
  assert.match(patchFlat, /line add/);
  assert.match(patchFlat, /line del/);
  assert.match(textOf(patchTurn), /Applied patch: 1 changes/);

  // Assistant record: live update replaced the markdown body.
  const assistantTurn = findByRecordId(nodes.transcript, 'assistant');
  assert.match(textOf(assistantTurn), /stream replacement/);

  // Tool row: detail line, truncation note, status chip.
  const toolTurn = findByRecordId(nodes.transcript, 'tool');
  assert.match(textOf(toolTurn), /head -5 notes\.txt/);
  assert.match(textOf(toolTurn), /truncated/);
  assert.match(textOf(toolTurn), /completed/);

  // Guest badge renders on the attributed prompt only.
  const guestTurn = findByRecordId(nodes.transcript, 'guest-user');
  assert.equal(guestTurn.dataset.role, 'guest');
  assert.match(textOf(guestTurn), /roland/);
  assert.match(textOf(guestTurn), /guest/);

  // Markdown + fontification: bold, inline code, and elisp keyword spans.
  await deliver({t: 'record', record: {
    id: 'assistant', kind: 'assistant', revision: 2,
    text: 'Some **bold** and `inline` text.\n\n```elisp\n(defun demo ()\n  "doc")\n```',
  }});
  const md = findByRecordId(nodes.transcript, 'assistant');
  const flat = JSON.stringify(md, (k, v) => (k === 'parent' ? undefined : v));
  assert.match(flat, /"tagName":"strong"/);
  assert.match(flat, /"tagName":"code"/);
  assert.match(flat, /"className":"tok-kw"/);
  assert.match(flat, /"className":"tok-str"/);

  // Live removal.
  await deliver({t: 'remove', ids: ['tool', 'patch']});
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
  // The host's queued acknowledgement surfaces as a notice, and names
  // the sender's place in line when the host knows it.
  await deliver({t: 'queued'});
  assert.match(textOf(nodes.notice), /queued/i);
  await deliver({t: 'queued', position: 2});
  assert.match(textOf(nodes.notice), /#2/);

  // Queue state tells a guest their prompt is still waiting; zero hides
  // the line, and a host-side pause is called out.
  assert.equal(nodes['queue-state'].hidden, true);
  await deliver({t: 'queue', pending: 2, paused: false});
  assert.equal(nodes['queue-state'].hidden, false);
  assert.match(textOf(nodes['queue-state']), /2 follow-ups waiting/);
  await deliver({t: 'queue', pending: 1, paused: true});
  assert.match(textOf(nodes['queue-state']), /1 follow-up waiting/);
  assert.match(textOf(nodes['queue-state']), /paused/);
  await deliver({t: 'queue', pending: 0, paused: false});
  assert.equal(nodes['queue-state'].hidden, true);
  nodes['stop-button'].dispatch('click');
  await waitFor(() => first.sent.length === 3, 'sealed abort');
  assert.deepEqual(await unseal(key, first.sent[2]), {t: 'abort'});

  // Directive-tagged records grow the client-side filter; selecting a
  // directive hides everything outside it, per guest, no round-trips.
  await deliver({t: 'record', record: {
    id: 'directive-user', kind: 'user', revision: 0,
    text: 'Refactor the parser\nwith details', directive: 'dir-1',
  }});
  assert.equal(nodes.filter.hidden, false);
  const labels = nodes.filter.children.map(textOf);
  assert.equal(labels.length, 3);
  assert.match(labels[0], /^All/);
  assert.match(labels[1], /^Main chat/);
  assert.match(labels[2], /Refactor the parser/);
  nodes.filter.children[2].dispatch('click');
  assert.equal(findByRecordId(nodes.transcript, 'assistant').hidden, true);
  assert.equal(findByRecordId(nodes.transcript, 'directive-user').hidden, false);
  nodes.filter.children[1].dispatch('click'); // Main chat
  assert.equal(findByRecordId(nodes.transcript, 'assistant').hidden, false);
  assert.equal(findByRecordId(nodes.transcript, 'directive-user').hidden, true);
  // A prompt sent under a directive filter carries that directive, so
  // the reply lands in the thread the guest is reading; main chat and
  // All carry none. The composer says where the prompt will go.
  nodes.filter.children[2].dispatch('click');
  assert.match(nodes['composer-input'].placeholder, /Refactor the parser/);
  // The composer scope pill names the directive the prompt will land in
  // and disappears for unscoped filters.
  assert.equal(nodes['composer-scope'].hidden, false);
  assert.match(textOf(nodes['composer-scope']), /Refactor the parser/);
  nodes['composer-input'].value = 'and this one?';
  nodes.composer.dispatch('submit');
  await waitFor(() => first.sent.length === 4, 'directive-scoped prompt');
  assert.equal((await unseal(key, first.sent[3])).directive, 'dir-1');
  nodes.filter.children[1].dispatch('click'); // Main chat
  assert.equal(nodes['composer-scope'].hidden, true);
  nodes['composer-input'].value = 'main chat';
  nodes.composer.dispatch('submit');
  await waitFor(() => first.sent.length === 5, 'unscoped prompt');
  assert.equal((await unseal(key, first.sent[4])).directive, undefined);

  // Activity in a thread the guest is not looking at marks its tab with
  // an unseen dot until the tab is selected.
  const dirTab = () => nodes.filter.children.find(
    b => /Refactor/.test(textOf(b)));
  await deliver({t: 'record', record: {
    id: 'directive-user', kind: 'user', revision: 1,
    text: 'Refactor the parser\nwith more details', directive: 'dir-1',
  }});
  assert.match(dirTab().className, /unseen/);
  dirTab().dispatch('click');
  assert.doesNotMatch(dirTab().className, /unseen/);
  nodes.filter.children[0].dispatch('click'); // All

  // Attachments are not only photos: an allowlisted text type rides the
  // frame verbatim, a disallowed one never leaves the browser, and the
  // extension decides when the browser reports no type at all.
  const api2 = context.window.mevedelViewer;
  await api2.addFiles([fakeFile('build.log', '', 'log line\n'),
                       fakeFile('notes.exe', 'application/x-msdownload', 'x')]);
  assert.equal(nodes.attachments.children.length, 1);
  assert.match(textOf(nodes.attachments), /build\.log/);
  nodes['composer-input'].value = 'see the log';
  nodes.composer.dispatch('submit');
  await waitFor(() => first.sent.length === 6, 'prompt with attachment');
  const withFile = await unseal(key, first.sent[5]);
  assert.equal(withFile.images.length, 1);
  assert.equal(withFile.images[0].mime, 'text/plain');
  assert.equal(Buffer.from(withFile.images[0].data, 'base64').toString(),
               'log line\n');
  assert.equal(nodes.attachments.children.length, 0);

  // A guest's own queued entries render as a persistent card with live
  // position and a retract control; a frame without them clears it.
  await deliver({t: 'queue', pending: 2, paused: false,
                 own: [{id: 7, position: 2, text: 'my queued question'}]});
  assert.equal(nodes['own-queue'].hidden, false);
  assert.match(textOf(nodes['own-queue']), /my queued question/);
  assert.match(textOf(nodes['own-queue']), /#2/);
  const ownButtons = [];
  (function collectOwn(node) {
    if (typeof node === 'string') return;
    if (node.tagName === 'button') ownButtons.push(node);
    node.children.forEach(collectOwn);
  })(nodes['own-queue']);
  const retractBefore = first.sent.length;
  ownButtons[0].dispatch('click');
  await waitFor(() => first.sent.length === retractBefore + 1, 'retract');
  assert.deepEqual(await unseal(key, first.sent[retractBefore]),
                   {t: 'retract', id: 7});
  await deliver({t: 'queue', pending: 1, paused: false});
  assert.equal(nodes['own-queue'].hidden, true);

  // Tapping a skill chip sends the typed skill frame; nothing is ever
  // parsed out of composer text.
  const skillBefore = first.sent.length;
  nodes['skill-chips'].children[0].dispatch('click');
  await waitFor(() => first.sent.length === skillBefore + 1, 'skill frame');
  assert.deepEqual(await unseal(key, first.sent[skillBefore]),
                   {t: 'skill', name: 'plan'});

  // Notifications are opt-in through the bell and fire only while the
  // tab is hidden: turn settlement (busy true -> false) and interaction
  // arrival notify; nothing else does.
  assert.equal(nodes['notify-button'].hidden, false);
  nodes['notify-button'].dispatch('click');
  await waitFor(() => FakeNotification.permission === 'granted',
                'notification permission');
  // Opting into notifications is the install use-case, so the share
  // credentials persist for the installed app to relaunch with.
  await waitFor(() => storage.has('mevedel-last-share'), 'persisted share');
  assert.equal(storage.get('mevedel-last-share'), `${roomId}.${fullSecret}`);
  await deliver({t: 'status', busy: true});
  assert.equal(shownNotifications.length, 0);
  document.hidden = true;
  await deliver({t: 'status', busy: false});
  assert.equal(shownNotifications.length, 1);
  assert.match(shownNotifications[0].title, /finished/i);
  // An unchanged busy state does not re-notify.
  await deliver({t: 'status', busy: false});
  assert.equal(shownNotifications.length, 1);

  // A ui-request renders a card whose buttons and feedback field answer
  // through sealed ui-response frames; a diff body gets diff rendering;
  // ui-request-end dismisses the card.
  await deliver({t: 'ui-request', reqId: 41, body: 'Run rm -rf /tmp/x?',
                 bodyKind: 'text',
                 options: [{id: 0, label: 'Allow once'}, {id: 1, label: 'Deny'}],
                 allowFeedback: true});
  assert.equal(nodes.requests.children.length, 1);
  // The hidden tab is told an interaction arrived.
  assert.equal(shownNotifications.length, 2);
  assert.match(shownNotifications[1].title, /interaction/i);
  // The host re-sends the same request on every head redraw and on
  // every re-hello; one interaction is one notification.
  await deliver({t: 'ui-request', reqId: 41, body: 'Run rm -rf /tmp/x?',
                 bodyKind: 'text',
                 options: [{id: 0, label: 'Allow once'}, {id: 1, label: 'Deny'}],
                 allowFeedback: true});
  assert.equal(shownNotifications.length, 2);
  document.hidden = false;
  const card = nodes.requests.children[0];
  assert.match(textOf(card), /Run rm -rf \/tmp\/x\?/);
  const controls = card.children.find(c => c.className === 'request-controls');
  const sentBefore = first.sent.length;
  controls.children[1].dispatch('click'); // Deny
  await waitFor(() => first.sent.length === sentBefore + 1, 'ui-response');
  assert.deepEqual(await unseal(key, first.sent[sentBefore]),
                   {t: 'ui-response', reqId: 41, option: 1});
  const feedbackRow = card.children[card.children.length - 1];
  feedbackRow.children[0].value = 'do a dry run first';
  feedbackRow.children[1].dispatch('click');
  await waitFor(() => first.sent.length === sentBefore + 2, 'feedback');
  assert.deepEqual(await unseal(key, first.sent[sentBefore + 1]),
                   {t: 'ui-response', reqId: 41,
                    feedback: 'do a dry run first'});
  await deliver({t: 'ui-request', reqId: 42,
                 body: '@@ -1,2 +1,2 @@\n-old line\n+new line',
                 bodyKind: 'diff', options: [{id: 0, label: 'Apply patch'}],
                 allowFeedback: true});
  const diffCard = nodes.requests.children.find(
    c => c.dataset.reqId === '42');
  const diffFlat = JSON.stringify(diffCard,
                                  (k, v) => (k === 'parent' ? undefined : v));
  assert.match(diffFlat, /line add/);
  assert.match(diffFlat, /line del/);
  // A questionnaire renders per-question option buttons plus a custom
  // field, requires every answer, and submits atomically.
  await deliver({t: 'ui-request', reqId: 43, body: 'Ask · 2 questions',
                 bodyKind: 'text', options: [], allowFeedback: false,
                 questions: [
                   {question: 'Which approach?',
                    options: [{label: 'MVP first (Recommended)'},
                              {label: 'Risk first', description: 'slower'}]},
                   {question: 'Which branch?', options: [{label: 'main'}],
                    answer: 'main'},
                 ]});
  const askCard = nodes.requests.children.find(
    c => c.dataset.reqId === '43');
  const askButtons = [];
  (function collect(node) {
    if (typeof node === 'string') return;
    if (node.tagName === 'button') askButtons.push(node);
    node.children.forEach(collect);
  })(askCard);
  const submitButton = askButtons[askButtons.length - 1];
  assert.equal(textOf(submitButton), 'Submit answers');
  const askBefore = first.sent.length;
  // Question 1 unanswered: submit refuses with a notice.
  submitButton.dispatch('click');
  await tick();
  assert.equal(first.sent.length, askBefore);
  assert.match(textOf(nodes.notice), /Answer every question/);
  // Answer question 1 by option, keep question 2's prefilled answer.
  askButtons[0].dispatch('click');
  submitButton.dispatch('click');
  await waitFor(() => first.sent.length === askBefore + 1, 'ask answers');
  assert.deepEqual(await unseal(key, first.sent[askBefore]),
                   {t: 'ui-response', reqId: 43,
                    answers: ['MVP first (Recommended)', 'main']});

  // A questionnaire that offers cancellation gets a Dismiss control,
  // which settles only the questionnaire host-side.
  await deliver({t: 'ui-request', reqId: 44, body: 'Ask · 1 question',
                 bodyKind: 'text', options: [], allowFeedback: false,
                 allowCancel: true,
                 questions: [{question: 'Keep going?',
                              options: [{label: 'Yes'}]}]});
  const cancelCard = nodes.requests.children.find(
    c => c.dataset.reqId === '44');
  const cancelButtons = [];
  (function collectCancel(node) {
    if (typeof node === 'string') return;
    if (node.tagName === 'button') cancelButtons.push(node);
    node.children.forEach(collectCancel);
  })(cancelCard);
  const dismiss = cancelButtons.find(b => /dismiss/i.test(textOf(b)));
  assert.ok(dismiss, 'questionnaire has a Dismiss control');
  const dismissBefore = first.sent.length;
  dismiss.dispatch('click');
  await waitFor(() => first.sent.length === dismissBefore + 1, 'dismiss');
  assert.deepEqual(await unseal(key, first.sent[dismissBefore]),
                   {t: 'ui-response', reqId: 44, cancel: true});

  await deliver({t: 'ui-request-end', reqId: 41});
  await deliver({t: 'ui-request-end', reqId: 42});
  await deliver({t: 'ui-request-end', reqId: 43});
  await deliver({t: 'ui-request-end', reqId: 44});
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
  await waitFor(() => textOf(nodes.connection).includes('Session ended'), 'bye');
  assert.equal(nodes.composer.hidden, true);
  // A dead room's persisted credentials die with it.
  assert.equal(storage.has('mevedel-last-share'), false);
  sockets[1].dispatch('close', {code: 4001});
  assert.equal(timer, null);

  console.log('viewer protocol passed');
}

main().catch(error => {
  console.error(error);
  process.exit(1);
});
