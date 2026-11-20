/* The way back: a lobby, the rooms it opens, and their panels, through the
 * header and through browser history.
 *
 * The real viewer runs with every script. Each room's relay socket is
 * answered by a stand-in host that seals frames with the room's key, so
 * links, reloads and history behave as they do against a relay.
 *
 * Run: node --test test/collaboration-viewer-navigation.browser.mjs
 * Requires `npm ci --prefix shared-editing` and Playwright Chromium.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdir, readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';
import {webcrypto as crypto} from 'node:crypto';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');

const TYPES = {html: 'text/html', css: 'text/css', js: 'text/javascript'};
// Visual evidence is opt-in: MEVEDEL_CAPTURE_SCREENSHOTS=DIR saves PNGs there.
const CAPTURE_DIR = process.env.MEVEDEL_CAPTURE_SCREENSHOTS;
async function capture(page, name) {
  if (!CAPTURE_DIR) return;
  await mkdir(resolve(CAPTURE_DIR), {recursive: true});
  await page.screenshot({path: resolve(CAPTURE_DIR, `${name}.png`)});
}

function base64url(bytes) {
  return Buffer.from(bytes).toString('base64')
    .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

// A room's owner secret: key, write token, owner token.
function makeRoom(id, seed) {
  const secret = new Uint8Array(64).map((_, index) => (seed * 31 + index) % 256);
  return {id, secret, link: `http://localhost:8765/#${id}.${base64url(secret)}`};
}

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

const lobbyRoom = makeRoom('lobbylobbylobby1', 1);
const sessionRoom = makeRoom('sessionsession01', 2);
const forkRoom = makeRoom('forkforkforkfork', 3);
const PAGE_HTML = '<!doctype html><h1 id="inside">Inside the artifact</h1>';
const SECONDARY_TEXT = 'Secondary artifact, not the primary page.';
const AGENT_ARTIFACT_TEXT = 'Artifact resolved through the retained agent transcript.';
const ARCHIVED_AGENT_ARTIFACT_TEXT = 'Archived agent artifact from segment 3.';
const FILE_TEXT = 'The project file reached the new tab.';

// What the stand-in host answers in each room.
function host(lobbyRunning, records, sharedItem, coldAgent, archives, agentRecords) {
  const store = {t: 'store-artifacts', artifacts: [
    {id: 'page', title: 'Page', kind: 'html', artifact: 'page/index.html',
     versions: 1, modified: 1, size: PAGE_HTML.length, attached: true},
    {id: 'board', title: 'Board', kind: 'whiteboard', artifact: 'board/board.excalidraw',
     versions: 1, modified: 2, size: 10, item: true}]};
  if (sharedItem) store.artifacts.push({id: sharedItem.id, title: sharedItem.title,
    kind: sharedItem.kind, artifact: `${sharedItem.id}/document.json`,
    versions: 1, modified: 3, item: true, attached: true});
  function content(frame, reply, guest) {
    if (frame.t === 'store-list') reply(store);
    else if (frame.t === 'store-action' && frame.action === 'versions') {
      reply({t: 'store-action', reqId: frame.reqId, ok: true,
             versions: [{n: 1, time: 1}]});
    } else if (frame.t === 'artifact-get') {
      assert.ok(['artifact:page', 'tool:page', 'tool:notes', 'agent-artifact-notes',
                 'agent-history-3:agent-artifact-notes'].includes(frame.id),
                `unexpected artifact identity: ${frame.id}`);
      const agent = frame.id.includes('agent-artifact-');
      if (agent && !guest.agentArtifacts.has(frame.id)) {
        reply({t: 'artifact', reqId: frame.reqId, error: 'Fetch the owning agent transcript first.'});
        return;
      }
      const secondary = frame.id === 'tool:notes';
      const archived = frame.id.startsWith('agent-history-3:');
      const text = archived ? ARCHIVED_AGENT_ARTIFACT_TEXT
        : agent ? AGENT_ARTIFACT_TEXT : secondary ? SECONDARY_TEXT : PAGE_HTML;
      reply({t: 'artifact', reqId: frame.reqId,
             name: agent ? 'agent-notes.md' : secondary ? 'page/notes.md' : 'page/index.html',
             mime: secondary || agent ? 'text/markdown' : 'text/html', size: text.length,
             data: Buffer.from(text).toString('base64'), final: true});
    } else if (frame.t === 'files') {
      reply({t: 'files', reqId: frame.reqId, dir: '',
             entries: [{name: 'notes & plans.txt', kind: 'file', size: FILE_TEXT.length}]});
    } else if (frame.t === 'history-get' && archives?.[frame.segment]) {
      reply({t: 'history', reqId: frame.reqId, segment: frame.segment,
             records: archives[frame.segment], final: true});
    } else if (frame.t === 'file-get') {
      reply({t: 'file', reqId: frame.reqId, mime: 'text/plain', size: FILE_TEXT.length,
             data: Buffer.from(FILE_TEXT).toString('base64'), final: true});
    } else if (frame.t === 'fetch-agent') {
      assert.equal(frame.path, '/root/reviewer');
      assert.ok(frame.segment === undefined || frame.segment === 3);
      // A cold live host starts hydration but intentionally sends no reply.
      // Each new peer must poll again; neither another tab nor reload warms it.
      if (coldAgent && frame.segment === undefined && ++guest.agentFetches === 1) return;
      const artifactId = frame.segment === 3 ? 'agent-history-3:agent-artifact-notes' : 'agent-artifact-notes';
      // The real host replaces this guest's cache on each transcript fetch.
      // A different tab or a reconnect must acquire its own authorization.
      guest.agentArtifacts = new Set([artifactId]);
      reply({t: 'agent', reqId: frame.reqId, final: true,
             records: agentRecords || [{id: 'agent-answer', kind: 'assistant', text: 'The retained agent transcript.'},
                       {id: artifactId, kind: 'tool', artifact: 'agent-notes.md', title: 'Agent notes'}]});
    } else if (frame.t === 'editing' && sharedItem) {
      // These small read-only fixture requests fit a single editing frame.
      assert.equal(frame.offset, 0);
      assert.equal(frame.total, frame.data.length);
      const args = JSON.parse(Buffer.from(frame.data, 'base64').toString());
      const {id, kind, title, revision} = sharedItem;
      const result = args.action === 'list' ? [{id, kind, title, revision}]
        : args.action === 'status' ? {available: true}
        : args.action === 'read' && args.id === sharedItem.id ? sharedItem : undefined;
      assert.notEqual(result, undefined, `unexpected shared fixture action: ${JSON.stringify(args)}`);
      const data = Buffer.from(JSON.stringify({result})).toString('base64');
      reply({t: 'editing', reqId: frame.reqId, offset: 0, total: data.length, data});
    }
  }
  return {
    [lobbyRoom.id]: (frame, reply, guest) => {
      if (frame.t === 'hello' || frame.t === 'lobby-refresh') {
        reply({t: 'lobby', project: 'proj', workspace: 'ws', omitted: 0, sessions: [
          {id: 's1', name: 'Feature work', updated: 1, preview: 'Hello',
           live: true, shared: true}]});
      } else if (frame.t === 'open-session') {
        reply({t: 'open-session', reqId: frame.reqId, ok: true, link: sessionRoom.link});
      } else if (frame.t === 'store-action' && frame.action === 'conversation') {
        // A whiteboard's conversation link names the item its editor opens.
        const hash = sessionRoom.link.indexOf('#');
        reply({t: 'store-action', reqId: frame.reqId, ok: true,
               link: `${sessionRoom.link.slice(0, hash)}?shared=${frame.id}${sessionRoom.link.slice(hash)}`});
      } else content(frame, reply, guest);
    },
    [sessionRoom.id]: (frame, reply, guest) => {
      if (frame.t === 'hello') {
        reply({t: 'welcome', proto: 5, readOnly: !frame.writeToken, workspace: 'ws',
               project: 'proj', lobby: lobbyRunning, recordCount: records.length});
        reply({t: 'snapshot-chunk', records, final: true});
        reply({t: 'status', name: 'Feature work', busy: false,
               turnCount: records.filter(record => record.kind === 'user').length});
        if (frame.ownerToken) reply({t: 'recovery', models: [], model: null,
                                    efforts: [], effort: null, presets: [], providers: []});
        if (sharedItem) reply(store);
      } else if (frame.t === 'rewind' || frame.t === 'retry-turn') {
        reply({t: 'history-action', reqId: frame.reqId, ok: true});
      } else content(frame, reply, guest);
    },
    [forkRoom.id]: (frame, reply) => {
      if (frame.t === 'hello') {
        reply({t: 'welcome', proto: 5, readOnly: false, workspace: 'ws',
               project: 'proj', lobby: lobbyRunning, recordCount: 0});
        reply({t: 'snapshot-chunk', records: [], final: true});
        reply({t: 'status', name: 'Implement plan', busy: false});
      }
    },
  };
}

async function open(browser, {lobbyRunning = true, records = [], sharedItem = null, coldAgent = false,
                              archives = null, agentRecords = null, viewport = {width: 1200, height: 800},
                              touch = false} = {}) {
  const context = await browser.newContext({viewport, hasTouch: touch});
  const page = await context.newPage();
  const answers = host(lobbyRunning, records, sharedItem, coldAgent, archives, agentRecords);
  const keys = {};
  for (const room of [lobbyRoom, sessionRoom, forkRoom]) {
    keys[room.id] = await crypto.subtle.importKey(
      'raw', room.secret.slice(0, 32), 'AES-GCM', false, ['encrypt', 'decrypt']);
  }
  page.joined = [];
  page.received = [];
  page.requests = [];
  page.network = new Map();
  page.routes = new Map();
  context.on('request', request => {
    page.requests.push(request.url());
    page.network.set(request, 'requested');
  });
  context.on('requestfinished', request => page.network.set(request, 'finished'));
  context.on('requestfailed', request => page.network.set(request, request.failure()?.errorText));
  // Frames the host sends unprompted, by room.
  page.push = {};
  // New tabs start independently: page-only routing misses their first request.
  await context.routeWebSocket(/\/r\//, socket => {
    const id = new URL(socket.url()).pathname.split('/').pop();
    const connection = page.joined.length;
    const guest = {agentArtifacts: new Set(), agentFetches: 0};
    page.joined.push(id);
    // Crypto completion order is not wire order; match the real transport's queues.
    let inbound = Promise.resolve(), outbound = Promise.resolve();
    const reply = frame => {
      outbound = outbound.then(async () => socket.send(await seal(keys[id], frame)));
      return outbound;
    };
    page.push[id] = reply;
    // Bind the connection as the host does; unbound frames are dropped.
    let last = 0;
    reply({t: 'challenge', challenge: 'c'});
    socket.onMessage(message => {
      if (typeof message === 'string') return;
      inbound = inbound.then(async () => {
        const {challenge, seq, nonce: _nonce, ...frame} = await unseal(keys[id], message);
        if (challenge !== 'c' || !(seq > last)) return;
        last = seq;
        page.received.push({room: id, connection, frame});
        page.emit('host-frame', frame);
        answers[id]?.(frame, reply, guest);
      });
    });
  });
  await context.route('http://localhost:8765/**', async route => {
    page.routes.set(route.request(), 'handling');
    let path = new URL(route.request().url()).pathname;
    if (path === '/') path = '/index.html';
    const type = TYPES[path.split('.').pop()];
    await route.fulfill(type
      ? {body: await readFile(resolve(root, 'relay/viewer', `.${path}`)), contentType: type}
      : {status: 204, body: ''});
    page.routes.set(route.request(), 'fulfilled');
  });
  page.urls = [];
  page.on('framenavigated', frame => {
    if (frame === page.mainFrame()) page.urls.push(frame.url());
  });
  return page;
}

const title = page => page.locator('#session-label').textContent();
const visible = (page, selector) => page.locator(selector).isVisible();

const gestures = [
  {name: 'middle-click', options: {button: 'middle'}},
  {name: 'modified click', options: {modifiers: [process.platform === 'darwin' ? 'Meta' : 'Control']}},
];

function tierLink(room, length) {
  return `http://localhost:8765/#${room.id}.${base64url(room.secret.slice(0, length))}`;
}

async function sourceState(page) {
  return page.evaluate(() => {
    const composer = document.getElementById('composer-input');
    return {
      url: location.href, history: history.state, length: history.length,
      title: document.getElementById('session-label').textContent,
      composer: {value: composer.value, start: composer.selectionStart,
                 end: composer.selectionEnd, hidden: composer.closest('form').hidden},
      panels: [...document.querySelectorAll('.viewer-panel')].map(node => [node.id, node.hidden]),
      tabs: [...document.querySelectorAll('[role="tab"]')].map(node => [node.id, node.getAttribute('aria-selected')]),
      store: document.getElementById('store-sheet').open,
    };
  });
}

async function draft(page) {
  const composer = page.locator('#composer-input');
  if (!await composer.isVisible()) return;
  await composer.fill('Keep this unsent draft, including its selection.');
  await composer.evaluate(node => node.setSelectionRange(5, 21));
}

async function liveRoom(browser, viewport) {
  const page = await open(browser, {viewport, records: [
    {id: 'question', kind: 'user', text: 'Earlier conversation.\n\n'.repeat(80)},
    {id: 'answer', kind: 'assistant', text: 'The response starts here.'},
  ]});
  await page.goto(sessionRoom.link);
  await page.locator('#transcript [data-record-id="answer"]').waitFor();
  await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
  await page.waitForFunction(() => document.getElementById('live-button').hidden);
  return page;
}

const atLiveEdge = () => document.documentElement.scrollHeight - scrollY - innerHeight < 2;

test('workspace notification bell belongs to full and owner lobby links only', async () => {
  const browser = await chromium.launch();
  try {
    for (const length of [32, 48, 64]) {
      const page = await open(browser);
      const link = tierLink(lobbyRoom, length);
      await page.context().addInitScript(viewOnly => {
        window.shown = [];
        window.Notification = class {
          static permission = 'granted';
          static async requestPermission() { return 'granted'; }
          constructor(title) { window.shown.push(title); }
        };
        if (viewOnly) localStorage.setItem(`mevedel-notify:${location.hash.slice(1)}`, 'on');
        Object.defineProperty(document, 'hidden', {value: true});
        let subscription;
        const registration = {unregister: async () => true, pushManager: {
          getSubscription: async () => subscription,
          subscribe: async options => (subscription = {options, endpoint: 'https://push.example/phone',
            unsubscribe: async () => { window.unsubscribed = true; subscription = null; }}),
        }};
        Object.defineProperty(navigator, 'serviceWorker', {value: {
          register: async () => registration, getRegistration: async () => registration,
        }});
      }, length === 32);
      await page.context().route('**/push-key', route => route.fulfill({json: {key: base64url(Buffer.alloc(65))}}));
      await page.goto(link);
      await page.locator('#lobby').waitFor();
      const bell = page.locator('#notify-button');
      assert.equal(await bell.isVisible(), length !== 32);
      if (length === 32) {
        await page.push[lobbyRoom.id]({t: 'notice', kind: 'failure'});
        await page.push[lobbyRoom.id]({t: 'notice', message: 'delivered'});
        await page.getByText('delivered', {exact: true}).waitFor();
        assert.deepEqual(await page.evaluate(() => window.shown), []);
        assert.equal(await page.evaluate(() => localStorage.getItem('mevedel-last-share')), null);
        assert.equal(page.received.some(({frame}) => frame.t === 'push-subscribe'), false);
      } else {
        assert.match(await bell.getAttribute('aria-label'), /workspace/);
        const next = type => page.waitForEvent('host-frame', {predicate: frame => frame.t === type, timeout: 5000});
        const subscribed = next('push-subscribe');
        await bell.click();
        assert.deepEqual(await subscribed, {t: 'push-subscribe', endpoint: 'https://push.example/phone', active: false});
        await page.push[lobbyRoom.id]({t: 'notice', kind: 'push-refused', message: 'Workspace push registration refused.'});
        await page.getByText('Workspace push registration refused.', {exact: true}).waitFor();
        assert.match(await bell.getAttribute('aria-label'), /while this tab is away/);
        assert.equal(await bell.getAttribute('aria-pressed'), 'true');
        await page.push[lobbyRoom.id]({t: 'notice', kind: 'goal'});
        await page.waitForFunction(() => window.shown.includes('New mevedel activity'));
        const reopened = await page.context().newPage();
        const restored = next('push-subscribe');
        await reopened.goto('http://localhost:8765/');
        await restored;
        await reopened.getByRole('button', {name: /background delivery enabled/}).waitFor();
        assert.equal(await reopened.locator('#notify-button').getAttribute('aria-pressed'), 'true');
        if (length === 48) {
          // A launch with no URL fragment reaches the relay while the host is down.
          const outage = await page.context().newPage();
          await outage.routeWebSocket(/\/r\//, socket => socket.close({code: 4004}));
          await outage.goto('http://localhost:8765/');
          await outage.locator('#terminal-state').waitFor();
          assert.equal(await outage.locator('#terminal-title').textContent(), 'Lobby unavailable');
          assert.equal(await outage.evaluate(() => localStorage.getItem('mevedel-last-share')), new URL(link).hash.slice(1));
        }
        const unsubscribed = next('push-unsubscribe');
        await reopened.locator('#notify-button').click();
        await unsubscribed;
        assert.equal(await reopened.evaluate(() => window.unsubscribed), true);
        assert.equal(await reopened.evaluate(() => localStorage.getItem('mevedel-last-share')), null);
        assert.equal(await reopened.evaluate(fragment => localStorage.getItem(`mevedel-notify:${fragment}`),
                                             new URL(link).hash.slice(1)), null);
        await reopened.evaluate(() => { delete window.PushManager; });
        await reopened.locator('#notify-button').click();
        await page.push[lobbyRoom.id]({t: 'notice', kind: 'goal'});
        await page.push[lobbyRoom.id]({t: 'bye', reason: 'rotated'});
        await reopened.locator('#terminal-state').waitFor();
        assert.deepEqual(await reopened.evaluate(() => window.shown), ['New mevedel activity']);
        assert.equal(await reopened.locator('#notify-button').isVisible(), false);
        assert.equal(await reopened.evaluate(() => localStorage.getItem('mevedel-last-share')), null);
      }
      await page.context().close();
    }
  } finally { await browser.close(); }
});

test('hidden viewer without Push only notifies for explicit host notices', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser);
    await page.addInitScript(() => {
      delete window.PushManager;
      Object.defineProperty(document, 'hidden', {value: true, configurable: true});
      localStorage.setItem(`mevedel-notify:${location.hash.slice(1)}`, 'on');
      window.shown = [];
      window.Notification = class {
        static permission = 'granted';
        constructor(title, {body}) { window.shown.push({title, body}); }
      };
    });
    await page.goto(sessionRoom.link);
    await page.locator('#composer-input').waitFor();
    const send = page.push[sessionRoom.id];
    await send({t: 'status', busy: true});
    await send({t: 'status', busy: false, outcome: 'error', name: 'Settled'});
    await page.waitForFunction(() => document.querySelector('#session-label').textContent === 'Settled');
    assert.deepEqual(await page.evaluate(() => window.shown), []);
    assert.doesNotMatch(await page.title(), /^●/);
    const shownAfter = async label => {
      // A message-only notice is an in-page receipt after all preceding sealed frames.
      await send({t: 'notice', message: label});
      await page.waitForFunction(text => document.querySelector('#notice').textContent === text, label);
      return page.evaluate(() => window.shown.splice(0));
    };
    for (const outcome of ['error', 'aborted', 'lost', 'success']) {
      await send({t: 'notice', kind: 'completion', outcome});
    }
    const idle = 'The mevedel session is idle again.';
    assert.deepEqual(await shownAfter('Completion notices received'), [
      {title: 'Turn failed', body: 'Open the session for the failure and recovery actions.'},
      {title: 'Turn interrupted', body: idle},
      {title: 'Turn interrupted', body: idle},
      {title: 'Turn finished', body: idle},
    ]);
    for (const kind of ['notify', 'goal', 'failure', 'other']) {
      await send({t: 'notice', kind, message: 'Private transcript details'});
    }
    const generic = {title: 'New mevedel activity', body: 'Open MevView to see the encrypted session update.'};
    assert.deepEqual(await shownAfter('Attention notices received'), [generic, generic, generic]);
    assert.equal(await page.evaluate(() => {
      delete document.hidden;
      return !document.hidden && document.hasFocus();
    }), true);
    await send({t: 'notice', kind: 'completion'});
    await send({t: 'notice', kind: 'notify'});
    assert.deepEqual(await shownAfter('Focused notices received'), []);
  } finally { await browser.close(); }
});

test('room composer keeps Enter for newlines and submits with Control or Command Enter', async () => {
  const browser = await chromium.launch();
  try {
    const page = await liveRoom(browser, {width: 1200, height: 800});
    const input = page.locator('#composer-input');
    assert.match(await page.locator('#send-button').getAttribute('title'), /Ctrl\+Enter|⌘\+Enter/);
    for (const modifier of ['Control', 'Meta']) {
      const count = page.received.filter(({frame}) => frame.t === 'prompt').length;
      await input.fill('First line');
      await input.press('Enter');
      assert.equal(await input.inputValue(), 'First line\n');
      assert.equal(page.received.filter(({frame}) => frame.t === 'prompt').length, count);
      await input.type('Second line');
      await input.press(`${modifier}+Enter`);
      await page.waitForFunction(() => document.getElementById('composer-input').value === '');
      assert.equal(page.received.filter(({frame}) => frame.t === 'prompt').length, count + 1);
      assert.equal(page.received.filter(({frame}) => frame.t === 'prompt').at(-1).frame.text,
        'First line\nSecond line');
    }
    await input.press('Control+Enter');
    assert.equal(page.received.filter(({frame}) => frame.t === 'prompt').length, 2);
  } finally { await browser.close(); }
});

test('history actions share one owner-only row and fit desktop and narrow phone widths', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 320, height: 568}]) {
      for (const tier of [32, 48, 64]) {
        const page = await open(browser, {viewport, records: [
          {id: 'prompt', kind: 'user', text: 'Please revise the result.'},
          {id: 'answer', kind: 'assistant', text: 'A settled answer.', forkPoint: 'fp-1', forkTurn: 1},
          {id: 'follow-up', kind: 'user', text: 'Please continue.'},
          {id: 'failed', kind: 'assistant', text: 'Assistant request failed.', status: 'failed', retryId: 'request-1'},
        ]});
        const link = `http://localhost:8765/#${sessionRoom.id}.${base64url(sessionRoom.secret.slice(0, tier))}`;
        await page.goto(link);
        await page.locator('[data-record-id="failed"]').waitFor();
        const answer = page.locator('[data-record-id="answer"]');
        const rewind = answer.getByRole('button', {name: 'Rewind to here', exact: true});
        const retry = page.getByRole('button', {name: 'Retry', exact: true});
        assert.equal(await rewind.isVisible(), tier === 64);
        assert.equal(await retry.isVisible(), tier === 64);
        assert.equal(await answer.getByRole('button', {name: 'Copy message'}).isVisible(), true);
        if (tier === 64) {
          await answer.hover();
          assert.equal(await answer.locator('.message-actions').count(), 1);
          assert.equal(await answer.locator('.message-actions .forkbtn').count(), 1);
          for (const button of await answer.locator('.message-action').all()) {
            const box = await button.boundingBox();
            assert.ok(box && box.x >= 0 && box.x + box.width <= viewport.width,
                      `message action fits ${viewport.width}px: ${JSON.stringify(box)}`);
          }
          await rewind.click();
          const confirmation = page.getByRole('dialog', {name: 'Rewind to here?'});
          await confirmation.waitFor();
          assert.match(await confirmation.textContent(),
                       /Remove 1 later turn\? Files stay unchanged; undo with Redo in Emacs\./);
          await capture(page, `history-rewind-confirm-${viewport.width}`);
          await confirmation.getByRole('button', {name: 'Cancel', exact: true}).click();
          assert.equal(page.received.some(({frame}) => frame.t === 'rewind'), false);
          await rewind.click();
          const rewound = page.waitForEvent('host-frame', {predicate: frame => frame.t === 'rewind'});
          await confirmation.getByRole('button', {name: 'Rewind', exact: true}).click();
          assert.deepEqual(await rewound, {t: 'rewind', reqId: 1, forkPoint: 'fp-1', turnCount: 2});
          await page.waitForFunction(() => !document.querySelector('[data-message-action="rewind"]').disabled);
          const retried = page.waitForEvent('host-frame', {predicate: frame => frame.t === 'retry-turn'});
          await retry.click();
          assert.deepEqual(await retried, {t: 'retry-turn', reqId: 2, failureId: 'request-1'});
          await page.waitForFunction(() => !document.querySelector('[data-message-action="retry-turn"]').disabled);
          await answer.getByRole('button', {name: 'Copy message'}).focus();
          await retry.hover();
          await capture(page, `history-actions-${viewport.width}`);
        }
        await page.context().close();
      }
    }
  } finally {
    await browser.close();
  }
});

test('multiline inputs expand reversibly and message actions copy source with a plain-text fallback', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1440, height: 900}, {width: 375, height: 667}]) {
      for (const theme of ['light', 'dark']) {
        const page = await open(browser, {viewport, touch: viewport.width < 640, records: [
          {id: 'question', kind: 'user', text: 'Review **this change**.\nKeep the draft intact.'},
          {id: 'answer', kind: 'assistant', text: '## Review\n\nThe shared helper preserves **Markdown**.\nSecond line.'},
        ]});
        await page.goto(sessionRoom.link);
        await page.locator('#transcript [data-record-id="answer"]').waitFor();
        await page.evaluate(theme => { document.documentElement.dataset.theme = theme; }, theme);
        const suffix = `${viewport.width}x${viewport.height}-${theme}`;
        const input = page.locator('#composer-input');
        const toggle = page.locator('#composer .input-expand');
        await input.fill('Please review this implementation.\nKeep this second line while expanding.');
        const collapsed = (await input.boundingBox()).height;
        assert.equal(await toggle.getAttribute('aria-label'), 'Expand');
        assert.equal(await toggle.getAttribute('aria-pressed'), 'false');
        assert.equal(await toggle.evaluate(node => getComputedStyle(node).backgroundColor), 'rgba(0, 0, 0, 0)',
          'the composer toggle must not paint over the textarea focus ring');
        const toggleBox = await toggle.boundingBox();
        if (viewport.width > 640) {
          const label = await page.locator('.composer-label').boundingBox();
          assert.ok(label.width > 1, 'the composer label stays visible');
          assert.ok(Math.abs(toggleBox.y + toggleBox.height / 2 - label.y - label.height / 2) < 3,
            'the toggle aligns with the visible label, not the placeholder');
          await page.evaluate(() => {
            const scope = document.getElementById('composer-scope');
            scope.textContent = 'Selected context'; scope.hidden = false;
          });
          const scopedLabel = await page.locator('.composer-label').boundingBox();
          const scopedToggle = await toggle.boundingBox();
          assert.ok(Math.abs(scopedToggle.y + scopedToggle.height / 2 - scopedLabel.y - scopedLabel.height / 2) < 3,
            'context above the input does not separate the toggle from its label');
          await page.evaluate(() => {
            const scope = document.getElementById('composer-scope');
            scope.textContent = ''; scope.hidden = true;
          });
        } else assert.ok(Math.abs(toggleBox.y - (await input.boundingBox()).y) < 1);
        await capture(page, `composer-collapsed-${suffix}`);
        await toggle.click();
        assert.equal(await toggle.getAttribute('aria-label'), 'Collapse');
        assert.equal(await toggle.getAttribute('aria-pressed'), 'true');
        await page.waitForTimeout(250);
        const expanded = await input.boundingBox();
        assert.ok(expanded.height > viewport.height * .5);
        assert.ok(expanded.height <= viewport.height * .75 + 1);
        assert.ok(expanded.height > collapsed);
        assert.equal(await input.inputValue(), 'Please review this implementation.\nKeep this second line while expanding.');
        const send = await page.locator('#send-button').boundingBox();
        assert.ok(send.y + send.height <= viewport.height, `Send must fit at ${suffix}`);
        const header = await page.locator('body > .topbar').boundingBox();
        assert.ok(Math.abs(header.y) <= 1, `Header must stay at viewport top at ${suffix}: ${JSON.stringify(header)}`);
        const dock = await page.locator('.dock').boundingBox();
        assert.ok(dock.y >= header.y + header.height + 7, `Dock must stay below header at ${suffix}`);
        await capture(page, `composer-expanded-${suffix}`);
        if (viewport.width < 640) {
          await page.evaluate(() => {
            Object.defineProperty(visualViewport, 'height', {configurable: true, value: 420});
            visualViewport.dispatchEvent(new Event('resize'));
          });
          await page.waitForFunction(() => document.querySelector('#composer-input').getBoundingClientRect().height < 300);
          const keyboardSend = await page.locator('#send-button').boundingBox();
          assert.ok(keyboardSend.y + keyboardSend.height <= 420, 'Send stays above a visual-viewport-only keyboard');
          await page.evaluate(() => { delete visualViewport.height; visualViewport.dispatchEvent(new Event('resize')); });
          await page.waitForFunction(() => document.querySelector('#composer-input').getBoundingClientRect().height > 350);
        }
        await toggle.click();
        assert.equal((await input.boundingBox()).height, collapsed);
        await toggle.click();
        await input.press('Control+Enter');
        await page.waitForFunction(() => document.querySelector('#composer-input').value === '');
        assert.equal(await toggle.getAttribute('aria-pressed'), 'false');
        if (!await page.locator('#new-session-button').isVisible()) await page.locator('#room-button').click();
        await page.locator('#new-session-button').click();
        const prompt = page.locator('#new-session-prompt');
        const promptToggle = page.locator('#new-session .input-expand');
        await prompt.fill('A separate task');
        assert.ok(Math.abs((await promptToggle.boundingBox()).y - (await prompt.boundingBox()).y) < 1);
        await promptToggle.click();
        assert.equal(await promptToggle.getAttribute('aria-pressed'), 'true');
        assert.ok((await prompt.boundingBox()).height > viewport.height * .65);
        await page.locator('#new-session-create').click();
        await page.waitForFunction(() => document.querySelector('#new-session-prompt').value === '');
        assert.equal(await promptToggle.getAttribute('aria-pressed'), 'false');
        if (viewport.width < 640 && await page.locator('#session-box').evaluate(node => node.open)) {
          await page.locator('#session-box > summary').click();
        }

        await page.evaluate(() => {
          window.copied = [];
          Object.defineProperty(navigator, 'clipboard', {configurable: true,
            value: {writeText: async text => window.copied.push(text)}});
        });
        for (const id of ['question', 'answer']) {
          const turn = page.locator(`#transcript [data-record-id="${id}"]`);
          await turn.hover();
          const copy = turn.getByRole('button', {name: 'Copy message', exact: true});
          await copy.focus();
          assert.equal(await copy.locator('..').locator('..').evaluate(node => getComputedStyle(node).opacity), '1');
          await capture(page, `message-copy-${id}-${suffix}`);
          await copy.click();
          await page.waitForFunction(id => document.querySelector(
            `[data-record-id="${id}"] [data-message-action="copy"]`).dataset.label === 'Copied', id);
        }
        assert.deepEqual(await page.evaluate(() => window.copied), [
          'Review **this change**.\nKeep the draft intact.',
          '## Review\n\nThe shared helper preserves **Markdown**.\nSecond line.',
        ]);
        await page.evaluate(() => {
          Object.defineProperty(navigator, 'clipboard', {configurable: true, value: undefined});
          document.execCommand = command => {
            if (command === 'copy') window.fallbackCopy = document.activeElement.value;
            return true;
          };
          const turn = window.mevedelTranscriptRenderer.renderRecord({id: 'rendered-only', kind: 'assistant'});
          turn.querySelector('.prose').innerHTML = '<p>First paragraph</p><p>Second<br>line</p>';
          document.getElementById('transcript').append(turn);
        });
        const fallback = page.locator('[data-record-id="rendered-only"] [data-message-action="copy"]');
        await fallback.focus();
        await fallback.click();
        await page.waitForFunction(() => window.fallbackCopy);
        assert.equal(await page.evaluate(() => window.fallbackCopy), 'First paragraph\n\nSecond\nline');
        assert.equal(await fallback.evaluate(node => document.activeElement === node), true);
        assert.equal(await page.locator('body > textarea').count(), 0, 'fallback textarea is removed');
        assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
        await page.evaluate(() => {
          document.getElementById('artifact-panel').hidden = false;
          document.getElementById('artifact-side').hidden = false;
          document.getElementById('artifact-title').textContent = 'Review notes';
          document.getElementById('artifact-body').innerHTML = '<article class="prose"><h1>Review notes</h1><p>A shared document for the implementation review.</p></article>';
          const drafts = new Map();
          window.discussionView = window.mevedelDiscussionSidebar.create({
            root: document.getElementById('artifact-discussion-sidebar'), noun: 'artifact',
            canWrite: () => true, draft: {get: key => drafts.get(key) || {},
              set: (key, value) => drafts.set(key, {...drafts.get(key), ...value})},
            submit: async () => true,
          });
          discussionView.update({discussions: [{id: 'review', title: 'Implementation review', messages: []}]});
          discussionView.show({kind: 'thread', id: 'review'});
        });
        const discussionInput = page.locator('#artifact-side .ds-input');
        const discussionToggle = page.locator('#artifact-side .input-expand');
        await discussionInput.fill('Please keep this discussion draft while expanding.');
        assert.ok(Math.abs((await discussionToggle.boundingBox()).y - (await discussionInput.boundingBox()).y) < 1);
        await discussionToggle.click();
        assert.equal(await discussionToggle.getAttribute('aria-pressed'), 'true');
        assert.ok((await discussionInput.boundingBox()).height > viewport.height * .5);
        const reply = await page.locator('#artifact-side .ds-send').boundingBox();
        assert.ok(reply.y + reply.height <= viewport.height, `Discussion Reply must fit at ${suffix}`);
        await capture(page, `discussion-expanded-${suffix}`);
        await discussionInput.press('Control+Enter');
        await page.waitForFunction(() => document.querySelector('#artifact-side .ds-input').value === '');
        assert.equal(await discussionToggle.getAttribute('aria-pressed'), 'false');
        await page.context().close();
      }
    }
  } finally { await browser.close(); }
});

test('Plan feedback supports multiline expansion, refresh continuity and shortcut sending', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1440, height: 900}, {width: 375, height: 667}]) {
      const page = await open(browser, {viewport, records: [
        {id: 'plan', kind: 'assistant', text: 'The implementation plan is ready for review.'},
      ]});
      await page.goto(sessionRoom.link);
      await page.locator('#transcript [data-record-id="plan"]').waitFor();
      await page.evaluate(() => { document.documentElement.dataset.theme = 'light'; });
      const frame = {t: 'ui-request', reqId: 99, body: 'Review the implementation plan.', allowFeedback: true};
      await page.push[sessionRoom.id](frame);
      await page.getByText('Suggest a change', {exact: true}).click();
      const input = page.getByRole('textbox', {name: 'Feedback', exact: true});
      assert.equal(await input.evaluate(node => node.tagName), 'TEXTAREA');
      await input.fill('Keep the existing API.');
      await input.press('Enter');
      await input.pressSequentially('Add the regression first.');
      const text = 'Keep the existing API.\nAdd the regression first.';
      assert.equal(await input.inputValue(), text);
      const collapsed = (await input.boundingBox()).height;
      const toggle = page.locator('.decision-feedback .input-expand');
      assert.ok(Math.abs((await toggle.boundingBox()).y - (await input.boundingBox()).y) < 1);
      await toggle.click();
      await page.waitForTimeout(250);
      assert.equal(await toggle.getAttribute('aria-pressed'), 'true');
      assert.ok((await input.boundingBox()).height > collapsed + 30);
      await input.evaluate(node => node.setSelectionRange(5, 12));
      await page.push[sessionRoom.id]({...frame, body: 'Updated plan, same feedback request.'});
      await page.waitForFunction(() => document.querySelector('.request-body').textContent.startsWith('Updated'));
      assert.equal(await input.inputValue(), text);
      assert.equal(await toggle.getAttribute('aria-pressed'), 'true');
      assert.deepEqual(await input.evaluate(node => [document.activeElement === node, node.selectionStart, node.selectionEnd]), [true, 5, 12]);
      await capture(page, `plan-feedback-expanded-${viewport.width}x${viewport.height}-light`);
      await input.press('Control+Enter');
      await page.waitForFunction(() => document.querySelector('.decision-feedback .input-expand').getAttribute('aria-pressed') === 'false');
      assert.equal(page.received.filter(({frame}) => frame.t === 'ui-response').at(-1).frame.feedback, text);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await toggle.click();
      await page.push[sessionRoom.id]({t: 'ui-request-end', reqId: 99});
      await page.waitForFunction(() => !document.querySelector('.decision-feedback'));
      assert.equal(await page.locator('.dock').evaluate(node => node.classList.contains('input-expanded')), false);
      await page.context().close();
    }
  } finally { await browser.close(); }
});

test('live arrow floats over the transcript without reserving a bar', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 375, height: 667}]) {
      const page = await liveRoom(browser, viewport);
      await page.mouse.wheel(0, -500);
      const button = page.getByRole('button', {name: 'Jump to latest', exact: true});
      await button.waitFor();
      const geometry = await button.evaluate(node => {
        const box = node.getBoundingClientRect();
        const dock = document.querySelector('.dock').getBoundingClientRect();
        const height = document.documentElement.scrollHeight;
        node.hidden = true;
        const hiddenHeight = document.documentElement.scrollHeight;
        node.hidden = false;
        const outside = document.elementFromPoint(box.left - 15, box.top + box.height / 2);
        return {width: box.width, height: box.height, bottom: box.bottom, dockTop: dock.top,
          pageHeight: height, hiddenHeight, outsideDock: !outside?.closest('.dock')};
      });
      assert.equal(geometry.width, 44);
      assert.equal(geometry.height, 44);
      assert.ok(geometry.bottom <= geometry.dockTop);
      assert.equal(geometry.pageHeight, geometry.hiddenHeight);
      assert.equal(geometry.outsideDock, true);
      await button.focus();
      await page.keyboard.press('Enter');
      await page.waitForFunction(() => document.getElementById('live-button').hidden
        && !document.body.hasAttribute('data-reading'));
      await page.waitForFunction(atLiveEdge);
      await page.push[sessionRoom.id]({t: 'record', record: {
        id: 'answer', kind: 'assistant', text: 'Following after the arrow.\n\n'.repeat(40)}});
      await page.waitForFunction(() => document.querySelector('[data-record-id="answer"]')
        .textContent.includes('Following after the arrow.'));
      await page.waitForFunction(atLiveEdge);
      await page.close();
    }
  } finally { await browser.close(); }
});

const paragraphs = (text, count = 40) => `${text}\n\n`.repeat(count);
const EXCHANGES = [
  {id: 'p1', kind: 'user', text: 'First prompt: outline the plan.'},
  {id: 'a1', kind: 'assistant', text: paragraphs('First answer paragraph.')},
  {id: 'p2', kind: 'user', text: 'Second prompt: refactor the **parser**\n\nand keep every '
    + 'test green, including the long names that truncate on a narrow phone.'},
  {id: 'a2', kind: 'assistant', text: paragraphs('Second answer paragraph.')},
  {id: 'p3', kind: 'user', text: 'Third prompt.'},
  {id: 'a3', kind: 'assistant', text: paragraphs('Third answer paragraph.')},
];

// Scroll so the top of the record ID sits OFFSET pixels below the sticky
// header's bottom edge (negative: above it).
function scrollRecord(page, id, offset) {
  return page.evaluate(([id, offset]) => {
    const edge = document.querySelector('body > .topbar').getBoundingClientRect().bottom;
    const top = document.querySelector(`[data-record-id="${id}"]`).getBoundingClientRect().top;
    window.scrollBy(0, top - edge - offset);
    return window.scrollY;
  }, [id, offset]);
}

// Wait until the pinned row settles on TEXT, or on nothing when TEXT is null.
function pinned(page, text) {
  return page.waitForFunction(text => {
    const pin = document.getElementById('pinned-prompt');
    // What a reader sees: the row's computed visibility and rendered label.
    const visible = getComputedStyle(pin).visibility === 'visible';
    const rendered = getComputedStyle(pin, '::before').content.replace(/^"|"$/g, '');
    return text === null ? !visible : visible && rendered.startsWith(text);
  }, text).catch(async error => {
    throw new Error(`Expected pin ${JSON.stringify(text)}; observed ${JSON.stringify(await page.evaluate(() => ({
      label: document.getElementById('pinned-prompt').dataset.label,
      shown: document.getElementById('pinned-prompt').hasAttribute('data-shown'), scrollY,
      ledgers: [...document.querySelectorAll('.history-ledger, #transcript')].map(node => ({
        id: node.id, top: node.getBoundingClientRect().top, bottom: node.getBoundingClientRect().bottom,
        open: node.closest('details')?.open})),
      prompts: [...document.querySelectorAll('[data-role="you"], [data-role="guest"]')].map(node => ({
        id: node.dataset.recordId, top: node.getBoundingClientRect().top,
        live: Boolean(node.closest('.history-chat[data-live]'))})),
    })))}`, {cause: error});
  });
}

function pinGeometry(page) {
  return page.evaluate(() => {
    const pin = document.getElementById('pinned-prompt');
    const box = pin.getBoundingClientRect();
    const style = getComputedStyle(pin);
    return {top: box.top, bottom: box.bottom, left: box.left, right: box.right,
            height: box.height, fontSize: parseFloat(style.fontSize),
            ellipsis: getComputedStyle(pin, '::before').textOverflow === 'ellipsis',
            headerBottom: document.querySelector('body > .topbar').getBoundingClientRect().bottom,
            dockTop: document.querySelector('.dock').getBoundingClientRect().top,
            column: document.getElementById('composer').getBoundingClientRect(),
            overflow: document.documentElement.scrollWidth > innerWidth,
            scrollY, scrollHeight: document.documentElement.scrollHeight};
  });
}

test('pinned prompt follows the exchange at the top without moving the transcript', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 375, height: 629},
                            {width: 390, height: 844}]) {
      const size = `${viewport.width}x${viewport.height}`;
      const page = await open(browser, {viewport, records: EXCHANGES});
      await page.goto(sessionRoom.link);
      await page.locator('#transcript [data-record-id="a3"]').waitFor();

      // Inside the second answer, its prompt has scrolled away.
      const scrolled = await scrollRecord(page, 'a2', -200);
      await pinned(page, 'Second prompt: refactor the parser and keep every test green');
      await capture(page, `pinned-prompt-${size}`);
      const shown = await pinGeometry(page);
      assert.equal(shown.scrollY, scrolled, `${size}: showing the row must not scroll`);
      assert.ok(Math.abs(shown.top - shown.headerBottom - 8) <= 1, `${size}: ${JSON.stringify(shown)}`);
      assert.ok(shown.height <= 40 && shown.bottom < shown.dockTop, `${size}: one row above the dock`);
      assert.ok(Math.abs(shown.left - shown.column.left) <= 1 && Math.abs(shown.right - shown.column.right) <= 1,
                `${size}: the row aligns with both composer edges`);
      assert.ok(shown.fontSize >= 12, `${size}: readable text`);
      assert.equal(shown.overflow, false, `${size}: no horizontal overflow`);
      assert.equal(shown.ellipsis, true, `${size}: long prompts truncate`);
      // An overlay, not a reserved row: hiding it moves nothing.
      const hiddenHeight = await page.evaluate(() => {
        const pin = document.getElementById('pinned-prompt');
        pin.hidden = true;
        const height = document.documentElement.scrollHeight;
        pin.hidden = false;
        return height;
      });
      assert.equal(hiddenHeight, shown.scrollHeight);

      // A host update can replace the governing prompt without resizing
      // anything; the row follows the new element and its text.
      const replaced = EXCHANGES[2].text.replace('**parser**', '**lexer!**');
      await page.push[sessionRoom.id]({t: 'record', record: {...EXCHANGES[2], text: replaced}});
      await page.waitForFunction(() => document.querySelector('[data-record-id="p2"]')
        .textContent.includes('lexer!'));
      assert.equal(await page.evaluate(() => document.documentElement.scrollHeight), shown.scrollHeight,
                   `${size}: the replacement keeps the transcript's size`);
      await pinned(page, 'Second prompt: refactor the lexer! and keep');
      await page.locator('#pinned-prompt').click();
      await page.waitForFunction(() => {
        const edge = document.querySelector('body > .topbar').getBoundingClientRect().bottom;
        const top = document.querySelector('[data-record-id="p2"]').getBoundingClientRect().top;
        return top >= edge && top <= edge + 24;
      });
      await pinned(page, null);

      // A visible later prompt does not replace the exchange at the top.
      await scrollRecord(page, 'p3', 150);
      await page.waitForTimeout(100);
      await pinned(page, 'Second prompt');
      await scrollRecord(page, 'a3', -100);
      await pinned(page, 'Third prompt.');

      // The preview leads back to its prompt, which then needs no pin.
      await page.locator('#pinned-prompt').click();
      await page.waitForFunction(() => {
        const edge = document.querySelector('body > .topbar').getBoundingClientRect().bottom;
        const top = document.querySelector('[data-record-id="p3"]').getBoundingClientRect().top;
        return top >= edge && top <= edge + 24;
      });
      await pinned(page, null);
      await capture(page, `pinned-prompt-clicked-${size}`);
      // At the start no prompt has scrolled away.
      await page.evaluate(() => window.scrollTo(0, 0));
      await pinned(page, null);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('pinned cards preserve words and speaker identity in rooms and agent panels', async () => {
  const browser = await chromium.launch();
  const prompt = 'Review the migration: removed\ncountry must remain separate, while coun**try** stays joined.\n\n'
    + '- Keep existing records\n- Preserve the audit trail\n\n'
    + 'Explain the validation, rollout and rollback steps before changing the storage format. '.repeat(8);
  const prefix = 'Review the migration: removed country must remain separate, while country stays joined. '
    + 'Keep existing records Preserve the audit trail Explain the validation';
  const review = [
    '## Migration review',
    'The schema change is small, but the rollout must preserve the audit trail and keep existing clients readable.',
    '### Validate before writing',
    '- Check a restored production-shaped snapshot, not the live database.\n'
      + '- Compare record counts and stable identifiers before and after migration.\n'
      + '- Keep the original country field until every reader uses the replacement.',
    'The important boundary is between an absent value and a value deliberately removed by the user. '
      + 'The migration should preserve that distinction rather than replacing both with an empty string.',
    '```sql\nBEGIN;\nALTER TABLE audit_records ADD COLUMN country_code TEXT;\n'
      + 'UPDATE audit_records\nSET country_code = country\nWHERE country IS NOT NULL;\n'
      + '-- Verify counts before committing the transaction.\nCOMMIT;\n```',
    '### Roll out in two steps',
    'Deploy readers that accept the new field first. Keep writes reversible while the old and new versions coexist.',
    '| Stage | Evidence |\n| --- | --- |\n| Dry run | Counts and identifiers match |\n'
      + '| Canary | Audit entries remain readable |\n| Rollout | No legacy readers remain |',
    '### Keep rollback explicit',
    'Retain the source column and the migration report until the canary has completed. '
      + 'A rollback should switch readers back without reconstructing deleted data.',
    '### Verification',
    'Exercise records with missing values, removed values, non-ASCII names and older audit entries. '
      + 'The final check reads the migrated records through the application, not just through SQL.',
  ].join('\n\n');
  const records = [
    {id: 'task', kind: 'user', text: prompt},
    {id: 'work', kind: 'assistant', text: review},
    {id: 'followup', kind: 'user', guest: 'Mara', text: prompt},
    {id: 'result', kind: 'assistant', text: review},
  ];
  try {
    for (const viewport of [{width: 1440, height: 900}, {width: 375, height: 667}]) {
      for (const theme of ['light', 'dark']) {
        const page = await open(browser, {viewport, records, agentRecords: records});
        const errors = [];
        page.on('pageerror', error => errors.push(error.message));
        await page.emulateMedia({colorScheme: theme});
        await page.goto(sessionRoom.link);
        await page.locator('#transcript [data-record-id="result"]').waitFor();
        const watchPins = () => page.evaluate(() => {
          window.pinMutations = [];
          document.querySelectorAll('.pinned-prompt').forEach(pin => {
            new MutationObserver(changes => window.pinMutations.push(...changes.map(change =>
              [change.type, change.attributeName]))).observe(pin,
                {subtree: true, attributes: true, childList: true, characterData: true});
          });
        });
        const checkMutations = async () => assert.deepEqual(await page.evaluate(() =>
          window.pinMutations.filter(([type, name]) => type !== 'attributes' || name === 'hidden')), []);
        await watchPins();
        await scrollRecord(page, 'work', -180);
        await pinned(page, prefix);
        await page.waitForFunction(() => document.getElementById('pinned-prompt').dataset.role === 'you');
        assert.equal(await page.locator('#pinned-prompt').getAttribute('data-role'), 'you');
        assert.equal(await page.locator('#pinned-prompt .pinned-speaker').getAttribute('data-speaker'), 'Host');
        const geometry = await pinGeometry(page);
        assert.ok(Math.abs(geometry.left - geometry.column.left) <= 1);
        assert.ok(Math.abs(geometry.right - geometry.column.right) <= 1);
        await capture(page, `pinned-room-${viewport.width}x${viewport.height}-${theme}`);
        await checkMutations();

        await page.goto(`${sessionRoom.link}?agent=${encodeURIComponent('/root/reviewer')}`);
        await page.locator('#agent-transcript [data-record-id="result"]').waitFor();
        await watchPins();
        // Agent scrolling must leave the underlying room at its reading position.
        const roomY = await page.evaluate(() => scrollY);
        async function agentScroll(id) {
          await page.evaluate(id => {
            const transcript = document.getElementById('agent-transcript');
            const edge = document.querySelector('#agent-panel > .topbar').getBoundingClientRect().bottom;
            const turn = transcript.querySelector(`[data-record-id="${id}"]`);
            transcript.scrollBy(0, turn.getBoundingClientRect().top - edge + 180);
          }, id);
          await page.waitForFunction(([prefix, role]) => {
            const pin = document.getElementById('agent-pinned-prompt');
            return pin.hasAttribute('data-shown') && pin.dataset.label.startsWith(prefix) && pin.dataset.role === role;
          }, [prefix, id === 'work' ? 'you' : 'guest']);
        }
        await agentScroll('work');
        assert.equal(await page.locator('#agent-pinned-prompt').getAttribute('data-role'), 'you');
        await agentScroll('result');
        const pin = page.locator('#agent-pinned-prompt');
        assert.equal(await pin.getAttribute('data-role'), 'guest');
        assert.equal(await pin.locator('.pinned-speaker').getAttribute('data-speaker'), 'Mara');
        assert.match(await pin.getAttribute('aria-label'), /Mara's prompt: Review the migration/);
        const box = await pin.boundingBox();
        const header = await page.locator('#agent-panel > .topbar').boundingBox();
        const content = await page.locator('#agent-transcript [data-record-id="followup"] > .content').boundingBox();
        assert.ok(Math.abs(box.y - header.y - header.height - 8) <= 1);
        assert.ok(Math.abs(box.x - content.x) <= 1 && Math.abs(box.width - content.width) <= 1);
        assert.ok(box.x >= 0 && box.x + box.width <= viewport.width);
        const band = await pin.evaluate(pin => {
          const band = pin.nextElementSibling;
          const rect = band.getBoundingClientRect();
          return {top: rect.top, bottom: rect.bottom, events: getComputedStyle(band).pointerEvents};
        });
        assert.equal(band.events, 'none');
        assert.ok(Math.abs(band.top - header.y - header.height) <= 1);
        assert.ok(Math.abs(band.bottom - box.y - box.height - 12) <= 1);
        assert.equal(await page.evaluate(() => scrollY), roomY);
        await capture(page, `pinned-agent-${viewport.width}x${viewport.height}-${theme}`);
        await pin.click();
        await page.waitForFunction(() => {
          const edge = document.querySelector('#agent-panel > .topbar').getBoundingClientRect().bottom;
          const top = document.querySelector('#agent-transcript [data-record-id="followup"]').getBoundingClientRect().top;
          return top >= edge && top <= edge + 24;
        });
        assert.equal(await page.evaluate(() => scrollY), roomY);
        await checkMutations();
        assert.deepEqual(errors, []);
        await page.close();
      }
    }
  } finally { await browser.close(); }
});

test('pinned prompt continues across compaction but not across /clear', async () => {
  const browser = await chromium.launch();
  const records = [
    {id: 'cont', kind: 'assistant', text: paragraphs('Continuing after compaction.')},
    {id: 'p-live', kind: 'user', text: 'Live prompt.'},
    {id: 'a-live', kind: 'assistant', text: paragraphs('Live answer.')},
  ];
  const archives = {1: [{id: 'old-p', kind: 'user', text: 'Archived prompt before compaction.'},
                        {id: 'old-a', kind: 'assistant', text: 'Archived answer.'}]};
  const compacted = [{kind: 'chat', number: 1, title: 'Work', firstSegment: 1, lastSegment: 2}];
  const cleared = [{kind: 'chat', number: 1, title: 'Old', firstSegment: 1, lastSegment: 1},
                   {kind: 'chat', number: 2, title: 'New', firstSegment: 2, lastSegment: 2}];
  try {
    for (const chats of [compacted, cleared]) {
      const page = await open(browser, {viewport: {width: 375, height: 629}, records, archives});
      await page.goto(sessionRoom.link);
      await page.locator('#transcript [data-record-id="a-live"]').waitFor();
      await page.push[sessionRoom.id]({t: 'history-index', currentSegment: 2,
                                       records: chats, final: true});
      const segment = page.locator('#history .history-segment');
      await segment.locator('summary').click();
      await page.locator('[data-record-id="old-p"]').waitFor();
      await segment.locator('.history-collapse').click();
      await scrollRecord(page, 'cont', -150);
      if (chats === cleared) {
        await page.waitForTimeout(100);
        await pinned(page, null);
        await page.close();
        continue;
      }
      await pinned(page, 'Archived prompt before compaction.');
      await capture(page, 'pinned-prompt-compaction-375x629');
      // A live prompt on screen takes precedence over the archived one.
      await scrollRecord(page, 'p-live', 200);
      await pinned(page, null);
      await scrollRecord(page, 'cont', -150);
      await pinned(page, 'Archived prompt before compaction.');
      await page.locator('#pinned-prompt').click();
      assert.equal(await segment.evaluate(node => node.open), true);
      await page.waitForFunction(() => {
        const edge = document.querySelector('body > .topbar').getBoundingClientRect().bottom;
        const top = document.querySelector('[data-record-id="old-p"]').getBoundingClientRect().top;
        return top >= edge && top <= edge + 24;
      });
      await page.close();
    }
  } finally { await browser.close(); }
});

test('pinned prompt picks up an archived segment that loads while collapsed', async () => {
  const browser = await chromium.launch();
  try {
    // No archives: the host leaves the read pending until the test answers.
    const page = await open(browser, {viewport: {width: 375, height: 629}, archives: {}, records: [
      {id: 'cont', kind: 'assistant', text: paragraphs('Continuing after compaction.')},
      {id: 'p-live', kind: 'user', text: 'Live prompt.'}]});
    await page.goto(sessionRoom.link);
    await page.locator('#transcript [data-record-id="p-live"]').waitFor();
    await page.push[sessionRoom.id]({t: 'history-index', currentSegment: 2, final: true,
      records: [{kind: 'chat', number: 1, title: 'Work', firstSegment: 1, lastSegment: 2}]});
    const segment = page.locator('#history .history-segment');
    await segment.locator('summary').click();
    await segment.locator('.history-collapse').click();
    await scrollRecord(page, 'cont', -150);
    await pinned(page, null);
    const request = page.received.find(entry => entry.frame.t === 'history-get').frame;
    await page.push[sessionRoom.id]({t: 'history', reqId: request.reqId, segment: 1, final: true,
      records: [{id: 'old-p', kind: 'user', text: 'Archived prompt before compaction.'}]});
    await pinned(page, 'Archived prompt before compaction.');
    assert.equal(await segment.evaluate(node => node.open), false);
    await page.close();
  } finally { await browser.close(); }
});

test('live following survives send, streaming and delayed transcript or dock growth', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 375, height: 667}]) {
      const page = await liveRoom(browser, viewport);
      await page.locator('#composer-input').fill('Continue please');
      await page.locator('#send-button').click();
      await page.waitForFunction(() => document.getElementById('composer-input').value === '');
      // Content can grow between host records, for example when an image loads.
      await page.locator('#transcript [data-record-id="answer"]').evaluate(node => {
        const image = document.createElement('div');
        image.style.height = '600px';
        node.append(image);
      });
      await page.waitForFunction(atLiveEdge, null, {timeout: 2000});
      for (let i = 1; i <= 3; i++) {
        await page.push[sessionRoom.id]({t: 'record', record: {
          id: 'answer', kind: 'assistant', text: `Stream part ${i}.\n\n`.repeat(i * 35)}});
        await page.waitForFunction(i => document.querySelector('[data-record-id="answer"]')
          .textContent.includes(`Stream part ${i}.`), i);
        await page.waitForFunction(atLiveEdge);
      }
      await page.locator('#composer-input').fill('> keep this draft\nsecond line');
      await page.locator('#composer-input').evaluate(node => { node.style.height = '180px'; });
      await page.waitForFunction(atLiveEdge);
      assert.equal(await page.locator('#composer-input').inputValue(), '> keep this draft\nsecond line');
      await page.push[sessionRoom.id]({t: 'record', record: {
        id: 'answer', kind: 'assistant', text: 'Shortened response.'}});
      await page.waitForFunction(() => document.querySelector('[data-record-id="answer"]')
        .textContent.includes('Shortened response.'));
      await page.waitForFunction(atLiveEdge);
      assert.equal(await page.locator('#live-button').isVisible(), false,
        'a shorter page at the bottom must keep following');
      await page.push[sessionRoom.id]({t: 'record', record: {
        id: 'answer', kind: 'assistant', text: 'Growing again.\n\n'.repeat(100)}});
      await page.waitForFunction(() => document.querySelector('[data-record-id="answer"]')
        .textContent.includes('Growing again.'));
      await page.waitForFunction(atLiveEdge);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('reading pauses following through updates and send, and returning to bottom resumes it', async () => {
  const browser = await chromium.launch();
  try {
    const page = await liveRoom(browser, {width: 1200, height: 800});
    await page.mouse.wheel(0, -600);
    await page.locator('#live-button:not([hidden])').waitFor();
    await page.waitForFunction(() => scrollY < document.documentElement.scrollHeight - innerHeight - 300);
    const before = await page.evaluate(() => scrollY);
    await page.push[sessionRoom.id]({t: 'record', record: {
      id: 'answer', kind: 'assistant', text: 'More streamed text.\n\n'.repeat(100)}});
    await page.waitForFunction(() => document.querySelector('[data-record-id="answer"]')
      .textContent.includes('More streamed text.'));
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    assert.equal(await page.evaluate(() => scrollY), before);
    // Submit without Playwright scrolling the composer into view first.
    await page.locator('#composer-input').evaluate(node => {
      node.value = 'A follow-up from this reading position';
      node.form.requestSubmit();
    });
    await page.waitForFunction(() => document.getElementById('composer-input').value === '');
    assert.equal(await page.evaluate(() => scrollY), before);
    await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
    await page.waitForFunction(() => document.getElementById('live-button').hidden);
    await page.push[sessionRoom.id]({t: 'record', record: {
      id: 'answer', kind: 'assistant', text: 'Following again.\n\n'.repeat(160)}});
    await page.waitForFunction(() => document.querySelector('[data-record-id="answer"]')
      .textContent.includes('Following again.'));
    await page.waitForFunction(atLiveEdge);
  } finally { await browser.close(); }
});

test('a small upward touchpad scroll releases following inside the bottom threshold', async () => {
  const browser = await chromium.launch();
  try {
    const page = await liveRoom(browser, {width: 1200, height: 800});
    await page.mouse.move(500, 350);
    const bottom = await page.evaluate(() => scrollY);
    await page.mouse.wheel(0, -6);
    await page.waitForFunction(() => !document.getElementById('live-button').hidden,
      null, {timeout: 1500});
    const reading = await page.evaluate(() => scrollY);
    assert.ok(reading < bottom && bottom - reading < 40,
      'one small gesture must release following without crossing the 40px threshold');
    await page.push[sessionRoom.id]({t: 'record', record: {
      id: 'answer', kind: 'assistant', text: 'The stream continues.\n\n'.repeat(100)}});
    await page.waitForFunction(() => document.querySelector('[data-record-id="answer"]')
      .textContent.includes('The stream continues.'));
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    assert.equal(await page.evaluate(() => scrollY), reading);
    assert.equal(await page.locator('#live-button').isVisible(), true);
  } finally { await browser.close(); }
});

async function newTab(source, link, gesture, {room, length, route, ready}) {
  // Target download/reload checks can activate another tab. Native gestures
  // originate in the visible source tab, as they do when a person clicks.
  await source.bringToFront();
  assert.equal(await link.evaluate(node => node.tagName), 'A', 'navigation uses a native anchor');
  const destination = new URL(await link.getAttribute('href'));
  const search = route.shared ? `?${new URLSearchParams({shared: route.shared})}` : '';
  assert.equal(destination.search, search, 'only the existing shared-editor selector uses an HTTP query');
  assert.equal(destination.hash.split('?')[0], new URL(tierLink(room, length)).hash);
  assert.deepEqual(Object.fromEntries(new URLSearchParams(route.shared
    ? destination.search : destination.hash.split('?')[1])), route);
  const before = await sourceState(source);
  const composer = await source.locator('#composer-input').elementHandle();
  const receivedCount = source.received.length;
  const helloCount = source.received.filter(({frame}) => frame.t === 'hello').length;
  const clickedAnchor = await link.elementHandle();
  await clickedAnchor.evaluate(node => {
    node.navigationEvents = [];
    for (const type of ['mousedown', 'mouseup', 'click', 'auxclick']) {
      node.addEventListener(type, event => node.navigationEvents.push({
        type, button: event.button, ctrlKey: event.ctrlKey, metaKey: event.metaKey,
        defaultPrevented: event.defaultPrevented,
      }), {once: true});
    }
  });
  const [target] = await Promise.all([
    source.context().waitForEvent('page', {timeout: 10000}),
    link.click(gesture.options),
  ]).catch(async error => {
    const events = await clickedAnchor.evaluate(node => ({
      connected: node.isConnected, events: node.navigationEvents,
    })).catch(() => null);
    throw new Error(`${gesture.name}, ${length}-byte tier, ${JSON.stringify(route)}: native new page failed; events=${JSON.stringify(events)}`, {cause: error});
  });
  await target.waitForLoadState('load').catch(async error => {
    const documentState = await target.evaluate(() => ({
      readyState: document.readyState, title: document.title,
      room: history.state?.room, location: `${location.origin}${location.pathname}`,
    })).catch(() => null);
    const requests = [...source.network].filter(([request]) => {
      try { return request.frame().page() === target; } catch { return true; }
    }).map(([request, state]) => ({url: request.url(), state, route: source.routes.get(request)}));
    throw new Error(`${gesture.name}, ${length}-byte tier, ${JSON.stringify(route)}: load failed; document=${JSON.stringify(documentState)}; requests=${JSON.stringify(requests)}`, {cause: error});
  });
  await ready(target);
  assert.deepEqual(await sourceState(source), before, `${gesture.name} leaves source URL, state and draft intact`);
  assert.equal(target.url(), `http://localhost:8765/${search}`, 'bearer and private route are scrubbed');
  const state = await target.evaluate(() => history.state);
  assert.equal(state.room, room.id);
  assert.equal(JSON.stringify(state).includes(base64url(room.secret.slice(0, 32))), false);
  const hellos = source.received.filter(({frame}) => frame.t === 'hello').slice(helloCount);
  assert.equal(hellos.length, 1, 'the fresh page joins independently');
  assert.equal(hellos[0].room, room.id);
  assert.equal(hellos[0].frame.writeToken,
               length >= 48 ? base64url(room.secret.slice(32, 48)) : undefined);
  assert.equal(hellos[0].frame.ownerToken,
               length === 64 ? base64url(room.secret.slice(48, 64)) : undefined);
  const requests = source.received.slice(receivedCount)
    .filter(entry => entry.connection === hellos[0].connection).map(({frame}) => frame);
  if (route.artifact || route.record) {
    const request = requests.find(frame => frame.t === 'artifact-get');
    assert.equal(request?.id, route.artifact ? `artifact:${route.artifact}` : route.record);
    assert.equal(request.version, route.version ? Number(route.version) : undefined);
    if (route.agent) {
      const transcript = requests.find(frame => frame.t === 'fetch-agent');
      assert.equal(transcript?.path, route.agent);
      assert.equal(transcript.segment, route.segment ? Number(route.segment) : undefined);
      assert.ok(requests.indexOf(transcript) < requests.indexOf(request),
                'the new guest resolves its agent artifact before requesting its bytes');
    }
  } else if (route.file) {
    assert.equal(requests.find(frame => frame.t === 'file-get')?.path, route.file);
  } else if (route.agent) {
    assert.equal(requests.find(frame => frame.t === 'fetch-agent')?.path, route.agent);
  } else if (route.shared) {
    assert.ok(requests.filter(frame => frame.t === 'editing').some(frame => {
      const args = JSON.parse(Buffer.from(frame.data, 'base64').toString());
      return args.action === 'read' && args.id === route.shared;
    }), 'the fresh editor requests the linked shared item');
  }
  if (room === sessionRoom) {
    assert.equal(await target.locator('#composer').evaluate(node => node.hidden), length === 32);
    if (length === 64) await target.locator('#modeline .settings-button').waitFor({state: 'attached'});
    assert.equal(await target.locator('#modeline .settings-button').count(), length === 64 ? 1 : 0);
  }
  assert.ok(source.requests.every(address => {
    const url = new URL(address);
    return !url.hash && (!url.search || (route.shared && url.search === search));
  }), 'HTTP requests contain neither credentials nor private destination selectors');
  // The route must survive without the bearer fragment, rather than opening
  // correctly once and then losing its destination on refresh.
  await target.reload();
  await ready(target);
  assert.equal(target.url(), `http://localhost:8765/${search}`);
  assert.deepEqual(await sourceState(source), before, 'target reload does not change the source');
  assert.equal(await composer.evaluate(node => node === document.getElementById('composer-input')), true,
               'the source composer is not replaced');
  await composer.dispose();
  await target.close();
}

test('native new-tab project navigation preserves drafts and every access tier', async () => {
  const browser = await chromium.launch();
  try {
    for (const length of [32, 48, 64]) {
      for (const gesture of gestures) {
        const page = await open(browser);
        await page.goto(tierLink(sessionRoom, length));
        await page.waitForFunction(() => document.getElementById('session-label').textContent === 'Feature work');
        await draft(page);
        await page.locator('#room-crumbs .crumb').click();
        await newTab(page, page.getByRole('menuitem', {name: 'Project artifacts'}), gesture, {
          room: sessionRoom, length, route: {view: 'artifacts'},
          ready: target => target.locator('#store-list [data-store-id="page"]').waitFor(),
        });
        await page.context().close();
      }
    }
  } finally {
    await browser.close();
  }
});

test('native new-tab artifact and version links restore the requested content without touching the source', async () => {
  const browser = await chromium.launch();
  try {
    for (const length of [32, 48, 64]) {
      for (const gesture of gestures) {
        const page = await open(browser);
        await page.goto(tierLink(sessionRoom, length));
        await page.waitForFunction(() => document.getElementById('session-label').textContent === 'Feature work');
        await draft(page);
        await page.locator('#room-crumbs .crumb').click();
        await page.getByRole('menuitem', {name: 'Project artifacts'}).click();
        const row = page.locator('#store-list [data-store-id="page"]');
        const ready = async target => {
          await target.frameLocator('#artifact-body iframe').locator('#inside').waitFor();
          // A fresh tab knows only the store ID. The host's canonical name
          // must hydrate downloads, including saved versions and reloads.
          const downloaded = target.waitForEvent('download');
          await target.locator('#artifact-download').click();
          const download = await downloaded;
          assert.equal(download.suggestedFilename(), 'index.html');
          assert.equal(await readFile(await download.path(), 'utf8'), PAGE_HTML);
          await download.delete();
        };
        await newTab(page, row.getByRole('link', {name: 'Open', exact: true}), gesture, {
          room: sessionRoom, length, route: {artifact: 'page'}, ready,
        });
        await row.getByRole('button', {name: 'More actions for Page'}).click();
        await page.locator('.store-menu:popover-open').getByRole('button', {name: 'Versions', exact: true}).click();
        await newTab(page, row.getByRole('link', {name: 'View', exact: true}), gesture, {
          room: sessionRoom, length, route: {artifact: 'page', version: '1'},
          ready: async target => {
            await ready(target);
            assert.match(await target.locator('#artifact-title').textContent(), /Version 1/);
          },
        });
        await page.context().close();
      }
    }
  } finally {
    await browser.close();
  }
});

test('native new-tab transcript artifact and agent links retain their destinations and access tiers', async () => {
  const browser = await chromium.launch();
  const records = [
    {id: 'tool:page', kind: 'tool', artifact: 'page/index.html', title: 'Transcript page'},
    {id: 'tool:notes', kind: 'tool', artifact: 'page/notes.md', store: 'page', title: 'Secondary notes'},
    {id: 'agent-notice', kind: 'agent-notice', sender: '/root/reviewer',
     mailType: 'result', outcome: 'completed', text: 'Review complete.'},
  ];
  try {
    for (const length of [32, 48, 64]) {
      for (const gesture of gestures) {
        const page = await open(browser, {records});
        await page.goto(tierLink(sessionRoom, length));
        const card = id => page.locator(`#transcript [data-record-id="${id}"] .artifact-open`);
        await card('tool:page').waitFor();
        await draft(page);
        await newTab(page, card('tool:page'), gesture, {
          room: sessionRoom, length, route: {record: 'tool:page'},
          ready: target => target.frameLocator('#artifact-body iframe').locator('#inside').waitFor(),
        });
        // Membership in a store does not make a secondary file its primary
        // page. The record identity is necessary to fetch the right bytes.
        await newTab(page, card('tool:notes'), gesture, {
          room: sessionRoom, length, route: {record: 'tool:notes'},
          ready: target => target.locator('#artifact-body').getByText(SECONDARY_TEXT, {exact: true}).waitFor(),
        });
        await page.locator('#transcript .agent-notice summary').click();
        await newTab(page, page.getByRole('link', {name: 'Open transcript', exact: true}), gesture, {
          room: sessionRoom, length, route: {agent: '/root/reviewer'},
          ready: async target => {
            await target.locator('#agent-transcript').getByText('The retained agent transcript.', {exact: true}).waitFor();
            assert.equal(await target.locator('#agent-title').textContent(), '/root/reviewer');
          },
        });
        await page.context().close();
      }
    }
  } finally {
    await browser.close();
  }
});

test('native new-tab agent artifacts restore per-guest authorization for current and archived transcripts', async () => {
  const browser = await chromium.launch();
  try {
    for (const length of [32, 48, 64]) {
      for (const gesture of gestures) {
        for (const segment of [null, 3]) {
          const page = await open(browser);
          const agent = {agent: '/root/reviewer', ...(segment ? {segment: String(segment)} : {})};
          await page.goto(`${tierLink(sessionRoom, length)}?${new URLSearchParams(agent)}`);
          const card = page.locator('#agent-transcript .artifact-open');
          await card.waitFor();
          const record = segment ? 'agent-history-3:agent-artifact-notes' : 'agent-artifact-notes';
          await newTab(page, card, gesture, {
            room: sessionRoom, length, route: {record, ...agent},
            ready: target => target.locator('#artifact-body').getByText(
              segment ? ARCHIVED_AGENT_ARTIFACT_TEXT : AGENT_ARTIFACT_TEXT, {exact: true}).waitFor(),
          });
          await page.context().close();
        }
      }
    }
  } finally {
    await browser.close();
  }
});

test('a cold live agent artifact polls again in the new tab and after reload', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser, {coldAgent: true, records: [
      {id: 'cold-agent-notice', kind: 'agent-notice', sender: '/root/reviewer',
       mailType: 'result', outcome: 'completed', text: 'Review complete.'},
    ]});
    await page.goto(tierLink(sessionRoom, 48));
    await page.locator('#transcript .agent-notice summary').waitFor();
    await draft(page);
    await page.locator('#transcript .agent-notice summary').click();
    await page.getByRole('link', {name: 'Open transcript', exact: true}).click();
    const card = page.locator('#agent-transcript .artifact-open');
    await card.waitFor();
    const firstTarget = page.joined.length;
    await newTab(page, card, gestures[0], {
      room: sessionRoom, length: 48,
      route: {record: 'agent-artifact-notes', agent: '/root/reviewer'},
      ready: target => target.locator('#artifact-body').getByText(AGENT_ARTIFACT_TEXT, {exact: true}).waitFor(),
    });
    assert.equal(page.joined.length, firstTarget + 2, 'new tab and reload each use a fresh peer');
    for (const connection of [firstTarget, firstTarget + 1]) {
      const frames = page.received.filter(entry => entry.connection === connection).map(entry => entry.frame);
      const resolved = frames.filter(frame => ['fetch-agent', 'artifact-get'].includes(frame.t));
      assert.deepEqual(resolved.map(frame => frame.t), ['fetch-agent', 'fetch-agent', 'artifact-get'],
                       'a silent first fetch must be followed by a poll before fetching the artifact');
      assert.ok(resolved[1].reqId > resolved[0].reqId, 'the second poll is a new request');
      const hello = frames.find(frame => frame.t === 'hello');
      assert.equal(hello.writeToken, base64url(sessionRoom.secret.slice(32, 48)));
      assert.equal(hello.ownerToken, undefined, 'polling and reload preserve the full-control tier');
    }
    assert.equal(await page.locator('#modeline .settings-button').count(), 0);
    await page.context().close();
  } finally {
    await browser.close();
  }
});

test('native new-tab shared item titles bootstrap the real editor at every access tier', async () => {
  const {handle} = await import('../shared-editing/host.mjs');
  const {state: sharedItem} = await handle({action: 'create', id: 'navigation-doc',
    kind: 'document', title: 'Navigation document', actor: 'Host', opId: 'create',
    content: {type: 'doc', content: [{type: 'paragraph', attrs: {id: 'intro'},
      content: [{type: 'text', text: 'The shared document reached the new tab.'}]}]}});
  const browser = await chromium.launch();
  try {
    for (const length of [32, 48, 64]) {
      for (const gesture of gestures) {
        const page = await open(browser, {sharedItem});
        await page.goto(tierLink(sessionRoom, length));
        await page.locator('#editing-status').getByText('Shared editing is ready.', {exact: true}).waitFor();
        await draft(page);
        await newTab(page, page.locator('a[data-item-id="navigation-doc"]'), gesture, {
          room: sessionRoom, length, route: {shared: 'navigation-doc'},
          ready: async target => {
            const document = target.frameLocator('#editing-body iframe').locator('.tiptap');
            await document.getByText('The shared document reached the new tab.', {exact: true}).waitFor();
            assert.equal(await document.getAttribute('contenteditable'), length === 32 ? 'false' : 'true');
            assert.equal(await target.locator('#editing-title').inputValue(), 'Navigation document');
          },
        });
        await page.context().close();
      }
    }
  } finally {
    await browser.close();
  }
});

test('native new-tab lobby links preserve the selected tab and open files privately', async () => {
  const browser = await chromium.launch();
  try {
    for (const length of [32, 48, 64]) {
      for (const gesture of gestures) {
        const page = await open(browser);
        await page.goto(tierLink(lobbyRoom, length));
        await page.locator('#lobby-list').getByText('Feature work', {exact: true}).waitFor();
        await page.locator('#lobby-tab-artifacts').click();
        await page.locator('#lobby-store-list [data-store-id="page"]').waitFor();
        await page.locator('#room-crumbs .crumb').click();
        for (const tab of length >= 48 ? ['sessions', 'artifacts', 'files'] : ['sessions', 'artifacts']) {
          await newTab(page, page.getByRole('menuitem', {name: tab, exact: false}), gesture, {
            room: lobbyRoom, length, route: {tab},
            ready: target => target.waitForFunction(tab =>
              document.getElementById(`lobby-tab-${tab}`).getAttribute('aria-selected') === 'true', tab),
          });
        }
        await page.keyboard.press('Escape');
        if (length >= 48) {
          await page.locator('#lobby-tab-files').click();
          await newTab(page, page.getByRole('link', {name: 'View notes & plans.txt'}), gesture, {
            room: lobbyRoom, length, route: {file: 'notes & plans.txt'},
            ready: target => target.locator('#artifact-body').getByText(FILE_TEXT, {exact: true}).waitFor(),
          });
        }
        await page.context().close();
      }
    }
  } finally {
    await browser.close();
  }
});

test('session reasoning effort follows host choices and confirmation on desktop and phone', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 375, height: 667}]) {
      const page = await open(browser, {viewport});
      await page.goto(sessionRoom.link);
      await page.waitForFunction(() => document.getElementById('session-label')
        ?.textContent === 'Feature work');
      const recovery = {t: 'recovery', model: 'Codex:reasoning',
        models: ['Codex:reasoning', 'Other:model'], efforts: ['low', 'medium', 'high', 'xhigh'],
        effort: 'medium', presets: [], providers: []};
      await page.push[sessionRoom.id](recovery);
      await page.getByRole('button', {name: 'Settings', exact: true}).click();
      const effort = page.getByLabel('Reasoning effort', {exact: true});
      assert.deepEqual(await effort.locator('option').allTextContents(),
                       ['Default', 'low', 'medium', 'high', 'xhigh']);
      assert.equal(await effort.inputValue(), 'medium');
      const requests = () => page.received.filter(({frame}) => frame.t === 'recovery')
        .map(({frame}) => frame);
      const request = async (select, value, expected, confirmed) => {
        const count = requests().length;
        await select.selectOption(value);
        assert.equal(await select.inputValue(), confirmed);
        // Wait for the stand-in host to decrypt the real outgoing frame.
        await new Promise((resolve, reject) => {
          const deadline = Date.now() + 5000;
          const check = () => {
            if (requests().length > count) resolve();
            else if (Date.now() > deadline) reject(new Error('recovery request not received'));
            else setTimeout(check, 10);
          };
          check();
        });
        assert.deepEqual(requests().at(-1), expected);
      };
      await request(effort, 'xhigh', {t: 'recovery', action: 'effort', value: 'xhigh'}, 'medium');
      await page.push[sessionRoom.id]({...recovery, effort: 'xhigh'});
      await page.waitForFunction(() => document.getElementById('settings-effort').value === 'xhigh');
      await request(effort, '', {t: 'recovery', action: 'effort', value: null}, 'xhigh');
      await page.push[sessionRoom.id]({...recovery, effort: null});
      await page.waitForFunction(() => document.getElementById('settings-effort').value === '');
      const model = page.getByRole('dialog', {name: 'Session settings'}).getByLabel('Model', {exact: true});
      await request(model, 'Other:model', {t: 'recovery', action: 'model', value: 'Other:model'}, 'Codex:reasoning');
      await page.push[sessionRoom.id]({...recovery, model: 'Other:model', efforts: ['minimal', 'high'], effort: 'minimal'});
      await page.waitForFunction(() => document.getElementById('settings-effort').value === 'minimal');
      assert.deepEqual(await effort.locator('option').allTextContents(), ['Default', 'minimal', 'high']);
      await effort.scrollIntoViewIfNeeded();
      const box = await effort.boundingBox();
      assert.ok(box && box.x >= 0 && box.x + box.width <= viewport.width
                && box.y >= 0 && box.y + box.height <= viewport.height,
                `effort selector fits ${viewport.width}x${viewport.height}`);
      await page.push[sessionRoom.id]({...recovery, model: 'Other:model', efforts: [], effort: null});
      await page.waitForFunction(() => document.getElementById('settings-effort-group').hidden);
      assert.equal(await effort.isVisible(), false);
      assert.equal(await effort.isDisabled(), true);
      assert.deepEqual(await effort.locator('option').allTextContents(), ['Default']);
      await page.context().close();
    }
  } finally {
    await browser.close();
  }
});

test('writer and view links cannot open session effort settings', async () => {
  const browser = await chromium.launch();
  try {
    for (const length of [48, 32]) {
      const page = await open(browser);
      await page.goto(`http://localhost:8765/#${sessionRoom.id}.${base64url(sessionRoom.secret.slice(0, length))}`);
      await page.waitForFunction(() => document.getElementById('session-label')
        ?.textContent === 'Feature work');
      // Even an erroneously delivered owner snapshot must not expose settings.
      await page.push[sessionRoom.id]({t: 'recovery', models: ['Codex:reasoning'],
        model: 'Codex:reasoning', efforts: ['low', 'high'], effort: 'high'});
      await page.push[sessionRoom.id]({t: 'status', name: 'Non-owner ready'});
      await page.waitForFunction(() => document.getElementById('session-label')
        ?.textContent === 'Non-owner ready');
      assert.equal(await page.locator('#modeline .settings-button').count(), 0);
      assert.equal(await page.locator('#settings').evaluate(node => node.open), false);
      assert.equal(await page.locator('#settings-effort').isDisabled(), true);
      assert.equal(page.received.some(({frame}) => frame.t === 'recovery'), false);
      await page.context().close();
    }
  } finally {
    await browser.close();
  }
});

test('a room handed off leads its guests to the room that continues it', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 375, height: 812}]) {
      const page = await open(browser, {viewport});
      await page.goto(sessionRoom.link);
      await page.waitForFunction(() => document.getElementById('session-label')
        ?.textContent === 'Feature work');
      await page.push[sessionRoom.id]({t: 'bye', reason: 'handoff',
                                       name: 'Implement plan', link: forkRoom.link});
      const action = page.locator('#terminal-action');
      await action.waitFor();
      assert.equal(await page.locator('#terminal-title').textContent(),
                   'Continued in Implement plan');
      assert.equal(await action.textContent(), 'Open Implement plan');
      // The page offers the room itself; no dock notice repeats it.
      assert.equal(await visible(page, '#invites'), false);
      const box = await action.boundingBox();
      assert.ok(box.x >= 0 && box.x + box.width <= viewport.width,
                `the Open action fits ${viewport.width}px`);
      await action.click();
      await page.waitForFunction(() => document.getElementById('session-label')
        ?.textContent === 'Implement plan');
      assert.equal(await visible(page, '#terminal-state'), false);
      assert.ok(page.joined.includes(forkRoom.id));

      if (viewport.width < 600) {
        // An unnamed successor is known only by its ID, so it goes unnamed.
        await page.push[forkRoom.id]({t: 'bye', reason: 'handoff', name: null,
                                      link: sessionRoom.link});
        await page.locator('#terminal-state').waitFor();
        assert.equal(await page.locator('#terminal-title').textContent(),
                     'Continued in a new session');
        assert.equal(await action.textContent(), 'Open the new session');
      } else {
        // A successor the host could not share leaves nowhere to go.
        await page.push[forkRoom.id]({t: 'bye', reason: 'handoff-failed'});
        await page.locator('#terminal-state').waitFor();
        assert.equal(await page.locator('#terminal-title').textContent(),
                     'Continued in Emacs');
        assert.equal(await visible(page, '#terminal-action'), false);
      }
      await page.context().close();
    }
  } finally {
    await browser.close();
  }
});

test('the lobby, a room and its artifact lead back through header and history', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser);
    await page.goto(lobbyRoom.link);
    await page.getByRole('button', {name: 'Open Feature work'}).waitFor();
    assert.equal(await title(page), 'Lobby · proj');
    // Standing in the lobby, there is nowhere further back.
    assert.equal(await visible(page, '#room-back'), false);
    assert.equal(await page.locator('#room-crumbs .crumb').textContent(), 'proj ▾');

    // Opening a session is a history entry, not a replacement.
    await page.getByRole('button', {name: 'Open Feature work'}).click();
    await page.waitForFunction(() => document.getElementById('session-label')
      ?.textContent === 'Feature work');
    assert.equal(await visible(page, '#room-back'), true);
    assert.equal(await page.locator('#room-back').getAttribute('aria-label'),
                 'Back to the project lobby');
    assert.equal(await page.evaluate(() => window.history.length), 3);

    // The project menu offers the lobby and the project's artifacts; the
    // lobby's stored link can be forgotten among the rooms.
    await page.locator('#room-crumbs .crumb').click();
    const menu = page.locator('.nav-menu:popover-open');
    assert.deepEqual(await menu.getByRole('menuitem').allTextContents(),
                     ['Lobby', 'Project artifacts', 'Manage rooms…']);
    assert.deepEqual(await menu.locator('a').allTextContents(), ['Lobby', 'Project artifacts']);
    await menu.getByRole('menuitem', {name: 'Project artifacts'}).click();
    await page.locator('#store-list li', {hasText: 'Page'})
      .getByRole('link', {name: 'Open', exact: true}).click();
    await page.frameLocator('#artifact-body iframe').locator('#inside').waitFor();
    assert.equal(await page.locator('#artifact-crumbs').textContent(),
                 'proj ▾›Feature work');
    assert.equal(await page.locator('#artifact-close').getAttribute('aria-label'),
                 'Back to Feature work');

    // Back closes the panel without leaving the room; Forward reopens it.
    await page.goBack();
    await page.waitForFunction(() => document.getElementById('artifact-panel').hidden);
    assert.equal(await title(page), 'Feature work');
    await page.goForward();
    await page.frameLocator('#artifact-body iframe').locator('#inside').waitFor();

    // A reload keeps the open artifact; its back button closes it.
    await page.reload();
    await page.frameLocator('#artifact-body iframe').locator('#inside').waitFor();
    await page.locator('#artifact-close').click();
    await page.waitForFunction(() => document.getElementById('artifact-panel').hidden);
    assert.equal(await title(page), 'Feature work');

    // The room's back button returns to the lobby, as a history entry.
    await page.locator('#room-back').click();
    await page.getByRole('button', {name: 'Open Feature work'}).waitFor();
    assert.equal(await title(page), 'Lobby · proj');
    // And browser Back returns to the room, Back again to the lobby.
    await page.goBack();
    await page.waitForFunction(() => document.getElementById('session-label')
      ?.textContent === 'Feature work');
    while (await title(page) !== 'Lobby · proj') {
      await page.goBack();
      await page.waitForFunction(() => document.getElementById('session-label')
        ?.textContent !== 'Live session');
    }
    assert.equal(await visible(page, '#lobby'), true);

    // A room's key never stays in a URL: each page wipes it on load, and
    // history holds only fragment-free entries.
    const now = await page.evaluate(() => window.location.href);
    assert.equal(now, 'http://localhost:8765/');
    for (const url of page.urls.slice(1)) {
      assert.ok(!url.includes(base64url(sessionRoom.secret.slice(0, 32))),
                `no session key in ${url}`);
    }
  } finally {
    await browser.close();
  }
});

test('the lobby tab survives a reload, and the menu switches tabs', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser);
    await page.goto(lobbyRoom.link);
    await page.getByRole('button', {name: 'Open Feature work'}).waitFor();
    await page.locator('#room-crumbs .crumb').click();
    const menu = page.locator('.nav-menu:popover-open');
    assert.deepEqual(await menu.locator('a').allTextContents(),
                     ['Sessions', 'Artifacts', 'Files']);
    await menu.getByRole('menuitem', {name: 'Artifacts'}).click();
    await page.locator('#lobby-store-list').getByText('Page').waitFor();
    await page.reload();
    await page.locator('#lobby-store-list').getByText('Page').waitFor();
    assert.equal(await page.locator('#lobby-tab-artifacts').getAttribute('aria-selected'),
                 'true');
  } finally {
    await browser.close();
  }
});

test('a host without a running lobby leaves the project as the only way out', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser, {lobbyRunning: false});
    await page.goto(lobbyRoom.link);
    await page.getByRole('button', {name: 'Open Feature work'}).click();
    await page.waitForFunction(() => document.getElementById('session-label')
      ?.textContent === 'Feature work');
    // The browser holds the lobby's link, but the host says it is stopped.
    assert.equal(await visible(page, '#room-back'), false);
    await page.locator('#room-crumbs .crumb').click();
    assert.deepEqual(await page.locator('.nav-menu:popover-open [role="menuitem"]').allTextContents(),
                     ['Project artifacts', 'Manage rooms…']);
  } finally {
    await browser.close();
  }
});

test('a phone keeps the way back and the project menu in its header', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser, {viewport: {width: 375, height: 667}});
    await page.goto(lobbyRoom.link);
    await page.getByRole('button', {name: 'Open Feature work'}).click();
    await page.waitForFunction(() => document.getElementById('session-label')
      ?.textContent === 'Feature work');
    for (const selector of ['#room-back', '#room-crumbs .crumb', '#session-label']) {
      const box = await page.locator(selector).boundingBox();
      assert.ok(box && box.width > 0 && box.y >= 0 && box.y + box.height <= 56,
                `${selector} in the header: ${JSON.stringify(box)}`);
    }
    // The title is not cut off.
    assert.equal(await page.locator('#session-label').evaluate(
      node => node.scrollWidth <= node.clientWidth), true);
  } finally {
    await browser.close();
  }
});

test('a whiteboard opened from the lobby opens in its editor', async () => {
  const browser = await chromium.launch();
  try {
    const page = await open(browser);
    await page.goto(lobbyRoom.link);
    await page.getByRole('button', {name: 'Open Feature work'}).waitFor();
    await page.locator('#lobby-tab-artifacts').click();
    const reloaded = page.waitForEvent('load');
    await page.locator('#lobby-store-list li', {hasText: 'Board'})
      .getByRole('button', {name: 'Open', exact: true}).click();
    await reloaded;
    // The item travels in the query; the key never does.
    assert.equal(new URL(page.url()).search, '?shared=board');
    assert.equal(new URL(page.url()).hash, '');
    assert.equal(await page.evaluate(() => window.history.state.room), sessionRoom.id);
  } finally {
    await browser.close();
  }
});
