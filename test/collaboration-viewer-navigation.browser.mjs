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
function host(lobbyRunning, records, sharedItem, coldAgent) {
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
             records: [{id: 'agent-answer', kind: 'assistant', text: 'The retained agent transcript.'},
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
        reply({t: 'welcome', proto: 3, readOnly: !frame.writeToken, workspace: 'ws',
               project: 'proj', lobby: lobbyRunning, recordCount: records.length});
        reply({t: 'snapshot-chunk', records, final: true});
        reply({t: 'status', name: 'Feature work', busy: false});
        if (frame.ownerToken) reply({t: 'recovery', models: [], model: null,
                                    efforts: [], effort: null, presets: [], providers: []});
        if (sharedItem) reply(store);
      } else content(frame, reply, guest);
    },
    [forkRoom.id]: (frame, reply) => {
      if (frame.t === 'hello') {
        reply({t: 'welcome', proto: 3, readOnly: false, workspace: 'ws',
               project: 'proj', lobby: lobbyRunning, recordCount: 0});
        reply({t: 'snapshot-chunk', records: [], final: true});
        reply({t: 'status', name: 'Implement plan', busy: false});
      }
    },
  };
}

async function open(browser, {lobbyRunning = true, records = [], sharedItem = null, coldAgent = false,
                              viewport = {width: 1200, height: 800}} = {}) {
  const context = await browser.newContext({viewport});
  const page = await context.newPage();
  const answers = host(lobbyRunning, records, sharedItem, coldAgent);
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
    page.push[id] = async frame => socket.send(await seal(keys[id], frame));
    socket.onMessage(async message => {
      if (typeof message === 'string') return;
      const frame = await unseal(keys[id], message);
      page.received.push({room: id, connection, frame});
      answers[id]?.(frame, async reply => socket.send(await seal(keys[id], reply)), guest);
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
