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

// What the stand-in host answers in each room.
function host(lobbyRunning) {
  const store = {t: 'store-artifacts', artifacts: [
    {id: 'page', title: 'Page', kind: 'html', artifact: 'page/index.html',
     versions: 1, modified: 1, size: PAGE_HTML.length, attached: true},
    {id: 'board', title: 'Board', kind: 'whiteboard', artifact: 'board/board.excalidraw',
     versions: 1, modified: 2, size: 10, item: true}]};
  return {
    [lobbyRoom.id]: (frame, reply) => {
      if (frame.t === 'hello' || frame.t === 'lobby-refresh') {
        reply({t: 'lobby', project: 'proj', workspace: 'ws', omitted: 0, sessions: [
          {id: 's1', name: 'Feature work', updated: 1, preview: 'Hello',
           live: true, shared: true}]});
      } else if (frame.t === 'open-session') {
        reply({t: 'open-session', reqId: frame.reqId, ok: true, link: sessionRoom.link});
      } else if (frame.t === 'store-list') reply(store);
      else if (frame.t === 'store-action' && frame.action === 'conversation') {
        // A whiteboard's conversation link names the item its editor opens.
        const hash = sessionRoom.link.indexOf('#');
        reply({t: 'store-action', reqId: frame.reqId, ok: true,
               link: `${sessionRoom.link.slice(0, hash)}?shared=${frame.id}${sessionRoom.link.slice(hash)}`});
      }
    },
    [sessionRoom.id]: (frame, reply) => {
      if (frame.t === 'hello') {
        reply({t: 'welcome', proto: 3, readOnly: false, workspace: 'ws',
               project: 'proj', lobby: lobbyRunning, recordCount: 0});
        reply({t: 'snapshot-chunk', records: [], final: true});
        reply({t: 'status', name: 'Feature work', busy: false});
      } else if (frame.t === 'store-list') reply(store);
      else if (frame.t === 'artifact-get') {
        reply({t: 'artifact', reqId: frame.reqId, mime: 'text/html', size: PAGE_HTML.length,
               data: Buffer.from(PAGE_HTML).toString('base64'), final: true});
      }
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

async function open(browser, {lobbyRunning = true, viewport = {width: 1200, height: 800}} = {}) {
  const context = await browser.newContext({viewport});
  const page = await context.newPage();
  const answers = host(lobbyRunning);
  const keys = {};
  for (const room of [lobbyRoom, sessionRoom, forkRoom]) {
    keys[room.id] = await crypto.subtle.importKey(
      'raw', room.secret.slice(0, 32), 'AES-GCM', false, ['encrypt', 'decrypt']);
  }
  page.joined = [];
  // Frames the host sends unprompted, by room.
  page.push = {};
  await page.routeWebSocket(/\/r\//, socket => {
    const id = new URL(socket.url()).pathname.split('/').pop();
    page.joined.push(id);
    page.push[id] = async frame => socket.send(await seal(keys[id], frame));
    socket.onMessage(async message => {
      if (typeof message === 'string') return;
      const frame = await unseal(keys[id], message);
      answers[id]?.(frame, async reply => socket.send(await seal(keys[id], reply)));
    });
  });
  await page.route('http://localhost:8765/**', async route => {
    let path = new URL(route.request().url()).pathname;
    if (path === '/') path = '/index.html';
    const type = TYPES[path.split('.').pop()];
    if (!type) return route.fulfill({status: 204, body: ''});
    return route.fulfill({body: await readFile(resolve(root, 'relay/viewer', `.${path}`)),
                          contentType: type});
  });
  page.urls = [];
  page.on('framenavigated', frame => {
    if (frame === page.mainFrame()) page.urls.push(frame.url());
  });
  return page;
}

const title = page => page.locator('#session-label').textContent();
const visible = (page, selector) => page.locator(selector).isVisible();

test('a room handed off leads its guests to the room that continues it', async () => {
  const browser = await chromium.launch();
  try {
    for (const viewport of [{width: 1200, height: 800}, {width: 375, height: 812}]) {
      const page = await open(browser, {viewport});
      await page.goto(sessionRoom.link);
      await page.waitForFunction(() => document.getElementById('session-label')
        .textContent === 'Feature work');
      await page.push[sessionRoom.id]({t: 'bye', reason: 'handoff',
                                       name: 'Implement plan', link: forkRoom.link});
      const action = page.locator('#terminal-action');
      await action.waitFor();
      assert.equal(await page.locator('#terminal-title').textContent(),
                   'Continued in Implement plan');
      assert.equal(await action.textContent(), 'Open Implement plan');
      const box = await action.boundingBox();
      assert.ok(box.x >= 0 && box.x + box.width <= viewport.width,
                `the Open action fits ${viewport.width}px`);
      await action.click();
      await page.waitForFunction(() => document.getElementById('session-label')
        .textContent === 'Implement plan');
      assert.equal(await visible(page, '#terminal-state'), false);
      assert.ok(page.joined.includes(forkRoom.id));

      // A successor the host could not share leaves nowhere to go.
      await page.push[forkRoom.id]({t: 'bye', reason: 'handoff-failed'});
      await page.locator('#terminal-state').waitFor();
      assert.equal(await page.locator('#terminal-title').textContent(),
                   'Continued in Emacs');
      assert.equal(await visible(page, '#terminal-action'), false);
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
      .textContent === 'Feature work');
    assert.equal(await visible(page, '#room-back'), true);
    assert.equal(await page.locator('#room-back').getAttribute('aria-label'),
                 'Back to the project lobby');
    assert.equal(await page.evaluate(() => window.history.length), 3);

    // The project menu offers the lobby and the project's artifacts; the
    // lobby's stored link can be forgotten among the rooms.
    await page.locator('#room-crumbs .crumb').click();
    const menu = page.locator('.nav-menu:popover-open');
    assert.deepEqual(await menu.locator('button').allTextContents(),
                     ['Lobby', 'Project artifacts', 'Manage rooms…']);
    await menu.getByRole('menuitem', {name: 'Project artifacts'}).click();
    await page.locator('#store-list li', {hasText: 'Page'})
      .getByRole('button', {name: 'Open', exact: true}).click();
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
      .textContent === 'Feature work');
    while (await title(page) !== 'Lobby · proj') {
      await page.goBack();
      await page.waitForFunction(() => document.getElementById('session-label')
        .textContent !== 'Live session');
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
    assert.deepEqual(await menu.locator('button').allTextContents(),
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
      .textContent === 'Feature work');
    // The browser holds the lobby's link, but the host says it is stopped.
    assert.equal(await visible(page, '#room-back'), false);
    await page.locator('#room-crumbs .crumb').click();
    assert.deepEqual(await page.locator('.nav-menu:popover-open button').allTextContents(),
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
      .textContent === 'Feature work');
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
