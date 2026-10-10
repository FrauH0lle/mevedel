/* Pending interactions stay reachable over the viewer's panels.
 *
 * The real room page and stylesheets load without their scripts; a panel
 * is opened and a request card placed the way the viewer places them.
 *
 * Run: node --test test/collaboration-viewer-requests.browser.mjs
 * Requires `npm ci --prefix shared-editing` and Playwright Chromium.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');

async function room(browser) {
  const page = await browser.newPage({viewport: {width: 1000, height: 700}});
  await page.route('http://viewer.test/**', async route => {
    const path = new URL(route.request().url()).pathname;
    if (!path.endsWith('.html') && !path.endsWith('.css')) {
      return route.fulfill({status: 204, body: ''});
    }
    const body = await readFile(resolve(root, 'relay/viewer', `.${path}`));
    return route.fulfill({body, contentType: path.endsWith('.css') ? 'text/css' : 'text/html'});
  });
  await page.goto('http://viewer.test/index.html');
  await page.evaluate(() => {
    document.getElementById('composer').hidden = false;
  });
  return page;
}

// Whether a pointer at the centre of SELECTOR lands on it.
function reachable(page, selector) {
  return page.evaluate(selector => {
    const node = document.querySelector(selector);
    const box = node.getBoundingClientRect();
    if (!box.width || !box.height) return false;
    const hit = document.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2);
    return node.contains(hit);
  }, selector);
}

function ask(page) {
  return page.evaluate(() => {
    const card = document.createElement('section');
    card.className = 'request-card';
    card.id = 'card';
    card.innerHTML = '<span class="rhead">Needs your decision</span>'
      + '<div class="request-controls"><button class="btn">Allow once</button></div>';
    document.getElementById('requests').append(card);
  });
}

test('a pending interaction rises above every open panel', async () => {
  const browser = await chromium.launch();
  try {
    for (const panel of ['editing-panel', 'artifact-panel', 'agent-panel']) {
      const page = await room(browser);
      assert.equal(await reachable(page, '#composer'), true, 'the room shows its composer');
      await page.evaluate(id => { document.getElementById(id).hidden = false; }, panel);
      // Without a decision pending the panel takes every pointer.
      assert.equal(await page.evaluate(() => {
        const hit = document.elementFromPoint(500, 690);
        return Boolean(hit.closest('.viewer-panel'));
      }), true, `${panel} takes the dock's strip`);
      await ask(page);
      assert.equal(await reachable(page, '#card button'), true, `card over ${panel}`);
      // The rest of the dock stays under the panel.
      assert.equal(await reachable(page, '#composer'), false, `composer under ${panel}`);
      // A sheet the panel opens, such as deleting its item, shows over it.
      await page.evaluate(() => document.getElementById('delete-shared').showModal());
      assert.equal(await reachable(page, '#delete-shared button[value="delete"]'), true,
                   `delete sheet over ${panel}`);
      await page.evaluate(() => document.getElementById('delete-shared').close());
      await page.evaluate(id => { document.getElementById(id).hidden = true; }, panel);
      assert.equal(await reachable(page, '#composer'), true, 'the room returns');
      assert.equal(await reachable(page, '#card button'), true, 'card in the room');
      await page.close();
    }
  } finally {
    await browser.close();
  }
});

test('store refresh keeps the open artifact menu actionable', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await page.addScriptTag({content: await readFile(
      resolve(root, 'relay/viewer/viewer-store.js'), 'utf8')});
    await page.evaluate(() => {
      window.storeSent = [];
      window.storeRows = {artifacts: [{id: 'page', title: 'Page', kind: 'html',
        artifact: 'page/index.html', versions: 1, modified: 1, attached: true}]};
      window.mevedelTranscriptRenderer = {formatBytes: bytes => `${bytes} B`};
      window.storeView = window.mevedelStoreView.create({
        send: frame => window.storeSent.push(frame),
        el: (tag, className, text) => {
          const node = document.createElement(tag);
          node.className = className || '';
          node.textContent = text || '';
          return node;
        },
        list: document.getElementById('store-list'),
        empty: document.getElementById('store-empty'),
        state: {writable: true}, room: true, open() {}, notice() {},
      });
      window.storeView.show(window.storeRows);
      document.getElementById('store-sheet').showModal();
      // Reproduce a refresh in the gap before the browser's toggle task.
      document.querySelector('.store-menu').showPopover();
      window.storeView.show(window.storeRows);
    });
    await page.locator('.store-menu:popover-open').getByRole('button', {name: 'Conversation', exact: true}).click();
    assert.equal(await page.evaluate(() => window.storeSent.at(-1).action), 'conversation');
  } finally {
    await browser.close();
  }
});
