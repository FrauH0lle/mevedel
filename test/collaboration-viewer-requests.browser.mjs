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

async function requestRenderer(page) {
  await page.addScriptTag({content: await readFile(
    resolve(root, 'relay/viewer/viewer-requests.js'), 'utf8')});
  await page.addScriptTag({content: `(() => {
    const requests = document.getElementById('requests');
    const el = (tag, className = '', text = '') => {
      const node = document.createElement(tag);
      node.className = className; node.textContent = text; return node;
    };
    window.requestSent = [];
    const send = frame => window.requestSent.push(frame);
    window.requestView = window.mevedelRequestView.create({requests, el, send});
  })();`});
}

test('changed pending cards keep their original arrival order', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await requestRenderer(page);
    await page.evaluate(() => {
      requestView.render({reqId: 1, body: 'First', options: []});
      requestView.render({reqId: 2, body: 'Second', options: []});
      requestView.render({reqId: 1, body: 'First updated', options: []});
    });
    assert.deepEqual(await page.locator('.request-card').evaluateAll(
      cards => cards.map(card => card.dataset.reqId)), ['1', '2']);
  } finally { await browser.close(); }
});

test('changed pending cards preserve feedback text, disclosure and focused selection', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await requestRenderer(page);
    await page.evaluate(() => requestView.render({reqId: 1, body: 'Original', allowFeedback: true}));
    await page.getByText('Suggest a change', {exact: true}).click();
    const input = page.locator('.request-feedback');
    await input.fill('> keep this draft');
    await input.evaluate(node => node.setSelectionRange(2, 6));
    await page.evaluate(() => requestView.render({reqId: 1, body: 'Updated', allowFeedback: true}));
    assert.equal(await input.inputValue(), '> keep this draft');
    assert.equal(await input.isVisible(), true);
    assert.deepEqual(await input.evaluate(node => [document.activeElement === node,
      node.selectionStart, node.selectionEnd]), [true, 2, 6]);
    await page.locator('#composer-input').focus();
    await page.evaluate(() => requestView.render({reqId: 1, body: 'Updated again', allowFeedback: true}));
    assert.equal(await page.locator('#composer-input').evaluate(node => document.activeElement === node), true);
    await page.getByRole('button', {name: 'Send feedback', exact: true}).click();
    assert.deepEqual(await page.evaluate(() => requestSent.at(-1)),
      {t: 'ui-response', reqId: 1, feedback: '> keep this draft'});
  } finally { await browser.close(); }
});

test('pending patch notices precede the diff and update safely in place', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1200]) {
      const page = await room(browser);
      await page.setViewportSize({width, height: 812});
      await page.addScriptTag({content: await readFile(
        resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await requestRenderer(page);
      await page.evaluate(() => {
        requestView.render({reqId: 1, bodyKind: 'diff', body: '+selected change',
          notice: 'Current host selection. Warning: parent request is still active.', options: []});
        requestView.render({reqId: 2, body: 'Later request', options: []});
      });
      const notice = page.locator('[data-req-id="1"] .request-notice');
      assert.match(await notice.innerText(), /parent request is still active/);
      assert.ok((await notice.boundingBox()).y < (await page.locator('.diff').boundingBox()).y);
      await page.evaluate(() => requestView.render({reqId: 1, bodyKind: 'diff',
        body: '+selected change', notice: '<img src=x onerror="alert(1)"> Conflict: file changed.', options: []}));
      assert.match(await notice.innerText(), /<img src=x.*Conflict: file changed/);
      assert.equal(await notice.locator('img').count(), 0);
      assert.deepEqual(await page.locator('.request-card').evaluateAll(
        cards => cards.map(card => card.dataset.reqId)), ['1', '2']);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('Ask focuses one question with inspectable samples and stable phone geometry', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1200]) {
      const page = await room(browser);
      await page.setViewportSize({width, height: 812});
      await requestRenderer(page);
      await page.evaluate(() => requestView.render({reqId: 3, allowCancel: true,
        questions: [
          {question: 'Which implementation?', options: [
            {label: 'Small', description: 'Ship the smallest useful implementation.',
             sample: 'short sample'},
            {label: 'Long', sample: 'long wrapped sample '.repeat(90)},
            {label: 'No sample'}]},
          {question: 'Which branch?', options: [{label: 'main'}]},
        ]}));
      assert.equal(await page.locator('.question-panel:not([inert])').count(), 1);
      assert.equal(await page.getByText('Ship the smallest useful implementation.').isVisible(), true);
      await page.waitForFunction(() => document.querySelector('.questionnaire').style.height);
      const height = () => page.locator('[data-req-id="3"]').evaluate(node => node.getBoundingClientRect().height);
      const before = await height();
      await page.getByRole('button', {name: 'Long', exact: true}).focus();
      assert.match(await page.locator('.question-sample:visible').innerText(), /long wrapped/);
      assert.equal(await height(), before, 'inspecting a longer sample holds height');
      await page.getByRole('button', {name: 'No sample', exact: true}).focus();
      assert.match(await page.locator('.question-sample:visible').innerText(), /long wrapped/);
      await page.getByRole('button', {name: 'Small', exact: true}).click();
      assert.equal(await page.getByRole('button', {name: 'main', exact: true}).isVisible(), true);
      assert.equal(await height(), before, 'advancing to a short question holds height');
      await page.getByRole('textbox', {name: 'Custom answer 2'}).fill('topic/browser');
      await page.getByRole('textbox', {name: 'Custom answer 2'}).press('Enter');
      assert.equal(await page.locator('.question-panel:not([inert])').count(), 0);
      assert.equal(await height(), before, 'reviewing answers holds height');
      await page.getByRole('button', {name: 'Previous question', exact: true}).click();
      assert.equal(await page.getByRole('button', {name: 'main', exact: true}).isVisible(), true);
      assert.equal(await page.getByRole('button', {name: 'Next question', exact: true}).isDisabled(), true);
      assert.equal(await height(), before, 'reopening answers holds height');
      await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1)),
        {t: 'ui-response', reqId: 3, answers: ['Small', 'topic/browser']});
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('a long closed question keeps its answer visible on a phone', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await page.setViewportSize({width: 375, height: 812});
    await requestRenderer(page);
    await page.evaluate(() => requestView.render({reqId: 5, questions: [
      {question: 'A very long question '.repeat(20), options: [{label: 'Chosen answer'}]},
      {question: 'Second?', options: [{label: 'Next'}]},
    ]}));
    await page.getByRole('button', {name: 'Chosen answer', exact: true}).click();
    const answer = page.locator('.question-summary-answer').first();
    assert.equal(await answer.isVisible(), true);
    assert.match(await answer.innerText(), /Chosen answer/);
    assert.ok((await answer.boundingBox()).width >= 100);
  } finally { await browser.close(); }
});

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
