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
    window.requestState = {owner: false};
    const send = frame => window.requestSent.push(frame);
    window.requestView = window.mevedelRequestView.create({requests, el, send, state: window.requestState});
  })();`});
}

test('plan cards render Markdown and expose full implementation instructions', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
    await requestRenderer(page);
    await page.evaluate(() => requestView.render({reqId: 1, bodyKind: 'markdown',
      body: '# Implementation plan\n\n## Instructions\n\nKeep **all checks** enabled.\n\n<script>unsafe()</script>', options: []}));
    assert.equal(await page.getByRole('heading', {name: 'Implementation plan', exact: true}).count(), 1);
    assert.equal(await page.locator('.request-card strong').innerText(), 'all checks');
    assert.equal(await page.locator('.request-card script').count(), 0);
  } finally { await browser.close(); }
});

test('only owners receive Worktree acceptance with a retained branch input', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1200]) {
      const page = await room(browser);
      await page.setViewportSize({width, height: 812});
      await requestRenderer(page);
      await page.evaluate(() => {
        window.planFrame = {reqId: 1, body: 'Worktree plan', optionsOwnerOnly: true,
          optionInput: {label: 'Worktree branch', value: 'plan/default'},
          options: [{id: 0, label: 'Implement plan'}], allowFeedback: true};
        requestView.render(planFrame);
      });
      assert.equal(await page.getByRole('button', {name: 'Implement plan', exact: true}).count(), 0);
      assert.equal(await page.getByRole('textbox', {name: 'Worktree branch', exact: true}).count(), 0);
      assert.equal(await page.getByText('Suggest a change', {exact: true}).count(), 1);
      await page.evaluate(() => { requestState.owner = true; requestView.render(planFrame); });
      const branch = page.getByRole('textbox', {name: 'Worktree branch', exact: true});
      assert.equal(await branch.inputValue(), 'plan/default');
      await branch.fill('plan/phone-choice');
      await page.evaluate(() => requestView.render({...planFrame, body: 'Updated settings'}));
      assert.equal(await branch.inputValue(), 'plan/phone-choice');
      assert.equal(await branch.evaluate(node => document.activeElement === node), true);
      await page.getByRole('button', {name: 'Implement plan', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1)),
        {t: 'ui-response', reqId: 1, option: 0, value: 'plan/phone-choice'});
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.close();
    }
  } finally { await browser.close(); }
});

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

test('multi-select keeps choices and custom text together without advancing or resizing', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1200]) {
      const page = await room(browser);
      await page.setViewportSize({width, height: 812});
      await requestRenderer(page);
      await page.evaluate(() => requestView.render({reqId: 7, questions: [
        {question: 'Which checks?', multiSelect: true, answer: [], options: [
          {label: 'Desktop', sample: 'Desktop checks'},
          {label: 'Phone', sample: 'Phone checks '.repeat(30)}]},
        {question: 'When?', options: [{label: 'Before shipping'}]},
        {question: 'Anything unknown?', multiSelect: true, answer: [], options: [{label: 'Uncertain'}]},
      ]}));
      await page.waitForFunction(() => document.querySelector('.questionnaire').style.height);
      const height = () => page.locator('[data-req-id="7"]').evaluate(node => node.getBoundingClientRect().height);
      const before = await height();
      const phone = page.getByRole('button', {name: 'Phone', exact: true});
      const desktop = page.getByRole('button', {name: 'Desktop', exact: true});
      await phone.click();
      assert.equal(await phone.isVisible(), true, 'multi choice stays on this question');
      assert.equal(await phone.getAttribute('aria-pressed'), 'true');
      await desktop.click();
      await phone.click();
      assert.equal(await phone.getAttribute('aria-pressed'), 'false');
      await phone.click();
      const custom = page.getByRole('textbox', {name: 'Custom answer 1'});
      await custom.fill('Keyboard');
      await custom.press('Enter');
      assert.equal(await desktop.isVisible(), true, 'multi custom stays on this question');
      assert.equal(await desktop.getAttribute('aria-pressed'), 'true');
      assert.equal(await phone.getAttribute('aria-pressed'), 'true');
      assert.equal(await height(), before);
      await page.getByRole('button', {name: 'Next question', exact: true}).click();
      assert.match(await page.locator('.question-summary-answer').first().innerText(), /Desktop, Phone, Keyboard/);
      await page.getByRole('button', {name: 'Before shipping', exact: true}).click();
      assert.equal(await height(), before);
      await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1)),
        {t: 'ui-response', reqId: 7, answers: [['Desktop', 'Phone', 'Keyboard'], 'Before shipping', []]});
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('Ask host updates preserve dirty answers and focused drafts but refresh untouched answers', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1200]) {
      const page = await room(browser);
      await page.setViewportSize({width, height: 812});
      await requestRenderer(page);
      await page.evaluate(() => {
        window.askFrame = {reqId: 8, questions: [
          {question: 'Checks?', multiSelect: true, answer: [], options: [{label: 'A'}, {label: 'B'}]},
          {question: 'When?', answer: '', options: [{label: 'Now'}, {label: 'Later'}]},
        ]};
        requestView.render(askFrame);
      });
      await page.getByRole('button', {name: 'A', exact: true}).click();
      const custom = page.getByRole('textbox', {name: 'Custom answer 1'});
      await custom.fill('My local draft');
      await custom.evaluate(node => node.setSelectionRange(3, 8));
      await page.evaluate(() => requestView.render({...askFrame, questions: askFrame.questions.map(
        (q, i) => ({...q, answer: i === 0 ? ['B'] : 'Later'}))}));
      assert.equal(await custom.inputValue(), 'My local draft');
      assert.deepEqual(await custom.evaluate(node => [document.activeElement === node,
        node.selectionStart, node.selectionEnd]), [true, 3, 8]);
      assert.equal(await page.getByRole('button', {name: 'A', exact: true}).getAttribute('aria-pressed'), 'true');
      await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1).answers), [['A', 'My local draft'], 'Later']);
      await page.getByRole('button', {name: 'Next question', exact: true}).click();
      await page.getByRole('textbox', {name: 'Custom answer 2'}).fill('Not yet');
      await page.getByRole('textbox', {name: 'Custom answer 2'}).fill('');
      await page.evaluate(() => requestView.render({...askFrame, questions: askFrame.questions.map(
        (q, i) => ({...q, answer: i === 0 ? [] : 'Now'}))}));
      assert.equal(await page.getByRole('textbox', {name: 'Custom answer 2'}).isVisible(), true);
      await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1).answers), [['A', 'My local draft'], '']);
      await page.getByRole('button', {name: 'Previous question', exact: true}).click();
      await page.getByRole('button', {name: 'A', exact: true}).click();
      await custom.fill('');
      await page.locator('#composer-input').fill('Independent composer draft');
      await page.evaluate(() => requestView.render({...askFrame, questions: askFrame.questions.map(
        (q, i) => ({...q, answer: i === 0 ? ['B'] : 'Later'}))}));
      assert.equal(await page.locator('#composer-input').evaluate(node => document.activeElement === node), true);
      await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1).answers), [[], '']);
      await custom.fill('Old question draft');
      await page.evaluate(() => requestView.render({...askFrame, questions: askFrame.questions.map(
        q => ({...q, question: `New ${q.question}`}))}));
      assert.equal(await custom.inputValue(), '');
      await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => requestSent.at(-1).answers), [[], '']);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('custom option commits survive further editing and whitespace stays unanswered', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await requestRenderer(page);
    await page.evaluate(() => requestView.render({reqId: 9, questions: [
      {question: 'Checks?', multiSelect: true, options: [{label: 'A'}]},
      {question: 'When?', options: [{label: 'Now'}]},
    ]}));
    const custom = page.getByRole('textbox', {name: 'Custom answer 1'});
    await custom.fill(' A ');
    await custom.press('Enter');
    assert.equal(await custom.inputValue(), '');
    await custom.fill('Other');
    assert.equal(await page.getByRole('button', {name: 'A', exact: true}).getAttribute('aria-pressed'), 'true');
    await page.getByRole('button', {name: 'Next question', exact: true}).click();
    const single = page.getByRole('textbox', {name: 'Custom answer 2'});
    await single.fill('   ');
    await single.press('Enter');
    assert.equal(await single.isVisible(), true, 'blank answer remains the unanswered question');
    await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
    assert.deepEqual(await page.evaluate(() => requestSent.at(-1).answers), [['A', 'Other'], '']);
    await single.fill('  Tomorrow  ');
    await page.getByRole('button', {name: 'Submit answers', exact: true}).click();
    assert.equal(await page.evaluate(() => requestSent.at(-1).answers[1]), 'Tomorrow');
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

// The room's sidebar and conversation with a turn holding a paragraph, a
// long code line and a table.
async function wideRoom(browser, width) {
  const page = await room(browser);
  await page.setViewportSize({width, height: 1300});
  await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
  await page.evaluate(() => {
    const box = document.getElementById('session-box');
    box.hidden = false;
    box.open = true;
    const turn = document.createElement('article');
    turn.className = 'turn ai';
    const rail = document.createElement('div');
    rail.className = 'rail';
    const content = document.createElement('div');
    content.className = 'content';
    content.append(window.mevedelTranscriptRenderer.renderMarkdown([
      'A paragraph keeps its reading measure. '.repeat(12), '',
      '```', 'GET /api/v1/indicators?region=TZ-07&from=2019-01 '.repeat(8), '```', '',
      '| Block | Before | After |', '| --- | --- | --- |', '| Request context | release_id, region | region |',
    ].join('\n')));
    turn.append(rail, content);
    document.getElementById('transcript').replaceChildren(turn);
  });
  return page;
}
const box = (page, selector) => page.locator(selector).evaluate(node => {
  const {left, right, width} = node.getBoundingClientRect();
  return {left: Math.round(left), right: Math.round(right), width: Math.round(width)};
});

test('a wide room keeps its sidebar at the edge and gives blocks the column', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [2560, 1400]) {
      const page = await wideRoom(browser, width);
      const side = await box(page, '#session-box');
      const ledger = await box(page, '#transcript');
      assert.equal(side.right, width, `sidebar at the right edge at ${width}`);
      assert.equal(side.width, width >= 1600 ? 360 : 304);
      assert.ok(ledger.right <= side.left, 'the conversation stays left of the sidebar');
      assert.equal(ledger.width, Math.min(1148, side.left - 64), `column width at ${width}`);
      const paragraph = await box(page, '.prose p');
      const code = await box(page, '.codeblock');
      assert.ok(paragraph.width < 720, 'prose keeps its measure');
      assert.equal(code.width, ledger.width - 48 - 44, 'code takes the column beside the rail');
      assert.equal((await box(page, '.tablewrap')).width, code.width);
      assert.equal((await box(page, '#composer')).width, ledger.width - 48);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('a docked item leaves the chat, its decisions and a room drawer reachable', async () => {
  const browser = await chromium.launch();
  try {
    const page = await wideRoom(browser, 2560);
    await page.evaluate(() => {
      document.getElementById('room-button').hidden = false;
      const panel = document.getElementById('editing-panel');
      panel.hidden = false;
      panel.classList.add('docked');
      // The room hands In this room to the header's button while docked.
      document.getElementById('session-box').open = false;
    });
    const panel = await box(page, '#editing-panel');
    assert.deepEqual([panel.left, panel.right], [860, 2560]);
    assert.ok((await box(page, '#composer')).right <= 860, 'the chat stays beside the item');
    assert.equal(await reachable(page, '#composer'), true);
    await ask(page);
    assert.equal(await reachable(page, '#card button'), true);
    assert.equal(await page.locator('#session-box').isVisible(), false);
    assert.equal(await reachable(page, '#room-button'), true);
    assert.equal(await page.locator('#editing-panel .wordmark').isVisible(), false);
    await page.evaluate(() => { document.getElementById('session-box').open = true; });
    const drawer = await box(page, '#session-box');
    assert.deepEqual([drawer.left, drawer.right], [2200, 2560]);
    assert.equal(await reachable(page, '#session-box > summary'), true, 'the drawer is over the item');
    await page.evaluate(() => document.getElementById('editing-panel').classList.remove('docked'));
    assert.equal(await reachable(page, '#composer'), false, 'undocked, the panel is full screen again');
    assert.equal(await reachable(page, '#card button'), true, 'its decisions stay above it');
  } finally { await browser.close(); }
});

test('store refresh keeps the open artifact menu actionable', async () => {
  const browser = await chromium.launch();
  try {
    const page = await room(browser);
    await page.addScriptTag({content: await readFile(
      resolve(root, 'relay/viewer/viewer-nav.js'), 'utf8')});
    await page.addScriptTag({content: await readFile(
      resolve(root, 'relay/viewer/viewer-store.js'), 'utf8')});
    await page.evaluate(() => {
      window.storeSent = [];
      window.storeRows = {artifacts: [{id: 'page', title: 'Page', kind: 'html',
        artifact: 'page/index.html', versions: 1, modified: 1, attached: true}]};
      window.mevedelTranscriptRenderer = {formatBytes: bytes => `${bytes} B`};
      window.storeView = window.mevedelStoreView.create({
        href: destination => '#room.secret?artifact=' + encodeURIComponent(destination.artifact.id),
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
