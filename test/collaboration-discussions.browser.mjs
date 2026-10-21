/* Artifact discussion picker and viewer controller in a real browser.
 *
 * A static page hosts the viewer-side controller and a sandboxed artifact
 * frame built the way the artifact panel builds it, then real pointer input
 * picks words, elements, text selections and boxes inside the frame.
 *
 * Run: node --test test/collaboration-discussions.browser.mjs
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
const viewer = name => readFile(resolve(root, 'relay/viewer', name), 'utf8');
const kit = await viewer('viewer-artifact-discussions.js');
const sidebar = await viewer('viewer-discussions.js');
const markdown = await viewer('markdown.js');
const renderer = await viewer('renderer.js');

const ARTIFACT = `<!doctype html><html><head><style>
  body{font:16px/1.5 sans-serif;margin:24px;width:640px}
  .cards{display:flex;gap:16px;margin-top:24px}
  .card{width:120px;height:80px;border:1px solid #999;padding:8px}
  #go{margin-top:24px}
</style></head><body>
  <h1>SNT Schema v2</h1>
  <h2>Decisions at a glance</h2>
  <p id="lead">Every table carries the country, so countries share the same tables and app code.</p>
  <div class="cards"><div class="card">213,150 rows</div><div class="card">42 columns</div><div class="card">637 simulations</div></div>
  <button id="go" onclick="document.title='clicked'">Run</button>
</body></html>`;

// The rewritten artifact the model might produce next: an inserted
// paragraph shifts every selector, and the lead sentence changes slightly.
const REWRITTEN = ARTIFACT
  .replace('<h2>Decisions at a glance</h2>',
           '<p>A new introduction paragraph.</p><h2>Decisions at a glance</h2>')
  .replace('and app code.', 'and the app code.');

const HOST = `<!doctype html><html><head><style>
  .artifact-main{display:flex;width:900px;height:560px}
  #artifact-body{position:relative;width:520px;height:560px;overflow:auto}
  .artifact-frame{border:0;width:500px;height:540px;display:block}
  .artifact-side{width:380px;display:flex;flex-direction:column}
  .artifact-peek{position:absolute;background:#fff;border:1px solid #333}
  [hidden]{display:none!important}
</style></head><body>
  <button id="artifact-discussion" hidden>Discuss</button>
  <button id="artifact-discussions" hidden aria-expanded="false">Discussions<span id="artifact-discussion-count" hidden></span></button>
  <div class="artifact-main">
    <div id="artifact-body"></div>
    <div id="artifact-peek" class="artifact-peek" hidden></div>
    <aside id="artifact-side" class="artifact-side" hidden>
      <button id="artifact-side-close">×</button>
      <div id="artifact-discussion-sidebar" class="discussion-sidebar"></div>
    </aside>
  </div>
</body></html>`;

async function setup(page) {
  await page.setContent(HOST);
  for (const script of [markdown, renderer, sidebar, kit]) await page.addScriptTag({content: script});
  await page.evaluate(() => {
    window.sent = [];
    window.openedConversations = [];
    window.located = [];
    addEventListener('message', event => {
      if (event.data && event.data.mevedelDiscussion === 'located') window.located.push(event.data);
    });
    const el = (tag, className, text) => {
      const node = document.createElement(tag);
      if (className) node.className = className;
      if (typeof text === 'string') node.textContent = text;
      return node;
    };
    // A small stand-in for the host's discussion store: it answers each
    // action and broadcasts the stored list, as the host does.
    window.store = [];
    window.failNext = null;
    const answer = frame => {
      const reply = {t: 'discussion', reqId: frame.reqId};
      const message = () => ({id: frame.messageId, actor: 'Alice', text: frame.text || '',
                              ...(frame.text ? {} : {alone: true})});
      if (window.failNext) {
        reply.error = window.failNext;
        window.failNext = null;
      } else if (frame.action === 'list') {
        reply.discussions = window.store;
      } else if (frame.action === 'history') {
        reply.conversation = [];
        reply.conversationDecision = window.decision || null;
      } else if (frame.action === 'post') {
        const id = frame.target.kind === 'whole' ? 'general' : frame.discussionId;
        const known = window.store.find(d => d.id === id);
        if (known) known.messages.push(message());
        else window.store.push({id, target: frame.target, quote: '', messages: [message()], resolved: false});
      } else if (frame.action === 'reply') {
        window.store.find(d => d.id === frame.discussionId).messages.push(message());
      } else if (frame.action === 'resolve') {
        window.store.find(d => d.id === frame.discussionId).resolved = frame.resolved;
      }
      if (!reply.error && frame.action !== 'list' && frame.action !== 'ask') {
        window.controller.stored({item: 'artifact:schema.html',
                                  discussions: JSON.parse(JSON.stringify(window.store))});
      }
      window.controller.handle(reply);
    };
    const $ = id => document.getElementById(id);
    window.controller = window.mevedelArtifactDiscussions.create({
      send: frame => {
        window.sent.push(frame);
        setTimeout(() => answer(frame), 0);
        return Promise.resolve(true);
      },
      el, toggle: $('artifact-discussion'), sidebarToggle: $('artifact-discussions'),
      side: $('artifact-side'), sideRoot: $('artifact-discussion-sidebar'),
      count: $('artifact-discussion-count'), close: $('artifact-side-close'), peek: $('artifact-peek'),
      reveal: id => { window.revealed = id; }, canDiscuss: () => window.writable !== false,
      openConversation: id => window.openedConversations.push(id),
      busy: () => window.busy === true,
    });
    window.show = (html, id) => {
      const body = document.getElementById('artifact-body');
      body.querySelectorAll('iframe').forEach(frame => frame.remove());
      const frame = document.createElement('iframe');
      frame.setAttribute('sandbox', 'allow-scripts');
      frame.className = 'artifact-frame';
      frame.srcdoc = '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; '
        + 'style-src \'unsafe-inline\'; script-src \'unsafe-inline\'">'
        + window.mevedelArtifactDiscussions.script() + html;
      body.append(frame);
      window.controller.attach(frame, id, 'schema.html');
    };
  });
}

async function artifactFrame(page) {
  let frame;
  for (let tries = 0; tries < 100 && !frame; tries++) {
    frame = page.frames().find(candidate => candidate !== page.mainFrame());
    if (!frame) await page.waitForTimeout(20);
  }
  await frame.waitForLoadState();
  await frame.waitForFunction(() => document.readyState === 'complete');
  return frame;
}

// Page coordinates of a rectangle the frame reports in its own viewport.
async function inPage(page, frame, rectInFrame) {
  const offset = await page.evaluate(() => {
    const box = document.querySelector('iframe').getBoundingClientRect();
    return {left: box.left, top: box.top};
  });
  return {x: offset.left + rectInFrame.left, y: offset.top + rectInFrame.top,
          width: rectInFrame.width, height: rectInFrame.height};
}

async function wordRect(frame, word) {
  return frame.evaluate(target => {
    const node = document.getElementById('lead').firstChild;
    const start = node.data.indexOf(target);
    const range = document.createRange();
    range.setStart(node, start);
    range.setEnd(node, start + target.length);
    const r = range.getBoundingClientRect();
    return {left: r.left, top: r.top, width: r.width, height: r.height};
  }, word);
}

const actions = (page, action) => page.evaluate(
  name => window.sent.filter(frame => frame.action === name), action);

async function commentMode(page) {
  await page.click('#artifact-discussion');
  await page.waitForFunction(() =>
    document.getElementById('artifact-discussion').getAttribute('aria-pressed') === 'true');
  // The mode message reaches the frame asynchronously.
  await page.waitForTimeout(50);
}

// The sidebar's views: the list, and one discussion or a new one.
const fresh = '#artifact-side .ds-thread:not([hidden])';
const subject = page => page.textContent(`${fresh} .ds-subject-title`);
const target = page => page.textContent(`${fresh} .ds-target`);
const start = page => page.click(`${fresh} .ds-send`);

test('a dedicated decision stays explicit, with no automatic conversation opening', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 812}});
      const errors = [];
      page.on('pageerror', error => errors.push(String(error)));
      await setup(page);
      await page.evaluate(() => {
        window.decision = {pending: true, title: 'Architecture review'};
        window.store.push({id: 'general', target: {kind: 'whole'}, messages: [
          {id: 'message-1', actor: 'Alice', text: 'Review this page'}]});
      });
      await page.evaluate(html => window.show(html, 'tool-1'), ARTIFACT);
      await artifactFrame(page);
      await page.click('#artifact-discussions');
      await page.click('.ds-item[data-discussion-id="general"]');
      await page.waitForSelector('.ds-status >> text=Waiting for a decision in Architecture review');
      assert.deepEqual(await page.evaluate(() => window.openedConversations), []);
      assert.equal((await actions(page, 'conversation')).length, 0);
      await page.getByRole('button', {name: 'Open conversation', exact: true}).click();
      assert.deepEqual(await page.evaluate(() => window.openedConversations), ['schema.html']);
      await page.evaluate(() => { window.writable = false; window.controller.activity(); });
      assert.equal(await page.getByRole('button', {name: 'Open conversation', exact: true}).count(), 0);
      assert.match(await page.textContent('.ds-status'), /Waiting for a decision/);
      assert.deepEqual(errors, []);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('artifact discussion rows show queued and working state without losing drafts', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 812}});
      await setup(page);
      await page.evaluate(html => window.show(html, 'tool-1'), ARTIFACT);
      await artifactFrame(page);
      await page.waitForFunction(() => window.sent.some(frame => frame.action === 'list'));
      await page.evaluate(() => {
        window.store = [{id: 'general', target: {kind: 'whole'}, messages: [
          {id: 'message-1', actor: 'Alice', text: 'Review this page'},
        ]}];
        window.controller.stored({item: 'artifact:schema.html', discussions: window.store});
        window.controller.queue([{shared: {kind: 'artifact', itemId: 'artifact:schema.html',
          discussionId: 'general', questionId: 'question-1'}}]);
      });
      await page.click('#artifact-discussions');
      assert.equal(await page.textContent('.ds-item .ds-state'), 'Queued for the assistant');
      await page.click('.ds-item');
      await page.fill('.ds-input', '> draft\nkeep this');
      await page.evaluate(() => {
        window.controller.queue([]);
        window.controller.records([{id: 'u1', kind: 'user', shared: {
          kind: 'artifact', itemId: 'artifact:schema.html', discussionId: 'general',
          questionId: 'question-1', messageId: 'message-1'}},
          {id: 'a1', kind: 'assistant', text: 'Reviewing the page'}]);
        window.busy = true;
        window.controller.activity();
      });
      assert.equal(await page.inputValue('.ds-input'), '> draft\nkeep this');
      await page.click('.ds-back');
      assert.equal(await page.textContent('.ds-item .ds-state'), 'Assistant working');
      await page.evaluate(() => { window.busy = false; window.controller.activity(); });
      assert.equal(await page.locator('.ds-item .ds-state').count(), 0);
      assert.match(await page.textContent('.ds-item .ds-snip'), /Reviewing the page/);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('artifact discussions: pick, send, marker thread, box, selection, relocation',
     {timeout: 120000}, async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({viewport: {width: 960, height: 700}});
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    await setup(page);
    await page.evaluate(html => window.show(html, 'tool-1'), ARTIFACT);
    const frame = await artifactFrame(page);
    assert.equal(await page.isVisible('#artifact-discussion'), true);
    assert.equal(await page.isVisible('#artifact-discussions'), true);
    assert.equal(await page.isVisible('#artifact-side'), false, 'the sidebar opens on demand');

    // Outside comment mode the page behaves normally.
    await frame.click('#go');
    assert.equal(await frame.evaluate(() => document.title), 'clicked');
    await frame.evaluate(() => { document.title = ''; });

    // The artifact cannot start a discussion on its own, and messages from
    // any other window are not heard at all.
    const forged = {mevedelDiscussion: 'pick', anchor: {selector: '#lead', label: 'Forged'},
                    rect: {left: 0, top: 0, width: 10, height: 10}, context: {}};
    await frame.evaluate(message => parent.postMessage(message, '*'), forged);
    await page.waitForTimeout(50);
    assert.equal(await page.isVisible('#artifact-side'), false);
    await page.click('#artifact-discussion');
    await page.evaluate(message => window.postMessage(message, '*'), forged);
    await page.waitForTimeout(50);
    assert.equal(await page.isVisible('#artifact-side'), false);
    await page.click('#artifact-discussion');
    assert.equal(await page.getAttribute('#artifact-discussion', 'aria-pressed'), 'false');

    // A click on a word picks the word and opens a new discussion in the
    // sidebar; the page's own handlers never see clicks while commenting.
    await commentMode(page);
    const share = await inPage(page, frame, await wordRect(frame, 'share'));
    await page.mouse.move(share.x + share.width / 2, share.y + share.height / 2);
    await page.mouse.down();
    await page.mouse.up();
    await page.waitForSelector(fresh);
    assert.equal(await subject(page), 'New discussion');
    assert.equal(await target(page), 'Decisions at a glance › word "share"');
    // Posting sends to the assistant by default: one action.
    assert.equal(await page.isChecked(`${fresh} .ds-choice input`), true);
    assert.equal(await page.evaluate(() => document.activeElement.className), 'ds-input');
    await page.fill(`${fresh} .ds-input`, 'Say who shares them');
    await page.keyboard.press('Control+Enter');
    await page.waitForFunction(() => document.querySelector(
      '#artifact-side .ds-thread:not([hidden]) .ds-subject-title')?.textContent !== 'New discussion');
    const [sent] = await actions(page, 'post');
    assert.equal(sent.t, 'discussion');
    assert.equal(sent.item, 'tool-1');
    assert.equal(sent.text, 'Say who shares them');
    assert.equal(sent.toAssistant, true);
    assert.ok(sent.messageId && sent.messageId !== sent.discussionId);
    assert.equal(sent.target.kind, 'word');
    assert.equal(sent.target.quote, 'share');
    assert.equal(sent.target.selector, '#lead');
    assert.match(sent.target.sig.h, /^[0-9a-f]{16}$/);
    assert.match(sent.context.text, /countries share the same tables/);
    assert.match(sent.context.html, /^<p id="lead">/);
    assert.equal(await page.getAttribute('#artifact-discussion', 'aria-pressed'), 'false');
    assert.equal(await subject(page), 'Decisions at a glance › word "share"',
                 'the started discussion stays open');
    await frame.click('#go');
    assert.equal(await frame.evaluate(() => document.title), 'clicked');

    // Once the assistant answered in the transcript, the open discussion
    // shows the answer beneath its message.
    await page.evaluate(([id, messageId]) => window.controller.records([
      {id: 'u1', kind: 'user', guest: 'Alice',
       shared: {kind: 'artifact', title: 'schema.html', questionId: 'q1', discussionId: id,
                messageId, itemId: 'artifact:schema.html', text: 'Say who shares them'}},
      {id: 'a1', kind: 'assistant', text: 'Named the country teams.'},
    ]), [sent.discussionId, sent.messageId]);
    await page.waitForSelector(`${fresh} .ds-message .ds-answers >> text=Named the country teams.`);
    await page.waitForFunction(id => window.located.some(entry => entry.found.includes(id)),
                               sent.discussionId);

    // Hovering a marker peeks at its latest message; clicking opens it.
    await page.click('#artifact-side-close');
    await page.mouse.move(share.x + 6, share.y - 12);
    await page.waitForSelector('#artifact-peek:not([hidden])');
    assert.match(await page.textContent('#artifact-peek'), /Alice.*Say who shares them/s);
    await page.mouse.down();
    await page.mouse.up();
    await page.waitForSelector(fresh);
    assert.equal(await subject(page), 'Decisions at a glance › word "share"');
    // An unticked reply is for people.
    await page.fill(`${fresh} .ds-input`, 'Only the regional teams');
    await page.uncheck(`${fresh} .ds-choice input`);
    await start(page);
    await page.waitForSelector(`${fresh} .ds-message >> text=Only the regional teams`);
    const [reply] = await actions(page, 'reply');
    assert.equal(reply.discussionId, sent.discussionId);
    assert.equal(reply.toAssistant, false);
    await page.click(`${fresh} .ds-menu > summary`);
    await page.click(`${fresh} .ds-menu-body >> text=Show in chat`);
    assert.equal(await page.evaluate(() => window.revealed), 'u1');

    // A box around the three cards names the area and its elements.
    await commentMode(page);
    const cards = await frame.evaluate(() => {
      const r = document.querySelector('.cards').getBoundingClientRect();
      return {left: r.left - 6, top: r.top - 6, width: r.width + 12, height: r.height + 12};
    });
    const area = await inPage(page, frame, cards);
    await page.mouse.move(area.x, area.y);
    await page.mouse.down();
    await page.mouse.move(area.x + area.width / 2, area.y + area.height / 2, {steps: 4});
    await page.mouse.move(area.x + area.width, area.y + area.height, {steps: 4});
    await page.mouse.up();
    await page.waitForFunction(() => document.querySelector(
      '#artifact-side .ds-thread:not([hidden]) .ds-target')?.textContent.includes('area'));
    assert.equal(await target(page), 'Decisions at a glance › area · 3 elements');
    await page.fill(`${fresh} .ds-input`, 'Make these a row of four');
    await page.evaluate(() => { window.failNext = 'The session cannot accept a message right now'; });
    await start(page);
    await page.waitForFunction(() =>
      document.querySelector('#artifact-side .ds-notice')?.textContent
        === 'The session cannot accept a message right now');
    assert.equal(await subject(page), 'New discussion', 'a failed post keeps the new discussion');
    assert.equal(await page.inputValue(`${fresh} .ds-input`), 'Make these a row of four');
    const [boxed] = await actions(page, 'post').then(list => list.slice(-1));
    assert.equal(boxed.target.kind, 'box');
    assert.equal(boxed.target.count, 3);
    assert.ok(boxed.target.region.x1 > boxed.target.region.x0);
    assert.match(boxed.context.text, /213,150 rows.*42 columns.*637 simulations/);

    // A box drawn a little past the grid and over two of its cards means
    // those two cards, not the grid as the page's one covered child. A new
    // pick replaces the new discussion's subject.
    const loose = await frame.evaluate(() => {
      const r = document.querySelector('.cards').getBoundingClientRect();
      const second = document.querySelectorAll('.card')[1].getBoundingClientRect();
      return {left: r.left - 14, top: r.top - 14, width: second.right + 4 - (r.left - 14),
              height: r.height + 28};
    });
    const looseArea = await inPage(page, frame, loose);
    await page.mouse.move(looseArea.x, looseArea.y);
    await page.mouse.down();
    await page.mouse.move(looseArea.x + looseArea.width, looseArea.y + looseArea.height,
                          {steps: 6});
    await page.mouse.up();
    await page.waitForFunction(() => document.querySelector(
      '#artifact-side .ds-thread:not([hidden]) .ds-target')?.textContent.includes('2 elements'));
    assert.equal(await target(page), 'Decisions at a glance › area · 2 elements');

    // A tight box around a wide heading's words names that heading.
    const title = await frame.evaluate(() => {
      const range = document.createRange();
      range.selectNodeContents(document.querySelector('h1'));
      const r = range.getBoundingClientRect();
      return {left: r.left - 4, top: r.top - 4, width: r.width + 8, height: r.height + 8};
    });
    const titleArea = await inPage(page, frame, title);
    await page.mouse.move(titleArea.x, titleArea.y);
    await page.mouse.down();
    await page.mouse.move(titleArea.x + titleArea.width, titleArea.y + titleArea.height,
                          {steps: 6});
    await page.mouse.up();
    await page.waitForFunction(() => document.querySelector(
      '#artifact-side .ds-thread:not([hidden]) .ds-target')?.textContent.startsWith('heading'));
    assert.equal(await target(page), 'heading "SNT Schema v2"');
    // Without words, the selection itself is the request, but only for the
    // assistant: a note between people needs a message.
    await page.fill(`${fresh} .ds-input`, '');
    await page.uncheck(`${fresh} .ds-choice input`);
    await start(page);
    assert.equal(await page.textContent('#artifact-side .ds-notice'),
                 'Write a message, or ask the assistant.');
    await page.check(`${fresh} .ds-choice input`);
    await start(page);
    await page.waitForFunction(() => document.querySelector(
      '#artifact-side .ds-thread:not([hidden]) .ds-subject-title')?.textContent === 'heading "SNT Schema v2"');
    const [alone] = await actions(page, 'post').then(list => list.slice(-1));
    assert.equal(alone.text, '');
    assert.equal(alone.toAssistant, true);
    assert.equal(alone.target.label, 'heading "SNT Schema v2"');
    assert.match(await page.textContent(`${fresh} .ds-text.alone`), /Sent the selection as the request/);
    await commentMode(page);

    // Dragging across text keeps native selection and sends the quote.
    // Escape drops the new discussion and keeps comment mode on.
    assert.equal(await page.getAttribute('#artifact-discussion', 'aria-pressed'), 'true');
    const every = await inPage(page, frame, await wordRect(frame, 'Every table'));
    await page.mouse.move(every.x + 1, every.y + every.height / 2);
    await page.mouse.down();
    await page.mouse.move(every.x + every.width - 1, every.y + every.height / 2, {steps: 6});
    await page.mouse.up();
    await page.waitForFunction(() => document.querySelector(
      '#artifact-side .ds-thread:not([hidden]) .ds-subject-title')?.textContent === 'New discussion');
    assert.match(await page.textContent(`${fresh} .ds-quote`), /^Every tabl/);
    await page.keyboard.press('Escape');
    await page.waitForSelector('#artifact-side .ds-list:not([hidden])');
    assert.equal(await page.getAttribute('#artifact-discussion', 'aria-pressed'), 'true');
    await page.click('#artifact-discussion');

    // The list: the whole artifact first, then the open discussions.
    const rows = await page.$$eval('#artifact-side .ds-item .ds-title', nodes => nodes.map(n => n.textContent));
    assert.deepEqual(rows, ['Whole artifact', 'Decisions at a glance › word "share"', 'heading "SNT Schema v2"']);
    assert.equal(await page.textContent('#artifact-discussion-count'), '2');

    // A rewritten artifact still places the marker: the selector changed
    // meaning, but the text fingerprint finds the paragraph again.
    await page.evaluate(() => { window.located = []; });
    await page.evaluate(html => window.show(html, 'tool-2'), REWRITTEN);
    await artifactFrame(page);
    await page.waitForFunction(id => window.located.some(entry => entry.found.includes(id)),
                               sent.discussionId);
    // Content that no longer exists drops its marker.
    await page.evaluate(() => { window.located = []; });
    await page.evaluate(() => window.show('<p>Entirely different page content.</p>', 'tool-3'));
    await artifactFrame(page);
    await page.waitForFunction(id => window.located.some(entry => entry.missing.includes(id)),
                               sent.discussionId);
    // Resolving takes the marker away for everyone and folds the discussion
    // into the list's resolved row.
    await page.evaluate(html => window.show(html, 'tool-4'), ARTIFACT);
    await artifactFrame(page);
    await page.waitForFunction(id => window.located.some(entry => entry.found.includes(id)),
                               sent.discussionId);
    await page.evaluate(() => { window.located = []; });
    const again = await inPage(page, await artifactFrame(page),
                               await wordRect(await artifactFrame(page), 'share'));
    await page.mouse.move(again.x + 6, again.y - 12);
    await page.mouse.down();
    await page.mouse.up();
    await page.waitForSelector(fresh);
    await page.click(`${fresh} .ds-menu > summary`);
    await page.click(`${fresh} .ds-menu-body >> text=Resolve`);
    await page.waitForSelector(`${fresh} .ds-closed >> text=Reopen to reply`);
    assert.equal((await actions(page, 'resolve'))[0].resolved, true);
    assert.equal(await page.textContent('#artifact-discussion-count'), '1',
                 'the selection sent alone is still open');
    await page.click(`${fresh} .ds-back`);
    assert.equal(await page.textContent('#artifact-side .ds-resolved > summary'), 'Resolved · 1');
    assert.deepEqual(errors, []);
  } finally {
    await browser.close();
  }
});

test('the whole artifact keeps its answers apart and a draft through streaming', async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({viewport: {width: 960, height: 700}});
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    await setup(page);
    await page.evaluate(html => window.show(html, 'tool-1'), ARTIFACT);
    await artifactFrame(page);
    await page.click('#artifact-discussions');
    await page.click('#artifact-side .ds-item.general');
    assert.equal(await subject(page), 'Whole artifact');
    await page.fill(`${fresh} .ds-input`, 'Shorten the intro');
    await start(page);
    await page.waitForSelector(`${fresh} .ds-message >> text=Shorten the intro`);
    const [posted] = await actions(page, 'post');
    assert.deepEqual(posted.target, {kind: 'whole'});
    await page.evaluate(() => window.controller.records([
      {id: 'u1', kind: 'user', shared: {kind: 'artifact', itemId: 'artifact:other',
        discussionId: 'general', questionId: 'q1'}},
      {id: 'a1', kind: 'assistant', text: 'OTHER ARTIFACT ANSWER'},
    ]));
    assert.doesNotMatch(await page.textContent(fresh), /OTHER ARTIFACT ANSWER/);
    await page.fill(`${fresh} .ds-input`, '> draft\nsecond line');
    await page.uncheck(`${fresh} .ds-choice input`);
    await page.focus(`${fresh} .ds-input`);
    await page.evaluate(messageId => window.controller.records([
      {id: 'u2', kind: 'user', shared: {kind: 'artifact', itemId: 'artifact:schema.html',
        discussionId: 'general', questionId: 'q2', messageId}},
      {id: 'a2', kind: 'assistant', text: 'THIS ARTIFACT ANSWER'},
    ]), posted.messageId);
    assert.match(await page.textContent(`${fresh} .ds-answers`), /THIS ARTIFACT ANSWER/);
    assert.equal(await page.inputValue(`${fresh} .ds-input`), '> draft\nsecond line');
    assert.equal(await page.isChecked(`${fresh} .ds-choice input`), false);
    assert.equal(await page.evaluate(() => document.activeElement.matches('.ds-input')), true);
    assert.deepEqual(errors, []);
  } finally {
    await browser.close();
  }
});

test('an answer to a discussion not on screen marks it, never takes the reader away', async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({viewport: {width: 960, height: 700}});
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    await setup(page);
    await page.evaluate(() => {
      window.store.push(
        {id: 'one', target: {kind: 'element', selector: '#lead', label: 'paragraph'},
         messages: [{id: 'm1', actor: 'Guest: Bob', text: 'Who shares them?'}], resolved: false},
        {id: 'two', target: {kind: 'element', selector: 'h1', label: 'heading'},
         messages: [{id: 'm2', actor: 'Alice', text: 'Shorter title?'}], resolved: false});
    });
    await page.evaluate(html => window.show(html, 'tool-1'), ARTIFACT);
    const frame = await artifactFrame(page);
    const history = [{id: 'u1', kind: 'user', shared: {kind: 'artifact', itemId: 'artifact:schema.html',
      discussionId: 'one', questionId: 'q1', messageId: 'm1'}}];
    await page.evaluate(records => window.controller.records(records), history);
    await page.click('#artifact-discussions');
    await page.waitForSelector('#artifact-side .ds-item[data-discussion-id="two"]');
    assert.equal(await page.$$eval('#artifact-side .ds-item.unread', nodes => nodes.length), 0,
                 'history loaded with the artifact marks nothing');
    await page.click('#artifact-side .ds-item[data-discussion-id="two"]');
    await page.fill(`${fresh} .ds-input`, 'Typing here');
    await page.evaluate(records => window.controller.records(records),
                        [...history, {id: 'a1', kind: 'assistant', text: 'The regional teams.'}]);
    assert.equal(await subject(page), 'heading');
    assert.equal(await page.inputValue(`${fresh} .ds-input`), 'Typing here');
    assert.match(await page.textContent(`${fresh} .ds-back`), /1 new/);
    assert.equal(await page.evaluate(() =>
      document.getElementById('artifact-discussions').classList.contains('unread')), true);
    await frame.waitForFunction(() => {
      const layer = document.querySelector('mevedel-discussion-layer');
      return !!layer;
    });
    await page.click(`${fresh} .ds-back`);
    const row = '#artifact-side .ds-item[data-discussion-id="one"]';
    assert.match(await page.getAttribute(row, 'class'), /unread/);
    assert.match(await page.textContent(row), /New answer.*Assistant: The regional teams\./s);
    await page.click(row);
    await page.click(`${fresh} .ds-back`);
    assert.doesNotMatch(await page.getAttribute(row, 'class'), /unread/, 'opening clears the mark');
    assert.equal(await page.evaluate(() =>
      document.getElementById('artifact-discussions').classList.contains('unread')), false);
    assert.deepEqual(errors, []);
  } finally {
    await browser.close();
  }
});
