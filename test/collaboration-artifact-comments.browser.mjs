/* Artifact comment picker and viewer controller in a real browser.
 *
 * A static page hosts the viewer-side controller and a sandboxed artifact
 * frame built the way the artifact panel builds it, then real pointer input
 * picks words, elements, text selections and boxes inside the frame.
 *
 * Run: node --test test/collaboration-artifact-comments.browser.mjs
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
const kit = await readFile(resolve(root, 'relay/viewer/viewer-artifact-comments.js'), 'utf8');

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
  #artifact-body{position:relative;width:760px;height:560px;overflow:auto}
  .artifact-frame{border:0;width:740px;height:540px;display:block}
  .artifact-comment-composer,.artifact-comment-card{position:absolute;background:#fff;border:1px solid #333}
</style></head><body>
  <button id="artifact-comment" hidden>Comment</button>
  <div id="artifact-body"></div>
</body></html>`;

async function setup(page) {
  await page.setContent(HOST);
  await page.addScriptTag({content: kit});
  await page.evaluate(() => {
    window.sent = [];
    window.located = [];
    addEventListener('message', event => {
      if (event.data && event.data.mevedelComment === 'located') window.located.push(event.data);
    });
    const el = (tag, className, text) => {
      const node = document.createElement(tag);
      if (className) node.className = className;
      if (typeof text === 'string') node.textContent = text;
      return node;
    };
    window.controller = window.mevedelArtifactComments.create({
      send: frame => { window.sent.push(frame); return Promise.resolve(true); },
      el, body: document.getElementById('artifact-body'),
      toggle: document.getElementById('artifact-comment'),
      flash: () => {}, renderMarkdown: text => el('div', '', text),
      reveal: id => { window.revealed = id; }, canComment: () => true,
    });
    window.show = (html, id) => {
      const body = document.getElementById('artifact-body');
      body.querySelectorAll('iframe').forEach(frame => frame.remove());
      const frame = document.createElement('iframe');
      frame.setAttribute('sandbox', 'allow-scripts');
      frame.className = 'artifact-frame';
      frame.srcdoc = '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; '
        + 'style-src \'unsafe-inline\'; script-src \'unsafe-inline\'">'
        + window.mevedelArtifactComments.script() + html;
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

async function commentMode(page) {
  await page.click('#artifact-comment');
  await page.waitForFunction(() =>
    document.getElementById('artifact-comment').getAttribute('aria-pressed') === 'true');
  // The mode message reaches the frame asynchronously.
  await page.waitForTimeout(50);
}

test('artifact comments: pick, send, marker thread, box, selection, relocation',
     {timeout: 120000}, async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({viewport: {width: 900, height: 700}});
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    await setup(page);
    await page.evaluate(html => window.show(html, 'tool-1'), ARTIFACT);
    const frame = await artifactFrame(page);
    assert.equal(await page.isVisible('#artifact-comment'), true);

    // Outside comment mode the page behaves normally.
    await frame.click('#go');
    assert.equal(await frame.evaluate(() => document.title), 'clicked');
    await frame.evaluate(() => { document.title = ''; });

    // The artifact cannot open a composer on its own, and messages from
    // any other window are not heard at all.
    const forged = {mevedelComment: 'pick', anchor: {selector: '#lead', label: 'Forged'},
                    rect: {left: 0, top: 0, width: 10, height: 10}, context: {}};
    await frame.evaluate(message => parent.postMessage(message, '*'), forged);
    await page.waitForTimeout(50);
    assert.equal(await page.$('.artifact-comment-composer'), null);
    await page.click('#artifact-comment');
    await page.evaluate(message => window.postMessage(message, '*'), forged);
    await page.waitForTimeout(50);
    assert.equal(await page.$('.artifact-comment-composer'), null);
    await page.click('#artifact-comment');
    assert.equal(await page.getAttribute('#artifact-comment', 'aria-pressed'), 'false');

    // A click on a word picks the word, and the page's own handlers
    // never see clicks while commenting.
    await commentMode(page);
    const share = await inPage(page, frame, await wordRect(frame, 'share'));
    await page.mouse.move(share.x + share.width / 2, share.y + share.height / 2);
    await page.mouse.down();
    await page.mouse.up();
    await page.waitForSelector('.artifact-comment-composer');
    const label = await page.textContent('.artifact-comment-target');
    assert.equal(label, 'Decisions at a glance › word "share"');
    await page.fill('.artifact-comment-input', 'Say who shares them');
    await page.keyboard.press('Control+Enter');
    await page.waitForFunction(() => window.sent.length === 1);
    const sent = await page.evaluate(() => window.sent[0]);
    assert.equal(sent.t, 'artifact-comment');
    assert.equal(sent.id, 'tool-1');
    assert.equal(sent.text, 'Say who shares them');
    assert.equal(sent.anchor.kind, 'word');
    assert.equal(sent.anchor.quote, 'share');
    assert.equal(sent.anchor.selector, '#lead');
    assert.match(sent.anchor.sig.h, /^[0-9a-f]{16}$/);
    assert.match(sent.context.text, /countries share the same tables/);
    assert.match(sent.context.html, /^<p id="lead">/);
    await page.evaluate(reqId => window.controller.handle(
      {t: 'artifact-comment', reqId, queued: true}), sent.reqId);
    await page.waitForSelector('.artifact-comment-composer', {state: 'detached'});
    assert.equal(await page.getAttribute('#artifact-comment', 'aria-pressed'), 'false');
    await frame.click('#go');
    assert.equal(await frame.evaluate(() => document.title), 'clicked');

    // Once delivered and answered, the marker opens its thread.
    await page.evaluate(({id, anchor}) => window.controller.records([
      {id: 'u1', kind: 'user', guest: 'Alice',
       shared: {kind: 'artifact', artifact: 'schema.html', questionId: id,
                text: 'Say who shares them', anchor}},
      {id: 'a1', kind: 'assistant', text: 'Named the country teams.'},
    ]), {id: sent.commentId, anchor: sent.anchor});
    await page.waitForFunction(id => window.located.some(entry => entry.found.includes(id)),
                               sent.commentId);
    await page.mouse.move(share.x + 6, share.y - 12);
    await page.waitForSelector('.artifact-comment-card');
    const card = await page.textContent('.artifact-comment-card');
    assert.match(card, /Alice · Answered/);
    assert.match(card, /Named the country teams\./);
    await page.click('.artifact-comment-card >> text=Show in chat');
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
    await page.waitForSelector('.artifact-comment-composer');
    assert.equal(await page.textContent('.artifact-comment-target'),
                 'Decisions at a glance › area · 3 elements');
    await page.fill('.artifact-comment-input', 'Make these a row of four');
    await page.click('.artifact-comment-composer >> text=Send to assistant');
    await page.waitForFunction(() => window.sent.length === 2);
    const boxed = await page.evaluate(() => window.sent[1]);
    assert.equal(boxed.anchor.kind, 'box');
    assert.equal(boxed.anchor.count, 3);
    assert.ok(boxed.anchor.region.x1 > boxed.anchor.region.x0);
    assert.match(boxed.context.text, /213,150 rows.*42 columns.*637 simulations/);
    await page.evaluate(reqId => window.controller.handle(
      {t: 'artifact-comment', reqId, error: 'The session cannot accept a comment right now'}),
                        boxed.reqId);
    assert.equal(await page.textContent('.artifact-comment-status'),
                 'The session cannot accept a comment right now');
    await page.click('.artifact-comment-composer >> text=Cancel');

    // A box drawn a little past the grid and over two of its cards means
    // those two cards, not the grid as the page's one covered child.
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
    await page.waitForSelector('.artifact-comment-composer');
    assert.equal(await page.textContent('.artifact-comment-target'),
                 'Decisions at a glance \u203a area \u00b7 2 elements');
    await page.click('.artifact-comment-composer >> text=Cancel');

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
    await page.waitForSelector('.artifact-comment-composer');
    assert.equal(await page.textContent('.artifact-comment-target'), 'heading "SNT Schema v2"');
    await page.click('.artifact-comment-composer >> text=Cancel');

    // Dragging across text keeps native selection and sends the quote.
    // Cancelling keeps comment mode on for another try.
    assert.equal(await page.getAttribute('#artifact-comment', 'aria-pressed'), 'true');
    const every = await inPage(page, frame, await wordRect(frame, 'Every table'));
    await page.mouse.move(every.x + 1, every.y + every.height / 2);
    await page.mouse.down();
    await page.mouse.move(every.x + every.width - 1, every.y + every.height / 2, {steps: 6});
    await page.mouse.up();
    await page.waitForSelector('.artifact-comment-composer');
    assert.match(await page.textContent('.artifact-comment-quote'), /^Every tabl/);
    await page.keyboard.press('Escape');
    await page.waitForSelector('.artifact-comment-composer', {state: 'detached'});

    // A rewritten artifact still places the marker: the selector changed
    // meaning, but the text fingerprint finds the paragraph again.
    await page.evaluate(() => { window.located = []; });
    await page.evaluate(html => window.show(html, 'tool-2'), REWRITTEN);
    await artifactFrame(page);
    await page.waitForFunction(id => window.located.some(entry => entry.found.includes(id)),
                               sent.commentId);
    // Content that no longer exists drops its marker.
    await page.evaluate(() => { window.located = []; });
    await page.evaluate(() => window.show('<p>Entirely different page content.</p>', 'tool-3'));
    await artifactFrame(page);
    await page.waitForFunction(id => window.located.some(entry => entry.missing.includes(id)),
                               sent.commentId);
    assert.deepEqual(errors, []);
  } finally {
    await browser.close();
  }
});
