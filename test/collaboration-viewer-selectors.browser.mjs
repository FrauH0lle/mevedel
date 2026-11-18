/* Body layout state must not make streamed records restyle the whole room.
 * MEVEDEL_SELECTOR_EVIDENCE=DIR saves screenshots and matched timing samples.
 * MEVEDEL_SELECTOR_ASSETS=DIR uses a frozen viewer for baseline comparisons.
 * Run: node --test test/collaboration-viewer-selectors.browser.mjs
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');
const evidence = process.env.MEVEDEL_SELECTOR_EVIDENCE;
const assets = resolve(process.env.MEVEDEL_SELECTOR_ASSETS || resolve(root, 'relay/viewer'));

async function room(browser, width, theme) {
  const page = await browser.newPage({viewport: {width, height: width === 375 ? 667 : 900}});
  await page.route('http://viewer.test/**', async route => {
    const path = new URL(route.request().url()).pathname;
    if (!/\.(html|css)$/.test(path)) return route.fulfill({status: 204});
    return route.fulfill({body: await readFile(resolve(assets, `.${path}`)),
      contentType: path.endsWith('.css') ? 'text/css' : 'text/html'});
  });
  await page.goto('http://viewer.test/index.html');
  for (const script of ['markdown.js', 'renderer.js', 'viewer-header.js']) {
    await page.addScriptTag({path: resolve(assets, script)});
  }
  await page.evaluate(theme => {
    document.documentElement.dataset.theme = theme;
    document.getElementById('composer').hidden = false;
    document.getElementById('composer-input').value = '> Keep this draft\nSecond line';
    document.getElementById('session-label').textContent = 'Selector regression';
    document.getElementById('session-box').hidden = false;
    document.getElementById('session-box').open = innerWidth >= 1100;
    document.getElementById('commands-box').hidden = false;
    document.getElementById('session-summary').textContent = '3 commands';
    document.getElementById('room-button').hidden = false;
    window.mevedelHeader.create();
    window.render = record => mevedelTranscriptRenderer.renderRecord(record, () => {});
  }, theme);
  return page;
}

async function capture(page, name) {
  if (evidence) await page.screenshot({path: resolve(evidence, `${name}.png`)});
}

test('room layout follows sidebar, transcript and docked-editor transitions', async () => {
  if (evidence) await mkdir(evidence, {recursive: true});
  const browser = await chromium.launch();
  try {
    for (const width of [1440, 375]) for (const theme of ['light', 'dark']) {
      const page = await room(browser, width, theme);
      const padding = () => page.evaluate(() => parseFloat(getComputedStyle(document.body).paddingRight));
      assert.equal(await padding(), width === 1440 ? 344 : 0);
      await page.evaluate(() => { document.getElementById('session-box').hidden = true; });
      assert.equal(await padding(), 0);
      await page.evaluate(() => {
        document.getElementById('session-box').hidden = false;
        document.getElementById('terminal-title').textContent = 'Session ended';
        document.getElementById('terminal-message').textContent = 'The host stopped sharing this room.';
        document.getElementById('terminal-state').hidden = false;
      });
      assert.equal(await page.locator('.dock').evaluate(node => getComputedStyle(node).marginTop), '0px');
      const emptyTop = (await page.locator('#terminal-state').boundingBox()).y;
      await capture(page, `terminal-${width}-${theme}`);
      await page.evaluate(() => document.getElementById('transcript').append(
        render({id: 'prompt', kind: 'user', text: 'Keep the room visually unchanged.'}),
        render({id: 'answer', kind: 'assistant', text: 'The **same layout** without global selector invalidation.'})));
      assert.ok((await page.locator('#terminal-state').boundingBox()).y < emptyTop);
      await page.evaluate(() => { document.getElementById('terminal-state').hidden = true; });
      await capture(page, `room-${width}-${theme}`);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      assert.equal(await page.evaluate(async () => {
        const changes = [];
        const observer = new MutationObserver(records => changes.push(...records));
        observer.observe(document.body, {attributes: true});
        for (let i = 0; i < 5; i++) {
          document.getElementById('transcript').lastElementChild.replaceWith(
            render({id: 'answer', kind: 'assistant', text: `Streaming chunk ${i}`}));
          await Promise.resolve();
        }
        await Promise.resolve();
        observer.disconnect();
        return changes.length;
      }), 0, 'streaming does not rewrite body layout attributes');
      await page.evaluate(() => document.getElementById('transcript').replaceChildren());
      await page.evaluate(() => { document.getElementById('terminal-state').hidden = false; });
      assert.equal((await page.locator('#terminal-state').boundingBox()).y, emptyTop);
      await page.close();
    }
    const page = await room(browser, 1920, 'light');
    await page.evaluate(() => {
      const panel = document.getElementById('editing-panel');
      panel.classList.add('docked'); panel.hidden = false;
      document.getElementById('session-box').open = false;
    });
    assert.equal(await page.locator('#session-box').isVisible(), false);
    assert.equal(await page.locator('#composer').isVisible(), true);
    assert.equal(await page.locator('#composer').evaluate(node => node.getBoundingClientRect().right <= 860), true);
    await page.evaluate(() => { document.getElementById('session-box').open = true; });
    assert.equal(await page.locator('#session-box').isVisible(), true);
    await page.evaluate(() => document.getElementById('editing-panel').classList.remove('docked'));
    assert.equal(await page.locator('#composer').isVisible(), false);
    assert.equal(await page.locator('#composer-input').inputValue(), '> Keep this draft\nSecond line');
  } finally { await browser.close(); }
});

test('long-page streamed record replacement measurements', {skip: !evidence}, async () => {
  await mkdir(evidence, {recursive: true});
  const browser = await chromium.launch();
  const results = [];
  try {
    for (const width of [1440, 375]) {
      const page = await room(browser, width, 'light');
      await page.evaluate(() => {
        const transcript = document.getElementById('transcript');
        transcript.replaceChildren(...Array.from({length: 2000}, (_, i) => render({
          id: `history-${i}`, kind: i % 2 ? 'assistant' : 'user',
          text: `Exchange ${i}: **rendered history** with a [reference](https://example.com).\n\nA second paragraph.`})));
        window.record = {id: 'stream', kind: 'assistant', text: 'Streaming response'};
        window.current = render(record);
        transcript.append(current);
        window.scrollTo(0, document.documentElement.scrollHeight);
        window.chunks = async () => {
          for (let i = 0; i < 100; i++) {
            record.text = `Streaming response${' chunk'.repeat(i + 1)}`;
            const turn = render(record);
            current.replaceWith(turn); current = turn;
            // Include observer delivery and force the layout a presented chunk needs.
            await Promise.resolve();
            void current.offsetHeight;
          }
        };
      });
      await page.waitForTimeout(100);
      const cdp = await page.context().newCDPSession(page);
      await cdp.send('Performance.enable');
      const metrics = async () => Object.fromEntries((await cdp.send('Performance.getMetrics')).metrics.map(m => [m.name, m.value]));
      for (let i = 0; i < 5; i++) await page.evaluate(() => chunks());
      const samples = [];
      for (let i = 0; i < 5; i++) {
        const before = await metrics();
        const wall = await page.evaluate(async () => { const start = performance.now(); await chunks(); return performance.now() - start; });
        const after = await metrics();
        samples.push({wallMsPerChunk: wall / 100,
          styleMsPerChunk: (after.RecalcStyleDuration - before.RecalcStyleDuration) * 10,
          layoutMsPerChunk: (after.LayoutDuration - before.LayoutDuration) * 10});
      }
      results.push({width, height: width === 375 ? 667 : 900, historyRecords: 2000,
        chunksPerSample: 100, warmups: 5, samples});
      await page.close();
    }
    await writeFile(resolve(evidence, 'measurements.json'), JSON.stringify({browser: browser.version(), results}, null, 2));
  } finally { await browser.close(); }
});
