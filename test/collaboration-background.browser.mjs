import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');

test('unified work panel retains expanded completions and checks writable controls', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 812}, hasTouch: width === 375});
      await page.route('http://viewer.test/**', async route => {
        const path = new URL(route.request().url()).pathname;
        if (!path.endsWith('.html') && !path.endsWith('.css')) return route.fulfill({status: 204, body: ''});
        return route.fulfill({body: await readFile(resolve(root, 'relay/viewer', `.${path}`)),
          contentType: path.endsWith('.css') ? 'text/css' : 'text/html'});
      });
      await page.goto('http://viewer.test/index.html');
      assert.equal(await page.locator('#background-work').count(), 1, 'running work has a visible surface');
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/viewer-background.js'), 'utf8')});
      await page.evaluate(() => {
        window.bgTime = 1000; window.bgSent = []; window.bgState = {connected: true, writable: true};
        window.bgRows = [
          {key: 'agent', kind: 'agent', path: '/root/reviewer', owner: '/root', status: 'running', statusLabel: 'Running',
            description: '/root/reviewer', model: 'accurate-model', effort: 'high', startedAt: 30},
          {key: 'bash', kind: 'bash', owner: '/root', id: 'bash-1', status: 'running', statusLabel: 'Running',
            description: 'Run test suite ' + 'long '.repeat(20), startedAt: 20,
            preview: ['one', 'two', 'three'], previewAvailable: true, previewLabel: 'Output preview'},
          {key: 'monitor', kind: 'monitor', owner: '/root', id: 'monitor-1', status: 'running', statusLabel: 'Watching',
            description: 'Watch build', startedAt: 10, preview: ['', '', ''], previewAvailable: true},
        ];
        const el = (tag, cls, text) => { const node = document.createElement(tag);
          node.className = cls || ''; node.textContent = text || ''; return node; };
        window.bg = window.mevedelBackgroundView.create({state: window.bgState, el,
          send: frame => window.bgSent.push(frame), now: () => window.bgTime,
          setTimer: fn => { window.bgTick = fn; return 1; }, clearTimer() {},
          openAgent: row => { window.bgOpened = row.path; }, openExecution() {}, showConversation() {}});
        window.bgTick = () => window.bg.refresh();
        window.bg.show({rows: window.bgRows});
      });
      assert.deepEqual(await page.locator('.background-row').evaluateAll(rows => rows.map(r => r.dataset.key)),
        ['agent', 'bash', 'monitor'], 'host order is preserved');
      const activate = selector => width === 375 ? page.locator(selector).tap() : page.locator(selector).click();
      await activate('[data-key="bash"] .background-toggle');
      assert.equal(await page.locator('[data-key="bash"] .background-preview').innerText(), 'one\ntwo\nthree');
      const height = await page.locator('[data-key="bash"]').evaluate(node => node.getBoundingClientRect().height);
      await page.evaluate(() => { window.bgRows[1].terminal = true; window.bgRows[1].status = 'completed'; window.bgRows[1].statusLabel = 'Completed';
        window.bg.show({rows: window.bgRows}); window.bgTime += 30000; window.bgTick(); });
      assert.equal(await page.locator('[data-key="bash"]').count(), 1, 'expanded completion remains visible');
      assert.equal(await page.locator('[data-key="bash"]').evaluate(node => node.getBoundingClientRect().height), height);
      assert.equal(await page.locator('[data-key="bash"] [data-stop]').isDisabled(), true);
      await page.evaluate(() => window.bg.show({rows: [window.bgRows[0], window.bgRows[2]]}));
      assert.deepEqual(await page.locator('.background-row').evaluateAll(rows => rows.map(r => r.dataset.key)),
        ['agent', 'bash', 'monitor'], 'retained middle inspector keeps its insertion anchor');
      await activate('[data-key="bash"] .background-toggle');
      if (width !== 375) await page.mouse.move(0, 0);
      await page.evaluate(() => { document.activeElement.blur(); window.bgTime += 9999; window.bgTick(); });
      assert.equal(await page.locator('[data-key="bash"]').count(), 1, 'collapse starts fresh grace');
      await page.evaluate(() => { window.bgTime += 2; window.bgTick(); });
      assert.equal(await page.locator('[data-key="bash"]').count(), 0, 'grace expires without reappearing from cache');
      await page.evaluate(() => window.bg.show({rows: window.bgRows}));
      assert.equal(await page.locator('[data-key="bash"]').count(), 0);
      await page.locator('[data-key="agent"] .background-toggle').click();
      assert.match(await page.locator('[data-key="agent"]').innerText(), /accurate-model:high/);
      await page.locator('[data-key="agent"] [data-stop]').click();
      assert.equal(await page.evaluate(() => window.bgSent.at(-1).t), 'background-stop');
      await page.evaluate(() => { window.bgState.writable = false; window.bg.refresh(); });
      assert.equal(await page.locator('[data-key="agent"] [data-stop]').isDisabled(), true);
      assert.equal(await page.locator('#background-work').evaluate(node => node.scrollWidth <= node.clientWidth), true);
      await page.evaluate(() => { window.bgState.writable = true; window.bg.reset();
        window.bg.show({rows: [window.bgRows[0], window.bgRows[2]]}); });
      await activate('[data-key="agent"] .background-toggle');
      await page.evaluate(() => window.bg.show({rows: [{...window.bgRows[2], key: 'newer', description: 'New work'},
        window.bgRows[0], window.bgRows[2]]}));
      assert.deepEqual(await page.locator('.background-row').evaluateAll(rows => rows.map(r => r.dataset.key)),
        ['agent', 'monitor'], 'focus freezes the shown set while new work is counted');
      await page.evaluate(() => document.activeElement.blur());
      if (width !== 375) await page.mouse.move(0, 0);
      await page.evaluate(() => window.bg.refresh());
      assert.deepEqual(await page.locator('.background-row').evaluateAll(rows => rows.map(r => r.dataset.key)),
        ['newer', 'agent', 'monitor']);
      await page.evaluate(() => window.bg.show({rows: [], truncated: true}));
      assert.match(await page.locator('[data-key="agent"]').innerText(), /Unavailable/);
      assert.equal(await page.locator('[data-key="agent"] [data-stop]').isDisabled(), true);
      await page.evaluate(() => { window.bg.reset(); window.bg.show({rows: [{...window.bgRows[1], terminalAgeSeconds: 25}]}); });
      assert.equal(await page.locator('#background-work').isVisible(), false, 'rejoin does not restart old terminal grace');
      await page.evaluate(() => { window.bg.reset(); window.bg.show({rows: [], truncated: true}); });
      assert.equal(await page.locator('#background-work').isVisible(), true);
      assert.match(await page.locator('#background-summary').innerText(), /omitted/);
      await page.close();
    }
  } finally { await browser.close(); }
});
