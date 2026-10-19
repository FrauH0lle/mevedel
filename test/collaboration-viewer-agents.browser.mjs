/* Agent/task layout against the real room markup and controller assets. */
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');

test('long agent attribution and task owners fit phone and desktop', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 812}});
      await page.route('http://viewer.test/**', async route => {
        const path = new URL(route.request().url()).pathname;
        if (!path.endsWith('.html') && !path.endsWith('.css')) {
          return route.fulfill({status: 204, body: ''});
        }
        return route.fulfill({body: await readFile(resolve(root, 'relay/viewer', `.${path}`)),
          contentType: path.endsWith('.css') ? 'text/css' : 'text/html'});
      });
      await page.goto('http://viewer.test/index.html');
      for (const asset of ['viewer-agent.js', 'viewer-task.js']) {
        await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer', asset), 'utf8')});
      }
      await page.evaluate(() => {
        const el = (tag, cls, text) => {
          const node = document.createElement(tag);
          node.className = cls || '';
          node.textContent = text || '';
          return node;
        };
        const path = '/root/' + 'long_agent_name_'.repeat(10);
        const model = 'provider-model-'.repeat(10);
        window.agentView = window.mevedelAgentView.create({el, send() {}, summarize() {}});
        window.agentView.show([
          {path, status: 'running', model, effort: 'high'},
          {path: path + '/finished', status: 'done', model, effort: 'high'},
        ]);
        window.mevedelTaskView.create({el, summarize() {}}).show({t: 'tasks',
          total: 1, completed: 0, omitted: 0, omittedActive: 0,
          tasks: [{id: 1, subject: 'Review', status: 'in-progress', owner: path,
            note: 'unbroken_status_note_'.repeat(10)}]});
        document.getElementById('session-box').open = true;
        document.getElementById('session-box').hidden = false;
        document.getElementById('agents-done-list').closest('details').open = true;
      });
      for (const selector of ['#agents .dock-chip', '.done-row', '#tasks-list .task']) {
        const fits = await page.locator(selector).evaluate(node => {
          const box = node.getBoundingClientRect();
          return box.width > 0 && box.left >= 0 && box.right <= innerWidth + 1
            && node.scrollWidth <= node.clientWidth + 1;
        });
        assert.equal(fits, true, `${selector} fits at ${width}px`);
      }
      await page.locator('#agents .dock-chip').click();
      assert.equal(await page.locator('#agent-panel').evaluate(node =>
        node.scrollWidth <= node.clientWidth + 1), true, `inspection fits at ${width}px`);
      await page.locator('#agent-close').click();
      await page.close();
    }
  } finally { await browser.close(); }
});
