import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');

test('activity summaries preserve failures, children and disclosure at phone and desktop widths', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 812}});
      await page.setContent('<main id="transcript"></main><textarea id="draft">&gt; exact\nmultiline draft</textarea>');
      await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer.css'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await page.evaluate(() => {
        window.activity = {id: 'tool-1', summary: 'Read 2 files, searched 1 pattern, evaluated 1 form',
          failures: '1 failed: Grep', running: 1, status: 'running'};
        window.render = () => {
          const target = document.getElementById('transcript');
          for (let n = 1; n <= 4; n++) {
            const old = target.querySelector(`[data-record-id="tool-${n}"]`);
            const turn = window.mevedelTranscriptRenderer.renderRecord({id: `tool-${n}`, kind: 'tool',
              activity: window.activity, presentation: {name: n === 2 ? 'Grep' : 'Read',
                body: `result-${n}`, collapsed: true}}, null, null, old);
            if (old) old.replaceWith(turn); else target.append(turn);
          }
          window.mevedelTranscriptRenderer.groupActivity(target);
        };
        window.render();
      });
      assert.equal(await page.locator('.activity-group').count(), 1);
      assert.equal(await page.locator('.activity-group').evaluate(node => node.open), false);
      assert.match(await page.locator('.activity-group > summary').innerText(), /1 failed: Grep/);
      assert.match(await page.locator('.activity-group > summary').innerText(), /1 running/);
      await page.locator('.activity-group > summary').click();
      await page.locator('[data-record-id="tool-2"] details > summary').click();
      await page.evaluate(() => { window.activity.running = 0; window.activity.status = 'failed'; window.render(); });
      assert.equal(await page.locator('.activity-group').evaluate(node => node.open), true);
      assert.equal(await page.locator('[data-record-id="tool-2"] details').evaluate(node => node.open), true);
      assert.equal(await page.locator('.activity-group article').count(), 4);
      assert.equal(await page.locator('#draft').inputValue(), '> exact\nmultiline draft');
      assert.equal(await page.locator('.activity-group > summary').evaluate(node =>
        node.scrollWidth <= node.clientWidth), true);
      await page.close();
    }
  } finally { await browser.close(); }
});
