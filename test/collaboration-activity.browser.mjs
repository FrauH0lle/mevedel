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
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/markdown.js'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await page.evaluate(() => {
        window.activity = {id: 'tool-1', summary: 'Read 2 files, searched 1 pattern, evaluated 1 form',
          failures: '1 failed: Grep', running: 1, status: 'running'};
        window.render = () => {
          const target = document.getElementById('transcript');
          for (let n = 1; n <= 4; n++) {
            const old = target.querySelector(`[data-record-id="tool-${n}"]`);
            const turn = window.mevedelTranscriptRenderer.renderRecord({id: `tool-${n}`, kind: 'tool', toolUseId: `source-${n}`,
              activity: window.activity, presentation: {name: n === 2 ? 'Grep' : 'Read',
                sourceId: n === 1 ? 'nested-control' : undefined,
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
      assert.equal(await page.evaluate(() => window.mevedelTranscriptRenderer.revealTool(
        document.getElementById('transcript'), 'source-4')), true);
      assert.equal(await page.locator('[data-record-id="tool-4"] details').evaluate(node => node.open), true);
      assert.equal(await page.evaluate(() => window.mevedelTranscriptRenderer.revealTool(
        document.getElementById('transcript'), 'nested-control')), true);
      assert.equal(await page.locator('[data-record-id="tool-1"] details').evaluate(node => node.open), true);
      await page.locator('.activity-group > summary').click();
      await page.evaluate(() => {
        const target = document.getElementById('transcript');
        target.replaceChildren(...target.querySelectorAll('[data-record-id]'));
        window.mevedelTranscriptRenderer.groupActivity(target);
      });
      assert.equal(await page.locator('.activity-group').evaluate(node => node.open), false,
        'an explicit group fold survives a snapshot or agent transcript rebuild');
      await page.close();
    }
  } finally { await browser.close(); }
});

test('source navigation loads exactly the host-located archive', async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage();
    await page.setContent('<section id="history"></section>');
    for (const asset of ['markdown.js', 'renderer.js', 'viewer-history.js']) {
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer', asset), 'utf8')});
    }
    await page.evaluate(() => {
      const el = (tag, cls, text) => { const node = document.createElement(tag);
        node.className = cls || ''; node.textContent = text || ''; return node; };
      window.sourceSent = [];
      window.sourceHistory = window.mevedelHistoryView.create({el, onArtifacts() {},
        send: frame => { window.sourceSent.push(frame); return Promise.resolve(true); },
        renderRecord: record => window.mevedelTranscriptRenderer.renderRecord(record)});
      window.sourceHistory.connection(true);
      window.sourceHistory.handle({t: 'history-index', currentSegment: 4, final: true,
        records: [{kind: 'chat', number: 1, title: 'Work', firstSegment: 1, lastSegment: 4}]});
      window.sourceReveal = window.sourceHistory.reveal(2, 'source-two');
    });
    assert.equal(await page.evaluate(() => window.sourceSent.length), 1);
    assert.equal(await page.evaluate(() => window.sourceSent[0].segment), 2);
    await page.evaluate(() => window.sourceHistory.handle({t: 'history', reqId: window.sourceSent[0].reqId,
      segment: 2, final: true, records: [{id: 'tool-two', kind: 'tool', toolUseId: 'source-two',
        presentation: {name: 'Bash', body: 'located output', collapsed: true}}]}));
    assert.equal(await page.evaluate(() => window.sourceReveal), true);
    assert.equal(await page.locator('.history-segment[open]').count(), 1);
    assert.equal(await page.locator('[data-record-id="tool-two"] details').evaluate(node => node.open), true);
    assert.equal(await page.evaluate(() => window.sourceSent.length), 1);
  } finally { await browser.close(); }
});

test('revealed sources use the unobscured transcript viewport', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 900}});
      await page.setContent(`<style>
        body { margin:0; } .topbar { position:fixed; top:0; height:64px; width:100%; background:white; z-index:3; }
        .dock { position:fixed; bottom:0; height:520px; width:100%; background:white; z-index:3; }
        .spacer { height:1100px; } .viewer-panel { position:fixed; inset:0; background:white; z-index:4; }
        .agent-ledger { position:absolute; top:64px; bottom:0; width:100%; overflow:auto; }
      </style><header class="topbar">Header</header><div class="spacer"></div>
      <main id="transcript"></main><div class="spacer"></div><section id="history-body"></section>
      <div class="spacer"></div><aside class="dock"><textarea id="draft">exact draft</textarea></aside>
      <section class="viewer-panel" hidden><header class="topbar">Agent header</header><div id="agent-transcript" class="agent-ledger"></div></section>`);
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/markdown.js'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await page.evaluate(() => {
        for (const id of ['transcript', 'history-body', 'agent-transcript']) {
          const container = document.getElementById(id);
          if (id === 'agent-transcript') container.innerHTML = '<div class="spacer"></div>';
          container.append(window.mevedelTranscriptRenderer.renderRecord({id, kind: 'tool', toolUseId: `source-${id}`,
            presentation: {name: 'Bash', body: id === 'history-body'
              ? Array.from({length: 60}, (_, n) => `Oversized source line ${n}`).join('\n')
              : 'first output line\nsecond output line', collapsed: true}}));
          if (id === 'agent-transcript') container.insertAdjacentHTML('beforeend', '<div class="spacer"></div>');
        }
      });
      for (const id of ['transcript', 'history-body', 'agent-transcript']) {
        const rootScroll = await page.evaluate(() => scrollY);
        await page.evaluate(id => {
          const container = document.getElementById(id);
          if (id === 'agent-transcript') container.closest('.viewer-panel').hidden = false;
          window.mevedelTranscriptRenderer.revealTool(container, `source-${id}`);
        }, id);
        await page.waitForTimeout(700);
        const geometry = await page.locator(`[data-tool-use-id="source-${id}"]`).evaluate(node => {
          const rect = node.getBoundingClientRect();
          const panel = node.closest('.viewer-panel');
          const top = panel ? panel.querySelector('.topbar').getBoundingClientRect().bottom
            : document.querySelector('body > .topbar').getBoundingClientRect().bottom;
          const bottom = panel ? innerHeight : document.querySelector('.dock').getBoundingClientRect().top;
          const hit = document.elementFromPoint(rect.left + 10, rect.top + 10);
          return {top: rect.top, visibleTop: top, visibleBottom: bottom,
            exposed: !!hit && (node.contains(hit) || hit.contains(node))};
        });
        assert.ok(geometry.top >= geometry.visibleTop && geometry.top + 10 < geometry.visibleBottom && geometry.exposed,
          `${id} is visible above sticky UI at ${width}px: ${JSON.stringify(geometry)}`);
        if (id === 'agent-transcript') assert.equal(await page.evaluate(() => scrollY), rootScroll,
          'sheet inspection must not scroll the parent conversation');
      }
      assert.equal(await page.locator('#draft').inputValue(), 'exact draft');
      await page.close();
    }
  } finally { await browser.close(); }
});
