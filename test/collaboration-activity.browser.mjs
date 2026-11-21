import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdir, readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');
const SUMMARY_LABEL = '── Worked for 2m 37s · Thu 29 Oct 14:32 ';
const SUMMARY_DETAILS = 'Started: Thu 29 Oct 2026 14:29:43 +0000\nFinished: Thu 29 Oct 2026 14:32:20 +0000\nActive work: 2m 37s';
// Visual evidence is opt-in: NIWA_CAPTURE_SCREENSHOTS=DIR saves PNGs there.
const CAPTURE_DIR = process.env.NIWA_CAPTURE_SCREENSHOTS;
async function capture(page, name) {
  if (!CAPTURE_DIR) return;
  await mkdir(resolve(CAPTURE_DIR), {recursive: true});
  await page.screenshot({path: resolve(CAPTURE_DIR, `${name}.png`)});
}

test('activity summaries preserve failures, children and disclosure at phone and desktop widths', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const touch = width < 600;
      const page = await browser.newPage({viewport: {width, height: 812}, hasTouch: touch, isMobile: touch});
      await page.setContent('<main id="transcript"></main><textarea id="draft">&gt; exact\nmultiline draft</textarea>');
      await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer-theme.css'), 'utf8')});
      await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer.css'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/markdown.js'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await page.evaluate(([label, details]) => {
        window.activity = {id: 'tool-1',
          summary: '4 calls: read 2 files, searched 1 pattern (1 failed), evaluated 1 form',
          running: 1, status: 'running'};
        window.render = () => {
          const target = document.getElementById('transcript');
          for (let n = 1; n <= 4; n++) {
            const old = target.querySelector(`[data-record-id="tool-${n}"]`);
            const turn = window.niwaTranscriptRenderer.renderRecord({id: `tool-${n}`, kind: 'tool', toolUseId: `source-${n}`,
              activity: window.activity, presentation: {name: n === 2 ? 'Grep' : 'Read',
                sourceId: n === 1 ? 'nested-control' : undefined,
                body: `result-${n}`, collapsed: true}}, null, null, old);
            if (old) old.replaceWith(turn); else target.append(turn);
          }
          // The request's closing rule follows its activity, outside the group.
          const old = target.querySelector('[data-record-id="summary-1"]');
          const closing = window.niwaTranscriptRenderer.renderRecord({id: 'summary-1',
            kind: 'request-summary', text: label, details, item: 'board-1'}, () => ({label: 'Dir'}), null, old);
          if (old) old.replaceWith(closing); else target.append(closing);
          window.niwaTranscriptRenderer.groupActivity(target);
          window.niwaTranscriptRenderer.markContinuations(target);
        };
        window.render();
      }, [SUMMARY_LABEL, SUMMARY_DETAILS]);
      assert.equal(await page.locator('.activity-group').count(), 1);
      assert.equal(await page.locator('.activity-group').evaluate(node => node.open), false);
      assert.match(await page.locator('.activity-group > summary .activity-label').innerText(),
                   /searched 1 pattern \(1 failed\)/);
      assert.equal(await page.locator('.activity-group > summary .activity-counts').innerText(), '1 running');

      const closing = page.locator('[data-record-id="summary-1"]');
      const rule = closing.locator('details.request-summary > summary');
      assert.equal(await page.locator('.activity-group [data-record-id="summary-1"]').count(), 0);
      assert.equal(await page.evaluate(() => document.getElementById('transcript').lastElementChild.dataset.recordId),
                   'summary-1');
      assert.equal(await closing.locator('.glyph, .who, .dirchip, .tool').count(), 0);
      assert.equal(await rule.textContent(), SUMMARY_LABEL);
      assert.equal(await rule.getAttribute('title'), SUMMARY_DETAILS, 'hover discloses the full details');
      assert.equal(await closing.locator('.request-summary-details').isVisible(), false);
      const geometry = await rule.evaluate(node => {
        const after = getComputedStyle(node, '::after');
        const rect = node.getBoundingClientRect();
        return {fits: node.scrollWidth <= node.clientWidth && rect.right <= innerWidth,
          page: document.documentElement.scrollWidth <= innerWidth,
          line: parseFloat(after.width) > 0 && after.borderTopStyle === 'solid',
          marker: getComputedStyle(node).display};
      });
      assert.deepEqual(geometry, {fits: true, page: true, line: true, marker: 'flex'},
                       `closing rule fits at ${width}px`);
      await capture(page, `request-summary-closed-${width}`);
      // Keyboard: Tab focus shows the full details; Enter/Space close and
      // reopen them; leaving closes only what focus opened.
      const isOpen = () => closing.locator('details').evaluate(node => node.open);
      await page.locator('.activity-group > summary').focus();
      await page.keyboard.press('Tab');
      assert.equal(await rule.evaluate(node => node === document.activeElement && node.matches(':focus-visible')), true);
      assert.equal(await isOpen(), true, 'keyboard focus reveals the details');
      assert.equal(await closing.locator('.request-summary-details').innerText(), SUMMARY_DETAILS);
      await page.keyboard.press('Enter');
      assert.equal(await isOpen(), false);
      await page.keyboard.press(' ');
      assert.equal(await isOpen(), true);
      await page.keyboard.press('Enter');
      await page.keyboard.press('Shift+Tab');
      await page.keyboard.press('Tab');
      assert.equal(await isOpen(), true, 'refocusing reveals again');
      await page.keyboard.press('Shift+Tab');
      assert.equal(await isOpen(), false, 'leaving closes what focus opened');
      await page.keyboard.press('Tab');
      await page.keyboard.press('Enter');
      await page.keyboard.press('Enter');
      await page.keyboard.press('Shift+Tab');
      assert.equal(await isOpen(), true, 'an explicit keyboard toggle outlives focus');
      await rule.click();
      assert.equal(await isOpen(), false);
      await page.locator('#draft').focus();
      // Tap on the phone, click on the desktop.
      if (touch) await rule.tap(); else await rule.click();
      assert.equal(await isOpen(), true, 'pointer activation toggles exactly once');
      assert.equal(await closing.locator('.request-summary-details').innerText(), SUMMARY_DETAILS);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await capture(page, `request-summary-open-${width}`);

      await page.locator('.activity-group > summary').click();
      await page.locator('[data-record-id="tool-2"] details > summary').click();
      await page.evaluate(() => { window.activity.running = 0; window.activity.status = 'failed'; window.render(); });
      assert.equal(await page.locator('.activity-group').evaluate(node => node.open), true);
      assert.equal(await page.locator('[data-record-id="tool-2"] details').evaluate(node => node.open), true);
      assert.equal(await page.locator('.activity-group article').count(), 4);
      assert.equal(await closing.locator('details').evaluate(node => node.open), true,
                   'a rebuilt closing rule keeps its disclosure');
      assert.equal(await page.locator('#draft').inputValue(), '> exact\nmultiline draft');
      assert.equal(await page.locator('.activity-group > summary').evaluate(node =>
        node.scrollWidth <= node.clientWidth), true);
      assert.equal(await page.evaluate(() => window.niwaTranscriptRenderer.revealTool(
        document.getElementById('transcript'), 'source-4')), true);
      assert.equal(await page.locator('[data-record-id="tool-4"] details').evaluate(node => node.open), true);
      assert.equal(await page.evaluate(() => window.niwaTranscriptRenderer.revealTool(
        document.getElementById('transcript'), 'nested-control')), true);
      assert.equal(await page.locator('[data-record-id="tool-1"] details').evaluate(node => node.open), true);
      await page.locator('.activity-group > summary').click();
      await page.evaluate(() => {
        const target = document.getElementById('transcript');
        target.replaceChildren(...target.querySelectorAll('[data-record-id]'));
        window.niwaTranscriptRenderer.groupActivity(target);
      });
      assert.equal(await page.locator('.activity-group').evaluate(node => node.open), false,
        'an explicit group fold survives a snapshot or agent transcript rebuild');
      assert.equal(await page.locator('.activity-group [data-record-id="summary-1"]').count(), 0,
        'a regrouped snapshot keeps the closing rule outside its activity');
      assert.equal(await closing.locator('details').evaluate(node => node.open), true);
      await page.close();
    }
  } finally { await browser.close(); }
});

test('a cut activity label still shows its failure total, without repeating it when it fits', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1200]) {
      const touch = width < 600;
      const page = await browser.newPage({viewport: {width, height: 812}, hasTouch: touch, isMobile: touch});
      await page.setContent('<meta name="viewport" content="width=device-width"><main id="transcript"></main>');
      await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer-theme.css'), 'utf8')});
      await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer.css'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/markdown.js'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await page.evaluate(() => {
        const activity = {id: 'tool-1', failed: 1, running: 1, status: 'running',
          summary: '9 calls: read 4 files, searched 2 patterns, evaluated 1 form, ran 2 commands (1 failed)'};
        const target = document.getElementById('transcript');
        for (let n = 1; n <= 3; n++) {
          target.append(window.niwaTranscriptRenderer.renderRecord({id: `tool-${n}`, kind: 'tool',
            toolUseId: `source-${n}`, activity, presentation: {name: 'Read', body: 'ok', collapsed: true}},
          null, null, null));
        }
        window.niwaTranscriptRenderer.groupActivity(target);
      });
      const label = page.locator('.activity-group > summary .activity-label');
      const total = page.locator('.activity-group > summary .activity-failed');
      const cut = await label.evaluate(node => node.scrollWidth > node.clientWidth);
      if (width === 375) {
        assert.equal(cut, true, 'the phone label cuts the inline failure count');
        assert.equal(await total.isVisible(), true);
        assert.equal(await total.innerText(), '(1 failed)');
        assert.equal(await total.evaluate(node => getComputedStyle(node).color),
                     await page.evaluate(() => {
                       const probe = document.createElement('span');
                       probe.style.color = 'var(--failed)';
                       document.body.append(probe);
                       return getComputedStyle(probe).color;
                     }));
        const box = await total.boundingBox();
        assert.ok(box && box.x + box.width <= width, 'the total stays on screen');
      } else {
        assert.equal(cut, false);
        assert.equal(await total.isVisible(), false, 'a fitting label is not repeated');
        assert.match(await label.innerText(), /ran 2 commands \(1 failed\)/);
      }
      await capture(page, `activity-failure-total-${width}`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
});

test('an archived segment measures its failure total once attached and when reopened', async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({viewport: {width: 375, height: 812}, hasTouch: true, isMobile: true});
    await page.setContent('<meta name="viewport" content="width=device-width"><section id="history" class="history" hidden></section>');
    await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer-theme.css'), 'utf8')});
    await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer/viewer.css'), 'utf8')});
    await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/markdown.js'), 'utf8')});
    await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
    await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/viewer-history.js'), 'utf8')});
    await page.evaluate(() => {
      const el = (tag, className, text) => {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
      };
      window.sent = [];
      window.archive = window.niwaHistoryView.create({
        send: async frame => { window.sent.push(frame); return true; }, el,
        renderRecord: record => window.niwaTranscriptRenderer.renderRecord(record, null, null, null),
        onArtifacts: () => {}, onLoaded: () => {}});
      window.archive.connection(true);
      window.archiveIndex = current => window.archive.handle({t: 'history-index', currentSegment: current,
        final: true, records: [{kind: 'chat', number: 1, title: 'Sweep', firstSegment: 1, lastSegment: current}]});
      window.archiveIndex(2);
      const activity = {id: 'old-1', failed: 1, running: 0, status: 'failed',
        summary: '9 calls: read 4 files, searched 2 patterns, evaluated 1 form, ran 2 commands (1 failed)'};
      window.reply = () => window.archive.handle({t: 'history', reqId: window.sent.at(-1).reqId, segment: 1,
        final: true, records: [1, 2, 3].map(n => ({id: `old-${n}`, kind: 'tool', toolUseId: `old-source-${n}`,
          activity, presentation: {name: 'Read', body: 'ok', collapsed: true}}))});
    });
    const segment = page.locator('#history .history-segment');
    const total = segment.locator('.activity-group > summary .activity-failed');
    // The ledger is grouped detached and attached while the segment is open.
    await segment.locator('summary').first().click();
    await page.waitForFunction(() => window.sent.length === 1);
    await page.evaluate(() => window.reply());
    assert.equal(await total.isVisible(), true, 'the attached ledger measured its cut label');
    assert.equal(await total.innerText(), '(1 failed)');
    // A ledger that arrives while its segment is closed shows the total once opened.
    await page.evaluate(() => window.archiveIndex(3));
    const second = page.locator('#history .history-segment').nth(1);
    await second.locator('summary').first().click();
    await page.waitForFunction(() => window.sent.length === 2);
    await second.evaluate(node => { node.open = false; });
    await page.evaluate(() => window.archive.handle({t: 'history', reqId: window.sent.at(-1).reqId, segment: 2,
      final: true, records: [1, 2, 3].map(n => ({id: `older-${n}`, kind: 'tool', toolUseId: `older-source-${n}`,
        activity: {id: 'older-1', failed: 2, running: 0, status: 'failed',
          summary: '9 calls: read 4 files, searched 2 patterns, evaluated 1 form, ran 2 commands (2 failed)'},
        presentation: {name: 'Read', body: 'ok', collapsed: true}}))}));
    const secondTotal = second.locator('.activity-group > summary .activity-failed');
    await second.locator('summary').first().click();
    await page.waitForFunction(() => {
      const node = document.querySelectorAll('#history .history-segment')[1]
        .querySelector('.activity-failed');
      return node && !node.hidden;
    });
    assert.equal(await secondTotal.innerText(), '(2 failed)');
    await capture(page, 'activity-failure-total-history-375');
  } finally {
    await browser.close();
  }
});

test('thinking disclosure keeps native toggles, draft and fit at phone and desktop widths', async () => {
  const browser = await chromium.launch();
  try {
    for (const width of [375, 1280]) {
      const page = await browser.newPage({viewport: {width, height: 812}, hasTouch: width === 375, isMobile: width === 375});
      await page.setContent('<meta name="viewport" content="width=device-width, initial-scale=1"><main id="transcript" class="ledger"></main><form class="composer"><textarea id="draft" rows="3"></textarea></form>');
      assert.equal(await page.evaluate(() => innerWidth), width, 'use the actual device layout width');
      for (const file of ['viewer-theme.css', 'viewer.css']) {
        await page.addStyleTag({content: await readFile(resolve(root, 'relay/viewer', file), 'utf8')});
      }
      for (const file of ['markdown.js', 'renderer.js']) {
        await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer', file), 'utf8')});
      }
      await page.locator('#draft').fill('> exact\nmultiline draft');
      await page.evaluate(() => {
        window.rebuildThinking = () => {
          const target = document.getElementById('transcript'), renderer = window.niwaTranscriptRenderer;
          const previous = new Map([...target.querySelectorAll('[data-record-id]')].map(node => [node.dataset.recordId, node]));
          target.replaceChildren(...[
            {id: 'before', kind: 'tool', name: 'Read', activity: {id: 'before', summary: 'Read 2 files'}},
            {id: 'reason', kind: 'thinking', text: '**Compare** the options.\n\nKeep reasoning separate from tool activity.'},
            {id: 'after', kind: 'tool', name: 'Grep', activity: {id: 'after', summary: 'Search 2 patterns'}},
          ].map(record => renderer.renderRecord(record, null, null, previous.get(record.id))));
          renderer.groupActivity(target);
          renderer.markContinuations(target);
        };
        window.rebuildThinking();
      });
      const fold = page.locator('details.thinking'), summary = fold.locator('summary');
      const isOpen = () => fold.evaluate(node => node.open);
      const checkFit = async () => assert.equal(await page.evaluate(() =>
        document.documentElement.scrollWidth <= innerWidth &&
        [...document.querySelectorAll('.thinking, .activity-group, #draft')].every(node => {
          const rect = node.getBoundingClientRect();
          return rect.left >= 0 && rect.right <= innerWidth && rect.top >= 0 && rect.bottom <= innerHeight && node.scrollWidth <= node.clientWidth;
        })), true, `thinking and draft fit at ${width}px`);
      assert.equal(await isOpen(), true, 'thinking starts expanded');
      assert.equal(await summary.innerText(), 'Thinking');
      assert.equal(await fold.locator('strong').innerText(), 'Compare');
      assert.equal(await page.locator('.activity-group').count(), 2);
      assert.equal(await page.locator('.activity-group .thinking').count(), 0);
      await checkFit();
      await capture(page, `thinking-open-${width}`);
      await summary.click();
      assert.equal(await isOpen(), false, 'real click collapses thinking');
      await page.locator('#draft').focus();
      await page.evaluate(() => window.rebuildThinking());
      assert.equal(await isOpen(), false, 'rebuild preserves collapse');
      assert.equal(await page.locator('#draft').evaluate(node => node === document.activeElement), true);
      assert.equal(await page.locator('#draft').inputValue(), '> exact\nmultiline draft');
      assert.equal(await summary.evaluate(node => {
        const style = getComputedStyle(node), probe = document.createElement('span');
        node.append(probe); probe.style.color = 'var(--muted)';
        const muted = getComputedStyle(probe).color; probe.remove();
        return style.color === muted && style.color !== getComputedStyle(document.body).color && style.fontWeight === '400';
      }), true, 'collapsed header uses muted color and regular weight');
      await checkFit();
      await capture(page, `thinking-closed-${width}`);
      await summary.focus();
      await page.keyboard.press('Space');
      assert.equal(await isOpen(), true, 'Space expands the native disclosure');
      await page.keyboard.press('Enter');
      assert.equal(await isOpen(), false, 'Enter collapses the native disclosure');
      await page.evaluate(() => window.rebuildThinking());
      assert.equal(await isOpen(), false, 'keyboard collapse survives rebuild');
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
      window.sourceHistory = window.niwaHistoryView.create({el, onArtifacts() {},
        send: frame => { window.sourceSent.push(frame); return Promise.resolve(true); },
        renderRecord: record => window.niwaTranscriptRenderer.renderRecord(record)});
      window.sourceHistory.connection(true);
      window.sourceHistory.handle({t: 'history-index', currentSegment: 4, final: true,
        records: [{kind: 'chat', number: 1, title: 'Work', firstSegment: 1, lastSegment: 4}]});
      window.sourceReveal = window.sourceHistory.reveal(2, 'source-two');
    });
    assert.equal(await page.evaluate(() => window.sourceSent.length), 1);
    assert.equal(await page.evaluate(() => window.sourceSent[0].segment), 2);
    await page.evaluate(() => window.sourceHistory.handle({t: 'history', reqId: window.sourceSent[0].reqId,
      segment: 2, final: true, records: [{id: 'tool-two', kind: 'tool', toolUseId: 'source-two',
        presentation: {name: 'Bash', body: 'located output', collapsed: true}},
      {id: 'summary-two', kind: 'request-summary', text: '── Worked for 12s '}]}));
    assert.equal(await page.evaluate(() => window.sourceReveal), true);
    assert.equal(await page.locator('.history-segment[open]').count(), 1);
    assert.equal(await page.locator('[data-record-id="tool-two"] details').evaluate(node => node.open), true);
    assert.equal(await page.locator('[data-record-id="summary-two"] div.request-summary').textContent(),
                 '── Worked for 12s ', 'archives render untimed closing rules without a disclosure');
    assert.equal(await page.locator('[data-record-id="summary-two"] details').count(), 0);
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
        .pinned-prompt { --pinned-prompt-h:36px; position:fixed; top:72px; height:36px; width:100%; z-index:3; }
      </style><header class="topbar">Header</header>
      <button id="pinned-prompt" class="pinned-prompt" type="button">A prompt pinned below the header</button><div class="spacer"></div>
      <main id="transcript"></main><div class="spacer"></div><section id="history-body"></section>
      <div class="spacer"></div><aside class="dock"><textarea id="draft">exact draft</textarea></aside>
      <section class="viewer-panel" hidden><header class="topbar">Agent header</header><div id="agent-transcript" class="agent-ledger"></div><button class="pinned-prompt" type="button">Pinned task</button></section>`);
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/markdown.js'), 'utf8')});
      await page.addScriptTag({content: await readFile(resolve(root, 'relay/viewer/renderer.js'), 'utf8')});
      await page.evaluate(() => {
        for (const id of ['transcript', 'history-body', 'agent-transcript']) {
          const container = document.getElementById(id);
          if (id === 'agent-transcript') container.innerHTML = '<div class="spacer"></div>';
          container.append(window.niwaTranscriptRenderer.renderRecord({id, kind: 'tool', toolUseId: `source-${id}`,
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
          window.niwaTranscriptRenderer.revealTool(container, `source-${id}`);
        }, id);
        await page.waitForTimeout(700);
        const geometry = await page.locator(`[data-tool-use-id="source-${id}"]`).evaluate(node => {
          const rect = node.getBoundingClientRect();
          const panel = node.closest('.viewer-panel');
          const top = (panel || document).querySelector('.pinned-prompt').getBoundingClientRect().bottom + 12;
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
