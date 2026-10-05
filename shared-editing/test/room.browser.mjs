import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import { mkdtemp, readdir, readFile, writeFile, rename, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { resolve, join } from 'node:path';
import { createServer } from 'node:net';
import { chromium } from 'playwright';
const root = resolve(import.meta.dirname, '../..');
const delay = (ms) => new Promise((r) => setTimeout(r, ms));
async function until(fn, ms = 15000) {
  const deadline = Date.now() + ms;
  let error;
  while (Date.now() < deadline) {
    try {
      const result = await fn();
      if (result) return result;
    } catch (e) {
      error = e;
    }
    await delay(50);
  }
  throw error || new Error('Timed out');
}
async function freePort() {
  const server = createServer();
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const port = server.address().port;
  await new Promise((r) => server.close(r));
  return port;
}

test(
  'real room: concurrent browsers, deterministic agent, isolation, laser, and host recovery',
  { timeout: 180000 },
  async () => {
    const directory = await mkdtemp(join(tmpdir(), 'mevedel-editing-browser-')),
      port = await freePort();
    const logs = [],
      errors = [];
    let relay, host, browser;
    try {
      execFileSync('go', ['build', '-o', join(directory, 'relay'), '.'], {
        cwd: join(root, 'relay'),
        stdio: 'pipe',
      });
      relay = spawn(join(directory, 'relay'), [
        '-addr',
        `127.0.0.1:${port}`,
        '-vapid-key-file',
        join(directory, 'vapid.pem'),
      ]);
      relay.stderr.on('data', (d) => logs.push(String(d)));
      await until(async () => {
        try {
          return (await fetch(`http://127.0.0.1:${port}/healthz`)).ok;
        } catch {
          return false;
        }
      });
      const deps = (await readdir(join(root, '.eask/31.1/elpa'), { withFileTypes: true }))
        .filter((d) => d.isDirectory())
        .flatMap((d) => ['-L', join(root, '.eask/31.1/elpa', d.name)]);
      const env = {
        ...process.env,
        HOME: directory,
        XDG_CONFIG_HOME: join(directory, 'config'),
        XDG_CACHE_HOME: join(directory, 'cache'),
        XDG_DATA_HOME: join(directory, 'data'),
        XDG_STATE_HOME: join(directory, 'state'),
        MEVEDEL_EDITING_TEST_ROOT: directory,
        MEVEDEL_EDITING_RELAY: `ws://127.0.0.1:${port}`,
      };
      host = spawn(
        'emacs',
        [
          '-Q',
          '--batch',
          '-L',
          root,
          '-L',
          join(root, 'test'),
          ...deps,
          '-l',
          join(root, 'shared-editing/test/room.el'),
        ],
        { env },
      );
      host.stdout.on('data', (d) => logs.push(String(d)));
      host.stderr.on('data', (d) => logs.push(String(d)));
      const links = await until(async () => {
        if (host.exitCode !== null) throw new Error(logs.join(''));
        return JSON.parse(await readFile(join(directory, 'links.json'), 'utf8'));
      });
      async function agent(tool, args = {}) {
        await writeFile(join(directory, 'request.tmp'), JSON.stringify({ tool, args }));
        await rename(join(directory, 'request.tmp'), join(directory, 'request.json'));
        const reply = await until(async () =>
          JSON.parse(await readFile(join(directory, 'reply.json'), 'utf8')),
        );
        await rm(join(directory, 'reply.json'));
        assert.equal(reply.error, undefined, JSON.stringify(reply));
        return reply;
      }
      browser = await chromium.launch({ headless: true });
      const contexts = await Promise.all([
        browser.newContext(),
        browser.newContext({
          viewport: { width: 1000, height: 800 },
          deviceScaleFactor: 2,
        }),
        browser.newContext(),
      ]);
      const sockets = [];
      await contexts[0].routeWebSocket('**/r/**', (socket) => {
        sockets.push(socket);
        socket.connectToServer();
      });
      const pages = await Promise.all(contexts.map((c) => c.newPage()));
      pages.forEach((p) => {
        p.on('pageerror', (e) => errors.push(e.message));
        p.on('console', (m) => {
          if (m.type() === 'error') logs.push(m.text());
        });
      });
      const standalone = (link) => {
        const url = new URL(link);
        url.searchParams.set('shared', '');
        return url.href;
      };
      await Promise.all(
        pages.map((p, i) =>
          p.goto(i === 0 ? links.full : standalone(i === 2 ? links.view : links.full)),
        ),
      );
      await Promise.all(
        pages.map((p) => p.waitForFunction(() => !document.getElementById('editing-box').hidden)),
      );
      for (const p of pages) {
        await p.locator('#session-box').evaluate((e) => (e.open = true));
        await p.locator('#editing-box').evaluate((e) => (e.open = true));
      }
      await pages[0].locator('#composer-input').fill('> Browser draft\nsecond line');
      const chatPage = pages[0];
      // The room's explicit theme crosses the opaque iframe boundary on open.
      await chatPage.locator('#appearance-menu > summary').click();
      await chatPage.locator('#appearance').selectOption('dark');
      await chatPage.locator('#appearance-menu > summary').click();
      const openingTab = chatPage.waitForEvent('popup');
      await chatPage.locator('[data-create-editor="whiteboard"]').click();
      pages[0] = await openingTab;
      pages[0].on('pageerror', (e) => errors.push(e.message));
      await pages[0].waitForURL((url) => url.searchParams.has('shared'));
      assert.equal(await pages[0].evaluate(() => window.opener), null);
      assert.equal(await chatPage.locator('#editing-panel').isVisible(), false);
      await pages[0].locator('#session-box').evaluate((e) => (e.open = true));
      await pages[0].locator('#editing-box').evaluate((e) => (e.open = true));
      const frame = (p) => p.frameLocator('#editing-body iframe');
      await frame(pages[0]).locator('#canvas').waitFor({ state: 'visible' });
      // Real request ownership publishes activity even without streamed text.
      for(let cycle=0;cycle<2;cycle++) {
        await agent('RequestState',{busy:true});
        await chatPage.locator('#assistant-working').waitFor({state:'visible'});
        await agent('RequestState',{busy:false});
        await chatPage.locator('#assistant-working').waitFor({state:'hidden'});
      }
      // Compaction must publish the new archive without waiting for another turn.
      await agent('CompactHistory');
      await chatPage.getByText('Earlier conversation · Segment 1',{exact:true}).click();
      await chatPage.locator('#history').getByText('A preserved earlier answer.',{exact:true}).waitFor();
      assert.equal(await chatPage.locator('#composer-input').inputValue(),'> Browser draft\nsecond line');
      const historyReader=await browser.newPage();
      await historyReader.goto(links.view);
      await historyReader.getByText('Earlier conversation · Segment 1',{exact:true}).click();
      await historyReader.locator('#history').getByText('A preserved earlier answer.',{exact:true}).waitFor();
      await historyReader.close();
      assert.equal(await frame(pages[0]).locator('html').getAttribute('data-theme'), 'dark');
      // Programmatic click reaches the room control under the editor panel,
      // exercising the same live theme forwarding without replacing the iframe.
      await pages[0].locator('#appearance-menu > summary').click();
      await pages[0].locator('#appearance').selectOption('system');
      await pages[0].locator('#appearance-menu > summary').click();
      await until(async () => (await frame(pages[0]).locator('html').getAttribute('data-theme')) === null);
      await pages[0].evaluate(() => window.dispatchEvent(new Event('focus')));
      assert.match(await pages[0].title(), /Whiteboard/);
      await until(async () => (await pages[1].locator('#editing-items button').count()) === 1);
      await pages[1].locator('#editing-items button').click();
      await pages[2].locator('#editing-items button').click();
      await Promise.all(
        pages.map((p) => frame(p).locator('#canvas').waitFor({ state: 'visible' })),
      );
      assert.equal(await frame(pages[2]).locator('[data-tool="rectangle"]').count(), 0);
      assert.equal(
        await frame(pages[0])
          .locator('body')
          .evaluate(() => {
            try {
              return parent.document.body ? false : false;
            } catch {
              return true;
            }
          }),
        true,
      );
      async function rectangle(p, x, y) {
        const f = frame(p);
        await f.locator('[data-tool="rectangle"]').click();
        const b = await f.locator('#canvas').boundingBox();
        await p.mouse.move(b.x + x, b.y + y);
        await p.mouse.down();
        await p.mouse.move(b.x + x + 100, b.y + y + 60);
        await p.mouse.up();
      }
      await Promise.all([rectangle(pages[0], 130, 120), rectangle(pages[1], 350, 140)]);
      await until(
        async () =>
          (await frame(pages[0]).locator('#scene [data-shape]').count()) === 2 &&
          (await frame(pages[1]).locator('#scene [data-shape]').count()) === 2,
      );
      const boardId = JSON.parse((await agent('SharedRead')).result).find(
        (i) => i.kind === 'whiteboard',
      ).id;
      const snapshot = JSON.parse((await agent('SharedRead', { id: boardId })).result);
      assert.equal(snapshot.content.length, 2);
      const shape = { id: 'agent_box', type: 'rectangle', x: 600, y: 80, width: 140, height: 70 };
      const label = { id: 'agent_label', type: 'text', x: 600, y: 80, width: 0, height: 0,
        text: 'Agent contribution', containerId: 'agent_box' };
      const applied = await agent('SharedEdit', {
        id: boardId,
        action: 'patch',
        changes: [{ id: shape.id, before: null, after: shape }, { id: label.id, before: null, after: label }],
      });
      assert.equal(applied.status, 'success', JSON.stringify(applied));
      await until(async () => (await frame(pages[1]).locator('#scene [data-shape]').count()) === 4);
      await frame(pages[0]).locator('#undo').click();
      await until(async () => (await frame(pages[1]).locator('#scene [data-shape]').count()) === 3);
      assert.equal(await frame(pages[1]).locator('[data-shape="agent_box"]').count(), 1);
      await frame(pages[0]).locator('#redo').click();
      await until(async () => (await frame(pages[1]).locator('#scene [data-shape]').count()) === 4);
      const savedBefore = JSON.parse((await agent('SharedRead', { id: boardId })).result).revision;
      await frame(pages[1]).getByRole('button', { name: 'Zoom in', exact: true }).click();
      await frame(pages[1]).getByRole('button', { name: 'Zoom in', exact: true }).click();
      await frame(pages[0]).locator('[data-tool="laser"]').click();
      const canvas = await frame(pages[0]).locator('#canvas').boundingBox();
      await pages[0].mouse.move(canvas.x + 200, canvas.y + 200);
      await pages[0].mouse.move(canvas.x + 240, canvas.y + 220, { steps: 8 });
      await until(async () => (await frame(pages[1]).locator('[data-mode="laser"] .pointer-tip').count()) > 0);
      await delay(70);
      await pages[0].mouse.move(canvas.x + 260, canvas.y + 230);
      // Both viewports must identify the same world position, despite different zoom.
      const finalPoint = await frame(pages[0])
        .locator('#canvas')
        .evaluate((svg) => {
          const r = svg.getBoundingClientRect(),
            p = new DOMPoint(r.x + 260, r.y + 230).matrixTransform(svg.getScreenCTM().inverse());
          return [p.x, p.y];
        });
      await until(async () => {
        const points = await frame(pages[1])
          .locator('[data-mode="laser"] .pointer-tip')
          .evaluateAll((nodes) => nodes.map(n => {
            const m = n.transform.baseVal.consolidate().matrix;
            return [m.e, m.f];
          }));
        return points.some(
          (p) => Math.abs(p[0] - finalPoint[0]) < 1 && Math.abs(p[1] - finalPoint[1]) < 1,
        );
      });
      assert.ok((await frame(pages[1]).locator('#presence text').first().textContent()).length > 0);
      // Carry the intermediate circle samples through iframe, relay and host.
      await frame(pages[0]).locator('#canvas').evaluate(async svg => {
        const r = svg.getBoundingClientRect();
        for (let i=0; i<24; i++) {
          svg.dispatchEvent(new PointerEvent('pointermove', {bubbles:true,pointerType:'mouse',
            clientX:r.x+300+70*Math.cos(i/23*Math.PI*2),clientY:r.y+260+70*Math.sin(i/23*Math.PI*2)}));
          await new Promise(resolve=>setTimeout(resolve,10));
        }
      });
      await until(async () => (await frame(pages[1]).locator('.pointer-trail > g').first().locator('path').count()) >= 18);
      await delay(700);
      assert.equal(await frame(pages[1]).locator('.pointer-trail path').count(), 0);
      await pages[0].mouse.move(5, 5);
      await until(async () => (await frame(pages[1]).locator('[data-mode="laser"]').count()) === 0);
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: boardId })).result).revision,
        savedBefore,
      );
      // Presence stays disposable while another browser commits durable content.
      await Promise.all([
        (async () => {
          for (let i = 0; i < 30; i++) {
            await pages[0].mouse.move(canvas.x + 240 + i * 3, canvas.y + 230);
            await delay(20);
          }
        })(),
        (async () => {
          await frame(pages[1]).locator('#title').fill('Live collaboration');
          await frame(pages[1]).locator('#title').press('Tab');
          await until(async () => (await frame(pages[0]).locator('#title').inputValue()) === 'Live collaboration');
        })(),
      ]);
      await pages[0].mouse.move(5, 5);
      await until(async () => (await frame(pages[1]).locator('[data-mode="laser"]').count()) === 0);
      assert.equal(JSON.parse((await agent('SharedRead', {id:boardId})).result).revision, savedBefore + 1);
      await frame(pages[1]).locator('#title').fill('Whiteboard');
      await frame(pages[1]).locator('#title').press('Tab');
      await until(async () => (await frame(pages[0]).locator('#title').inputValue()) === 'Whiteboard');
      // Missing host dependencies leave chat and the catalog usable. A recheck
      // repairs an already-open room without a reload or draft replacement.
      await agent('RuntimeAvailable', { available: false });
      const unavailableContext = await browser.newContext();
      const unavailablePage = await unavailableContext.newPage();
      await unavailablePage.goto(links.full);
      await unavailablePage.locator('#session-box').evaluate(e => e.open = true);
      await unavailablePage.locator('#editing-box').evaluate(e => e.open = true);
      await until(async () => (await unavailablePage.locator('#editing-status').innerText()).includes('Install Node'));
      assert.equal(await unavailablePage.locator('[data-create-editor="document"]').isDisabled(), true);
      assert.equal(await unavailablePage.locator(`#editing-items [data-item-id="${boardId}"]`).isDisabled(), true);
      await unavailablePage.locator('#composer-input').fill('> Optional editing\nChat draft');
      await agent('RuntimeAvailable', { available: true });
      await unavailablePage.locator('#editing-recheck').click();
      await until(async () => (await unavailablePage.locator('#editing-status').innerText()).includes('ready'));
      assert.equal(await unavailablePage.locator('[data-create-editor="document"]').isEnabled(), true);
      assert.equal(await unavailablePage.locator(`#editing-items [data-item-id="${boardId}"]`).isEnabled(), true);
      assert.equal(await unavailablePage.locator('#composer-input').inputValue(), '> Optional editing\nChat draft');
      assert.equal((await agent('InspectTest')).draft, '> Host draft\nsecond line');
      await unavailableContext.close();
      await agent('RestartHelper');
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: boardId })).result).content.length,
        4,
      );
      await pages[0].locator('#editing-close').click();
      await pages[0].locator('[data-create-editor="document"]').click();
      await frame(pages[0]).locator('.tiptap').waitFor({ state: 'visible' });
      await until(async () => (await pages[1].locator('#editing-items button').count()) === 2);
      await pages[1].locator('#editing-close').click();
      await pages[1].locator('#editing-items button').filter({ hasText: 'Document' }).click();
      await frame(pages[1]).locator('.tiptap').waitFor({ state: 'visible' });
      await Promise.all(pages.slice(0, 2).map((p) => frame(p).locator('.tiptap').click()));
      await Promise.all([
        pages[0].keyboard.type('Alice writes. '),
        pages[1].keyboard.type('Bob writes. '),
      ]);
      const documentText = (p) =>
        frame(p)
          .locator('.tiptap')
          .evaluate((e) => {
            const copy = e.cloneNode(true);
            copy.querySelectorAll('.collaboration-carets__caret').forEach((n) => n.remove());
            return copy.textContent;
          });
      await until(async () => {
        const a = await documentText(pages[0]),
          b = await documentText(pages[1]);
        return a === b && a.length >= 24;
      });
      await until(
        async () => (await frame(pages[0]).locator('#saved').innerText()) === 'Saved on host',
      );
      let undone = 0;
      while ((await documentText(pages[0])).trim() !== 'Bob writes.' && undone < 14) {
        const before = await documentText(pages[0]);
        await frame(pages[0]).locator('#undo').click();
        undone++;
        await until(async () => {
          const a = await documentText(pages[0]),
            b = await documentText(pages[1]);
          return a !== before && a === b;
        });
        assert.ok((await documentText(pages[1])).includes('Bob writes.'));
      }
      assert.equal((await documentText(pages[1])).trim(), 'Bob writes.');
      while (undone--) {
        await frame(pages[0]).locator('#redo').click();
      }
      await until(async () => (await documentText(pages[1])).includes('Alice writes.'));
      await until(
        async () => (await frame(pages[0]).locator('#saved').innerText()) === 'Saved on host',
      );
      await frame(pages[0]).locator('.tiptap p').first().click();
      await pages[0].keyboard.press('Home');
      await pages[0].keyboard.press('Shift+End');
      await frame(pages[1]).locator('.tiptap p').first().click();
      await pages[1].keyboard.press('End');
      await Promise.all([
        frame(pages[0]).getByRole('button', { name: 'Bold', exact: true }).click(),
        pages[1].keyboard.type(' With formatting.'),
      ]);
      await until(
        async () =>
          (await documentText(pages[0])) === (await documentText(pages[1])) &&
          (await documentText(pages[0])).includes('With formatting.') &&
          (await frame(pages[1]).locator('.tiptap strong').count()) > 0,
      );
      await until(
        async () => (await frame(pages[0]).locator('#saved').innerText()) === 'Saved on host',
      );
      const documentId = JSON.parse((await agent('SharedRead')).result).find(
        (i) => i.kind === 'document',
      ).id;
      // Deleting an item removes it for everyone; an editor showing it closes.
      await pages[0].locator('#editing-close').click();
      await pages[0].locator('[data-create-editor="whiteboard"]').click();
      await frame(pages[0]).locator('#canvas').waitFor({ state: 'visible' });
      const throwaway = JSON.parse((await agent('SharedRead')).result)
        .find((i) => i.id !== boardId && i.id !== documentId).id;
      await pages[1].locator('#editing-close').click();
      await pages[1].locator(`#editing-items [data-item-id="${throwaway}"]`).click();
      await frame(pages[1]).locator('#canvas').waitFor({ state: 'visible' });
      await frame(pages[0]).locator('#menu').evaluate((e) => (e.open = true));
      await frame(pages[0]).locator('#delete-item').click();
      await pages[0].locator('#delete-shared').waitFor({ state: 'visible' });
      await pages[0].locator('#delete-shared').getByRole('button', { name: 'Delete', exact: true }).click();
      await pages[1].locator('#editing-panel').waitFor({ state: 'hidden' });
      await until(async () =>
        (await pages[1].locator(`#editing-items [data-item-id="${throwaway}"]`).count()) === 0);
      assert.equal(JSON.parse((await agent('SharedRead')).result).some((i) => i.id === throwaway), false);
      for (const p of pages.slice(0, 2)) {
        await p.locator(`#editing-items [data-item-id="${documentId}"]`).click();
        await frame(p).locator('.tiptap').waitFor({ state: 'visible' });
      }
      const docRead = JSON.parse((await agent('SharedRead', { id: documentId })).result);
      const formattedBefore = docRead.content.content[0],
        formattedAfter = structuredClone(formattedBefore);
      formattedAfter.content[0].text += ' Agent review.';
      assert.equal(
        (
          await agent('SharedEdit', {
            id: documentId,
            action: 'patch',
            changes: [
              {
                id: formattedBefore.attrs.id,
                before: formattedBefore,
                after: formattedAfter,
              },
            ],
          })
        ).status,
        'success',
      );
      await until(async () => (await documentText(pages[1])).includes('Agent review.'));
      const paragraph = {
        type: 'paragraph',
        attrs: { id: 'agent_paragraph' },
        content: [{ type: 'text', text: 'An agent added this.' }],
      };
      assert.equal(
        (
          await agent('SharedEdit', {
            id: documentId,
            action: 'patch',
            changes: [{ id: 'agent_paragraph', before: null, after: paragraph }],
          })
        ).status,
        'success',
      );
      await until(async () => (await documentText(pages[1])).includes('An agent added this.'));
      const captured = JSON.parse((await agent('SharedRead', { id: documentId })).result);
      if (!await frame(pages[0]).locator('#assistant').isVisible()) await frame(pages[0]).locator('#ask-toggle').click();
      await frame(pages[0]).locator('#whole-question').click();
      await frame(pages[0]).locator('#question').fill('Review the current notes');
      await frame(pages[0]).locator('#question-send').click();
      const queued = await until(async () => {
        const info = await agent('InspectTest');
        return info.queue.length === 1 ? info : null;
      });
      assert.equal(queued.draft, '> Host draft\nsecond line');
      assert.match(queued.queue[0], /An agent added this/);
      await frame(pages[0]).locator('.tiptap p').last().click();
      await pages[0].keyboard.press('End');
      await pages[0].keyboard.type(' Human revision.');
      await until(
        async () => (await frame(pages[0]).locator('#saved').innerText()) === 'Saved on host',
      );
      const stale = await agent('SharedEdit', {
        id: documentId,
        action: 'patch',
        changes: [
          {
            id: 'never_insert',
            before: null,
            after: { ...paragraph, attrs: { id: 'never_insert' } },
          },
          {
            id: 'agent_paragraph',
            before: captured.content.content.find((n) => n.attrs.id === 'agent_paragraph'),
            after: paragraph,
          },
        ],
      });
      assert.equal(stale.status, 'error');
      assert.equal(JSON.parse(stale.result).code, 'stale');
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: documentId })).result).content.content.some(
          (n) => n.attrs.id === 'never_insert',
        ),
        false,
      );
      assert.equal((await agent('InspectTest')).queue[0], queued.queue[0]);
      await frame(pages[1]).locator('.tiptap p').last().click();
      await pages[1].keyboard.press('Home');
      await pages[1].keyboard.press('Shift+ArrowRight');
      await pages[1].keyboard.press('Shift+ArrowRight');
      assert.equal(
        await frame(pages[1])
          .locator('.tiptap')
          .evaluate(() => window.getSelection().toString()),
        'An',
      );
      await frame(pages[1]).locator('#comment-selection').click();
      await frame(pages[1]).locator('#comment-text').fill('Explain the selected opening');
      await frame(pages[1]).locator('#comment-assistant').uncheck();
      await frame(pages[1]).locator('#comment-post').click();
      await frame(pages[1]).locator('#comments .comment').waitFor();
      assert.equal((await agent('InspectTest')).queue.length, 1, 'posting a comment does not queue a model turn');
      await frame(pages[0]).locator('#comments-tab').click();
      await frame(pages[0]).locator('#comments .comment > summary').click();
      await frame(pages[0]).locator('.reply-form textarea').fill('Include a concrete example.');
      await frame(pages[0]).locator('.reply-assistant').uncheck();
      await frame(pages[0]).getByText('Post reply', {exact:true}).click();
      await frame(pages[1]).locator('.thread-message').getByText('Include a concrete example.', {exact:true}).waitFor();
      assert.equal((await agent('InspectTest')).queue.length, 1, 'human replies do not queue a model turn');
      await frame(pages[0]).locator('.reply-form textarea').fill('> Private reply draft\nsecond line');
      await frame(pages[0]).locator('#assistant-tab').click();
      await frame(pages[0]).locator('#question').fill('Separate private AI question');
      await pages[0].waitForFunction(() => Object.keys(localStorage).some(k=>k.startsWith('mevedel-editing:') && JSON.parse(localStorage[k]).assistant?.question?.text === 'Separate private AI question'));
      await pages[0].reload();
      await frame(pages[0]).locator('.tiptap[contenteditable="true"]').waitFor();
      if (!await frame(pages[0]).locator('#assistant').isVisible()) await frame(pages[0]).locator('#ask-toggle').click();
      assert.equal(await frame(pages[0]).locator('#question').inputValue(),'Separate private AI question');
      await frame(pages[0]).locator('#comments-tab').click();
      await frame(pages[0]).locator('#comments .comment > summary').click();
      assert.equal(await frame(pages[0]).locator('.reply-form textarea').inputValue(),'> Private reply draft\nsecond line');
      await frame(pages[0]).locator('#assistant-close').click();
      await frame(pages[1]).locator('.thread-send').click();
      const rangeAsk = await until(async () => {
        const info = await agent('InspectTest');
        return info.queue.length === 2 ? info.queue[1] : null;
      });
      assert.doesNotMatch(rangeAsk, /"anchors":/);
      assert.match(rangeAsk, /"text":"An"/);
      assert.match(rangeAsk, /Human revision/);
      assert.match(rangeAsk, /Include a concrete example/);
      assert.match(rangeAsk, /"discussion":/);
      await frame(pages[0]).locator('.tiptap p').last().click();
      await pages[0].keyboard.press('End');
      await pages[0].keyboard.type(' Later edit.');
      await until(
        async () => (await frame(pages[0]).locator('#saved').innerText()) === 'Saved on host',
      );
      assert.equal((await agent('InspectTest')).queue[1], rangeAsk);
      await pages[1].locator('#editing-close').click();
      await pages[1].getByRole('button', { name: 'Retract', exact: true }).click();
      await until(async () => (await agent('InspectTest')).queue.length === 1);
      await pages[1].locator(`#editing-items [data-item-id="${documentId}"]`).click();
      await frame(pages[0]).locator('.tiptap p').last().click();
      await pages[0].keyboard.press('End');
      await pages[0].keyboard.press('Enter');
      await until(async () => (await frame(pages[1]).locator('.tiptap p').count()) === 3);
      await until(
        async () => (await frame(pages[0]).locator('#saved').innerText()) === 'Saved on host',
      );
      await Promise.all(pages.slice(0, 2).map((p) => frame(p).locator('.tiptap p').last().click()));
      await Promise.all([
        pages[0].keyboard.type('New Alice. '),
        pages[1].keyboard.type('New Bob. '),
      ]);
      await until(async () => {
        const a = await documentText(pages[0]),
          b = await documentText(pages[1]);
        return a === b && a.includes('New Alice.') && a.includes('New Bob.');
      });
      let freshUndos = 0;
      while ((await documentText(pages[0])).includes('New Alice.') && freshUndos < 12) {
        const before = await documentText(pages[0]);
        await frame(pages[0]).locator('#undo').click();
        freshUndos++;
        await until(async () => {
          const a = await documentText(pages[0]);
          return a !== before && a === (await documentText(pages[1]));
        });
        assert.ok((await documentText(pages[1])).includes('New Bob.'));
      }
      while (freshUndos--) await frame(pages[0]).locator('#redo').click();
      await until(async () => (await documentText(pages[1])).includes('New Alice.'));
      await contexts[0].setOffline(true);
      for (const socket of sockets) await socket.close();
      await frame(pages[0]).locator('.tiptap p').last().click();
      await pages[0].keyboard.press('End');
      await pages[0].keyboard.type(' Offline draft.');
      await until(async () =>
        (await frame(pages[0]).locator('#saved').innerText()).includes('Offline'),
      );
      await frame(pages[1]).locator('.tiptap p').first().click();
      await pages[1].keyboard.press('Home');
      await pages[1].keyboard.type('Meanwhile. ');
      await contexts[0].setOffline(false);
      await until(async () => {
        const a = await documentText(pages[0]),
          b = await documentText(pages[1]);
        return a === b && a.includes('Offline draft.') && a.includes('Meanwhile. ');
      });
      assert.equal(
        await chatPage.locator('#composer-input').inputValue(),
        '> Browser draft\nsecond line',
      );
      assert.equal((await agent('InspectTest')).queue.length, 1);
      // An embedded document image reaches another writer and survives reload.
      const imageData = await pages[0].evaluate(() => {
        const canvas = document.createElement('canvas'); canvas.width = 80; canvas.height = 40;
        canvas.getContext('2d').fillRect(0, 0, 80, 40);
        return canvas.toDataURL();
      });
      await frame(pages[0]).locator('.tiptap').click();
      await pages[0].keyboard.press('Control+End');
      await frame(pages[0]).locator('#image-upload').setInputFiles({
        name:'diagram.png', mimeType:'image/png', buffer:Buffer.from(imageData.split(',')[1],'base64'),
      });
      await frame(pages[1]).locator('.tiptap img').waitFor();
      assert.equal(await frame(pages[1]).locator('.tiptap img').getAttribute('src'),imageData);
      await pages[1].reload();
      await frame(pages[1]).locator('.tiptap img').waitFor();
      assert.equal(await frame(pages[1]).locator('.tiptap img').getAttribute('src'),imageData);
      const illustrated = JSON.parse((await agent('SharedRead', {id:documentId})).result);
      assert.equal(illustrated.content.content.find(n=>n.type==='image').attrs.src,imageData);
      const downloadPromise = pages[2].waitForEvent('download');
      await frame(pages[2])
        .locator('#menu')
        .evaluate((e) => (e.open = true));
      await frame(pages[2]).locator('#download').click();
      const download = await downloadPromise;
      const exported = await readFile(await download.path());
      assert.equal(JSON.parse(exported).type, 'excalidraw', 'a whiteboard downloads as an Excalidraw file');
      await pages[0].locator('#editing-close').click();
      await pages[0].locator('#editing-file').setInputFiles({
        name: 'copy.excalidraw',
        mimeType: 'application/json',
        buffer: exported,
      });
      await frame(pages[0]).locator('#canvas').waitFor({ state: 'visible' });
      const imported = JSON.parse((await agent('SharedRead')).result).filter(
        (i) => i.kind === 'whiteboard' && i.id !== boardId,
      );
      assert.equal(imported.length, 1);
      assert.deepEqual(
        JSON.parse((await agent('SharedRead', { id: imported[0].id })).result).content.map(e => e.id),
        JSON.parse(exported).elements.map(e => e.id),
      );
      if (!await frame(pages[0]).locator('#assistant').isVisible()) await frame(pages[0]).locator('#ask-toggle').click();
      await frame(pages[0]).locator('#question').fill('Private recovered question');
      const capturedContext = await frame(pages[0]).locator('#context-detail').innerText();
      await pages[0].waitForFunction(() => Object.keys(localStorage).some(k=>k.startsWith('mevedel-editing:') && JSON.parse(localStorage[k]).assistant?.question?.text === 'Private recovered question'));
      for (let reload = 0; reload < 2; reload++) {
        await pages[0].reload();
        await frame(pages[0]).locator('[data-tool="rectangle"]').waitFor({state:'visible'});
        if (!await frame(pages[0]).locator('#assistant').isVisible()) await frame(pages[0]).locator('#ask-toggle').click();
        assert.equal(await frame(pages[0]).locator('#question').inputValue(),'Private recovered question',JSON.stringify({reload,storage:await pages[0].evaluate(()=>Object.fromEntries(Object.keys(localStorage).filter(k=>k.startsWith('mevedel-editing:')).map(k=>[k,JSON.parse(localStorage[k]).assistant])))}));
        assert.equal(await frame(pages[0]).locator('#context-detail').innerText(),capturedContext);
      }
      await frame(pages[0]).locator('#assistant-close').click();
      await pages[0].reload();
      await frame(pages[0]).locator('#canvas').waitFor({ state: 'visible' });
      await frame(pages[0]).locator('[data-tool="rectangle"]').waitFor({ state: 'visible', timeout: 2000 });
      async function forged(link, args) {
        return pages[2].evaluate(
          async ({ link, args }) => {
            const api = window.mevedelViewerTransport,
              c = api.parseFragment(new URL(link).hash),
              key = await api.importKey(c.keyBytes);
            return new Promise((resolve, reject) => {
              let transport;
              const timer = setTimeout(() => {
                transport.end();
                reject(new Error('Forged request did not settle'));
              }, 10000);
              transport = api.create({
                roomId: c.roomId,
                key,
                giveUpMs: 10000,
                hello: () => ({
                  t: 'hello',
                  proto: 3,
                  name: 'Forged role',
                  owner: true,
                  writable: true,
                  ...(c.writeToken ? { writeToken: api.base64urlEncode(c.writeToken) } : {}),
                }),
                onConnection: () => {},
                onGiveUp: () => reject(new Error('Room ended')),
                onOpen: () => {},
                onFrame: (frame) => {
                  if (frame.t === 'welcome') {
                    const data = btoa(JSON.stringify(args));
                    transport.send({
                      t: 'editing',
                      reqId: 9001,
                      offset: 0,
                      total: data.length,
                      data,
                    });
                  }
                  if (frame.t === 'editing' && frame.reqId === 9001) {
                    clearTimeout(timer);
                    transport.end();
                    resolve(JSON.parse(atob(frame.data)));
                  }
                },
              });
              transport.connect();
            });
          },
          { link, args },
        );
      }
      assert.match(
        (
          await forged(links.view, {
            action: 'rename',
            id: boardId,
            title: 'Forged rename',
            opId: 'forged',
            actor: 'Agent: /root',
          })
        ).error,
        /does not permit/,
      );
      assert.match(
        (await forged(links.full, { action: 'read', id: '../../secret' })).error,
        /identity/,
      );
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: boardId })).result).title,
        'Whiteboard',
      );
      // Real parent/iframe viewport changes, with space for an iPhone-sized keyboard.
      const phone = await browser.newContext({
        viewport: { width: 375, height: 812 },
        isMobile: true,
        hasTouch: true,
      });
      const phonePage = await phone.newPage();
      const phoneUrl = new URL(links.full);
      phoneUrl.searchParams.set('shared', documentId);
      await phonePage.goto(phoneUrl.href);
      await frame(phonePage).locator('.tiptap').waitFor();
      await phonePage.setViewportSize({ width: 375, height: 340 });
      await frame(phonePage).locator('.tiptap p').last().click();
      await phonePage.keyboard.press('End');
      await phonePage.keyboard.type(' Phone typing.');
      const geometry = await frame(phonePage)
        .locator('#document')
        .evaluate((container) => {
          const rect = container.getBoundingClientRect();
          const range = document.getSelection().getRangeAt(0).cloneRange();
          range.collapse(false);
          const caret = range.getBoundingClientRect();
          return {
            height: rect.height,
            top: rect.top,
            bottom: rect.bottom,
            caretTop: caret.top,
            caretBottom: caret.bottom,
          };
        });
      // The editor chrome's height follows the system font's line height, so
      // the room left for text is held to a margin rather than a font's pixel.
      assert.ok(geometry.height >= 140, JSON.stringify(geometry));
      assert.ok(
        geometry.caretTop >= geometry.top && geometry.caretBottom <= geometry.bottom,
        JSON.stringify(geometry),
      );
      await until(
        async () => (await frame(phonePage).locator('#saved').innerText()) === 'Saved on host',
      );
      await phone.close();
      await Promise.all(contexts.map((c) => c.close()));
      // Repeated image movement must not fill durable history or saturate
      // the relay with repeated before/after image bytes on every save.
      const imageContext = await browser.newContext();
      const imageWriter = await imageContext.newPage();
      const src = await imageWriter.evaluate(() => {
        const canvas = document.createElement('canvas');
        canvas.width = 512; canvas.height = 512;
        const ctx = canvas.getContext('2d'), pixels = ctx.createImageData(512,512);
        let seed = 12345;
        for (let i=0;i<pixels.data.length;i++) {
          seed = (Math.imul(seed,1664525)+1013904223)>>>0;
          pixels.data[i] = i%4===3 ? 255 : seed>>>24;
        }
        ctx.putImageData(pixels,0,0);
        return canvas.toDataURL();
      });
      assert.ok(src.length>900000);
      // Image bytes arrive as files of an imported Excalidraw scene; the model cannot author them.
      const imageBoard = JSON.parse((await agent('ImportShared',{title:'Large image synchronization',
        data:JSON.stringify({type:'excalidraw',version:2,
          elements:[{id:'large-image',type:'image',x:100,y:100,width:300,height:300,fileId:'large'}],
          files:{large:{id:'large',mimeType:'image/png',dataURL:src}}})})).result);
      const imageURL = new URL(links.full);
      imageURL.searchParams.set('shared',imageBoard.id);
      const imageReader = await browser.newPage();
      await Promise.all([imageWriter.goto(imageURL.href),imageReader.goto(imageURL.href)]);
      await frame(imageWriter).locator('#scene image').waitFor();
      await frame(imageReader).locator('#scene image').waitFor();
      await frame(imageWriter).locator('#scene image').click();
      const moveLatencies=[];
      for(let move=1;move<=12;move++) {
        await frame(imageWriter).locator('#canvas').focus();
        const started=performance.now();
        await imageWriter.keyboard.press('ArrowRight');
        await until(async()=>(await frame(imageReader).locator('[data-shape="large-image"] > g').getAttribute('transform')).startsWith(`translate(${100+move} 100)`));
        await until(async()=>await frame(imageWriter).locator('#saved').innerText()==='Saved on host');
        moveLatencies.push(performance.now()-started);
      }
      const medianMove=moveLatencies.toSorted((a,b)=>a-b)[Math.floor(moveLatencies.length/2)];
      console.log(`Image board single-move median ${Math.round(medianMove)} ms; max ${Math.round(Math.max(...moveLatencies))} ms`);
      assert.ok(medianMove<1200,`single moves must not stall in large pipe writes: ${moveLatencies.map(Math.round)}`);
      await rectangle(imageWriter,400,250);
      await until(async()=>await frame(imageReader).locator('#scene [data-shape]').count()===2);
      await until(async()=>await frame(imageWriter).locator('#saved').innerText()==='Saved on host');
      const imageState=JSON.parse((await agent('SharedRead',{id:imageBoard.id})).result);
      assert.equal(imageState.content.length,2);
      assert.ok(!JSON.stringify(imageState.transactions).includes('base64'),
        'image bytes live in files, never in contribution snapshots');
      const typedShape=imageState.content.find(shape=>shape.type==='rectangle');
      const moving=frame(imageWriter).locator(`#scene [data-shape="${typedShape.id}"]`);
      const watching=frame(imageReader).locator(`#scene [data-shape="${typedShape.id}"]`);
      const original=await watching.boundingBox(), handle=await moving.boundingBox();
      await imageWriter.mouse.move(handle.x+handle.width/2,handle.y+handle.height/2);
      await imageWriter.mouse.down();
      const previewAt=performance.now();
      await imageWriter.mouse.move(handle.x+handle.width/2+70,handle.y+handle.height/2+30);
      await until(async()=>await watching.getAttribute('data-live-preview')==='true',1500);
      assert.ok(Math.abs((await watching.boundingBox()).x-original.x)>30,'other participant sees movement before release');
      console.log(`Remote drag preview arrived in ${Math.round(performance.now()-previewAt)} ms before release`);
      const whileDragging=JSON.parse((await agent('SharedRead',{id:imageBoard.id})).result);
      const place=({x,y,width,height})=>[x,y,width,height];
      assert.deepEqual(place(whileDragging.content.find(s=>s.id===typedShape.id)),place(typedShape),'preview does not save a revision');
      await frame(imageWriter).locator('#canvas').dispatchEvent('pointercancel');
      await imageWriter.mouse.up();
      await until(async()=>await watching.getAttribute('data-live-preview')==='false');
      assert.ok(Math.abs((await watching.boundingBox()).x-original.x)<2,'cancel restores the saved geometry');
      await imageWriter.mouse.move(handle.x+handle.width/2,handle.y+handle.height/2);
      await imageWriter.mouse.down();
      await imageWriter.mouse.move(handle.x+handle.width/2+70,handle.y+handle.height/2+30);
      await until(async()=>await watching.getAttribute('data-live-preview')==='true');
      const previewBox=await watching.boundingBox();
      await imageWriter.mouse.up();
      await until(async()=>await frame(imageWriter).locator('#saved').innerText()==='Saved on host');
      await until(async()=>await watching.getAttribute('data-live-preview')==='false');
      assert.ok(Math.abs((await watching.boundingBox()).x-previewBox.x)<2,'saved geometry replaces the preview without a jump');
      await frame(imageWriter).locator('#canvas').focus();
      await imageWriter.keyboard.press('Enter');
      await frame(imageWriter).locator('#shape-text').fill('');
      await imageWriter.keyboard.type('test1234\n3333',{delay:350});
      const typedAt=performance.now();
      await until(async()=>/test1234\s*3333/.test(await frame(imageReader).locator('#scene').textContent()),8000);
      await until(async()=>await frame(imageWriter).locator('#saved').innerText()==='Saved on host',8000);
      console.log(`Image board typing caught up in ${Math.round(performance.now()-typedAt)} ms`);
      await imageReader.close();
      await imageContext.close();
      await agent('RestartHelper');
      const headless = { id: 'headless', type: 'stickynote', x: 100, y: 300, width: 160, height: 100 };
      const headlessLabel = { id: 'headless_label', type: 'text', x: 100, y: 300, width: 0, height: 0,
        text: 'Created with all browsers closed', containerId: 'headless' };
      assert.equal(
        (
          await agent('SharedEdit', {
            id: boardId,
            action: 'patch',
            changes: [{ id: headless.id, before: null, after: headless },
              { id: headlessLabel.id, before: null, after: headlessLabel }],
          })
        ).status,
        'success',
      );
      const owner = await browser.newContext({
        hasTouch: true,
        viewport: { width: 1000, height: 900 },
        deviceScaleFactor: 2,
      });
      const ownerSockets = [];
      await owner.routeWebSocket('**/r/**', (socket) => {
        ownerSockets.push(socket);
        socket.connectToServer();
      });
      const ownerPage = await owner.newPage();
      ownerPage.on('pageerror', (error) => errors.push(error.message));
      await ownerPage.goto(standalone(links.owner));
      await ownerPage.waitForFunction(() => !document.getElementById('editing-box').hidden);
      await ownerPage.locator('#session-box').evaluate((e) => (e.open = true));
      await ownerPage.locator('#editing-box').evaluate((e) => (e.open = true));
      let releaseEditor,
        editorRequested = false;
      const editorGate = new Promise((resolve) => {
        releaseEditor = resolve;
      });
      await ownerPage.route('**/shared-editor.html', async (route) => {
        editorRequested = true;
        await editorGate;
        await route.continue();
      });
      await ownerPage.locator(`#editing-items [data-item-id="${boardId}"]`).click();
      await until(() => editorRequested);
      const duringLoad = { id: 'during_load', type: 'rectangle', x: 350, y: 300, width: 120, height: 60 };
      assert.equal(
        (
          await agent('SharedEdit', {
            id: boardId,
            action: 'patch',
            changes: [{ id: duringLoad.id, before: null, after: duringLoad }],
          })
        ).status,
        'success',
      );
      releaseEditor();
      await frame(ownerPage).locator('[data-shape="headless"]').waitFor({ state: 'visible' });
      await frame(ownerPage).locator('[data-shape="during_load"]').waitFor({ state: 'visible' });
      for (const format of ['svg', 'png']) {
        await frame(ownerPage)
          .locator('#menu')
          .evaluate((e) => (e.open = true));
        await frame(ownerPage).locator('#export').selectOption(format);
        const waiting = ownerPage.waitForEvent('download');
        await frame(ownerPage)
          .locator('#menu')
          .evaluate((e) => (e.open = true));
        await frame(ownerPage).locator('#download').click();
        const file = await readFile(await (await waiting).path());
        if (format === 'png') assert.equal(file.subarray(0, 8).toString('hex'), '89504e470d0a1a0a');
        // The note's label wraps into one positioned line per row.
        else assert.equal([...file.toString().matchAll(/>([^<>]*)<\/text>/g)].map(m => m[1]).join(' ')
          .includes('Created with all browsers closed'), true);
      }
      const large = {
        type: 'excalidraw',
        version: 2,
        elements: [
          {
            id: 'stroke',
            type: 'freedraw',
            x: 0, y: 0, width: 400, height: 100,
            points: Array.from({ length: 4000 }, (_, i) => [i / 10, 50 + Math.sin(i / 10) * 40]),
          },
        ],
      };
      const largeBytes = Buffer.from(JSON.stringify(large));
      assert.ok(largeBytes.length > 65536);
      await ownerPage.locator('#editing-close').click();
      await ownerPage.locator('#editing-file').setInputFiles({
        name: 'Transferred drawing.excalidraw',
        mimeType: 'application/json',
        buffer: largeBytes,
      });
      await frame(ownerPage).locator('[data-shape="stroke"]').waitFor({ state: 'visible' });
      const transferred = JSON.parse((await agent('SharedRead')).result).find(
        (i) => i.title === 'Transferred drawing.excalidraw',
      );
      assert.deepEqual(
        JSON.parse((await agent('SharedRead', { id: transferred.id })).result).content,
        large.elements,
      );
      const priorRevision = transferred.revision;
      await agent('StorageWritable', { writable: false });
      await rectangle(ownerPage, 150, 160);
      await until(
        async () =>
          (await frame(ownerPage).locator('#saved').getAttribute('data-error')) === 'true',
      );
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: transferred.id })).result).revision,
        priorRevision,
      );
      const recoveryWaiting = ownerPage.waitForEvent('download');
      await frame(ownerPage)
        .locator('#menu')
        .evaluate((e) => (e.open = true));
      await frame(ownerPage).locator('#recovery').click();
      const recoveryDownload = await recoveryWaiting;
      assert.match(recoveryDownload.suggestedFilename(), /recovery/);
      assert.equal(
        JSON.parse(await readFile(await recoveryDownload.path(), 'utf8')).elements.length,
        2,
      );
      await agent('StorageWritable', { writable: true });
      await agent('RetryPublication');
      await frame(ownerPage)
        .locator('#menu')
        .evaluate((e) => (e.open = true));
      await frame(ownerPage).locator('#retry').click();
      await until(
        async () => (await frame(ownerPage).locator('#saved').innerText()) === 'Saved on host',
      );
      await agent('RestartHelper');
      const recovered = JSON.parse((await agent('SharedRead', { id: transferred.id })).result);
      assert.equal(recovered.content.length, 2);
      assert.equal(recovered.revision, priorRevision + 1);
      await frame(ownerPage).locator('#canvas').focus();
      await ownerPage.keyboard.press('d');
      assert.equal(
        await frame(ownerPage).locator('[data-tool="diamond"]').getAttribute('aria-pressed'),
        'true',
      );
      const device = await owner.newCDPSession(ownerPage),
        area = await frame(ownerPage).locator('#canvas').boundingBox();
      await device.send('Input.dispatchTouchEvent', {
        type: 'touchStart',
        touchPoints: [{ x: area.x + 250, y: area.y + 120, id: 1 }],
      });
      await device.send('Input.dispatchTouchEvent', {
        type: 'touchMove',
        touchPoints: [{ x: area.x + 350, y: area.y + 190, id: 1 }],
      });
      await device.send('Input.dispatchTouchEvent', {
        type: 'touchEnd',
        touchPoints: [],
      });
      await until(
        async () => (await frame(ownerPage).locator('#scene [data-shape]').count()) === 3,
      );
      await until(
        async () => (await frame(ownerPage).locator('#saved').innerText()) === 'Saved on host',
      );
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: transferred.id })).result).content.filter(
          (s) => s.type === 'diamond',
        ).length,
        1,
      );
      await frame(ownerPage).locator('[data-tool="freedraw"]').click();
      await device.send('Input.dispatchMouseEvent', {
        type: 'mousePressed',
        x: area.x + 350,
        y: area.y + 220,
        button: 'left',
        buttons: 1,
        clickCount: 1,
        pointerType: 'pen',
      });
      await device.send('Input.dispatchMouseEvent', {
        type: 'mouseMoved',
        x: area.x + 400,
        y: area.y + 230,
        button: 'left',
        buttons: 1,
        pointerType: 'pen',
      });
      await device.send('Input.dispatchMouseEvent', {
        type: 'mouseReleased',
        x: area.x + 430,
        y: area.y + 235,
        button: 'left',
        buttons: 0,
        clickCount: 1,
        pointerType: 'pen',
      });
      await until(
        async () => (await frame(ownerPage).locator('#scene [data-shape]').count()) === 4,
      );
      await until(
        async () => (await frame(ownerPage).locator('#saved').innerText()) === 'Saved on host',
      );
      // The pen keeps drawing, so the stroke is selected to ask about it.
      await frame(ownerPage).locator('[data-tool="select"]').click();
      await ownerPage.mouse.click(area.x + 400, area.y + 230);
      await frame(ownerPage).locator('#selection-question').click();
      await frame(ownerPage).locator('body').evaluate(() => {
        const post = MessagePort.prototype.postMessage;
        MessagePort.prototype.postMessage = function(data, ...rest) {
          if (data?.type === 'request' && data.args?.action === 'ask')
            window.retryQuestion = () => post.call(this, {...data, reqId:crypto.randomUUID()});
          return post.call(this, data, ...rest);
        };
      });
      await frame(ownerPage).locator('#question').fill('Explain this stroke');
      // A file dropped on the form rides beside the board snapshot.
      await frame(ownerPage).locator('#ask').evaluate((form) => {
        const transfer = new DataTransfer();
        transfer.items.add(new File(['stroke notes'], 'notes.txt', { type: 'text/plain' }));
        for (const type of ['dragenter', 'dragover', 'drop'])
          form.dispatchEvent(new DragEvent(type, { bubbles: true, cancelable: true, dataTransfer: transfer }));
      });
      await frame(ownerPage).locator('#question-attachments .attachment').waitFor();
      await frame(ownerPage).locator('#question-send').click();
      const asked = await until(async () => {
        const info = await agent('InspectTest');
        return info.queue.length === 2 ? info : null;
      });
      assert.deepEqual(asked.attachments, [0, 2]);
      assert.equal(await frame(ownerPage).locator('#question-attachments .attachment').count(), 0);
      assert.match(asked.queue[1], /"type":"freedraw"/);
      assert.doesNotMatch(asked.queue[1], /"type":"diamond"/);
      await frame(ownerPage).locator('body').evaluate(() => window.retryQuestion());
      await delay(300);
      assert.equal((await agent('InspectTest')).queue.length, 2);
      await ownerPage.locator('#editing-close').click();
      await ownerPage.getByRole('button', { name: 'Retract', exact: true }).click();
      await until(async () => (await agent('InspectTest')).queue.length === 1);
      await ownerPage.locator(`#editing-items [data-item-id="${transferred.id}"]`).click();
      await ownerPage.evaluate(() => {
        window.originalSetItem = Storage.prototype.setItem;
        Storage.prototype.setItem = function (key, value) {
          if (key.startsWith('mevedel-editing:'))
            throw new DOMException('Quota exceeded', 'QuotaExceededError');
          return window.originalSetItem.call(this, key, value);
        };
      });
      await rectangle(ownerPage, 260, 210);
      await until(async () =>
        (await frame(ownerPage).locator('#saved').innerText()).includes('recovery storage'),
      );
      await delay(700);
      assert.match(
        await frame(ownerPage).locator('#saved').innerText(),
        /Download a recovery copy/,
      );
      await ownerPage.evaluate(() => {
        Storage.prototype.setItem = window.originalSetItem;
        delete window.originalSetItem;
      });
      await rectangle(ownerPage, 320, 270);
      await until(
        async () => (await frame(ownerPage).locator('#saved').innerText()) === 'Saved on host',
      );
      const beforeEnd = JSON.parse((await agent('SharedRead', { id: transferred.id })).result);
      await owner.setOffline(true);
      for (const socket of ownerSockets) await socket.close();
      await rectangle(ownerPage, 200, 250);
      await until(async () =>
        (await frame(ownerPage).locator('#saved').innerText()).includes('Offline'),
      );
      await agent('EndShare');
      await owner.setOffline(false);
      const endedRecovery = ownerPage.waitForEvent('download');
      await frame(ownerPage)
        .locator('#menu')
        .evaluate((e) => (e.open = true));
      await frame(ownerPage).locator('#recovery').click();
      assert.equal(
        JSON.parse(await readFile(await (await endedRecovery).path(), 'utf8')).elements.length,
        beforeEnd.content.length + 1,
      );
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: transferred.id })).result).revision,
        beforeEnd.revision,
      );
      await ownerPage.reload();
      await ownerPage.locator('#session-box').evaluate((e) => (e.open = true));
      await ownerPage.locator('#editing-box').evaluate((e) => (e.open = true));
      const recoveryButton = ownerPage.locator(`#editing-items [data-item-id="${transferred.id}"]`);
      await frame(ownerPage).locator('#canvas').waitFor({ state: 'visible' });
      await ownerPage.locator('#editing-close').click();
      await recoveryButton.waitFor({ state: 'visible' });
      assert.match(await recoveryButton.innerText(), /local recovery/);
      await recoveryButton.click();
      await frame(ownerPage).locator('#canvas').waitFor({ state: 'visible' });
      assert.equal(await frame(ownerPage).locator('[data-tool="rectangle"]').count(), 0);
      const recoveredDownload = ownerPage.waitForEvent('download');
      await frame(ownerPage)
        .locator('#menu')
        .evaluate((e) => (e.open = true));
      await frame(ownerPage).locator('#recovery').click();
      assert.equal(
        JSON.parse(await readFile(await (await recoveredDownload).path(), 'utf8')).elements.length,
        beforeEnd.content.length + 1,
      );
      await agent('FenceHost');
      const fenced = await agent('SharedEdit', {
        id: transferred.id,
        action: 'rename',
        title: 'Stale owner',
      });
      assert.notEqual(fenced.status, 'success');
      assert.equal(
        JSON.parse((await agent('SharedRead', { id: transferred.id })).result).title,
        beforeEnd.title,
      );
      assert.deepEqual(errors, []);
    } catch (error) {
      for (const context of browser?.contexts() || [])
        for (const page of context.pages())
          for (const frame of page.frames()) {
            logs.push(
              await frame
                .locator('body')
                .innerText()
                .catch(() => ''),
            );
          }
      throw new Error(`${error.stack}\n${errors.join('\n')}\n${logs.join('\n').slice(-10000)}`);
    } finally {
      await browser?.close();
      if (host && host.exitCode === null) {
        await writeFile(join(directory, 'stop'), '');
        await until(() => host.exitCode !== null, 10000).catch(() => host.kill());
      }
      relay?.kill();
      await rm(directory, { recursive: true, force: true });
    }
  },
);
