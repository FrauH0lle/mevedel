import test from 'node:test';
import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { handle } from '../host.mjs';

// The real packaged iframe and port, without room setup, for interaction regressions.
test('editor interaction regressions', async (t) => {
  const server = createServer(async (req, res) => {
    const name = req.url.slice(1);
    if (['shared-editor.html', 'shared-editor.css', 'shared-editor.js'].includes(name)) {
      res.setHeader(
        'Content-Type',
        name.endsWith('.js') ? 'text/javascript' : name.endsWith('.css') ? 'text/css' : 'text/html',
      );
      res.end(await readFile(new URL(`../../relay/viewer/${name}`, import.meta.url)));
    } else
      res.end(
        '<style>body{margin:0}iframe{width:100vw;height:100dvh;border:0}</style><iframe sandbox="allow-scripts allow-forms" src="/shared-editor.html"></iframe>',
      );
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const browser = await chromium.launch({ headless: true });
  async function open(kind = 'whiteboard', viewport = { width: 1000, height: 700 }) {
    const page = await browser.newPage({ viewport });
    const created = await handle({
      action: 'create',
      id: 'test',
      opId: 'create',
      actor: 'Guest: Alice',
      kind,
      content:
        kind === 'whiteboard'
          ? [{ id: 'ellipse', type: 'ellipse', box: [100, 100, 300, 160] }]
          : undefined,
    });
    let state = created.state;
    await page.exposeFunction('apply', async (args) => {
      const reply = await handle({ ...args, state, actor: 'Guest: Alice' });
      state = reply.state || state;
      return { ...reply.result, transactions: state.transactions };
    });
    await page.goto(`http://127.0.0.1:${server.address().port}`);
    await page.evaluate(
      (item) => {
        const channel = new MessageChannel();
        window.port = channel.port1;
        window.messages = [];
        channel.port1.onmessage = async ({ data }) => {
          window.messages.push(data);
          if (data.type === 'request') {
            const result = await window.apply(data.args);
            channel.port1.postMessage({ type: 'changed', ...result });
            channel.port1.postMessage({
              type: 'reply',
              reqId: data.reqId,
              result,
            });
          }
        };
        document
          .querySelector('iframe')
          .contentWindow.postMessage(
            { type: 'mevedel-editor', item, readOnly: false, name: 'Alice' },
            '*',
            [channel.port2],
          );
      },
      { ...state, crdt: state.crdt },
    );
    const frame = page.frameLocator('iframe');
    await frame.locator(kind === 'whiteboard' ? '#scene [data-shape]' : '.tiptap').waitFor();
    return { page, frame };
  }
  try {
    await t.test('ellipse interior selects and double click edits its text', async () => {
      const { page, frame } = await open();
      await frame.locator('#scene ellipse').click({ position: { x: 150, y: 80 }, force: true });
      assert.equal(await frame.locator('#selection [data-resize="ellipse"]').count(), 1);
      await frame.locator('#scene ellipse').dblclick({ force: true });
      await frame.locator('#shape-text').waitFor({ state: 'visible', timeout: 1000 });
      await frame.locator('#shape-text').fill('Database');
      await page.keyboard.press('Control+Enter');
      assert.equal(await frame.locator('#scene text').textContent(), 'Database');
      await frame.locator('#scene ellipse').dblclick({ force: true });
      await frame.locator('#shape-text').fill('Discard');
      await page.keyboard.press('Escape');
      assert.equal(await frame.locator('#scene text').textContent(), 'Database');
      if (process.env.MEVEDEL_EDITOR_SCREENSHOTS)
        await page.screenshot({
          path: '.scratch/shared-collaborative-editing/board-revised.png',
        });
      await page.close();
    });
    await t.test('one moving cursor per peer, with a single name', async () => {
      const { page, frame } = await open();
      await page.evaluate(() => {
        for (let i = 0; i < 8; i++)
          window.port.postMessage({
            type: 'presence',
            peer: 2,
            name: 'Bob',
            mode: 'cursor',
            point: [100 + i * 10, 150],
          });
      });
      await frame.locator('#presence text').first().waitFor();
      assert.equal(await frame.locator('#presence text').count(), 1);
      await page.close();
    });
    await t.test('style palette changes selected shapes and subsequent drawings', async () => {
      const { page, frame } = await open();
      await frame.locator('#scene ellipse').click({ force: true });
      await frame.locator('#properties > summary').click();
      await frame.getByRole('button', { name: 'Fill: #a5d8ff', exact: true }).click();
      assert.equal(await frame.locator('#scene ellipse').getAttribute('fill'), '#a5d8ff');
      await frame.getByRole('button', { name: 'Rectangle', exact: true }).click();
      const box = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(box.x + 650, box.y + 180);
      await page.mouse.down();
      await page.mouse.move(box.x + 750, box.y + 250);
      await page.mouse.up();
      assert.equal(await frame.locator('#scene [data-shape] > rect').getAttribute('fill'), '#a5d8ff');
      await page.close();
    });
    await t.test(
      'local and remote laser use a line and one label, then disappear without edits',
      async () => {
        const { page, frame } = await open();
        await frame.getByRole('button', { name: 'Laser pointer', exact: true }).click();
        const box = await frame.locator('#canvas').boundingBox();
        await page.mouse.move(box.x + 160, box.y + 180);
        await page.mouse.down();
        await page.mouse.move(box.x + 250, box.y + 200, { steps: 8 });
        await page.mouse.up();
        assert.equal(await frame.locator('#presence text').textContent(), 'Alice');
        assert.ok(
          (await frame.locator('#presence polyline').getAttribute('points')).split(' ').length > 1,
        );
        await page.evaluate(() => {
          for (let i = 0; i < 8; i++)
            window.port.postMessage({
              type: 'presence',
              peer: 2,
              name: 'Bob',
              mode: 'laser',
              point: [100 + i * 10, 150],
            });
        });
        await frame.locator('#presence text').filter({ hasText: 'Bob' }).waitFor();
        assert.equal(await frame.locator('#presence text').count(), 2);
        assert.equal(await frame.locator('#presence circle').count(), 2);
        await page.waitForTimeout(1600);
        assert.equal(await frame.locator('#presence > *').count(), 0);
        assert.equal(
          await page.evaluate(
            () =>
              window.messages.filter((m) => m.type === 'request' && m.args.action === 'update')
                .length,
          ),
          0,
        );
        await page.close();
      },
    );
    await t.test(
      'contributions group typing bursts and highlight assistant targets without changing exports',
      async () => {
        const { page, frame } = await open('document');
        await page.evaluate(async () => {
          const before = (await window.apply({ action: 'read' })).content.content[0];
          const result = await window.apply({
            action: 'patch',
            opId: 'agent-patch',
            changes: [
              {
                id: before.attrs.id,
                before,
                after: {
                  ...before,
                  content: [{ type: 'text', text: 'Assistant text' }],
                },
              },
            ],
          });
          window.port.postMessage({
            type: 'changed',
            ...result,
            transactions: [
              { ...result.transactions[0], actor: 'Agent: /root' },
              { actor: 'Guest: Bob', revision: 5, time: 14000, changes: [] },
              { actor: 'Guest: Bob', revision: 4, time: 13000, changes: [] },
              { actor: 'Guest: Alice', revision: 3, time: 12000, changes: [] },
              { actor: 'Guest: Alice', revision: 2, time: 11000, changes: [] },
              { actor: 'Guest: Alice', revision: 1, time: 1000, changes: [] },
            ],
          });
        });
        await frame.locator('.agent-contribution').waitFor();
        await frame.locator('#menu > summary').click();
        await frame.locator('#history > summary').click();
        assert.equal(await frame.locator('#contributions > div').count(), 4);
        assert.match(await frame.locator('#contributions').textContent(), /revisions 4–5/);
        await frame.locator('#show-agent').uncheck();
        assert.equal(await frame.locator('.agent-contribution').count(), 0);
        const html = await page.evaluate(
          async () => (await window.apply({ action: 'export', format: 'html' })).text,
        );
        assert.match(html, /Assistant text/);
        assert.doesNotMatch(html, /agent-contribution|Agent:|efedff/);
        await page.close();
      },
    );
    await t.test('phone with keyboard leaves room for document and avoids input zoom', async () => {
      const { page, frame } = await open('document', {
        width: 375,
        height: 340,
      });
      await frame.locator('.tiptap').click();
      const bounds = await frame.locator('#document').boundingBox();
      assert.ok(bounds.height >= 150, `Only ${bounds.height}px left for typing`);
      assert.ok(
        await frame
          .locator('#question')
          .evaluate((e) => parseFloat(getComputedStyle(e).fontSize) >= 16),
      );
      await page.keyboard.type('A document we can work on together.');
      if (process.env.MEVEDEL_EDITOR_SCREENSHOTS)
        await page.screenshot({
          path: '.scratch/shared-collaborative-editing/phone-revised.png',
        });
      await page.close();
    });
  } finally {
    await browser.close();
    await new Promise((r) => server.close(r));
  }
});
