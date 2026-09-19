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
    if (name === 'menu') {
      res.end((await readFile(new URL('../../relay/viewer/index.html', import.meta.url), 'utf8'))
        .replace(/<script[\s\S]*?<\/script>/g, ''));
    } else if (/^viewer[\w-]*\.css$/.test(name) || ['shared-editor.html', 'shared-editor.css', 'shared-editor.js', 'renderer.js'].includes(name)) {
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
      const reply = await handle({ ...args, action:args.action === 'ask' ? 'read' : args.action, question:args.action === 'ask', state, actor: 'Guest: Alice' });
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
            try {
              if (window.rejectQuestion && data.args.action === 'ask') throw new Error('Host refused this submission');
              const result = await window.apply(data.args);
              channel.port1.postMessage({ type: 'changed', ...result });
              channel.port1.postMessage({ type: 'reply', reqId: data.reqId, result });
            } catch (error) {
              channel.port1.postMessage({ type: 'reply', reqId: data.reqId, error:error.message });
            }
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
    await t.test('Shared controls retain readable proportions across item counts, viewport and zoom', async () => {
      const page = await browser.newPage();
      await page.goto(`http://127.0.0.1:${server.address().port}/menu`);
      await page.evaluate(() => {
        for (const id of ['session-box','editing-box']) {
          const el = document.getElementById(id); el.hidden = false; el.open = true;
        }
        document.getElementById('composer').hidden = false;
        document.getElementById('composer-input').value = '> Draft\nsecond line';
      });
      for (const width of [320, 375, 1100]) for (const zoom of [1, 2]) for (const count of [0, 1, 30]) {
        await page.setViewportSize({width, height:760});
        await page.evaluate(({count,zoom}) => {
          document.body.style.zoom = zoom;
          const list = document.getElementById('editing-items'); list.replaceChildren();
          for (let i = 0; i < count; i++) {
            const b = document.createElement('button'); b.className = 'btn quiet';
            b.textContent = 'Architecture discussion and a longer item title ' + i; list.append(b);
          }
        }, {count,zoom});
        const boxes = await page.locator('[data-create-editor], #editing-import').evaluateAll(nodes => nodes.map(n => {
          const b = n.getBoundingClientRect(); return {x:b.x, right:b.right, width:b.width, height:b.height};
        }));
        for (const b of boxes) {
          assert.ok(b.x >= 0 && b.right <= width, JSON.stringify({width,zoom,count,b}));
          assert.ok(b.height >= 40 * zoom && b.height <= 65 * zoom, JSON.stringify(b));
          assert.ok(b.width > b.height, JSON.stringify(b));
        }
        assert.equal(await page.locator('#composer-input').inputValue(), '> Draft\nsecond line');
        assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
      }
      await page.close();
    });
    await t.test('ellipse interior selects and double click edits its text', async () => {
      const { page, frame } = await open();
      await frame
        .locator('#scene [data-shape="ellipse"] path')
        .click({ position: { x: 150, y: 80 }, force: true });
      assert.equal(await frame.locator('#selection [data-resize="ellipse"]').count(), 1);
      await frame.locator('#scene [data-shape="ellipse"] path').dblclick({ force: true });
      await frame.locator('#shape-text').waitFor({ state: 'visible', timeout: 1000 });
      await frame.locator('#shape-text').fill('Database');
      await page.keyboard.press('Control+Enter');
      assert.equal(await frame.locator('#scene text').textContent(), 'Database');
      await frame.locator('#scene [data-shape="ellipse"] path').dblclick({ force: true });
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
    await t.test('style panel changes selected shapes and subsequent drawings', async () => {
      const { page, frame } = await open();
      assert.equal(await frame.locator('#properties').isHidden(), true, 'nothing to style');
      await frame.locator('#scene [data-shape="ellipse"] path').click({ force: true });
      await frame.locator('#properties > summary').click();
      const shown = async () =>
        frame.locator('.sec:not([hidden]) h4').allTextContents();
      assert.deepEqual(await shown(), [
        'Stroke',
        'Background',
        'Stroke width',
        'Stroke style',
        'Sloppiness',
        'Opacity',
        'Layers',
        'Actions',
      ]);
      await frame.getByRole('button', { name: 'Background: #a5d8ff', exact: true }).click();
      assert.equal(
        await frame.locator('#scene [data-shape="ellipse"] path').first().getAttribute('fill'),
        '#a5d8ff',
      );
      assert.ok((await shown()).includes('Fill'), 'a filled shape offers fill patterns');
      await frame.getByRole('button', { name: 'Cartoonist', exact: true }).click();
      assert.equal(await frame.locator('#scene [data-shape="ellipse"] path').count(), 3);
      await frame.getByRole('button', { name: 'Rectangle', exact: true }).click();
      assert.equal(await frame.locator('.sec[data-sec="edges"]').isHidden(), false);
      const box = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(box.x + 650, box.y + 180);
      await page.mouse.down();
      await page.mouse.move(box.x + 750, box.y + 250);
      await page.mouse.up();
      const rect = frame.locator('#scene [data-shape]:not([data-shape="ellipse"])');
      assert.equal(await rect.locator('clipPath').count(), 1, 'the new rectangle is hatched blue');
      assert.equal(await rect.locator('g > path').getAttribute('stroke'), '#a5d8ff');
      await frame.getByRole('button', { name: 'Send to back', exact: true }).click();
      assert.equal(
        await frame.locator('#scene [data-shape]').first().getAttribute('data-shape'),
        await rect.getAttribute('data-shape'),
      );
      await frame.getByRole('button', { name: 'Duplicate', exact: true }).click();
      assert.equal(await frame.locator('#scene [data-shape]').count(), 3);
      await frame.getByRole('button', { name: 'Delete', exact: true }).click();
      assert.equal(await frame.locator('#scene [data-shape]').count(), 2);
      await page.close();
    });
    await t.test('laser hover is visible, fades by age, and never edits content', async () => {
      const { page, frame } = await open();
      await frame.getByRole('button', { name: 'Laser pointer', exact: true }).click();
      const box = await frame.locator('#canvas').boundingBox();
      for (let i = 0; i < 15; i++) {
        await page.mouse.move(box.x + 250 + 75 * Math.cos(i / 5), box.y + 240 + 75 * Math.sin(i / 5));
        await page.waitForTimeout(16);
      }
      assert.equal(await frame.locator('#presence text').textContent(), 'Alice');
      assert.ok(await frame.locator('.pointer-trail path').count() > 3);
      assert.ok((await frame.locator('.pointer-trail path').first().getAttribute('d')).includes('Q'));
      await page.waitForTimeout(700);
      assert.equal(await frame.locator('.pointer-trail path').count(), 0);
      assert.equal(await frame.locator('#presence text').textContent(), 'Alice', 'stationary pointing remains visible');
      const messages = await page.evaluate(() => window.messages);
      assert.ok(messages.some(m => m.type === 'presence' && m.mode === 'laser'));
      assert.equal(messages.filter(m => m.type === 'request').length, 0);
      await page.mouse.move(box.x + 10, 5);
      await frame.locator('#presence > g').waitFor({ state: 'detached' });
      assert.equal(await page.evaluate(() => window.messages.at(-1).mode), 'clear');
      await page.close();
    });
    await t.test('remote movement interpolates, settles, and resets after a stale gap', async () => {
      const { page, frame } = await open();
      await page.evaluate(() => window.port.postMessage({type:'presence', peer:2, name:'Bob', mode:'laser', point:[100,100]}));
      await frame.locator('#presence text').waitFor();
      await page.waitForTimeout(65);
      await page.evaluate(() => window.port.postMessage({type:'presence', peer:2, name:'Bob', mode:'laser', point:[200,200]}));
      const observed = await frame.locator('#presence').evaluate(async el => {
        const points = [];
        for (let i = 0; i < 10; i++) {
          await new Promise(requestAnimationFrame);
          const m = el.querySelector('.pointer-tip').transform.baseVal.consolidate().matrix;
          points.push([m.e,m.f]);
        }
        return points;
      });
      assert.ok(observed.some(([x]) => x > 100 && x < 200), 'there are rendered intermediate positions');
      assert.deepEqual(observed.at(-1), [200,200], 'no drift after stopping');
      await page.waitForTimeout(270);
      await page.evaluate(() => window.port.postMessage({type:'presence', peer:2, name:'Bob', mode:'laser', point:[600,100]}));
      await page.waitForTimeout(30);
      assert.equal(await frame.locator('.pointer-trail path').count(), 0, 'no bridge to a stale position');
      await page.evaluate(() => window.port.postMessage({type:'presence', peer:2, mode:'clear'}));
      await frame.locator('#presence > g').waitFor({state:'detached'});
      await page.close();
    });
    await t.test('pointer labels stay legible at the canvas edge and across zoom', async () => {
      const { page, frame } = await open();
      const point = await frame.locator('#canvas').evaluate(canvas => {
        const rect = canvas.getBoundingClientRect();
        const p = new DOMPoint(rect.right - 10, rect.bottom - 10).matrixTransform(canvas.getScreenCTM().inverse());
        return [p.x,p.y];
      });
      await page.evaluate(point => window.port.postMessage({type:'presence',peer:2,name:'Morgan',mode:'cursor',point}), point);
      await frame.locator('.pointer-label').waitFor();
      const check = async () => frame.locator('#canvas').evaluate(canvas => {
        const viewport = canvas.getBoundingClientRect(), label = canvas.querySelector('.pointer-label').getBoundingClientRect();
        return {width:label.width,inside:label.left >= viewport.left && label.right <= viewport.right && label.top >= viewport.top && label.bottom <= viewport.bottom};
      });
      const before = await check();
      assert.ok(before.inside, 'the edge label flips inward');
      await frame.getByRole('button', {name:'Zoom out',exact:true}).click();
      await page.waitForTimeout(40);
      const after = await check();
      assert.ok(after.inside);
      assert.ok(Math.abs(after.width - before.width) < .1, 'label size stays in screen pixels');
      await page.close();
    });
    await t.test('reduced motion and touch pointing retain useful tips without drawing', async () => {
      const { page, frame } = await open();
      await page.emulateMedia({reducedMotion:'reduce'});
      await frame.locator('[data-tool="laser"]').click();
      for (const [type, x] of [['pointerdown',200], ['pointermove',260]])
        await frame.locator('#canvas').dispatchEvent(type, {pointerId:1, pointerType:'touch', clientX:x, clientY:250, button:0, buttons:1});
      await frame.locator('#presence text').waitFor();
      assert.equal(await frame.locator('.pointer-trail path').count(), 0);
      await frame.locator('#canvas').dispatchEvent('pointerup', {pointerId:1, pointerType:'touch', clientX:260, clientY:250, button:0});
      await frame.locator('#presence > g').waitFor({state:'detached'});
      assert.equal(await frame.locator('#scene [data-shape]').count(), 1);
      await page.close();
    });
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
    await t.test('anchored comment, explicit send, failed retry and follow-up retain captured context', async () => {
      const { page, frame } = await open('document');
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await frame.locator('.tiptap p').evaluate(p => {
        const s = document.getSelection(); s.setBaseAndExtent(p.firstChild,2,p.firstChild,8);
      });
      await frame.locator('#comment-selection').click();
      assert.equal(await frame.locator('#context-quote').textContent(), 'useful');
      await frame.locator('#question').fill('Explain this word');
      await frame.locator('#assistant-close').click();
      await frame.locator('#ask-toggle').click();
      assert.equal(await frame.locator('#context-quote').textContent(), 'useful');
      assert.equal(await frame.locator('#question').inputValue(), 'Explain this word');
      await frame.locator('#question-send').click();
      await frame.locator('#comments .comment').waitFor();
      assert.equal(await page.evaluate(() => window.messages.filter(m => m.args?.action === 'ask').length), 0);
      assert.equal(await frame.locator('.comment-anchor').textContent(), 'useful');
      assert.equal(await frame.locator('#question-send').textContent(), 'Send to assistant');
      await page.evaluate(() => window.rejectQuestion = true);
      await frame.locator('#question-send').click();
      await frame.locator('#assistant-notice[data-error="true"]').waitFor();
      assert.equal(await frame.locator('#question').inputValue(), 'Explain this word');
      await page.evaluate(() => window.rejectQuestion = false);
      await frame.locator('#question-send').click();
      await page.waitForFunction(() => window.messages.filter(m => m.args?.action === 'ask').length === 2);
      const attempts = await page.evaluate(() => window.messages.filter(m => m.args?.action === 'ask').map(m => m.args));
      assert.equal(attempts[0].questionId, attempts[1].questionId);
      assert.equal(attempts[1].expected.content.text, 'useful');
      assert.equal(attempts[1].expected.scope, 'selection');
      await frame.locator('#question').waitFor();
      await page.waitForTimeout(50);
      assert.equal(await frame.locator('#question').inputValue(), '');
      const shared = {questionId:attempts[1].questionId,commentId:attempts[1].commentId,
        itemId:'test',scope:'selection',revision:3,quote:'useful',text:'Explain this word'};
      await page.evaluate(shared => window.port.postMessage({type:'conversation',connected:true,own:[],records:[
        {id:'q',kind:'user',guest:'Alice',shared,text:'model snapshot is hidden in the UI'},
        {id:'a',kind:'assistant',text:'Useful means helpful for the task.'},
      ]}), shared);
      await frame.locator('#conversation').getByText('Useful means helpful for the task.').waitFor();
      assert.doesNotMatch(await frame.locator('#conversation').textContent(), /model snapshot/);
      assert.equal(await frame.locator('#assistant-notice').innerText(), '');
      await frame.locator('.sent-context > summary').click();
      await page.evaluate(shared => window.port.postMessage({type:'conversation',connected:true,own:[],records:[
        {id:'q',kind:'user',guest:'Alice',shared,text:'model snapshot is hidden in the UI'},
        {id:'a',kind:'assistant',text:'Useful means helpful for the task. For example, a clear test.'},
      ]}), shared);
      await frame.locator('#conversation').getByText(/For example, a clear test/).waitFor();
      assert.equal(await frame.locator('.sent-context').evaluate(e=>e.open), true);
      if (process.env.MEVEDEL_POLISH_SCREENSHOTS) await page.screenshot({path:new URL('../../.scratch/shared-editing-polish/after/assistant-document.png',import.meta.url).pathname});
      await frame.locator('#question').fill('Give an example');
      await frame.locator('#question-send').click();
      await page.waitForFunction(() => window.messages.filter(m => m.args?.action === 'ask').length === 3);
      const followup = await page.evaluate(() => window.messages.filter(m => m.args?.action === 'ask').at(-1).args);
      assert.notEqual(followup.questionId, attempts[1].questionId);
      assert.equal(followup.commentId, attempts[1].commentId);
      await page.close();
    });
    await t.test('stale and disconnected questions retain scope and draft until explicitly refreshed', async () => {
      const {page, frame} = await open();
      await frame.locator('[data-shape="ellipse"]').click({position:{x:150,y:80}});
      await frame.locator('#comment-selection').click();
      await frame.locator('#question').fill('What does this shape mean?');
      if (process.env.MEVEDEL_POLISH_SCREENSHOTS) await page.screenshot({path:new URL('../../.scratch/shared-editing-polish/after/assistant-board.png',import.meta.url).pathname});
      await page.evaluate(async () => {
        const current = await window.apply({action:'read'});
        const before = current.content[0];
        const reply = await window.apply({action:'patch',opId:'concurrent',changes:[{id:before.id,before,after:{...before,text:'Changed remotely'}}]});
        window.port.postMessage({type:'changed',...reply});
      });
      await frame.locator('#question-send').click();
      await frame.locator('#assistant-notice').getByText(/Content changed/).waitFor();
      assert.equal(await frame.locator('#question').inputValue(), 'What does this shape mean?');
      assert.match(await frame.locator('#context-title').innerText(), /Selected content/);
      await frame.locator('#refresh-context').click();
      assert.match(await frame.locator('#context-quote').innerText(), /Changed remotely/);
      await page.evaluate(() => window.port.postMessage({type:'offline'}));
      await frame.locator('#question-send').click();
      await frame.locator('#assistant-notice').getByText(/reconnect/).waitFor();
      assert.equal(await frame.locator('#question').inputValue(), 'What does this shape mean?');
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').length),1);
      await page.close();
    });
    await t.test('phone assistant overlays the editor, keeps draft and preserves its scroll position', async () => {
      const { page, frame } = await open('document', {width:375,height:500});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      const before = await frame.locator('#document').boundingBox();
      await frame.locator('#ask-toggle').click();
      await frame.locator('#question').fill('Keep this question');
      const panel = await frame.locator('#assistant').boundingBox();
      const during = await frame.locator('#document').boundingBox();
      assert.equal(during.width, before.width);
      assert.equal(panel.width, 375);
      await page.setViewportSize({width:375,height:340});
      await frame.locator('#question-send').evaluate(e => new Promise(resolve => {
        const deadline = Date.now() + 1500;
        const settled = () => e.getBoundingClientRect().bottom <= innerHeight || Date.now() > deadline
          ? resolve() : requestAnimationFrame(settled);
        settled();
      }));
      if (process.env.MEVEDEL_POLISH_SCREENSHOTS) await page.screenshot({path: new URL('../../.scratch/shared-editing-polish/after/assistant-keyboard.png', import.meta.url).pathname});
      assert.ok(await frame.locator('#question-send').evaluate(e => e.getBoundingClientRect().bottom <= innerHeight), JSON.stringify(await frame.locator('#assistant').evaluate(e => ({height:innerHeight,active:document.activeElement.id,nodes:[...e.querySelectorAll('[id]')].map(n=>[n.id,n.getBoundingClientRect().y,n.getBoundingClientRect().height])}))));
      await page.keyboard.press('Escape');
      assert.equal(await frame.locator('#assistant').isHidden(), true);
      await frame.locator('#ask-toggle').click();
      assert.equal(await frame.locator('#question').inputValue(), 'Keep this question');
      await page.close();
    });
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
