import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {editorFixture} from './editor-fixture.mjs';
import {handle} from '../host.mjs';
import {box} from './sidebar.mjs';

// Real room DOM, packaged opaque editor and native shared-item actions. Only
// the encrypted room transport is replaced so requests and reconnects are exact.
test('a wide room docks an editor, preserves its work through resize and keeps chat decisions usable', async t => {
  const {browser, url} = await editorFixture(t);
  const context = await browser.newContext({viewport:{width:2560, height:1300}});
  t.after(() => context.close());
  let state = (await handle({action:'create', id:'board', opId:'create-board', kind:'whiteboard',
    title:'Wide board', actor:'Guest: Alice',
    content:[{id:'ellipse', type:'ellipse', x:100, y:100, width:300, height:160}]})).state;
  const row = {id:'board', kind:'whiteboard', title:'Wide board', item:true, attached:true};
  await context.exposeFunction('editingRequest', async args => {
    if (args.action === 'list') return [row];
    if (args.action === 'status') return {available:true};
    const result = await handle({...args, sync:args.action === 'read', state, actor:'Guest: Alice'});
    state = result.state || state;
    return {...result.result, transactions:state.transactions};
  });
  await context.route('**/transport.js', async route => {
    const transport = await readFile(new URL('../../relay/viewer/transport.js', import.meta.url), 'utf8');
    await route.fulfill({contentType:'text/javascript', body:transport + `
      window.mevedelViewerTransport = {...window.mevedelViewerTransport, create(options) {
        window.receiveTest = options.onFrame;
        window.connectionTest = options.onConnection;
        window.sentTest = [];
        window.welcomeTest = () => {
          options.onConnection('Connected', 'connected');
          options.onFrame({t:'welcome', readOnly:false, commands:[]});
          options.onFrame({t:'status', name:'Wide room', model:'Test model', mode:'ask', busy:false});
          options.onFrame({t:'store-artifacts', artifacts:[${JSON.stringify(row)}]});
        };
        return {connect() {
          window.welcomeTest();
          options.onFrame({t:'snapshot-chunk', final:true, records:[
            {id:'main-answer', kind:'assistant', text:'The main conversation stays in the room.'}
          ]});
        }, end() {}, async send(frame) {
          window.sentTest.push(frame);
          if (frame.t === 'editing') {
            try {
              const result = await window.editingRequest(JSON.parse(atob(frame.data)));
              const data = btoa(JSON.stringify({result}));
              options.onFrame({t:'editing', reqId:frame.reqId, offset:0, total:data.length, data});
            } catch (error) {
              const data = btoa(JSON.stringify({error:error.message}));
              options.onFrame({t:'editing', reqId:frame.reqId, offset:0, total:data.length, data});
            }
          }
          return true;
        }};
      }};`});
  });
  const page = await context.newPage();
  page.setDefaultTimeout(7000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(`${url}/room#dockedroom000001.${Buffer.alloc(64).toString('base64url')}`);
  await page.locator('#editing-status').getByText('Shared editing is ready.', {exact:true}).waitFor();
  await page.locator('#composer-input').fill('> Keep the chat draft\nsecond line');
  await page.locator('#editing-items [data-item-id="board"]').click();
  const panel = page.locator('#editing-panel');
  const editor = page.frameLocator('#editing-body iframe');
  await editor.locator('#canvas').waitFor().catch(async error => {
    const details = JSON.stringify({errors, saved:await page.locator('#editing-saved').textContent(), layout:await page.evaluate(() =>
      ['editing-panel', 'editing-body'].map(id => {
        const node = document.getElementById(id), r = node.getBoundingClientRect(), s = getComputedStyle(node);
        return {id, hidden:node.hidden, className:node.className, width:r.width, height:r.height,
          top:r.top, left:r.left, display:s.display, visibility:s.visibility};
      }))});
    throw new Error(details + '\n' + error.message);
  });
  assert.equal(context.pages().length, 1, 'wide open does not reserve a new tab');
  assert.equal(await panel.evaluate(node => node.classList.contains('docked')), true);
  assert.equal(await panel.getAttribute('role'), 'region');
  assert.equal(await page.locator('#filter button[aria-pressed=true]').innerText(), '◇ Wide board');
  assert.equal(await page.locator('#composer').isVisible(), true);
  assert.equal(await page.locator('#composer-input').inputValue(), '> Keep the chat draft\nsecond line');
  assert.equal(await page.title(), 'Wide room · mevedel');
  assert.equal(await editor.locator('#assistant').isVisible(), true);
  const panelBox = await panel.boundingBox();
  const composerBox = await page.locator('#composer').boundingBox();
  assert.ok(panelBox.x >= 800 && composerBox.x + composerBox.width <= panelBox.x,
    JSON.stringify({panelBox, composerBox}));
  assert.equal(await page.locator('#session-box').isVisible(), false);
  await page.locator('#room-button').click();
  assert.equal(await page.locator('#session-box').isVisible(), true);
  const drawer = await page.locator('#session-box').boundingBox();
  assert.ok(drawer.x > panelBox.x && drawer.x + drawer.width <= 2560, JSON.stringify(drawer));
  await page.locator('#room-button').click();
  assert.equal(await page.locator('#session-box').isVisible(), false);
  await page.locator('#filter').getByRole('button', {name:/^All/}).click();
  await editor.locator('.ds-item.general').click();
  await page.waitForFunction(() => document.activeElement?.id === 'composer-input');
  assert.equal(await page.locator('#filter button[aria-pressed=true]').innerText(), '◇ Wide board',
    'the whole-item link selects its chat before focusing the composer');

  await page.evaluate(() => window.receiveTest({t:'ui-request', reqId:17,
    body:'Keep this decision usable beside the board?', bodyKind:'text',
    options:[{id:'keep', label:'Keep working'}]}));
  const decision = page.getByRole('button', {name:'Keep working', exact:true});
  await decision.waitFor();
  assert.equal(await decision.evaluate(node => {
    const r = node.getBoundingClientRect();
    return document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2) === node;
  }), true, 'pending decisions remain clickable beside the docked item');
  await decision.click();
  assert.ok(await page.evaluate(() => window.sentTest.some(frame => frame.t === 'ui-response' && frame.reqId === 17)));

  await editor.locator('[data-shape="ellipse"]').click();
  await editor.locator('#discuss-selection').click();
  await box(editor).fill('A draft about this shape');
  await editor.locator('body').evaluate(() => { window.editorIdentity = 'same-editor'; });
  await page.evaluate(() => window.connectionTest('Disconnected', 'disconnected'));
  await page.locator('#editing-saved').getByText('Offline · saved copy', {exact:true}).waitFor();
  await page.evaluate(() => window.welcomeTest());
  await page.locator('#editing-saved').getByText('Saved on host', {exact:true}).waitFor();
  assert.equal(await box(editor).inputValue(), 'A draft about this shape');
  assert.equal(await editor.locator('body').evaluate(() => window.editorIdentity), 'same-editor');

  await page.setViewportSize({width:1600, height:1000});
  await page.waitForFunction(() => !document.getElementById('editing-panel').classList.contains('docked'));
  assert.equal(await panel.getAttribute('role'), 'dialog');
  const full = await panel.boundingBox();
  assert.ok(full.x === 0 && full.y === 0 && full.width === 1600, JSON.stringify(full));
  assert.equal(await editor.locator('body').evaluate(() => window.editorIdentity), 'same-editor');
  assert.equal(await box(editor).inputValue(), 'A draft about this shape');
  await page.setViewportSize({width:2560, height:1300});
  assert.equal(await panel.evaluate(node => node.classList.contains('docked')), false, 'growing does not redock automatically');
  await page.locator('#editing-close').click();
  assert.equal(await panel.isVisible(), false);
  await page.evaluate(row => window.receiveTest({t:'store-artifacts', artifacts:[row]}), row);
  assert.equal(await page.locator('#filter button[aria-pressed=true]').innerText(), '◇ Wide board',
    'closing retains the selected item discussion even before its first turn');
  assert.equal(await page.locator('#composer-input').inputValue(), '> Keep the chat draft\nsecond line');
  await page.locator('#editing-items [data-item-id="board"]').click();
  await page.waitForFunction(() => document.getElementById('editing-panel').classList.contains('docked'));
  const openingFull = page.waitForEvent('popup');
  await page.locator('#editing-full').click();
  const fullTab = await openingFull;
  fullTab.on('pageerror', error => errors.push(error.message));
  await fullTab.waitForURL(url => url.searchParams.get('shared') === 'board');
  const fullEditor = fullTab.frameLocator('#editing-body iframe');
  await fullEditor.locator('#canvas').waitFor();
  assert.equal(await fullTab.locator('#editing-panel').evaluate(node => node.classList.contains('docked')), false);
  assert.equal(await panel.isVisible(), false, 'Open full returns the original tab to the room');
  assert.equal(await page.locator('#composer-input').inputValue(), '> Keep the chat draft\nsecond line');
  await fullEditor.locator('#ask-toggle').click();
  assert.equal(await box(fullEditor).inputValue(), 'A draft about this shape', 'Open full restores the private discussion draft');
  await fullTab.close();
  assert.deepEqual(errors, []);
});
