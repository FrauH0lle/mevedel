import test from 'node:test';
import assert from 'node:assert/strict';
import {editorFixture} from './editor-fixture.mjs';

// The real packaged iframe and port, without room setup, for interaction regressions.
test('editor interaction regressions', async (t) => {
  const {open, browser, url} = await editorFixture(t);
    await t.test('remote movement is visible before saving without changing content and expires safely', async () => {
      const {page,frame}=await open();
      await page.clock.install();
      const shape=frame.locator('[data-shape="ellipse"]');
      const before=await shape.boundingBox();
      const show=()=>page.evaluate(()=>window.port.postMessage({type:'presence',peer:42,mode:'cursor',name:'Bob',point:[500,300],
        preview:{opId:'moving',shapes:[{id:'ellipse',box:[140,130,300,160]}]}}));
      await show();
      await frame.locator('[data-live-preview="true"]').waitFor();
      assert.ok((await shape.boundingBox()).x>before.x+50);
      assert.equal(await frame.locator('#saved').innerText(),'Live movement · not saved yet');
      assert.deepEqual((await page.evaluate(()=>window.apply({action:'read'}))).content[0].box,[100,100,300,160]);
      const exported=await page.evaluate(()=>window.apply({action:'export',format:'native'}));
      assert.deepEqual(JSON.parse(exported.text).content[0].box,[100,100,300,160]);
      // Starting another gesture uses the position the participant can see.
      const displayed=await shape.boundingBox();
      await page.mouse.move(displayed.x+displayed.width/2,displayed.y+displayed.height/2);
      await page.mouse.down();
      await page.mouse.move(displayed.x+displayed.width/2+20,displayed.y+displayed.height/2);
      assert.ok(Math.abs((await shape.boundingBox()).x-displayed.x-20)<2);
      await frame.locator('#canvas').dispatchEvent('pointercancel');
      await page.mouse.up();
      await page.clock.runFor(5100);
      assert.ok(Math.abs((await shape.boundingBox()).x-before.x)<1);
      await show();
      await frame.locator('[data-live-preview="true"]').waitFor();
      await page.evaluate(()=>window.port.postMessage({type:'presence',peer:42,mode:'clear'}));
      await frame.locator('[data-live-preview="true"]').waitFor({state:'hidden'});
      await show();
      await frame.locator('[data-live-preview="true"]').waitFor();
      await page.evaluate(()=>window.port.postMessage({type:'changed',revision:2,
        transactions:[{id:'moving',revision:2,actor:'Guest: Bob',changes:[{id:'ellipse',after:true}]}]}));
      await frame.locator('[data-live-preview="true"]').waitFor({state:'hidden'});
      await show(); // A late preview must not revive an acknowledged gesture.
      await page.clock.runFor(100);
      assert.equal(await frame.locator('[data-live-preview="true"]').count(),0);
      await page.close();
    });
    await t.test('failed saves withdraw outgoing movement previews', async () => {
      const {page,frame}=await open();
      await page.evaluate(()=>{
        const apply=window.apply;
        window.apply=args=>args.action==='update' ? Promise.reject(new Error('Save refused')) : apply(args);
      });
      const shape=frame.locator('[data-shape="ellipse"]');
      const box=await shape.boundingBox();
      await page.mouse.move(box.x+box.width/2,box.y+box.height/2);
      await page.mouse.down();
      await page.mouse.move(box.x+box.width/2+50,box.y+box.height/2);
      await page.waitForFunction(()=>window.messages.some(m=>m.type==='presence' && m.preview?.shapes.length));
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.type==='request'&&m.args.action==='update').length),0);
      await page.mouse.up();
      await frame.locator('#saved').getByText('Save refused',{exact:true}).waitFor();
      await page.waitForFunction(()=>!window.messages.filter(m=>m.type==='presence').at(-1).preview);
      await page.close();
    });
    for (const loseAck of [false,true]) await t.test(`typing during a slow save merges safely${loseAck ? ' after a lost acknowledgement' : ''}`, async () => {
      const {page,frame}=await open();
      await page.evaluate(loseAck=>{
        const apply=window.apply;
        window.updateCalls=0;
        window.apply=async args=>{
          const first=args.action==='update' && ++window.updateCalls===1;
          if(first)
            await new Promise(resolve=>{window.releaseSave=resolve;});
          const result=await apply(args);
          if(first && loseAck) throw new Error('No save acknowledgement');
          return result;
        };
      },loseAck);
      await frame.locator('[data-shape="ellipse"]').dblclick();
      await frame.locator('#shape-text').fill('t');
      await page.waitForFunction(()=>window.updateCalls===1);
      for(const text of ['te','test','test1234','test1234\n3333']) {
        await frame.locator('#shape-text').fill(text);
        await page.waitForTimeout(350);
      }
      const drafts=await page.evaluate(()=>window.messages.filter(m=>m.type==='draft').at(-1).draft);
      assert.equal(drafts.pending.length,2,'one in-flight save and one merged recoverable follow-up');
      await page.evaluate(()=>window.releaseSave());
      if(loseAck) {
        await frame.locator('#saved').getByText('No save acknowledgement',{exact:true}).waitFor();
        await frame.locator('#retry').evaluate(button=>button.click());
      }
      await frame.locator('#saved').getByText('Saved on host',{exact:true}).waitFor();
      assert.equal(await page.evaluate(()=>window.updateCalls),loseAck ? 3 : 2,'no stale keystroke backlog');
      const stored=await page.evaluate(()=>window.apply({action:'read'}));
      assert.equal(stored.content.find(s=>s.id==='ellipse').text,'test1234\n3333');
      assert.equal(stored.revision,3,'retry does not commit the in-flight operation twice');
      await page.close();
    });
    await t.test('save failure remains visible above a recovery-storage warning', async () => {
      const {page,frame}=await open({kind:'document'});
      await page.evaluate(()=>{
        const apply=window.apply;
        window.apply=args=>args.action==='update'
          ? Promise.reject(new Error('Shared content is too large')) : apply(args);
      });
      await frame.locator('.tiptap').click();
      await page.keyboard.type('Keep this pending edit');
      await frame.locator('#saved').getByText('Shared content is too large',{exact:true}).waitFor();
      await page.evaluate(()=>window.port.postMessage({type:'storage-error',message:'Recovery storage unavailable'}));
      await page.waitForTimeout(350);
      assert.equal(await frame.locator('#saved').innerText(),'Shared content is too large');
      assert.match(await frame.locator('.tiptap').innerText(),/Keep this pending edit/);
      await page.close();
    });
    await t.test('editor theme follows its trusted port without losing document or question drafts', async () => {
      const {page, frame} = await open({kind:'document', viewport:{width:1280,height:800}, appearance:{theme:'dark'}});
      assert.equal(await frame.locator('html').getAttribute('data-theme'), 'dark');
      await frame.locator('.tiptap').click();
      await page.keyboard.type('Keep this document.');
      if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
      await frame.locator('#question').fill('> Keep this question\nsecond line');
      for (const theme of ['light', 'dark', 'system', 'invalid']) {
        await page.evaluate(theme => window.port.postMessage({type:'appearance',appearance:{theme,palette:'cool',accents:'selective'}}), theme);
        await frame.locator('html').evaluate((root, expected) => new Promise((resolve,reject) => {
          const deadline = Date.now()+1500;
          const check = () => root.getAttribute('data-theme') === expected ? resolve()
            : Date.now()>deadline ? reject(new Error('Theme was not applied')) : requestAnimationFrame(check);
          check();
        }), ['light','dark'].includes(theme) ? theme : null);
        const colors = await frame.locator('#question-send').evaluate(button => {
          const style = getComputedStyle(button);
          return {text:style.color,background:style.backgroundColor};
        });
        const luminance = color => color.match(/[\d.]+/g).slice(0,3).map(Number)
          .map(c=>c/255).map(c=>c<=.04045?c/12.92:((c+.055)/1.055)**2.4)
          .reduce((sum,c,i)=>sum+c*[.2126,.7152,.0722][i],0);
        const a=luminance(colors.text), b=luminance(colors.background);
        assert.ok((Math.max(a,b)+.05)/(Math.min(a,b)+.05)>=4.5,JSON.stringify(colors));
        assert.equal(await frame.locator('#question').inputValue(), '> Keep this question\nsecond line');
        assert.match(await frame.locator('.tiptap').innerText(), /Keep this document/);
      }
      await page.close();
    });
    for (const kind of ['whiteboard', 'document']) {
      await t.test(`${kind} shows assistant activity and clears it on idle or disconnect`, async () => {
        const {page, frame} = await open({kind});
        if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
        const state = frame.locator('#conversation-state');
        await frame.locator('#question').fill('> Preserved question\nsecond line');
        await page.evaluate(()=>window.port.postMessage({type:'conversation',connected:true,
          conversationError:'Archive is unavailable',records:[],own:[]}));
        await frame.locator('#conversation-history-notice').getByText('Earlier conversation unavailable: Archive is unavailable',{exact:true}).waitFor();
        assert.equal(await frame.locator('#question').inputValue(),'> Preserved question\nsecond line');
        assert.match(await frame.locator('#conversation-context').innerText(),/own conversation/);
        for (const activity of [{connected:true,busy:true,paused:true}, {connected:true,busy:false}, {connected:false,busy:true}]) {
          await page.evaluate(activity=>window.port.postMessage({type:'conversation',records:[],own:[],...activity}),activity);
          const active = activity.connected && activity.busy;
          if (active) await state.getByText('Assistant working',{exact:true}).waitFor();
          else await state.getByText(activity.connected ? 'Ready' : 'Disconnected · drafts kept',{exact:true}).waitFor();
          assert.equal(await state.evaluate(e=>e.classList.contains('assistant-working')),active);
          assert.equal(await frame.locator('#ask-toggle').evaluate(e=>e.classList.contains('assistant-working')),active);
        }
        await page.close();
      });
      await t.test(`${kind} inserts images from picker, drop, and paste with undo`, async () => {
        const {page, frame} = await open({kind});
        const data = await page.evaluate(()=>{
          const canvas=document.createElement('canvas');canvas.width=80;canvas.height=40;
          canvas.getContext('2d').fillRect(0,0,80,40);return canvas.toDataURL().split(',')[1];
        });
        const images = frame.locator(kind === 'document' ? '.tiptap img' : '#scene image');
        const surface = frame.locator(kind === 'document' ? '.tiptap' : '#canvas');
        await frame.locator('#image-upload').setInputFiles({name:'diagram.png',mimeType:'image/png',buffer:Buffer.from(data,'base64')});
        await images.waitFor();
        const target = await surface.boundingBox();
        const x = target.x + target.width/2, y = target.y + target.height/2;
        await surface.evaluate((element,{data,x,y})=>{
          const transfer=new DataTransfer();
          transfer.items.add(new File([Uint8Array.from(atob(data),c=>c.charCodeAt(0))],'dropped.png',{type:'image/png'}));
          element.dispatchEvent(new DragEvent('drop',{bubbles:true,cancelable:true,dataTransfer:transfer,clientX:x,clientY:y}));
        },{data,x,y});
        await images.nth(1).waitFor();
        if (kind === 'whiteboard') {
          const boxes = await images.evaluateAll(nodes=>nodes.map(n=>{const b=n.getBoundingClientRect();return [b.x,b.y];}));
          assert.ok(boxes.some(([left,top])=>Math.abs(left-x)<2 && Math.abs(top-y)<2),'drop lands at the pointer');
        }
        await frame.locator('#undo').click();
        await images.nth(1).waitFor({state:'hidden'});
        await frame.locator('#redo').click();
        await images.nth(1).waitFor();
        await surface.evaluate((element,data)=>{
          const transfer=new DataTransfer();
          transfer.items.add(new File([Uint8Array.from(atob(data),c=>c.charCodeAt(0))],'pasted.png',{type:'image/png'}));
          element.dispatchEvent(new ClipboardEvent('paste',{bubbles:true,cancelable:true,clipboardData:transfer}));
        },data);
        await images.nth(2).waitFor();
        await frame.locator('#saved').getByText('Saved on host',{exact:true}).waitFor();
        const saved = await page.evaluate(async()=>window.apply({action:'read'}));
        assert.equal((JSON.stringify(saved.content).match(/data:image\/png;base64/g)||[]).length,3);
        await frame.locator('#image-upload').setInputFiles({name:'bad.svg',mimeType:'image/svg+xml',buffer:Buffer.from('<svg/>')});
        await frame.locator('#saved').getByText('Use a PNG, JPEG, or WebP image up to 4 MB',{exact:true}).waitFor();
        assert.equal(await images.count(),3);
        if (kind === 'document') {
          // Ordinary HTML copy/paste must retain numeric image dimensions.
          await surface.click();
          await surface.evaluate(element=>element.editor.commands.focus('end'));
          await page.frames()[1].waitForFunction(()=>{const e=document.querySelector('.tiptap').editor;return e.state.selection.empty&&e.state.selection.from===e.state.doc.content.size-1;});
          await surface.evaluate((element,data)=>{
            const transfer=new DataTransfer();
            transfer.setData('text/html',`<img src="data:image/png;base64,${data}" width="80" height="40" alt="Copied">`);
            element.dispatchEvent(new ClipboardEvent('paste',{bubbles:true,cancelable:true,clipboardData:transfer}));
          },data);
          await images.nth(3).waitFor();
          await frame.locator('#saved').getByText('Saved on host',{exact:true}).waitFor();
          const copied = await page.evaluate(async()=>window.apply({action:'read'}));
          assert.equal(copied.content.content.find(n=>n.type==='image' && n.attrs.alt==='Copied').attrs.width,80);
        }
        await page.close();
      });
    }
    await t.test('board objects follow a drag before release and cancellation restores them', async () => {
      const {page, frame} = await open();
      const shape = frame.locator('#scene [data-shape="ellipse"]');
      const before = await shape.boundingBox();
      await page.mouse.move(before.x + before.width / 2, before.y + before.height / 2);
      await page.mouse.down();
      await page.mouse.move(before.x + before.width / 2 + 80, before.y + before.height / 2 + 40);
      const moving = await shape.boundingBox();
      assert.ok(Math.abs(moving.x - before.x - 80) < 2, 'shape follows the pointer while held');
      assert.ok(Math.abs(moving.y - before.y - 40) < 2);
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='update').length),0);
      await frame.locator('#canvas').dispatchEvent('pointercancel');
      assert.ok(Math.abs((await shape.boundingBox()).x - before.x) < 2);
      await page.mouse.up();
      await page.mouse.move(before.x + before.width/2, before.y + before.height/2);
      await page.mouse.down();
      await page.mouse.move(before.x + before.width/2 + 80, before.y + before.height/2 + 40);
      const preview = await shape.boundingBox();
      await page.mouse.up();
      assert.ok(Math.abs((await shape.boundingBox()).x - preview.x) < 2, 'release keeps the preview position');
      await frame.locator('#undo').click();
      assert.ok(Math.abs((await shape.boundingBox()).x - before.x) < 2, 'one undo restores the move');
      const handle = await frame.locator('[data-resize="ellipse"]').boundingBox();
      await page.mouse.move(handle.x + handle.width/2,handle.y + handle.height/2);
      await page.mouse.down();
      await page.mouse.move(handle.x + handle.width/2 + 60,handle.y + handle.height/2 + 30);
      const resized = await shape.boundingBox();
      assert.ok(resized.width > before.width + 55, 'resize also previews before release');
      await page.mouse.up();
      assert.ok(Math.abs((await shape.boundingBox()).width - resized.width) < 2);
      await page.close();
    });
    await t.test('Shared controls retain readable proportions across item counts, viewport and zoom', async () => {
      const page = await browser.newPage();
      await page.goto(`${url}/menu`);
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
          assert.ok(b.height >= (width <= 640 ? 40 : 32) * zoom && b.height <= 65 * zoom, JSON.stringify(b));
          assert.ok(b.width > b.height, JSON.stringify(b));
        }
        assert.equal(await page.locator('#composer-input').inputValue(), '> Draft\nsecond line');
        assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
      }
      await page.close();
    });
    await t.test('room session navigation sits beside wide conversations and above narrow composers', async () => {
      const page = await browser.newPage();
      await page.goto(`${url}/menu`);
      await page.evaluate(() => {
        const menu = document.getElementById('session-box');
        menu.hidden = false; menu.open = true;
        document.getElementById('editing-box').hidden = false;
        document.getElementById('editing-box').open = true;
        document.getElementById('composer').hidden = false;
      });
      for (const width of [1440,1280,1000,375]) {
        await page.setViewportSize({width,height:800});
        const menu = await page.locator('#session-box').boundingBox();
        const composer = await page.locator('#composer').boundingBox();
        if (width >= 1280) assert.ok(menu.x >= composer.x + composer.width);
        else assert.ok(menu.y + menu.height <= composer.y);
        assert.ok(menu.x >= 0 && menu.x + menu.width <= width);
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
      const label = await frame.locator('#scene text').boundingBox();
      const input = await frame.locator('#shape-text').boundingBox();
      assert.ok(Math.abs(input.y + input.height / 2 - label.y - label.height / 2) < 5,
        'editing stays at the rendered label position');
      assert.equal(await frame.locator('#scene text').evaluate(e => getComputedStyle(e).visibility), 'hidden');
      assert.equal(await frame.locator('#shape-text').evaluate(e => getComputedStyle(e).backgroundColor), 'rgba(0, 0, 0, 0)');
      await frame.locator('#shape-text').fill('Discard');
      await page.keyboard.press('Escape');
      assert.equal(await frame.locator('#scene text').textContent(), 'Database');
      if (process.env.MEVEDEL_EDITOR_SCREENSHOTS)
        await page.screenshot({
          path: '.scratch/shared-collaborative-editing/board-revised.png',
        });
      await page.close();
    });
    await t.test('box selection contains, touches with Alt, adds with Shift and keeps its area', async () => {
      const {page, frame} = await open({content:[
        {id:'a', type:'rect', box:[0,0,100,100], text:'A'},
        {id:'b', type:'rect', box:[300,0,100,100], text:'B'},
        {id:'c', type:'ellipse', box:[0,300,100,100], text:'C'},
      ]});
      const at = (x, y) => frame.locator('#canvas').evaluate((canvas, [x, y]) => {
        const p = new DOMPoint(x, y).matrixTransform(canvas.getScreenCTM());
        return [p.x, p.y];
      }, [x, y]);
      const offset = await page.locator('iframe').boundingBox();
      const drag = async (from, to, modifier) => {
        const [x0, y0] = await at(...from), [x1, y1] = await at(...to);
        if (modifier) await page.keyboard.down(modifier);
        await page.mouse.move(offset.x + x0, offset.y + y0);
        await page.mouse.down();
        await page.mouse.move(offset.x + (x0 + x1) / 2, offset.y + (y0 + y1) / 2);
        await page.mouse.move(offset.x + x1, offset.y + y1);
        await page.mouse.up();
        if (modifier) await page.keyboard.up(modifier);
      };
      const chosen = () => frame.locator('#selection [data-resize]').evaluateAll(n => n.map(e => e.dataset.resize).sort());
      await drag([-20,-20], [200,150]);
      assert.deepEqual(await chosen(), ['a']);
      assert.equal(await frame.locator('#selection .selection-region').count(), 1);
      assert.equal(await frame.locator('#selection-question').isDisabled(), false);
      await drag([-20,-20], [320,150], 'Alt');
      assert.deepEqual(await chosen(), ['a','b']);
      await drag([-20,280], [150,450], 'Shift');
      assert.deepEqual(await chosen(), ['a','b','c']);
      await drag([140,140], [260,260]);
      assert.deepEqual(await chosen(), []);
      assert.equal(await frame.locator('#selection .selection-region').count(), 1,
        'an empty area remains selected for questions');
      assert.equal(await frame.locator('#selection-question').isDisabled(), false);
      const [x, y] = await at(200, 200);
      await page.mouse.click(offset.x + x, offset.y + y);
      assert.equal(await frame.locator('#selection .selection-region').count(), 0);
      assert.equal(await frame.locator('#selection-question').isDisabled(), true);
      const content = await page.evaluate(async () => (await window.apply({action:'read'})).content);
      assert.deepEqual(content.map(s => s.box), [[0,0,100,100],[300,0,100,100],[0,300,100,100]],
        'box selection never edits content');
      await page.close();
    });
    await t.test('asking about a boxed area sends its region with the contained objects', async () => {
      const {page, frame} = await open({content:[
        {id:'a', type:'rect', box:[0,0,100,100], text:'A'},
        {id:'b', type:'rect', box:[300,0,100,100], text:'B'},
      ]});
      const [x0, y0, x1, y1] = await frame.locator('#canvas').evaluate(canvas => {
        const at = (x, y) => new DOMPoint(x, y).matrixTransform(canvas.getScreenCTM());
        const a = at(-20, -20), b = at(220, 140);
        return [a.x, a.y, b.x, b.y];
      });
      const offset = await page.locator('iframe').boundingBox();
      await page.mouse.move(offset.x + x0, offset.y + y0);
      await page.mouse.down();
      await page.mouse.move(offset.x + x1, offset.y + y1, {steps: 4});
      await page.mouse.up();
      await frame.locator('#selection-question').click();
      assert.match(await frame.locator('#context-title').innerText(), /Selected area/);
      assert.equal(await frame.locator('#selected-question').innerText(), 'Selected area');
      assert.match(await frame.locator('#context-quote').textContent(), /^Area \d+ × \d+ at -2\d, -2\d\n1 object\nrect: A$/);
      await frame.locator('#question').fill('Put a legend here');
      await frame.locator('#question-send').click();
      await page.waitForFunction(() => window.messages.some(m => m.args?.action === 'ask'));
      const ask = await page.evaluate(() => window.messages.find(m => m.args?.action === 'ask').args);
      assert.deepEqual(ask.selection, ['a']);
      assert.equal(ask.region.length, 4);
      assert.deepEqual(ask.expected.region, ask.region);
      await frame.locator('#whole-question').click();
      await frame.locator('#selected-question').click();
      assert.match(await frame.locator('#context-title').innerText(), /Selected area/,
        'switching scopes retains the captured area');
      await page.close();
    });
    await t.test('board comments: comment tool, numbered markers, hover card, threads and resolution', async () => {
      const {page, frame} = await open({content:[
        {id:'a', type:'rect', box:[0,0,100,100], text:'A'},
        {id:'b', type:'rect', box:[300,0,100,100], text:'B'},
      ]});
      const offset = await page.locator('iframe').boundingBox();
      const at = async (x, y) => {
        const p = await frame.locator('#canvas').evaluate((canvas, [x, y]) => {
          const p = new DOMPoint(x, y).matrixTransform(canvas.getScreenCTM());
          return [p.x, p.y];
        }, [x, y]);
        return [offset.x + p[0], offset.y + p[1]];
      };
      await frame.locator('#canvas').focus();
      await page.keyboard.press('m');
      assert.equal(await frame.locator('[data-tool="comment"]').getAttribute('aria-pressed'), 'true');
      await page.mouse.move(...await at(50, 50));
      assert.equal(await frame.locator('#selection .comment-hover').count(), 1, 'hover outlines the target');
      await page.mouse.down(); await page.mouse.up();
      await frame.locator('#comment-text').waitFor();
      assert.equal(await frame.locator('#comment-quote').textContent(), '1 object\nrect: A');
      await frame.locator('#comment-text').fill('Make this blue');
      await frame.locator('#comment-assistant').uncheck();
      await frame.locator('#comment-post').click();
      const card = frame.locator('#comments .comment').first();
      await card.waitFor();
      assert.equal(await card.locator('.comment-passage').textContent(), 'Show objects');
      const pin = frame.locator('#comment-markers .comment-pin');
      await pin.waitFor();
      assert.equal(await pin.textContent(), '1');
      // An area comment from a drag in comment mode.
      await frame.locator('#assistant-close').click();
      const [x0, y0] = await at(150, 20), [x1, y1] = await at(250, 80);
      await page.mouse.move(x0, y0); await page.mouse.down();
      await page.mouse.move(x1, y1, {steps: 3}); await page.mouse.up();
      assert.match(await frame.locator('#comment-quote').textContent(), /^Area 10\d × 6\d at 1(49|50), (19|20)\n0 objects$/);
      await frame.locator('#comment-text').fill('Legend here');
      await frame.locator('#comment-assistant').uncheck();
      await frame.locator('#comment-post').click();
      await frame.locator('#comment-markers .comment-pin').nth(1).waitFor();
      assert.match(await frame.locator('#comments .comment summary').nth(1).textContent(),
                   /^Area 10\d × 6\d at 1(49|50), (19|20) · 0 objects$/, 'thread headers keep quote lines apart');
      const posted = await page.evaluate(() => window.messages.filter(m => m.args?.action === 'comment').map(m => m.args));
      assert.deepEqual(posted[0].selection, ['a']);
      assert.equal(posted[1].region.length, 4);
      // Hovering a pin shows its thread; clicking opens it in the panel.
      await frame.locator('#assistant-close').click();
      await pin.first().hover();
      await frame.locator('#comment-peek:not([hidden])').waitFor();
      assert.match(await frame.locator('#comment-peek').textContent(), /Alice.*Make this blue/s);
      await page.mouse.move(offset.x + 5, offset.y + offset.height - 5);
      assert.equal(await frame.locator('#comment-peek').isHidden(), true);
      await pin.first().click();
      assert.equal(await card.evaluate(e => e.open), true);
      await page.keyboard.press('Escape');
      await frame.locator('[data-tool="select"]').click();
      await frame.locator('#ask-toggle').click();
      await frame.locator('#comments-tab').click();
      await card.locator('.comment-passage').click();
      assert.deepEqual(await frame.locator('#selection [data-resize]').evaluateAll(n => n.map(e => e.dataset.resize)), ['a']);
      // Sending the thread asks about its objects; resolving removes its marker.
      await card.getByText('Send thread to assistant', {exact: true}).click();
      await page.waitForFunction(() => window.messages.some(m => m.args?.action === 'ask'));
      const ask = await page.evaluate(() => window.messages.find(m => m.args?.action === 'ask').args);
      assert.deepEqual(ask.selection, ['a']);
      assert.equal(ask.commentId, posted[0].opId);
      assert.equal(ask.text, 'Make this blue', 'the comment itself is the request');
      await card.getByText('Resolve', {exact: true}).click();
      await card.waitFor({state: 'hidden'});
      assert.equal(await frame.locator('#comment-markers .comment-pin').count(), 1);
      assert.equal(await frame.locator('#comment-markers .comment-pin').textContent(), '1', 'open markers renumber');
      // Deleting the objects of an object comment marks its thread removed.
      await frame.locator('#show-resolved').check();
      await card.getByText('Reopen', {exact: true}).click();
      await page.evaluate(questionId => window.port.postMessage({type:'conversation', connected:true, own:[], records:[
        {id:'q', kind:'user', guest:'Alice', shared:{questionId}, text:'Make this blue'},
        {id:'r', kind:'assistant', text:'Done.'}]}), ask.questionId);
      await page.evaluate(async () => {
        const read = await window.apply({action:'read'});
        const result = await window.apply({action:'patch', opId:'agent-delete',
          changes:[{id:'a', before:read.content.find(s => s.id === 'a'), after:null}]});
        window.port.postMessage({type:'changed', ...result});
      });
      await card.locator('.thread-status').getByText('Referenced objects were removed').waitFor();
      assert.equal(await card.getByText('Send thread to assistant', {exact: true}).isDisabled(), true);
      assert.equal(await frame.locator('#comment-markers .comment-pin').count(), 1, 'a removed anchor has no marker');
      await page.close();
    });
    await t.test('whole and selection actions stay together beside the composer', async () => {
      const {page, frame} = await open();
      await frame.locator('#scene [data-shape="ellipse"]').click({position:{x:150,y:80}});
      await frame.locator('#selection-question').click();
      const selection = await frame.locator('#selected-question').boundingBox();
      const whole = await frame.locator('#whole-question').boundingBox();
      assert.ok(Math.abs(selection.y - whole.y) < 45);
      assert.equal(await frame.locator('#context-scope button:visible').count(), 2);
      assert.ok(Math.abs(selection.x - whole.x) < 200);
      assert.match(await frame.locator('#context-title').innerText(), /Selected content/);
      if (process.env.MEVEDEL_EDITOR_SCREENSHOTS) await page.screenshot({path:'.scratch/shared-editing-followup/context-buttons.png'});
      await frame.locator('#whole-question').click();
      assert.match(await frame.locator('#context-title').innerText(), /Whole whiteboard/);
      await page.close();
    });
    await t.test('drawing a shape returns to selection for immediate text editing', async () => {
      const {page, frame} = await open();
      await frame.locator('#board-zoom').click();
      await frame.locator('[data-tool="rect"]').click();
      const canvas = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(canvas.x + 620, canvas.y + 220);
      await page.mouse.down();
      await page.mouse.move(canvas.x + 820, canvas.y + 330);
      await page.mouse.up();
      assert.equal(await frame.locator('[data-tool="select"]').getAttribute('aria-pressed'), 'true');
      await frame.locator('#scene [data-shape]:not([data-shape="ellipse"])').dblclick();
      await frame.locator('#shape-text').fill('Frontend');
      if (process.env.MEVEDEL_EDITOR_SCREENSHOTS) await page.screenshot({path:'.scratch/shared-editing-followup/inline-text.png'});
      assert.equal(await frame.locator('#scene [data-shape]').count(), 2);
      await page.keyboard.press('Control+Enter');
      assert.equal(await frame.locator('#scene [data-shape]:not([data-shape="ellipse"]) text').textContent(), 'Frontend');
      await page.close();
    });
    await t.test('received laser batches retain the circle between network updates', async () => {
      const {page, frame} = await open();
      await page.evaluate(() => {
        const trail = Array.from({length:32}, (_,i) => [250+100*Math.cos(i/31*Math.PI*2),250+100*Math.sin(i/31*Math.PI*2),(31-i)*10]);
        window.port.postMessage({type:'presence',peer:2,name:'Bob',mode:'laser',point:trail.at(-1).slice(0,2),trail});
      });
      await frame.locator('.pointer-trail path').first().waitFor();
      const paths = await frame.locator('.pointer-trail > g').first().locator('path').evaluateAll(nodes=>nodes.map(n=>n.getAttribute('d')));
      assert.ok(paths.length >= 25, 'receiver retains input samples, not only packet endpoints');
      for (const path of paths) {
        const [x,y] = path.match(/Q([\d.e+-]+) ([\d.e+-]+)/).slice(1).map(Number);
        assert.ok(Math.abs(Math.hypot(x-250,y-250)-100) < 1, 'curve follows the original circle');
      }
      await page.close();
    });
    await t.test('bound arrows end at borders in the editor and after target movement', async () => {
      const {page, frame} = await open();
      await page.evaluate(async () => {
        const reply = await window.apply({action:'patch',opId:'connect',changes:[
          {id:'target',before:null,after:{id:'target',type:'rect',box:[550,100,200,160]}},
          {id:'arrow',before:null,after:{id:'arrow',type:'arrow',box:[250,180,400,0],from:'ellipse',to:'target'}},
        ]});
        window.port.postMessage({type:'changed',...reply});
      });
      const arrow = frame.locator('[data-shape="arrow"] path[stroke="#242424"]');
      await arrow.waitFor({state:'attached'});
      const endpoints = () => arrow.evaluate(path => {
        const a = path.getPointAtLength(0), b = path.getPointAtLength(path.getTotalLength());
        return [[a.x,a.y],[b.x,b.y]];
      });
      assert.deepEqual(await endpoints(), [[400,180],[550,180]]);
      await frame.getByRole('button',{name:'Fit',exact:true}).click();
      await frame.locator('[data-shape="target"]').click({position:{x:100,y:80}});
      await page.keyboard.press('Shift+ArrowRight');
      assert.deepEqual(await endpoints(), [[400,180],[560,180]]);
      const exported = await page.evaluate(async () => {
        await new Promise(resolve => setTimeout(resolve,400));
        return (await window.apply({action:'export',format:'svg'})).text;
      });
      assert.match(exported, /M560 180L546 187L546 173Z/);
      assert.match(exported, /560 180/);
      if (process.env.MEVEDEL_CONNECTOR_SCREENSHOTS)
        await page.screenshot({path:'.scratch/connector-borders/editor.png'});
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
      assert.equal(await frame.locator('#properties > summary').getAttribute('aria-disabled'), 'true', 'nothing to style');
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
      ]);
      await frame.getByRole('button', { name: 'Background: #a5d8ff', exact: true }).click();
      assert.equal(
        await frame.locator('#scene [data-shape="ellipse"] path').first().getAttribute('fill'),
        '#a5d8ff',
      );
      assert.ok((await shown()).includes('Fill'), 'a filled shape offers fill patterns');
      await frame.getByRole('button', { name: 'Cartoonist', exact: true }).click();
      assert.equal(await frame.locator('#scene [data-shape="ellipse"] path').count(), 3);
      await frame.locator('#properties > summary').click();
      await frame.locator('#board-zoom').click();
      await frame.getByRole('button', { name: 'Rectangle', exact: true }).click();
      await frame.locator('#properties > summary').click();
      assert.equal(await frame.locator('.sec[data-sec="edges"]').isHidden(), false);
      await frame.locator('#properties > summary').click();
      const box = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(box.x + 650, box.y + 180);
      await page.mouse.down();
      await page.mouse.move(box.x + 750, box.y + 250);
      await page.mouse.up();
      const rect = frame.locator('#scene [data-shape]:not([data-shape="ellipse"])');
      assert.equal(await rect.locator('clipPath').count(), 1, 'the new rectangle is hatched blue');
      assert.equal(await rect.locator('g > path').getAttribute('stroke'), '#a5d8ff');
      await frame.locator('#object-menu > summary').click();
      await frame.locator('.object-arrange > summary').click();
      await frame.getByRole('button', { name: 'Send to back', exact: true }).click();
      assert.equal(
        await frame.locator('#scene [data-shape]').first().getAttribute('data-shape'),
        await rect.getAttribute('data-shape'),
      );
      await frame.locator('#object-menu > summary').click();
      await frame.getByRole('button', { name: 'Duplicate', exact: true }).click();
      assert.equal(await frame.locator('#scene [data-shape]').count(), 3);
      await frame.locator('#object-menu > summary').click();
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
    for (const kind of ['whiteboard', 'document']) {
      await t.test(`${kind} assistant highlights expire without content changes or replay`, async () => {
        const {page, frame} = await open({kind, actor:'Agent: /root'});
        await page.clock.install();
        await page.clock.pauseAt(new Date());
        const highlighted = frame.locator(kind === 'whiteboard' ? '#scene [data-agent="true"]' : '.agent-contribution');
        assert.equal(await highlighted.count(),0,'opening an item does not highlight old edits');
        await page.evaluate(async kind => {
          const content = (await window.apply({action:'read'})).content;
          const before = kind === 'whiteboard' ? content[0] : content.content[0];
          const after = kind === 'whiteboard' ? {...before,text:'Assistant text'} :
            {...before,content:[{type:'text',text:'Assistant text'}]};
          const result = await window.apply({action:'patch',opId:'timed-edit',changes:[{
            id:kind === 'whiteboard' ? before.id : before.attrs.id,before,after,
          }]});
          result.transactions[0].actor = 'Agent: /root';
          window.agentChange = {type:'changed',...result};
          window.port.postMessage(window.agentChange);
        },kind);
        await highlighted.waitFor();
        if (process.env.MEVEDEL_EXPORT_SCREENSHOTS)
          await page.screenshot({path:`.scratch/editor-polish/${kind}-highlight.png`});
        const before = await page.evaluate(async () => (await window.apply({action:'read'})).content);
        await page.clock.runFor(5000);
        // Repeated publication must not extend the highlight's lifetime.
        await page.evaluate(() => window.port.postMessage(window.agentChange));
        await page.clock.runFor(3500);
        assert.equal(await highlighted.count(),0,'assistant glow expires without a human edit');
        await page.evaluate(() => window.port.postMessage(window.agentChange));
        await page.clock.runFor(100);
        assert.equal(await highlighted.count(),0,'old contribution does not light up again');
        assert.deepEqual(await page.evaluate(async () => (await window.apply({action:'read'})).content),before);
        if (process.env.MEVEDEL_EXPORT_SCREENSHOTS)
          await page.screenshot({path:`.scratch/editor-polish/${kind}-expired.png`});
        assert.match(await frame.locator('#contributions').textContent(),/Agent: \/root/,'attribution remains available');
        for (const actor of ['Agent: /root', 'Guest: Bob']) {
          await page.evaluate(async ({kind,actor}) => {
            const content = (await window.apply({action:'read'})).content;
            const before = kind === 'whiteboard' ? content[0] : content.content[0];
            const after = kind === 'whiteboard' ? {...before,text:actor} :
              {...before,content:[{type:'text',text:actor}]};
            const result = await window.apply({action:'patch',opId:actor.startsWith('Agent:') ? 'renew-agent' : 'renew-human',changes:[{
              id:kind === 'whiteboard' ? before.id : before.attrs.id,before,after,
            }]});
            result.transactions[0].actor = actor;
            window.port.postMessage({type:'changed',...result});
          },{kind,actor});
          if (actor.startsWith('Agent:')) await highlighted.waitFor();
          else await highlighted.waitFor({state:'detached'});
        }
        await frame.locator('#menu > summary').click();
        await page.close();
      });
    }
    await t.test(
      'contributions group typing bursts and highlight assistant targets without changing exports',
      async () => {
        const { page, frame } = await open({kind:'document'});
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
    await t.test('separate drafts, human replies, direct thread send and inline AI answers', async () => {
      const {page, frame} = await open({kind:'document'});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await frame.locator('.tiptap p').evaluate(p => document.getSelection().setBaseAndExtent(p.firstChild,2,p.firstChild,8));
      if (process.env.MEVEDEL_DISCUSSION_SCREENSHOTS) await page.screenshot({path:new URL('../../.scratch/document-discussion/selection.png',import.meta.url).pathname});
      await frame.locator('#selection-question').click();
      assert.equal(await frame.locator('#context-quote').textContent(),'useful');
      await frame.locator('#question').fill('> Keep this question\nsecond line');
      await frame.locator('#assistant-close').click();
      await frame.locator('.tiptap p').evaluate(p => document.getSelection().setBaseAndExtent(p.firstChild,2,p.firstChild,8));
      await frame.locator('#comment-selection').click();
      await frame.locator('#comment-text').fill('Explain this word');
      await frame.locator('#assistant-tab').click();
      assert.equal(await frame.locator('#question').inputValue(),'> Keep this question\nsecond line');
      assert.equal(await frame.locator('#context-quote').textContent(),'useful');
      await frame.locator('#comments-tab').click();
      assert.equal(await frame.locator('#comment-text').inputValue(),'Explain this word');
      await frame.locator('#comment-assistant').uncheck();
      await frame.locator('#comment-post').click();
      const card = frame.locator('#comments .comment');
      await card.waitFor();
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').length),0);
      await card.locator('textarea').fill('Please include an example');
      await card.locator('.reply-assistant').uncheck();
      await card.getByText('Post reply',{exact:true}).click();
      await card.locator('.thread-message').getByText('Please include an example',{exact:true}).waitFor();
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').length),0);
      await card.locator('textarea').fill('> Unposted reply\nkept while streaming');
      await page.evaluate(()=>window.rejectQuestion=true);
      await card.getByText('Send thread to assistant',{exact:true}).click();
      await frame.locator('#assistant-notice[data-error="true"]').waitFor();
      await page.evaluate(()=>window.rejectQuestion=false);
      await card.getByText('Send thread to assistant',{exact:true}).click();
      await page.waitForFunction(()=>window.messages.filter(m=>m.args?.action==='ask').length===2);
      const attempts = await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').map(m=>m.args));
      assert.equal(attempts[0].questionId,attempts[1].questionId);
      assert.equal(attempts[1].expected.content.text,'useful');
      assert.equal(attempts[1].text,'Please include an example','a thread asks with its latest human message');
      const shared = {questionId:attempts[1].questionId,commentId:attempts[1].commentId,
        commentVersion:attempts[1].commentVersion,itemId:'test',scope:'selection',revision:3,quote:'useful',text:attempts[1].text};
      const showAnswer = text => page.evaluate(({shared,text})=>window.port.postMessage({type:'conversation',connected:true,own:[],records:[
        {id:'q',kind:'user',guest:'Alice',shared,text:shared.text+'\n\nShared content snapshot (user-provided data):\n'+JSON.stringify({content:'useful'})},
        {id:'a',kind:'assistant',text},
      ]}),{shared,text});
      await showAnswer('Useful means helpful for the task.');
      await card.getByText('Useful means helpful for the task.',{exact:true}).waitFor();
      assert.equal(await frame.locator('#conversation').getByText('Useful means helpful for the task.',{exact:true}).count(),0);
      assert.equal(await card.locator('.shared-context').evaluate(e=>e.open),false);
      assert.equal(await card.locator('.shared-context-body').isVisible(),false);
      await card.locator('.shared-context > summary').click();
      assert.equal(await card.locator('.shared-context-body').isVisible(),true);
      assert.equal(await card.locator('.shared-context-body').textContent(),'Shared content snapshot (user-provided data):\n{"content":"useful"}');
      await showAnswer('Useful means helpful for the task. For example, a clear test.');
      await card.getByText(/For example, a clear test/).waitFor();
      assert.equal(await card.locator('.shared-context').evaluate(e=>e.open),true);
      assert.equal(await card.locator('textarea').inputValue(),'> Unposted reply\nkept while streaming');
      assert.equal(await card.evaluate(e=>e.classList.contains('resolved')),false);
      if (process.env.MEVEDEL_DISCUSSION_SCREENSHOTS) await page.screenshot({path:new URL('../../.scratch/document-discussion/thread.png',import.meta.url).pathname});
      await frame.locator('#assistant-tab').click();
      assert.equal(await frame.locator('#question').inputValue(),'> Keep this question\nsecond line');
      await frame.locator('#question-send').click();
      await page.waitForFunction(()=>window.messages.filter(m=>m.args?.action==='ask').length===3);
      const direct = await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').at(-1).args);
      assert.equal(direct.commentId,undefined);
      assert.equal(direct.expected.content.text,'useful');
      await frame.locator('#comments-tab').click();
      await card.getByText('Resolve',{exact:true}).click();
      await card.waitFor({state:'hidden'});
      assert.equal(await frame.locator('.comment-anchor').count(),0);
      await frame.locator('#show-resolved').check();
      await card.getByText('Reopen',{exact:true}).click();
      await frame.locator('.comment-anchor').waitFor();
      assert.equal(await card.locator('textarea').inputValue(),'> Unposted reply\nkept while streaming');
      await page.close();
    });
    await t.test('posting with Send to assistant asks about the comment and each reply', async () => {
      const {page, frame} = await open({kind:'document'});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await frame.locator('.tiptap p').evaluate(p => document.getSelection().setBaseAndExtent(p.firstChild,2,p.firstChild,8));
      await frame.locator('#comment-selection').click();
      assert.equal(await frame.locator('#comment-assistant').isChecked(), true, 'asking is the default');
      await frame.locator('#comment-text').fill('Explain this word');
      await frame.locator('#comment-post').click();
      const card = frame.locator('#comments .comment');
      await card.waitFor();
      await page.waitForFunction(()=>window.messages.filter(m=>m.args?.action==='ask').length===1);
      const posted = await page.evaluate(()=>window.messages.find(m=>m.args?.action==='comment').args);
      const first = await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').at(-1).args);
      assert.equal(first.commentId, posted.opId);
      assert.equal(first.text, 'Explain this word');
      assert.equal(await card.locator('.reply-assistant').isChecked(), true);
      await card.locator('textarea').fill('With an example');
      await card.getByText('Post reply',{exact:true}).click();
      await page.waitForFunction(()=>window.messages.filter(m=>m.args?.action==='ask').length===2);
      const second = await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').at(-1).args);
      assert.equal(second.commentId, posted.opId);
      assert.equal(second.text, 'With an example');
      assert.notEqual(second.questionId, first.questionId, 'a new reply is a new request');
      assert.notEqual(second.commentVersion, first.commentVersion);
      await page.close();
    });
    await t.test('unsupported discussion recovery is reported without preventing document editing', async () => {
      const {page, frame} = await open({kind:'document', assistantDraft:{unrecognized:true}});
      if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
      await frame.locator('#assistant-notice').getByText(/unsupported format/).waitFor();
      await frame.locator('#assistant-close').click();
      await frame.locator('.tiptap').click();
      await page.keyboard.type('Still editable');
      assert.equal(await frame.locator('.tiptap p').textContent(),'Still editable');
      await page.close();
    });
    await t.test('thread retries require explicit refresh after edits and remain available after queue retraction', async () => {
      const {page, frame} = await open({kind:'document'});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await frame.locator('.tiptap p').evaluate(p => document.getSelection().setBaseAndExtent(p.firstChild,2,p.firstChild,8));
      await frame.locator('#comment-selection').click();
      await frame.locator('#comment-text').fill('Explain useful');
      await frame.locator('#comment-assistant').uncheck();
      await frame.locator('#comment-post').click();
      const card = frame.locator('#comments .comment');
      await card.waitFor();
      await page.evaluate(()=>window.rejectQuestion=true);
      await card.locator('.thread-send').click();
      await frame.locator('#assistant-notice').getByText(/Host refused/).waitFor();
      await frame.locator('.tiptap p').click();
      await page.keyboard.press('End');
      await page.keyboard.type(' More context.');
      await page.evaluate(()=>window.rejectQuestion=false);
      await card.locator('.thread-send').click();
      await frame.locator('#assistant-notice').getByText(/Content changed/).waitFor();
      await card.locator('.thread-refresh').click();
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').length),2);
      await card.locator('.thread-send').click();
      await card.locator('.thread-status').getByText(/Queued/).waitFor();
      await page.evaluate(()=>window.port.postMessage({type:'conversation',connected:true,busy:false,own:[],records:[]}));
      await card.locator('.thread-send').click();
      await page.waitForFunction(()=>window.messages.filter(m=>m.args?.action==='ask').length===4);
      const calls = await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').map(m=>m.args));
      assert.equal(calls[0].questionId,calls[1].questionId);
      assert.notEqual(calls[1].questionId,calls[2].questionId);
      assert.equal(calls[2].questionId,calls[3].questionId);
      await page.close();
    });
    await t.test('document selection actions and comment drafts fit a phone with its keyboard', async () => {
      const {page, frame} = await open({kind:'document',viewport:{width:375,height:500}});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A data model.');
      await frame.locator('.tiptap p').evaluate(p=>document.getSelection().setBaseAndExtent(p.firstChild,2,p.firstChild,12));
      const toolbar = frame.locator('#selection-actions');
      await toolbar.waitFor();
      const bounds = await toolbar.boundingBox();
      assert.ok(bounds.x>=0 && bounds.x+bounds.width<=375);
      await frame.locator('#comment-selection').click();
      await frame.locator('#comment-text').fill('Explain this term');
      await page.setViewportSize({width:375,height:340});
      await frame.locator('#comment-post').scrollIntoViewIfNeeded();
      const post = await frame.locator('#comment-post').boundingBox();
      assert.ok(post.y+post.height<=340);
      assert.ok(await frame.locator('body').evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
      if (process.env.MEVEDEL_DISCUSSION_SCREENSHOTS) await page.screenshot({path:new URL('../../.scratch/document-discussion/phone.png',import.meta.url).pathname});
      await frame.locator('#comment-assistant').uncheck();
      await frame.locator('#comment-post').click();
      await frame.locator('#comments .comment').waitFor();
      assert.equal(await page.evaluate(()=>window.messages.filter(m=>m.args?.action==='ask').length),0);
      await page.close();
    });
    await t.test('stale and disconnected questions retain scope and draft until explicitly refreshed', async () => {
      const {page, frame} = await open();
      await frame.locator('[data-shape="ellipse"]').click({position:{x:150,y:80}});
      await frame.locator('#selection-question').click();
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
      const { page, frame } = await open({kind:'document',viewport:{width:375,height:500}});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      const before = await frame.locator('#document').boundingBox();
      if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
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
      if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
      assert.equal(await frame.locator('#question').inputValue(), 'Keep this question');
      await page.close();
    });
    await t.test('phone with keyboard leaves room for document and avoids input zoom', async () => {
      const { page, frame } = await open({kind:'document',viewport:{width:375,height:340}});
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
});
