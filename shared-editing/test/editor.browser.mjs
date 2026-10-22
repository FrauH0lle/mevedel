import test from 'node:test';
import assert from 'node:assert/strict';
import {editorFixture} from './editor-fixture.mjs';
import {shown, box, askBox, send, notice, menu, openWhole} from './sidebar.mjs';

/* An element and, with TEXT, its bound label `${id}-label`. */
const shape = (id, type, [x, y, width, height], text) => [{id, type, x, y, width, height},
  ...(text ? [{id:`${id}-label`, type:'text', x, y, width:0, height:0, text, containerId:id}] : [])];
const geometry = ({x, y, width, height}) => [x, y, width, height];
/* The editor's discussion requests of operation OP, in the order sent. */
const sent = (page, op) => page.evaluate(op => window.messages
  .filter(m => m.args?.action === 'discussion' && m.args.op === op).map(m => m.args), op);
/* Wait until the assistant was asked N times; the requests live in Node. */
async function askedCount(page, n) {
  const deadline = Date.now() + 5000;
  for (;;) {
    const asked = await page.evaluate(() => window.asked());
    if (asked.length === n) return asked;
    if (Date.now() > deadline) throw new Error(`Asked ${asked.length} times, expected ${n}`);
    await new Promise(resolve => setTimeout(resolve, 25));
  }
}

// Set up both editor and DOM selection. A DOM-only range can race
// ProseMirror's deferred focus reconciliation and revert to the old caret.
async function selectPassage(frame, from, to) {
  const {actual, expected} = await frame.locator('.tiptap p').evaluate((p, {from, to}) => {
    const editor = p.closest('.tiptap').editor;
    editor.commands.setTextSelection({
      from: editor.view.posAtDOM(p.firstChild, from),
      to: editor.view.posAtDOM(p.firstChild, to),
    });
    return {actual: document.getSelection().toString(), expected: p.textContent.slice(from, to)};
  }, {from, to});
  assert.equal(actual, expected);
}

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
      assert.ok((await shape.boundingBox()).x>before.x+30);
      assert.equal(await page.locator('#saved').innerText(),'Live movement · not saved yet');
      const geometry=({x,y,width,height})=>[x,y,width,height];
      assert.deepEqual(geometry((await page.evaluate(()=>window.apply({action:'read'}))).content[0]),[100,100,300,160]);
      const exported=await page.evaluate(()=>window.apply({action:'export',format:'native'}));
      assert.deepEqual(geometry(JSON.parse(exported.text).elements[0]),[100,100,300,160]);
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
    await t.test('a skipped revision rereads the item instead of trusting its diff', async () => {
      const {page, frame} = await open();
      await page.evaluate(async () => {
        // Another Emacs committed the first edit; only the second arrives.
        await window.apply({action:'patch', opId:'elsewhere-1',
          changes:[{id:'one', after:{type:'rectangle', x:0, y:0, width:50, height:50}}]});
        const second = await window.apply({action:'patch', opId:'elsewhere-2',
          changes:[{id:'two', after:{type:'rectangle', x:100, y:0, width:50, height:50}}]});
        window.port.postMessage({type:'changed', ...second});
      });
      await frame.locator('[data-shape="two"]').waitFor();
      await frame.locator('[data-shape="one"]').waitFor();
      assert.ok(await page.evaluate(() => window.messages.some(m => m.args?.action === 'read')));
      await page.close();
    });
    await t.test('a failed reread after a skipped revision stays visible and retries', async () => {
      const {page, frame} = await open();
      await page.evaluate(async () => {
        const apply = window.apply;
        let refused = false;
        window.apply = args => args.action === 'read' && !refused
          ? (refused = true, Promise.reject(new Error('Busy'))) : apply(args);
        await apply({action:'patch', opId:'elsewhere-1',
          changes:[{id:'one', after:{type:'rectangle', x:0, y:0, width:50, height:50}}]});
        const second = await apply({action:'patch', opId:'elsewhere-2',
          changes:[{id:'two', after:{type:'rectangle', x:100, y:0, width:50, height:50}}]});
        window.port.postMessage({type:'changed', ...second});
      });
      await page.locator('#saved', {hasText: 'reopen this item'}).waitFor();
      // The next change, though contiguous, reads the item again.
      await page.evaluate(async () => {
        const third = await window.apply({action:'patch', opId:'elsewhere-3',
          changes:[{id:'three', after:{type:'rectangle', x:200, y:0, width:50, height:50}}]});
        window.port.postMessage({type:'changed', ...third});
      });
      await frame.locator('[data-shape="one"]').waitFor();
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
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
      await page.locator('#saved').getByText('Save refused',{exact:true}).waitFor();
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
        await page.locator('#saved').getByText('No save acknowledgement',{exact:true}).waitFor();
        await frame.locator('#retry').evaluate(button=>button.click());
      }
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      assert.equal(await page.evaluate(()=>window.updateCalls),loseAck ? 3 : 2,'no stale keystroke backlog');
      const stored=await page.evaluate(()=>window.apply({action:'read'}));
      assert.equal(stored.content.find(s=>s.containerId==='ellipse').text,'test1234\n3333','the label is a bound text element');
      assert.equal(stored.revision,3,'retry does not commit the in-flight operation twice');
      await page.close();
    });
    await t.test('an idle edit saves at once; edits during a save follow its reply as one', async () => {
      const {page,frame}=await open({clock:true});
      // Time passes only through runFor, so each wait below is exact.
      await page.clock.pauseAt(Date.now()+1000);
      await page.evaluate(()=>{
        const apply=window.apply;
        window.saves=[];
        window.apply=async args=>{
          if(args.action==='update') {
            window.saves.push(args.opId);
            if(window.holdSaves) await new Promise(resolve=>{window.releaseSave=resolve;});
          }
          return apply(args);
        };
        window.holdSaves=true;
      });
      const saves=()=>page.evaluate(()=>window.saves.length);
      const text=async value=>{ await frame.locator('#shape-text').fill(value); await page.clock.runFor(1); };
      await frame.locator('[data-shape="ellipse"]').dblclick();
      await text('a');
      await page.waitForFunction(()=>window.saves.length===1);
      await text('ab');
      await text('abc');
      await page.clock.runFor(1000);
      await new Promise(resolve=>setTimeout(resolve,200));
      assert.equal(await saves(),1,'one save in flight');
      await page.evaluate(()=>{ window.holdSaves=false; window.releaseSave(); });
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      assert.equal(await saves(),2,'the edits made during the save follow it as one');
      // A fast host's next save waits out the floor since the previous one began.
      await page.clock.runFor(100);
      await text('abcd');
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      await text('abcde');
      await new Promise(resolve=>setTimeout(resolve,200));
      assert.equal(await saves(),3);
      await page.clock.runFor(60);
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      assert.equal(await saves(),4);
      const stored=await page.evaluate(()=>window.apply({action:'read'}));
      assert.equal(stored.content.find(s=>s.containerId==='ellipse').text,'abcde');
      await page.close();
    });
    await t.test('a completed save leaves no timer that erases a later validation error', async () => {
      const {page,frame}=await open({clock:true});
      await page.clock.pauseAt(Date.now()+1000);
      await frame.locator('[data-shape="ellipse"]').dblclick();
      await frame.locator('#shape-text').fill('Saved');
      await page.clock.runFor(1);
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      await frame.locator('#image-upload').setInputFiles({
        name:'bad.svg',mimeType:'image/svg+xml',buffer:Buffer.from('<svg/>'),
      });
      const message='Use a PNG, JPEG, or WebP image up to 4 MB';
      await page.locator('#saved').getByText(message,{exact:true}).waitFor();
      await page.clock.runFor(100);
      assert.equal(await page.locator('#saved').innerText(),message);
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
      await page.locator('#saved').getByText('Shared content is too large',{exact:true}).waitFor();
      await page.evaluate(()=>window.port.postMessage({type:'storage-error',message:'Recovery storage unavailable'}));
      await page.waitForTimeout(350);
      assert.equal(await page.locator('#saved').innerText(),'Shared content is too large');
      assert.match(await frame.locator('.tiptap').innerText(),/Keep this pending edit/);
      await page.close();
    });
    await t.test('editor theme follows its trusted port without losing document or question drafts', async () => {
      const {page, frame} = await open({kind:'document', viewport:{width:1280,height:800}, appearance:{theme:'dark'}});
      assert.equal(await frame.locator('html').getAttribute('data-theme'), 'dark');
      await frame.locator('.tiptap').click();
      await page.keyboard.type('Keep this document.');
      await openWhole(frame);
      await box(frame).fill('> Keep this question\nsecond line');
      for (const theme of ['light', 'dark', 'system', 'invalid']) {
        await page.evaluate(theme => window.port.postMessage({type:'appearance',appearance:{theme,palette:'cool',accents:'selective'}}), theme);
        await frame.locator('html').evaluate((root, expected) => new Promise((resolve,reject) => {
          const deadline = Date.now()+1500;
          const check = () => root.getAttribute('data-theme') === expected ? resolve()
            : Date.now()>deadline ? reject(new Error('Theme was not applied')) : requestAnimationFrame(check);
          check();
        }), ['light','dark'].includes(theme) ? theme : null);
        const colors = await shown(frame).locator('.ds-send').evaluate(button => {
          const style = getComputedStyle(button);
          return {text:style.color,background:style.backgroundColor};
        });
        const luminance = color => color.match(/[\d.]+/g).slice(0,3).map(Number)
          .map(c=>c/255).map(c=>c<=.04045?c/12.92:((c+.055)/1.055)**2.4)
          .reduce((sum,c,i)=>sum+c*[.2126,.7152,.0722][i],0);
        const a=luminance(colors.text), b=luminance(colors.background);
        assert.ok((Math.max(a,b)+.05)/(Math.min(a,b)+.05)>=4.5,JSON.stringify(colors));
        assert.equal(await box(frame).inputValue(), '> Keep this question\nsecond line');
        assert.match(await frame.locator('.tiptap').innerText(), /Keep this document/);
      }
      await page.close();
    });
    for (const kind of ['whiteboard', 'document']) {
      await t.test(`${kind} shows assistant activity and clears it on idle or disconnect`, async () => {
        const {page, frame} = await open({kind});
        await openWhole(frame);
        const state = frame.locator('#conversation-state');
        await box(frame).fill('> Preserved question\nsecond line');
        await page.evaluate(()=>window.port.postMessage({type:'conversation',connected:true,
          conversationError:'Archive is unavailable',records:[],own:[]}));
        await frame.locator('#conversation-history-notice').getByText('Earlier conversation unavailable: Archive is unavailable',{exact:true}).waitFor();
        assert.equal(await box(frame).inputValue(),'> Preserved question\nsecond line');
        for (const activity of [{connected:true,busy:true,paused:true}, {connected:true,busy:false}, {connected:false,busy:true}]) {
          await page.evaluate(activity=>window.port.postMessage({type:'conversation',records:[],own:[],...activity}),activity);
          const active = activity.connected && activity.busy;
          if (active) await state.getByText('Assistant working',{exact:true}).waitFor();
          // An idle, connected session has nothing to report.
          else if (activity.connected) await state.waitFor({state:'hidden'});
          else await state.getByText('Disconnected · drafts kept',{exact:true}).waitFor();
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
        const pasted = await surface.evaluate((element,data)=>{
          const transfer=new DataTransfer();
          transfer.items.add(new File([Uint8Array.from(atob(data),c=>c.charCodeAt(0))],'pasted.png',{type:'image/png'}));
          // Supply the payload explicitly: ClipboardEventInit does not retain
          // clipboardData in every engine. Still exercise the real paste listener.
          const event=new ClipboardEvent('paste',{bubbles:true,cancelable:true});
          Object.defineProperty(event,'clipboardData',{value:transfer});
          const files=[...event.clipboardData.files].map(file=>file.type);
          return {files,handled:!element.dispatchEvent(event)};
        },data);
        assert.deepEqual(pasted,{files:['image/png'],handled:true});
        await images.nth(2).waitFor();
        await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
        const saved = await page.evaluate(async()=>window.apply({action:'read'}));
        if (kind === 'document')
          assert.equal((JSON.stringify(saved.content).match(/data:image\/png;base64/g)||[]).length,3);
        else {
          // Three image elements share one content-addressed file, as in Excalidraw.
          const fileIds = saved.content.filter(e=>e.type==='image').map(e=>e.fileId);
          assert.deepEqual(fileIds.map(id=>/^[0-9a-f]{40}$/.test(id)),[true,true,true]);
          assert.equal(new Set(fileIds).size,1);
          const file = JSON.parse((await page.evaluate(()=>window.apply({action:'export',format:'native'}))).text);
          assert.deepEqual(Object.keys(file.files),[fileIds[0]]);
          assert.equal(file.files[fileIds[0]].dataURL,`data:image/png;base64,${data}`);
        }
        await frame.locator('#image-upload').setInputFiles({name:'bad.svg',mimeType:'image/svg+xml',buffer:Buffer.from('<svg/>')});
        await page.locator('#saved').getByText('Use a PNG, JPEG, or WebP image up to 4 MB',{exact:true}).waitFor();
        assert.equal(await images.count(),3);
        if (kind === 'document') {
          // Ordinary HTML copy/paste must retain numeric image dimensions.
          await surface.click();
          await surface.evaluate(element=>element.editor.commands.focus('end'));
          await page.frames()[1].waitForFunction(()=>{const e=document.querySelector('.tiptap').editor;return e.state.selection.empty&&e.state.selection.from===e.state.doc.content.size-1;});
          const copiedHTML = await surface.evaluate((element,data)=>{
            const transfer=new DataTransfer();
            transfer.setData('text/html',`<img src="data:image/png;base64,${data}" width="80" height="40" alt="Copied">`);
            const event=new ClipboardEvent('paste',{bubbles:true,cancelable:true});
            Object.defineProperty(event,'clipboardData',{value:transfer});
            return {html:event.clipboardData.getData('text/html'),handled:!element.dispatchEvent(event)};
          },data);
          assert.deepEqual(copiedHTML,{html:`<img src="data:image/png;base64,${data}" width="80" height="40" alt="Copied">`,handled:true});
          await images.nth(3).waitFor();
          await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
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
      const handle = await frame.locator('#selection [data-handle="se"]').boundingBox();
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
    await t.test('room session navigation sits beside wide conversations, above narrow composers and over a phone', async () => {
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
        // A phone opens it as a sheet over the foot of the room.
        else if (width <= 640) assert.equal(Math.round(menu.y + menu.height), 800);
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
      assert.equal(await frame.locator('#selection').getAttribute('data-selected'), 'ellipse');
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
        ...shape('a', 'rectangle', [0,0,100,100], 'A'),
        ...shape('b', 'rectangle', [300,0,100,100], 'B'),
        ...shape('c', 'ellipse', [0,300,100,100], 'C'),
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
      const chosen = () => frame.locator('#selection').evaluate(n => n.dataset.selected ? n.dataset.selected.split(' ') : []);
      await drag([-20,-20], [200,150]);
      assert.deepEqual(await chosen(), ['a']);
      assert.equal(await frame.locator('#selection .selection-region').count(), 0, 'the box disappears on release');
      assert.equal(await frame.locator('#discuss-selection').isDisabled(), false);
      // With a selection the panel covers the board's left, so these boxes
      // start at their right; a box selects the same either way round.
      await drag([320,150], [-20,-20], 'Alt');
      assert.deepEqual(await chosen(), ['a','b']);
      await drag([150,450], [-20,280], 'Shift');
      assert.deepEqual(await chosen(), ['a','b','c']);
      await drag([140,140], [260,260]);
      assert.deepEqual(await chosen(), []);
      assert.equal(await frame.locator('#discuss-selection').isDisabled(), false,
        'an empty area remains selected for questions');
      const [x, y] = await at(200, 200);
      await page.mouse.click(offset.x + x, offset.y + y);
      assert.equal(await frame.locator('#discuss-selection').isDisabled(), true);
      const content = await page.evaluate(async () => (await window.apply({action:'read'})).content);
      assert.deepEqual(content.filter(s => s.type !== 'text').map(geometry), [[0,0,100,100],[300,0,100,100],[0,300,100,100]],
        'box selection never edits content');
      await page.close();
    });
    await t.test('discussing a boxed area sends its region with the contained objects', async () => {
      const {page, frame} = await open({content:[
        ...shape('a', 'rectangle', [0,0,100,100], 'A'),
        ...shape('b', 'rectangle', [300,0,100,100], 'B'),
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
      await frame.locator('#discuss-selection').click();
      assert.equal(await frame.locator('#selection .selection-region').count(), 1, 'the attached area shows again');
      assert.equal(await shown(frame).locator('.ds-subject-title').innerText(), 'New discussion');
      assert.equal(await shown(frame).locator('.ds-target').innerText(), 'Selected area');
      assert.equal(await shown(frame).locator('.ds-send').innerText(), 'Start discussion');
      assert.match(await shown(frame).locator('.ds-quote').textContent(), /^Area \d+ × \d+ at -2\d, -2\d\n1 object\nrectangle: A$/);
      await box(frame).fill('Put a legend here');
      await send(frame);
      await askedCount(page, 1);
      const [post] = await sent(page, 'post');
      assert.deepEqual(post.target.selection, ['a']);
      assert.equal(post.target.region.length, 4);
      // The host checks the reviewed capture by its hash; the capture stays here.
      assert.match(post.expected, /^[0-9a-f]{12}$/);
      const [ask] = await page.evaluate(() => window.asked());
      assert.deepEqual([ask.discussionId, ask.version], [post.discussionId, post.messageId]);
      assert.deepEqual(ask.selection, ['a']);
      // The started discussion stays open in the sidebar.
      await shown(frame).locator('.ds-message').getByText('Put a legend here', {exact: true}).waitFor();
      assert.match(await shown(frame).locator('.ds-subject-title').innerText(), /^Area/);
      await page.close();
    });
    await t.test('a selection is sent without a message, a whole-board message is not', async () => {
      const {page, frame} = await open({content:[
        ...shape('a', 'rectangle', [0,0,100,100], 'A'),
        ...shape('b', 'rectangle', [300,0,100,100], 'B'),
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
      await frame.locator('#discuss-selection').click();
      assert.match(await box(frame).getAttribute('placeholder'), /leave empty to send the selected area/);
      assert.equal(await box(frame).inputValue(), '');
      // Without the assistant, a selection alone says nothing.
      await askBox(frame).uncheck();
      assert.doesNotMatch(await box(frame).getAttribute('placeholder'), /leave empty/);
      await send(frame);
      assert.match(await notice(frame).innerText(), /choose Ask assistant to send the selection alone/);
      assert.equal((await sent(page, 'post')).length, 0);
      await askBox(frame).check();
      await send(frame);
      await askedCount(page, 1);
      const [post] = await sent(page, 'post');
      assert.equal(post.text, '', 'the host, not the browser, words a selection sent alone');
      assert.equal(post.toAssistant, true);
      const [ask] = await page.evaluate(() => window.asked());
      assert.deepEqual(ask.selection, ['a']);
      await shown(frame).locator('.ds-message .ds-text.alone')
        .getByText('Sent the selection as the request', {exact: true}).waitFor();
      // The whole board has nothing of its own to say: it needs a message.
      await openWhole(frame);
      assert.match(await box(frame).getAttribute('placeholder'), /Write about the whole whiteboard/);
      await send(frame);
      assert.match(await notice(frame).innerText(), /Write a message first/);
      assert.equal((await sent(page, 'post')).length, 1);
      await page.close();
    });
    await t.test('board discussions: Discuss tool, numbered markers, peek, messages and resolution', async () => {
      const {page, frame} = await open({content:[
        ...shape('a', 'rectangle', [0,0,100,100], 'A'),
        ...shape('b', 'rectangle', [300,0,100,100], 'B'),
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
      assert.equal(await frame.locator('[data-tool="discuss"]').getAttribute('aria-pressed'), 'true');
      await page.mouse.move(...await at(50, 50));
      assert.equal(await frame.locator('#selection .discussion-hover').count(), 1, 'hover outlines the target');
      await page.mouse.down(); await page.mouse.up();
      await box(frame).waitFor();
      assert.equal(await shown(frame).locator('.ds-quote').textContent(), '1 object\nrectangle: A');
      await box(frame).fill('Make this blue');
      await askBox(frame).uncheck();
      await send(frame);
      await shown(frame).locator('.ds-message').waitFor();
      assert.equal(await shown(frame).locator('.ds-reveal').textContent(), 'Show on board');
      const pin = frame.locator('#discussion-markers .discussion-pin');
      await pin.waitFor();
      assert.equal(await pin.textContent(), '1');
      // An area discussion from a drag with the Discuss tool.
      await frame.locator('#assistant-close').click();
      const [x0, y0] = await at(150, 20), [x1, y1] = await at(250, 80);
      await page.mouse.move(x0, y0); await page.mouse.down();
      await page.mouse.move(x1, y1, {steps: 3}); await page.mouse.up();
      assert.match(await shown(frame).locator('.ds-quote').textContent(), /^Area 10\d × 6\d at 1(49|50), (19|20)\n0 objects$/);
      assert.equal(await askBox(frame).isChecked(), false, 'the choice is kept for the next message');
      await box(frame).fill('Legend here');
      await send(frame);
      await frame.locator('#discussion-markers .discussion-pin').nth(1).waitFor();
      assert.match(await shown(frame).locator('.ds-subject-title').textContent(),
                   /^Area 10\d × 6\d at 1(49|50), (19|20) · 0 objects$/, 'titles keep quote lines apart');
      const posted = await sent(page, 'post');
      assert.deepEqual(posted[0].target.selection, ['a']);
      assert.equal(posted[1].target.region.length, 4);
      assert.equal((await page.evaluate(() => window.asked())).length, 0);
      const id = posted[0].discussionId;
      // Hovering a pin peeks at its latest message; clicking opens it in the sidebar.
      await frame.locator('#assistant-close').click();
      await pin.first().hover();
      await frame.locator('#discussion-peek:not([hidden])').waitFor();
      assert.match(await frame.locator('#discussion-peek').textContent(), /Alice.*Make this blue/s);
      await page.mouse.move(offset.x + 5, offset.y + offset.height - 5);
      assert.equal(await frame.locator('#discussion-peek').isHidden(), true);
      await pin.first().click();
      assert.equal(await shown(frame).locator('.ds-subject-title').textContent(), '1 object · rectangle: A');
      await page.keyboard.press('Escape');
      await frame.locator('[data-tool="select"]').click();
      await frame.locator('#ask-toggle').click();
      await shown(frame).locator('.ds-reveal').click();
      assert.equal(await frame.locator('#selection').getAttribute('data-selected'), 'a');
      // Asking again sends the discussion about its objects; resolving removes its marker.
      await menu(frame, 'Ask assistant again');
      await askedCount(page, 1);
      const [ask] = await page.evaluate(() => window.asked());
      assert.deepEqual(ask.selection, ['a']);
      assert.deepEqual([ask.discussionId, ask.version], [id, posted[0].messageId],
        'the latest message is the request');
      // Its pin carries a turning ring while the assistant works on it.
      const marker = `#discussion-markers [data-discussion-id="${id}"]`;
      const spinning = () => frame.locator(`${marker}[data-working] .discussion-spin`).count();
      await frame.locator('#discussion-markers [data-working] .discussion-spin').waitFor();
      const delivered = {id:'q', kind:'user', guest:'Alice',
        shared:{questionId:ask.questionId, discussionId:id, messageId:ask.version}, text:'Make this blue'};
      const update = (records, busy, active) => page.evaluate(([records, busy, active]) =>
        window.port.postMessage({type:'conversation', connected:true, busy, active, own:[], records}),
      [records, busy, active]);
      await update([delivered], true, ask.questionId);
      await page.waitForTimeout(50);
      assert.equal(await spinning(), 1, 'a delivered request spins while the session works');
      await update([delivered, {id:'r', kind:'assistant', text:'On it.'}], true, ask.questionId);
      await page.waitForTimeout(50);
      assert.equal(await spinning(), 1, 'a first reply does not end the turn');
      await update([delivered, {id:'r', kind:'assistant', text:'On it.'}], true, 'another-question');
      await frame.locator(`${marker}[data-working]`).waitFor({state:'detached'});
      assert.equal(await spinning(), 0, 'another turn running is not this discussion');
      await update([delivered, {id:'r', kind:'assistant', text:'Done.'}], true, ask.questionId);
      await frame.locator(`${marker}[data-working]`).waitFor();
      await update([delivered, {id:'r', kind:'assistant', text:'Done.'}], false, null);
      await frame.locator(`${marker}[data-working]`).waitFor({state:'detached'});
      assert.equal(await spinning(), 0, 'a finished turn stops spinning');
      await shown(frame).getByText('Done.', {exact: true}).waitFor();
      // Resolved elsewhere while its pin is hovered, the peek closes with the pin.
      await frame.locator('#assistant-close').click();
      await frame.locator(`${marker} .discussion-pin`).hover();
      await frame.locator('#discussion-peek:not([hidden])').waitFor();
      await page.evaluate(async discussionId => {
        const result = await window.apply({action:'discussion', op:'resolve', discussionId, resolved:true});
        window.port.postMessage({type:'changed', ...result});
      }, id);
      await frame.locator(marker).waitFor({state:'detached'});
      assert.equal(await frame.locator('#discussion-peek').isHidden(), true);
      await page.evaluate(async discussionId => {
        const result = await window.apply({action:'discussion', op:'resolve', discussionId, resolved:false});
        window.port.postMessage({type:'changed', ...result});
      }, id);
      await frame.locator(marker).waitFor();
      await frame.locator('#ask-toggle').click();
      await menu(frame, 'Resolve');
      await shown(frame).locator('.ds-closed').getByText('Reopen to reply').waitFor();
      assert.equal(await box(frame).isVisible(), false, 'a resolved discussion takes no replies');
      assert.equal(await frame.locator('#discussion-markers .discussion-pin').count(), 1);
      assert.equal(await frame.locator('#discussion-markers .discussion-pin').textContent(), '1', 'open markers renumber');
      // Deleting the objects of a discussion marks it removed.
      await shown(frame).locator('.ds-closed').getByText('Reopen to reply').click();
      await page.evaluate(delivered => window.port.postMessage({type:'conversation', connected:true, own:[], records:[
        delivered, {id:'r', kind:'assistant', text:'Done.'}]}), delivered);
      await page.evaluate(async () => {
        const read = await window.apply({action:'read'});
        const result = await window.apply({action:'patch', opId:'agent-delete',
          changes:[{id:'a', hash:await window.contentHash(read.content.find(s => s.id === 'a')), after:null}]});
        window.port.postMessage({type:'changed', ...result});
      });
      await shown(frame).locator('.ds-status').getByText('Objects removed').waitFor();
      await shown(frame).locator('.ds-menu > summary').click();
      assert.equal(await shown(frame).locator('.ds-menu-body').getByText('Ask assistant again').isDisabled(), true);
      assert.equal(await frame.locator('#discussion-markers .discussion-pin').count(), 1, 'a removed target has no marker');
      await page.close();
    });
    await t.test('the sidebar box starts compact and grows with its text', async () => {
      const {page, frame} = await open({viewport:{width:1280,height:640}});
      await frame.locator('#scene [data-shape="ellipse"]').click({position:{x:150,y:80}});
      await frame.locator('#discuss-selection').click();
      assert.ok((await box(frame).boundingBox()).height <= 50);
      await box(frame).fill('Is this the database?');
      await send(frame);
      await shown(frame).locator('.ds-message').first().waitFor();
      const empty = (await box(frame).boundingBox()).height;
      assert.ok(empty <= 50, `a reply starts compact: ${empty}`);
      await box(frame).fill('First line\nSecond line\nThird line');
      assert.ok((await box(frame).boundingBox()).height > empty + 15, 'the box grows with its text');
      assert.ok(await shown(frame).locator('.ds-messages').evaluate(n => n.scrollWidth <= n.clientWidth));
      await page.close();
    });
    await t.test('the sidebar lists discussions, opens one at a time and marks news elsewhere', async () => {
      const {page, frame} = await open({viewport:{width:1280,height:800}, content:[
        ...shape('a', 'rectangle', [0,0,100,100], 'A'),
        ...shape('b', 'rectangle', [300,0,100,100], 'B'),
      ]});
      const offset = await page.locator('iframe').boundingBox();
      const pick = async id => {
        const [x, y] = await frame.locator('#canvas').evaluate((canvas, id) => {
          const p = new DOMPoint(id === 'a' ? 50 : 350, 50).matrixTransform(canvas.getScreenCTM());
          return [p.x, p.y];
        }, id);
        await page.mouse.click(offset.x + x, offset.y + y);
      };
      const discuss = async (id, text) => {
        await pick(id);
        await frame.locator('#discuss-selection').click();
        await box(frame).fill(text);
        await askBox(frame).uncheck();
        await send(frame);
        await shown(frame).locator('.ds-message').getByText(text, {exact: true}).waitFor();
      };
      await discuss('a', 'About A');
      await shown(frame).locator('.ds-back').click();
      assert.deepEqual(await frame.locator('.ds-list .ds-title').allInnerTexts(),
                       ['Whole whiteboard', '1 object · rectangle: A']);
      // With objects selected, the list's foot discusses them.
      await pick('b');
      await frame.locator('.ds-start').getByText('Discuss 1 selected object').waitFor();
      await frame.locator('.ds-start').click();
      await box(frame).fill('Dropped');
      // Back drops a new discussion.
      await shown(frame).locator('.ds-back').click();
      assert.equal(await frame.locator('.ds-list .ds-item').count(), 2);
      await frame.locator('.ds-start').click();
      assert.equal(await box(frame).inputValue(), '');
      await shown(frame).locator('.ds-back').click();
      await discuss('b', 'About B');
      const [posted] = await sent(page, 'post');
      // An answer in discussion 1 arrives while discussion 2 is open.
      // The conversation loaded with the item is already seen.
      await page.evaluate(() => window.port.postMessage({type:'conversation', connected:true, own:[], records:[]}));
      await box(frame).fill('Still typing');
      await page.evaluate(posted => window.port.postMessage({type:'conversation', connected:true, own:[], records:[
        {id:'q', kind:'user', guest:'Alice', text:'About A',
         shared:{questionId:'q1', discussionId:posted.discussionId, messageId:posted.messageId}},
        {id:'r', kind:'assistant', text:'Answer to A'}]}), posted);
      await shown(frame).locator('.ds-badge').getByText('1 new').waitFor();
      assert.match(await shown(frame).locator('.ds-subject-title').innerText(), /B$/, 'the open discussion stays');
      assert.equal(await box(frame).inputValue(), 'Still typing');
      const pinA = frame.locator(`#discussion-markers [data-discussion-id="${posted.discussionId}"]`);
      assert.equal(await pinA.getAttribute('data-unread'), 'true', 'its pin carries a dot');
      assert.equal(await frame.locator('#ask-toggle').evaluate(n => n.classList.contains('unread')), true);
      await shown(frame).locator('.ds-back').click();
      const rowA = frame.locator(`.ds-item[data-discussion-id="${posted.discussionId}"]`);
      assert.match(await rowA.getAttribute('class'), /unread/);
      assert.match(await rowA.innerText(), /New answer[^]*Assistant: Answer to A/);
      await rowA.click();
      await shown(frame).locator('.ds-answers').getByText('Answer to A').waitFor();
      await shown(frame).locator('.ds-back').click();
      assert.doesNotMatch(await rowA.getAttribute('class'), /unread/, 'opening clears the mark');
      assert.equal(await pinA.getAttribute('data-unread'), null);
      // Resolved discussions fold into one row at the list's end.
      await rowA.click();
      await menu(frame, 'Resolve');
      await shown(frame).locator('.ds-closed').waitFor();
      await shown(frame).locator('.ds-back').click();
      assert.equal(await frame.locator('.ds-resolved > summary').innerText(), 'Resolved · 1');
      assert.equal(await rowA.isVisible(), false);
      await page.close();
    });
    await t.test('drawing a shape returns to selection for immediate text editing', async () => {
      const {page, frame} = await open();
      await frame.locator('#board-zoom').click();
      await frame.locator('[data-tool="rectangle"]').click();
      const canvas = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(canvas.x + 620, canvas.y + 220);
      await page.mouse.down();
      await page.mouse.move(canvas.x + 820, canvas.y + 330);
      await page.mouse.up();
      assert.equal(await frame.locator('[data-tool="select"]').getAttribute('aria-pressed'), 'true');
      await frame.locator('#scene [data-shape]:not([data-shape="ellipse"])').dblclick();
      await frame.locator('#shape-text').fill('Frontend');
      if (process.env.MEVEDEL_EDITOR_SCREENSHOTS) await page.screenshot({path:'.scratch/shared-editing-followup/inline-text.png'});
      assert.equal(await frame.locator('#scene [data-shape]').count(), 3, 'the ellipse, the rectangle and its label');
      await page.keyboard.press('Control+Enter');
      assert.equal(await frame.locator('#scene text').textContent(), 'Frontend');
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      const {content} = await page.evaluate(()=>window.apply({action:'read'}));
      assert.equal(content.find(s=>s.type==='text').containerId, content.find(s=>s.type==='rectangle').id,
        'the typed label is bound to the new rectangle');
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
        const bind = elementId => ({elementId, fixedPoint:[0.5, 0.5], mode:'orbit'});
        const reply = await window.apply({action:'patch',opId:'connect',changes:[
          {id:'target',after:{id:'target',type:'rectangle',x:550,y:100,width:200,height:160}},
          {id:'arrow',after:{id:'arrow',type:'arrow',x:250,y:180,width:400,height:0,
            points:[[0,0],[400,0]],startBinding:bind('ellipse'),endBinding:bind('target')}},
        ]});
        window.port.postMessage({type:'changed',...reply});
      });
      const shaft = frame.locator('[data-shape="arrow"] g[stroke-linecap] > path').first();
      await shaft.waitFor({state:'attached'});
      const endpoints = () => shaft.evaluate(path => {
        const a = path.getPointAtLength(0), b = path.getPointAtLength(path.getTotalLength());
        return [[a.x,a.y],[b.x,b.y]].map(p => p.map(Math.round));
      });
      // Bound ends sit 5 + strokeWidth / 2 outside each outline, as in Excalidraw.
      assert.deepEqual(await endpoints(), [[406,180],[544,180]]);
      await frame.getByRole('button',{name:'Fit',exact:true}).click();
      await frame.locator('[data-shape="target"]').click({position:{x:100,y:80}});
      await page.keyboard.press('Shift+ArrowRight');
      assert.deepEqual(await endpoints(), [[406,180],[554,180]]);
      const exported = await page.evaluate(async () => {
        await new Promise(resolve => setTimeout(resolve,400));
        return JSON.parse((await window.apply({action:'export',format:'native'})).text);
      });
      const arrow = exported.elements.find(e => e.id === 'arrow');
      assert.deepEqual([arrow.x, arrow.x + arrow.points.at(-1)[0]].map(Math.round), [406, 554],
        'the file stores the ends where they are drawn');
      assert.deepEqual(exported.elements.find(e => e.id === 'target').boundElements, [{id:'arrow', type:'arrow'}]);
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
      const { page, frame } = await open({viewport:{width:1280,height:700}});
      assert.equal(await frame.locator('#properties').isVisible(), false, 'nothing to style');
      await frame.locator('#scene [data-shape="ellipse"] path').click({ force: true });
      assert.equal(await frame.locator('#properties').isVisible(), true, 'selecting opens the panel');
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
      const outline = () => frame.locator('#scene [data-shape="ellipse"] path[fill="none"]').first().getAttribute('d');
      const artist = await outline();
      await frame.getByRole('button', { name: 'Cartoonist', exact: true }).click();
      assert.notEqual(await outline(), artist, 'sloppiness redraws the outline');
      await frame.locator('#board-zoom').click();
      // On a wide board the panel styles the next object of a drawing tool.
      await frame.getByRole('button', { name: 'Rectangle', exact: true }).click();
      assert.equal(await frame.locator('.property-caption').innerText(), 'New rectangle');
      assert.equal(await frame.locator('.sec[data-sec="roundness"]').isHidden(), false);
      assert.equal(await frame.locator('.sec[data-sec="actions"]').isHidden(), true, 'nothing to act on yet');
      const box = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(box.x + 650, box.y + 180);
      await page.mouse.down();
      await page.mouse.move(box.x + 750, box.y + 250);
      await page.mouse.up();
      const rect = frame.locator('#scene [data-shape]:not([data-shape="ellipse"])');
      assert.equal(await rect.locator('g > path').first().getAttribute('stroke'), '#a5d8ff', 'the new rectangle is hatched blue');
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
      await page.mouse.down();
      for (let i = 0; i < 10; i++) {
        await page.mouse.move(box.x + 200 + 15 * i, box.y + 200);
        await page.waitForTimeout(16);
      }
      await page.mouse.up();
      await page.waitForTimeout(1200);
      assert.ok(await frame.locator('.pointer-trail path').count() > 3, 'held ink outlasts the hover trail');
      await page.waitForTimeout(2200);
      assert.equal(await frame.locator('.pointer-trail path').count(), 0, 'held ink expires');
      const messages = await page.evaluate(() => window.messages);
      assert.ok(messages.some(m => m.type === 'presence' && m.mode === 'laser'));
      assert.ok(messages.some(m => m.trail?.some(s => s[3] > 0)), 'held samples carry ink indices');
      assert.equal(messages.filter(m => m.type === 'request').length, 0);
      // Moving onto the board's controls leaves the canvas.
      const menu = await frame.locator('#menu > summary').boundingBox();
      await page.mouse.move(menu.x + menu.width / 2, menu.y + menu.height / 2);
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
    await t.test('reduced motion retains a moving laser tip without trails or content edits', async () => {
      const { page, frame } = await open();
      await page.emulateMedia({reducedMotion:'reduce'});
      await frame.locator('[data-tool="laser"]').click();
      const box = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(box.x + 200, box.y + 250);
      await frame.locator('#presence text').waitFor({state:'visible'});
      const before = await frame.locator('.pointer-tip').getAttribute('transform');
      await page.mouse.down();
      for (let i = 1; i <= 4; i++) {
        await page.mouse.move(box.x + 200 + 15 * i, box.y + 250);
        await page.waitForTimeout(16);
      }
      const after = await frame.locator('.pointer-tip').evaluate(async tip => {
        await new Promise(requestAnimationFrame);
        return tip.getAttribute('transform');
      });
      assert.notEqual(after, before, 'the visible tip follows held movement');
      assert.equal(await frame.locator('#presence text').textContent(), 'Alice');
      assert.equal(await frame.locator('.pointer-trail path').count(), 0);
      await page.mouse.up();
      assert.equal(await frame.locator('#scene [data-shape]').count(), 1);
      assert.equal(await page.evaluate(() => window.messages.filter(m => m.type === 'request').length), 0);
      await page.close();
    });
    await t.test('native touch captures the pointer and clears laser presence on release', async () => {
      const { page, frame } = await open({hasTouch:true});
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await frame.locator('[data-tool="laser"]').click();
      await frame.locator('#canvas').dispatchEvent('pointermove', {pointerType:'touch', clientX:200, clientY:250});
      assert.equal(await frame.locator('#presence > g').count(), 0, 'touch does not point without contact');
      await frame.locator('#canvas').evaluate(canvas => {
        window.touchEvents = [];
        for (const type of ['pointerdown', 'pointerup']) canvas.addEventListener(type, event => {
          // These listeners run after the editor's handlers. Playwright's native
          // tap releases before rendering, so test visible movement separately.
          window.touchEvents.push({type, pointerType:event.pointerType, trusted:event.isTrusted,
            captured:canvas.hasPointerCapture(event.pointerId), label:canvas.querySelector('#presence text')?.textContent ?? null});
        });
      });
      const box = await frame.locator('#canvas').boundingBox();
      await page.touchscreen.tap(box.x + 200, box.y + 250);
      assert.deepEqual(await frame.locator('#canvas').evaluate(() => window.touchEvents), [
        {type:'pointerdown', pointerType:'touch', trusted:true, captured:true, label:'Alice'},
        {type:'pointerup', pointerType:'touch', trusted:true, captured:true, label:null},
      ]);
      await frame.locator('#presence > g').waitFor({state:'detached'});
      assert.equal(await frame.locator('#scene [data-shape]').count(), 1);
      assert.equal(await page.evaluate(() => window.messages.filter(m => m.type === 'request').length), 0);
      assert.deepEqual(errors, []);
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
          const after = kind === 'whiteboard' ? {...before,strokeColor:'#e03131'} :
            {...before,content:[{type:'text',text:'Assistant text'}]};
          const result = await window.apply({action:'patch',opId:'timed-edit',changes:[{
            id:kind === 'whiteboard' ? before.id : before.attrs.id,hash:await window.contentHash(before),after,
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
            const after = kind === 'whiteboard' ? {...before,strokeColor:actor.startsWith('Agent:') ? '#2f9e44' : '#1971c2'} :
              {...before,content:[{type:'text',text:actor}]};
            const result = await window.apply({action:'patch',opId:actor.startsWith('Agent:') ? 'renew-agent' : 'renew-human',changes:[{
              id:kind === 'whiteboard' ? before.id : before.attrs.id,hash:await window.contentHash(before),after,
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
                hash: await window.contentHash(before),
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
    await t.test('a selection starts a discussion with replies, a retried request and answers in place', async () => {
      const {page, frame} = await open({kind:'document'});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await selectPassage(frame, 2, 8);
      await frame.locator('#discuss-selection').click();
      assert.equal(await shown(frame).locator('.ds-quote').textContent(),'useful');
      await box(frame).fill('Explain this word');
      // The draft and what it discusses survive closing the sidebar.
      await frame.locator('#assistant-close').click();
      await frame.locator('#ask-toggle').click();
      assert.equal(await box(frame).inputValue(),'Explain this word');
      assert.equal(await shown(frame).locator('.ds-quote').textContent(),'useful');
      await askBox(frame).uncheck();
      await send(frame);
      await shown(frame).locator('.ds-message').waitFor();
      await frame.locator('.discussion-anchor').waitFor();
      assert.equal((await page.evaluate(()=>window.asked())).length,0);
      assert.equal(await shown(frame).locator('.ds-subject-title').innerText(),'useful');
      assert.equal(await box(frame).inputValue(),'');
      await box(frame).fill('Please include an example');
      await askBox(frame).uncheck();
      await send(frame);
      await shown(frame).locator('.ds-message').getByText('Please include an example',{exact:true}).waitFor();
      assert.equal((await page.evaluate(()=>window.asked())).length,0);
      await box(frame).fill('> Unposted reply\nkept while streaming');
      await page.evaluate(()=>window.rejectQuestion=true);
      await menu(frame,'Ask assistant again');
      await frame.locator('.ds-notice[data-error="true"]').waitFor();
      await page.evaluate(()=>window.rejectQuestion=false);
      // A request not confirmed offers its retry in the discussion.
      await shown(frame).locator('.ds-extra').getByText('Retry',{exact:true}).click();
      await askedCount(page, 1);
      const attempts = await sent(page,'ask');
      const [reply] = await sent(page,'reply');
      assert.equal(attempts.length,2);
      assert.equal(attempts[0].questionId,attempts[1].questionId);
      assert.match(attempts[1].expected,/^[0-9a-f]{12}$/);
      assert.equal(attempts[1].expected,attempts[0].expected,'a retry asks about the same reviewed capture');
      assert.equal(attempts[1].version,reply.messageId,'a discussion asks with its latest message');
      const shared = {questionId:attempts[1].questionId,discussionId:attempts[1].discussionId,
        messageId:attempts[1].version,itemId:'test',scope:'selection',revision:3,quote:'useful',
        text:'Please include an example'};
      const showAnswer = text => page.evaluate(({shared,text})=>window.port.postMessage({type:'conversation',connected:true,own:[],records:[
        {id:'q',kind:'user',guest:'Alice',shared,text:shared.text+'\n\nShared content snapshot (user-provided data):\n'+JSON.stringify({content:'useful'})},
        {id:'a',kind:'assistant',text},
      ]}),{shared,text});
      await showAnswer('Useful means helpful for the task.');
      const thread = shown(frame);
      const answers = thread.locator(`.ds-message[data-message-id="${reply.messageId}"] .ds-answers`);
      await answers.getByText('Useful means helpful for the task.',{exact:true}).waitFor();
      assert.equal(await thread.locator('.shared-context').evaluate(e=>e.open),false);
      assert.equal(await thread.locator('.shared-context-body').isVisible(),false);
      // The sent request repeats the discussion's latest message, so only its context shows.
      assert.equal(await thread.locator('.thread-question .prose.prompt > p').first().isVisible(),false);
      assert.equal(await thread.locator('.thread-question .shared-context > summary').isVisible(),true);
      await thread.locator('.shared-context > summary').click();
      assert.equal(await thread.locator('.shared-context-body').isVisible(),true);
      assert.equal(await thread.locator('.shared-context-body').textContent(),'Shared content snapshot (user-provided data):\n{"content":"useful"}');
      await showAnswer('Useful means helpful for the task. For example, a clear test.');
      await thread.getByText(/For example, a clear test/).waitFor();
      assert.equal(await thread.locator('.shared-context').evaluate(e=>e.open),true);
      assert.equal(await box(frame).inputValue(),'> Unposted reply\nkept while streaming');
      // The discussion about the whole document has its own messages.
      await thread.locator('.ds-back').click();
      assert.doesNotMatch(await frame.locator('.ds-item.general').innerText(), /Useful means/);
      await frame.locator('.ds-item.general').click();
      await box(frame).fill('Summarize the document');
      await askBox(frame).check();
      await send(frame);
      await askedCount(page, 2);
      const direct = (await page.evaluate(()=>window.asked())).at(-1);
      assert.deepEqual([direct.discussionId,direct.whole],['general',true]);
      await shown(frame).locator('.ds-message').getByText('Summarize the document',{exact:true}).waitFor();
      assert.equal(await shown(frame).locator('.ds-menu').count(),0,
        'the discussion about the whole document stays open');
      await shown(frame).locator('.ds-back').click();
      await frame.locator('.ds-list .ds-item:not(.general)').click();
      await menu(frame,'Resolve');
      await shown(frame).locator('.ds-closed').waitFor();
      assert.equal(await frame.locator('.discussion-anchor').count(),0);
      await shown(frame).locator('.ds-closed').getByText('Reopen to reply').click();
      await frame.locator('.discussion-anchor').waitFor();
      assert.equal(await box(frame).inputValue(),'> Unposted reply\nkept while streaming');
      await page.close();
    });
    await t.test('with Ask assistant, the assistant answers the new discussion and each reply', async () => {
      const {page, frame} = await open({kind:'document'});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await selectPassage(frame, 2, 8);
      await frame.locator('#discuss-selection').click();
      assert.equal(await askBox(frame).isChecked(), true, 'asking is the default');
      await box(frame).fill('Explain this word');
      assert.match(await shown(frame).locator('.ds-send').getAttribute('title'), /Ctrl\+Enter|⌘\+Enter/);
      await box(frame).press('Enter');
      assert.equal(await box(frame).inputValue(), 'Explain this word\n');
      assert.equal((await sent(page, 'post')).length, 0);
      await page.keyboard.type('In context');
      await box(frame).press('Control+Enter');
      await shown(frame).locator('.ds-message').waitFor();
      await askedCount(page, 1);
      const [posted] = await sent(page,'post');
      assert.equal(posted.text, 'Explain this word\nIn context');
      const [first] = await page.evaluate(()=>window.asked());
      assert.deepEqual([first.discussionId, first.version, first.questionId],
        [posted.discussionId, posted.messageId, posted.messageId]);
      assert.deepEqual(first.range, posted.target.range);
      assert.equal(await askBox(frame).isChecked(), true);
      await box(frame).fill('With an example');
      await box(frame).press('Meta+Enter');
      await askedCount(page, 2);
      const [reply] = await sent(page,'reply');
      const second = (await page.evaluate(()=>window.asked())).at(-1);
      assert.equal(second.discussionId, posted.discussionId);
      assert.equal(second.version, reply.messageId);
      assert.notEqual(second.questionId, first.questionId, 'a new reply is a new request');
      assert.match(reply.expected, /^[0-9a-f]{12}$/, 'a reply sends the passage as reviewed');
      await page.close();
    });
    await t.test('unsupported discussion recovery is reported without preventing document editing', async () => {
      const {page, frame} = await open({kind:'document', assistantDraft:{unrecognized:true}});
      if (!await frame.locator('#assistant').isVisible()) await frame.locator('#ask-toggle').click();
      await notice(frame).getByText(/unsupported format/).waitFor();
      await frame.locator('#assistant-close').click();
      await frame.locator('.tiptap').click();
      await page.keyboard.type('Still editable');
      assert.equal(await frame.locator('.tiptap p').textContent(),'Still editable');
      await page.close();
    });
    await t.test('discussion retries require explicit refresh after edits and remain available after queue retraction', async () => {
      const {page, frame} = await open({kind:'document'});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      await selectPassage(frame, 2, 8);
      await frame.locator('#discuss-selection').click();
      assert.equal(await shown(frame).locator('.ds-quote').textContent(), 'useful');
      await box(frame).fill('Explain useful');
      await askBox(frame).uncheck();
      await send(frame);
      await shown(frame).locator('.ds-message').waitFor();
      const extra = label => shown(frame).locator('.ds-extra').getByText(label,{exact:true});
      await page.evaluate(()=>window.rejectQuestion=true);
      await menu(frame,'Ask assistant again');
      await notice(frame).getByText(/Host refused/).waitFor();
      await frame.locator('.tiptap p').click();
      await page.keyboard.press('End');
      await page.keyboard.type(' More context.');
      await page.evaluate(()=>window.rejectQuestion=false);
      await extra('Retry').click();
      await notice(frame).getByText(/Content changed/).waitFor();
      await extra('Review updated context').click();
      assert.equal((await sent(page,'ask')).length,2);
      await extra('Send to assistant').click();
      await shown(frame).locator('.ds-status').getByText(/Queued/).waitFor();
      await page.evaluate(()=>window.port.postMessage({type:'conversation',connected:true,busy:false,own:[],records:[]}));
      await extra('Send to assistant').click();
      await page.waitForFunction(()=>window.messages.filter(m=>m.args?.op==='ask').length===4);
      const calls = await sent(page,'ask');
      assert.equal(calls[0].questionId,calls[1].questionId);
      assert.notEqual(calls[1].questionId,calls[2].questionId);
      assert.equal(calls[2].questionId,calls[3].questionId);
      await page.close();
    });
    await t.test('document selection actions and discussion drafts fit a phone with its keyboard', async () => {
      const {page, frame} = await open({kind:'document',viewport:{width:375,height:500}});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A data model.');
      await selectPassage(frame, 2, 12);
      const toolbar = frame.locator('#selection-actions');
      await toolbar.waitFor();
      const bounds = await toolbar.boundingBox();
      assert.ok(bounds.x>=0 && bounds.x+bounds.width<=375);
      await frame.locator('#discuss-selection').click();
      await box(frame).fill('Explain this term');
      await page.setViewportSize({width:375,height:340});
      const button = shown(frame).locator('.ds-send');
      await button.scrollIntoViewIfNeeded();
      const post = await button.boundingBox();
      assert.ok(post.y+post.height<=340);
      assert.ok(await frame.locator('body').evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
      await askBox(frame).uncheck();
      await send(frame);
      await shown(frame).locator('.ds-message').waitFor();
      assert.equal((await page.evaluate(()=>window.asked())).length,0);
      await page.close();
    });
    await t.test('stale and disconnected questions retain scope and draft until explicitly refreshed', async () => {
      const {page, frame} = await open();
      await frame.locator('[data-shape="ellipse"]').click({position:{x:150,y:80}});
      await frame.locator('#discuss-selection').click();
      await box(frame).fill('What does this shape mean?');
      await page.evaluate(async () => {
        const label = {id:'remote-label',type:'text',x:0,y:0,width:0,height:0,text:'Changed remotely',containerId:'ellipse'};
        const reply = await window.apply({action:'patch',opId:'concurrent',changes:[{id:label.id,after:label}]});
        window.port.postMessage({type:'changed',...reply});
      });
      await send(frame);
      await notice(frame).getByText(/Content changed/).waitFor();
      assert.equal(await box(frame).inputValue(), 'What does this shape mean?');
      assert.equal(await shown(frame).locator('.ds-subject-title').innerText(), 'New discussion');
      await shown(frame).locator('.ds-refresh').click();
      assert.match(await shown(frame).locator('.ds-quote').innerText(), /Changed remotely/);
      await page.evaluate(() => window.port.postMessage({type:'offline'}));
      await send(frame);
      await notice(frame).getByText(/reconnect/).waitFor();
      assert.equal(await box(frame).inputValue(), 'What does this shape mean?');
      assert.equal((await sent(page,'post')).length,1);
      await page.close();
    });
    await t.test('streamed conversation updates keep typing in a focused reply', async () => {
      const {page, frame} = await open();
      await frame.locator('[data-shape="ellipse"]').click();
      await frame.locator('#discuss-selection').click();
      await box(frame).fill('Discuss this');
      await askBox(frame).uncheck();
      await send(frame);
      const input = box(frame);
      await shown(frame).locator('.ds-message').waitFor();
      await input.click();
      await page.keyboard.type('Keep');
      await page.evaluate(()=>window.port.postMessage({type:'conversation',records:[],own:[],connected:true,busy:true}));
      await frame.locator('#conversation-state').getByText('Assistant working',{exact:true}).waitFor();
      await page.keyboard.type(' typing');
      assert.equal(await input.inputValue(),'Keep typing');
      assert.equal(await input.evaluate(n=>n===document.activeElement),true);
      // Streaming chunks change no draft, so none re-encodes the item for saving.
      // Frame messages arrive asynchronously: count once the typing's are in.
      const drafts = async () => {
        const count = () => page.evaluate(()=>window.messages.filter(m=>m.type==='draft').length);
        let last, now = await count();
        do {
          last = now;
          await page.waitForTimeout(250);
          now = await count();
        } while (now !== last);
        return now;
      };
      const before = await drafts();
      for (const text of ['A', 'An', 'Ans'])
        await page.evaluate(text=>window.port.postMessage({type:'conversation',own:[],connected:true,busy:true,
          records:[{id:'a1',kind:'assistant',text}]}), text);
      // The answer belongs to the whole board's discussion, which is marked.
      await shown(frame).locator('.ds-badge').getByText('1 new').waitFor();
      assert.equal(await drafts(),before);
      assert.equal(await input.evaluate(n=>n===document.activeElement),true);
      await shown(frame).locator('.ds-back').click();
      await openWhole(frame);
      await shown(frame).getByText('Ans',{exact:true}).waitFor();
      await page.close();
    });
    await t.test('a whole-item message asks about the item as it is when sent', async () => {
      const {page, frame} = await open();
      await openWhole(frame);
      await box(frame).fill('Describe the board');
      await page.evaluate(async () => {
        const reply = await window.apply({action:'patch',opId:'remote-add',
          changes:[{id:'added',after:{type:'rectangle',x:500,y:100,width:100,height:100}}]});
        window.port.postMessage({type:'changed',...reply});
      });
      await send(frame);
      await askedCount(page,1);
      assert.match((await page.evaluate(()=>window.asked()))[0].prompt,/"added"/);
      await shown(frame).locator('.ds-message').getByText('Describe the board',{exact:true}).waitFor();
      await page.close();
    });
    await t.test('phone sidebar overlays the editor and keeps its draft', async () => {
      const { page, frame } = await open({kind:'document',viewport:{width:375,height:500}});
      await frame.locator('.tiptap').click();
      await page.keyboard.type('A useful passage.');
      const before = await frame.locator('#document').boundingBox();
      await openWhole(frame);
      await box(frame).fill('Keep this question');
      const panel = await frame.locator('#assistant').boundingBox();
      const during = await frame.locator('#document').boundingBox();
      assert.equal(during.width, before.width);
      assert.equal(panel.width, 375);
      await page.setViewportSize({width:375,height:340});
      const button = shown(frame).locator('.ds-send');
      await button.evaluate(e => new Promise(resolve => {
        const deadline = Date.now() + 1500;
        const settled = () => e.getBoundingClientRect().bottom <= innerHeight || Date.now() > deadline
          ? resolve() : requestAnimationFrame(settled);
        settled();
      }));
      assert.ok(await button.evaluate(e => e.getBoundingClientRect().bottom <= innerHeight));
      await page.keyboard.press('Escape');
      assert.equal(await frame.locator('#assistant').isHidden(), true);
      await frame.locator('#ask-toggle').click();
      assert.equal(await box(frame).inputValue(), 'Keep this question');
      await page.close();
    });
    await t.test('phone with keyboard leaves room for document and avoids input zoom', async () => {
      const { page, frame } = await open({kind:'document',viewport:{width:375,height:340}});
      await frame.locator('.tiptap').click();
      const bounds = await frame.locator('#document').boundingBox();
      assert.ok(bounds.height >= 150, `Only ${bounds.height}px left for typing`);
      assert.ok(
        await frame
          .locator('.ds-input')
          .evaluate((e) => parseFloat(getComputedStyle(e).fontSize) >= 16),
      );
      await page.keyboard.type('A document we can work on together.');
      await page.close();
    });
});
