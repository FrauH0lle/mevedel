import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile,mkdir} from 'node:fs/promises';
import {editorFixture} from './editor-fixture.mjs';

// Exercise the packaged room DOM/renderers; deterministic frames replace the transport only.
// The real encrypted relay and host are covered by room.browser.mjs.
test('room appearance, navigation and decisions preserve work', async t => {
  const {browser,url}=await editorFixture(t),context=await browser.newContext();
  await context.route('**/transport.js',async route => {
    const source=await readFile(new URL('../../relay/viewer/transport.js',import.meta.url),'utf8');
    await route.fulfill({contentType:'text/javascript',body:source+`
      window.mevedelViewerTransport={...window.mevedelViewerTransport,create(options){
        window.receiveTest=options.onFrame;window.connectionTest=options.onConnection;window.giveUpTest=options.onGiveUp;window.sentTest=[];
        return {connect(){
          if(window.closedRoomTest){options.onGiveUp();return;}
          options.onConnection('Connected','connected');
          options.onFrame({t:'welcome',readOnly:false,commands:[{kind:'skill',name:'review'},{kind:'skill',name:'research'},{kind:'command',name:'plan'}]});
          options.onFrame({t:'snapshot-chunk',final:true,records:[
            {id:'person',kind:'user',guest:'Roland',text:'Keep the room, documents and whiteboard consistent.'},
            {id:'reply',kind:'assistant',text:'## A clearer place to work together\\n\\nThe room holds the conversation. Shared documents and whiteboards keep focused work close by, with their own discussions.\\n\\nChoose a skill before sending your next message.'},
            {id:'artifact',kind:'tool',artifact:'Workspace guide.html',size:2400}
          ]});
          options.onFrame({t:'status',model:'Test model',mode:'ask',busy:false});
        },end(){},async send(frame){
          window.sentTest.push(frame);
          if(frame.t==='history-get'){
            queueMicrotask(()=>options.onFrame(window.historyFailureTest
              ? {t:'history',reqId:frame.reqId,segment:frame.segment,error:'Archive temporarily unavailable.'}
              : {t:'history',reqId:frame.reqId,segment:frame.segment,final:true,records:[
                {id:'history-1-person',kind:'user',text:'Earlier design discussion'},
                {id:'history-1-reply',kind:'assistant',text:'The archived answer remains readable.'},
                {id:'history-1-tool',kind:'tool',name:'Read',summary:'notes.md',status:'done',result:'Archived tool output'},
                {id:'history-1-tool-2',kind:'tool',name:'Glob',summary:'*.md',status:'done',result:'notes.md'},
                {id:'history-1-followup',kind:'user',text:'A follow-up question'},
                {id:'history-1-answer',kind:'assistant',text:'A new assistant turn'}
              ]}));
          }
          if(frame.t==='editing'){
            const args=JSON.parse(atob(frame.data));
            const result=args.action==='list'?[{id:'doc',kind:'document',title:'Editing principles'},{id:'board',kind:'whiteboard',title:'The save boundary'}]:{available:true};
            const data=btoa(JSON.stringify({result}));
            queueMicrotask(()=>options.onFrame({t:'editing',reqId:frame.reqId,offset:0,total:data.length,data}));
          }
          return true;
        }};
      }};`});
  });
  const page=await context.newPage();page.setDefaultTimeout(7000);
  const errors=[];page.on('pageerror',error=>errors.push(error.message));
  await page.goto(`${url}/room#designroom000001.${Buffer.alloc(64).toString('base64url')}`);
  await page.locator('#editing-status').getByText('Shared editing is ready.',{exact:true}).waitFor();
  const generatedName = await page.locator('#composer-name').inputValue();
  assert.match(generatedName,/^[A-Z][a-z]+ [A-Z][a-z]+$/);
  assert.equal(generatedName[0],generatedName.split(' ')[1][0]);
  await page.reload();
  await page.locator('#editing-status').getByText('Shared editing is ready.',{exact:true}).waitFor();
  assert.equal(await page.locator('#composer-name').inputValue(),generatedName);
  await page.evaluate(()=>localStorage.setItem('mevedel-guest-name','Roland'));
  await page.reload();
  await page.locator('#editing-status').getByText('Shared editing is ready.',{exact:true}).waitFor();
  assert.equal(await page.locator('#composer-name').inputValue(),'Roland','chosen names are preserved');
  await page.locator('#composer-input').fill('Keep this unsent draft');
  const nameInput=page.locator('#composer-name');
  await nameInput.fill('  Roland55  ');
  await page.locator('#composer-input').focus();
  assert.equal(await nameInput.inputValue(),'Roland55');
  assert.deepEqual(await page.evaluate(()=>window.sentTest.at(-1)),{t:'set-name',name:'Roland55'});
  assert.match(await page.locator('#modeline').innerText(),/Roland55/);
  await nameInput.fill('New Name');
  await nameInput.press('Enter');
  assert.deepEqual(await page.evaluate(()=>window.sentTest.at(-1)),{t:'set-name',name:'New Name'});
  assert.equal(await page.locator('#composer-input').inputValue(),'Keep this unsent draft');
  await nameInput.fill('   ');
  await page.locator('#composer-input').focus();
  assert.equal(await nameInput.inputValue(),generatedName);
  assert.deepEqual(await page.evaluate(()=>window.sentTest.at(-1)),{t:'set-name',name:generatedName});
  assert.equal(await page.evaluate(()=>localStorage.getItem('mevedel-guest-name')),generatedName);
  await page.reload();
  await page.locator('#editing-status').getByText('Shared editing is ready.',{exact:true}).waitFor();
  assert.equal(await nameInput.inputValue(),generatedName);
  await page.locator('#composer-input').fill('Keep this unsent draft');
  await page.locator('#skills-button').click();await page.locator('#skill-search').fill('review');
  assert.equal(await page.locator('.skill-chip:visible').count(),1);
  await page.locator('.skill-chip:visible').click();
  assert.equal(await page.locator('.skill-chip:visible').getAttribute('aria-pressed'),'true');
  assert.equal(await page.locator('#composer-input').inputValue(),'Keep this unsent draft');
  await page.locator('#commands-box > summary').click();
  const request={t:'ui-request',reqId:7,body:'Apply the proposed room layout?\nReview the changes before continuing.',bodyKind:'text',options:[{id:'apply',label:'Apply changes'},{id:'review',label:'Keep reviewing'}],allowFeedback:true};
  await page.evaluate(frame=>window.receiveTest(frame),request);
  await page.locator('.decision-feedback > summary').click();await page.getByLabel('Feedback',{exact:true}).fill('Keep the wider sidebar.');
  await page.evaluate(frame=>window.receiveTest(frame),request);
  assert.equal(await page.getByLabel('Feedback',{exact:true}).inputValue(),'Keep the wider sidebar.');
  await page.getByRole('button',{name:'Send feedback',exact:true}).click();
  assert.deepEqual(await page.evaluate(()=>window.sentTest.at(-1)),{t:'ui-response',reqId:7,feedback:'Keep the wider sidebar.'});
  await page.locator('.decision-feedback > summary').click();
  const directory=new URL('../../.scratch/design-integration/screenshots/',import.meta.url).pathname;
  if(process.env.MEVEDEL_DESIGN_SCREENSHOTS) await mkdir(directory,{recursive:true});
  await page.evaluate(()=>window.receiveTest({t:'status',busy:true,model:'Test model',mode:'ask'}));
  for(const width of [1600,390]) for(const palette of ['cool','warm']) for(const theme of ['light','dark']) {
    await page.setViewportSize({width,height:1000});
    await page.locator('#appearance-menu > summary').click();
    await page.locator('#palette').selectOption(palette);await page.locator('#appearance').selectOption(theme);
    const menu=await page.locator('.appearance-body').boundingBox();
    assert.ok(menu.x>=0&&menu.x+menu.width<=width,JSON.stringify({width,palette,theme,menu}));
    assert.equal(await page.locator('#palette').evaluate(node=>{const b=node.getBoundingClientRect();return document.elementFromPoint(b.x+b.width/2,b.y+b.height/2)===node;}),true,'appearance sits above sidebar and dock');
    for(const accents of ['minimal','selective']) {
      await page.locator('#accents').selectOption(accents);
      const color=await page.locator('#session-box').evaluate(node=>getComputedStyle(node).scrollbarColor);
      assert.notEqual(color,'auto');
    }
    assert.equal(await page.locator('#composer-input').inputValue(),'Keep this unsent draft');
    if(process.env.MEVEDEL_DESIGN_SCREENSHOTS) await page.screenshot({path:`${directory}/room-${palette}-${theme}-${width}.png`});
    await page.keyboard.press('Escape');
    assert.equal(await page.locator('#appearance-menu').getAttribute('open'),null);
    assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
    const send=await page.locator('#send-button').boundingBox();
    assert.ok(send.y+send.height<=1000,'composer stays in reach');
    const working=await page.locator('#assistant-working').boundingBox();
    const composer=await page.locator('#composer').boundingBox();
    assert.ok(working.height>=24&&working.y+working.height<=composer.y,'working status is readable above the composer');
  }
  await page.evaluate(()=>window.receiveTest({t:'status',busy:false}));
  assert.equal(await page.locator('#assistant-working').isVisible(),false);
  await page.evaluate(()=>window.receiveTest({t:'status',busy:true}));
  assert.equal(await page.locator('#assistant-working').isVisible(),true);
  await page.evaluate(()=>window.connectionTest('Reconnecting','disconnected'));
  assert.equal(await page.locator('#assistant-working').isVisible(),false);
  await page.evaluate(()=>{window.connectionTest('Connected','connected');window.receiveTest({t:'status',busy:false});});
  const other=await context.newPage();await other.goto(`${url}/room`);
  await page.locator('#appearance-menu > summary').click();await page.locator('#palette').selectOption('cool');
  await other.waitForFunction(()=>document.documentElement.dataset.palette==='cool');
  assert.equal(await page.locator('#composer-input').inputValue(),'Keep this unsent draft');
  await page.locator('#appearance-menu > summary').click();
  await page.getByRole('button',{name:'Keep reviewing',exact:true}).click();
  assert.deepEqual(await page.evaluate(()=>window.sentTest.at(-1)),{t:'ui-response',reqId:7,option:'review'});

  await page.setViewportSize({width:1600,height:1000});
  await page.evaluate(()=>{
    window.receiveTest({t:'history-index',currentSegment:2,records:[{id:'history-1-artifact',artifact:'Workspace guide.html',size:2400}],final:true});
    window.receiveTest({t:'remove',ids:['person','reply','artifact']});
    window.receiveTest({t:'record',record:{id:'after-compaction',kind:'assistant',text:'Continuing with compacted context.'}});
  });
  assert.equal(await page.locator('#artifacts .dock-chip').count(),1,'archived artifact remains in sidebar');
  assert.equal(await page.evaluate(()=>window.sentTest.filter(f=>f.t==='history-get').length),0,'archive is loaded on demand');
  await page.evaluate(()=>{window.historyFailureTest=true;});
  await page.getByText('Earlier conversation · Segment 1',{exact:true}).click();
  await page.locator('#history').getByText('Archive temporarily unavailable.').waitFor();
  await page.evaluate(()=>{window.historyFailureTest=false;});
  await page.locator('#history').getByRole('button',{name:'Retry',exact:true}).click();
  await page.getByText('The archived answer remains readable.').waitFor();
  for (const id of ['history-1-tool','history-1-tool-2']) {
    const tool=page.locator(`#history [data-record-id="${id}"]`);
    assert.equal(await tool.locator('.who').isVisible(),false,'archived tools continue the assistant turn without repeated labels');
    assert.equal(await tool.locator('.glyph').isVisible(),false,'archived tools do not repeat the assistant avatar');
  }
  assert.equal(await page.locator('#history [data-record-id="history-1-answer"] .who').isVisible(),true,'a user message starts a new assistant group');
  await page.locator('#history [data-record-id="history-1-tool"] summary').click();
  assert.equal(await page.getByText('Archived tool output',{exact:true}).isVisible(),true,'grouped tool output still expands');
  assert.equal(await page.locator('#composer-input').inputValue(),'Keep this unsent draft');
  const collapse=page.getByRole('button',{name:'Collapse segment 1',exact:true});
  const rail=await collapse.boundingBox(),segment=await page.locator('.history-segment').boundingBox();
  assert.ok(rail.height>=segment.height-4,'collapse rail spans the full segment');
  await collapse.click({position:{x:10,y:200}});
  assert.equal(await page.locator('.history-segment').getAttribute('open'),null);
  assert.equal(await page.getByText('Earlier conversation · Segment 1',{exact:true}).evaluate(el=>el===document.activeElement),true);
  await page.getByText('Earlier conversation · Segment 1',{exact:true}).click();
  assert.equal(await page.evaluate(()=>window.sentTest.filter(f=>f.t==='history-get').length),2,'reopening a loaded segment preserves its content');
  for(const width of [1600,390]) {
    await page.setViewportSize({width,height:1000});
    if(process.env.MEVEDEL_DESIGN_SCREENSHOTS) await page.screenshot({path:`${directory}/room-history-${width}.png`});
    assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
  }
  const conversation=await page.locator('#transcript').innerText();
  await page.evaluate(()=>window.giveUpTest());
  await page.getByRole('heading',{name:'Room closed',exact:true}).waitFor();
  assert.equal(await page.locator('#notice').isVisible(),false);
  assert.equal(await page.locator('#composer').isVisible(),false);
  assert.equal(await page.locator('#transcript').innerText(),conversation,'ending the room preserves the transcript');
  assert.equal(await page.evaluate(()=>scrollY),0,'the terminal notice is brought into view');

  // An expired invitation has no transcript: its explanation fills the main area.
  const closed=await context.newPage();
  closed.on('pageerror',error=>errors.push(error.message));
  await closed.addInitScript(()=>{window.closedRoomTest=true;});
  await closed.goto(`${url}/room#closedroom000001.${Buffer.alloc(64).toString('base64url')}`);
  await closed.getByRole('heading',{name:'Room closed',exact:true}).waitFor();
  assert.match(await closed.locator('#terminal-message').innerText(),/new invitation/);
  for(const width of [1600,390]) for(const palette of ['cool','warm']) for(const theme of ['light','dark']) {
    await closed.setViewportSize({width,height:900});
    await closed.locator('#appearance-menu > summary').click();
    await closed.locator('#palette').selectOption(palette);
    await closed.locator('#appearance').selectOption(theme);
    await closed.keyboard.press('Escape');
    const panel=await closed.locator('#terminal-state').boundingBox();
    assert.ok(panel.x>=0&&panel.x+panel.width<=width,'closed-room panel fits the viewport');
    assert.ok(Math.abs(panel.y+panel.height/2-450)<80,'closed-room notice sits near the center');
    assert.ok(await closed.locator('#terminal-title').evaluate(node=>parseFloat(getComputedStyle(node).fontSize)>=24),'terminal heading is prominent');
    assert.ok(await closed.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
    if(process.env.MEVEDEL_DESIGN_SCREENSHOTS) await closed.screenshot({path:`${directory}/room-closed-${palette}-${theme}-${width}.png`});
  }
  assert.deepEqual(errors,[]);
  await context.close();
});
