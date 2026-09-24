import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {editorFixture} from './editor-fixture.mjs';
import {handle} from '../host.mjs';

async function room(t, failRead = false) {
  const {browser,url}=await editorFixture(t), context=await browser.newContext();
  const items=await Promise.all(['whiteboard','document'].map(async kind => {
    const {state}=await handle({action:'create',id:kind,kind,title:`Test ${kind}`,opId:kind,actor:'Alice'});
    return (await handle({action:'read',state,sync:true,actor:'Alice'})).result;
  }));
  await context.route('**/transport.js',async route => {
    const source=await readFile(new URL('../../relay/viewer/transport.js',import.meta.url),'utf8');
    await route.fulfill({contentType:'text/javascript',body:source+`
      window.mevedelViewerTransport={...window.mevedelViewerTransport,create(options){
        const items=${JSON.stringify(items)};
        let failRead=${failRead};
        return {connect(){
          options.onConnection('Connected','connected');
          options.onFrame({t:'welcome',readOnly:false,commands:[]});
          options.onFrame({t:'snapshot-chunk',final:true,records:[{id:'brief',kind:'tool',artifact:'brief.html',size:400}]});
        },end(){},async send(frame){
          if(frame.t==='editing'){
            const args=JSON.parse(atob(frame.data));
            const result=args.action==='list'?items:args.action==='status'?{available:true}:items.find(item=>item.id===args.id);
            const reply=failRead&&args.action==='read'?{error:'Temporary read failure'}:{result};
            if(args.action==='read') failRead=false;
            const data=btoa(unescape(encodeURIComponent(JSON.stringify(reply))));
            const deliver=()=>options.onFrame({t:'editing',reqId:frame.reqId,offset:0,total:data.length,data});
            if(args.action==='read'&&window.holdReadTest) window.releaseReadTest=deliver;
            else queueMicrotask(deliver);
          }
          if(frame.t==='artifact-get'){
            const html='<a href="#decision">Decision</a><a href="#caf%C3%A9">Encoded heading</a><div style="height:1800px"></div><h2 id="decision" tabindex="-1">The decision</h2><div style="height:900px"></div><h2 id="café">Café</h2><div style="height:900px"></div>';
            queueMicrotask(()=>options.onFrame({t:'artifact',reqId:frame.reqId,mime:'text/html',size:html.length,data:btoa(unescape(encodeURIComponent(html))),final:true}));
          }
          return true;
        }};
      }};`});
  });
  const page=await context.newPage();page.setDefaultTimeout(5000);
  await page.goto(`${url}/room#designroom000001.${Buffer.alloc(64).toString('base64url')}`);
  await page.getByText('Shared editing is ready.',{exact:true}).waitFor();
  return {page,context};
}

test('Shared work opens each requested editor in its own tab',async t => {
  const {page}=await room(t);
  for(const kind of ['whiteboard','document']) {
    const opened=page.waitForEvent('popup');
    await page.locator(`#editing-items [data-item-id="${kind}"]`).click();
    const tab=await opened;
    await tab.waitForURL(url=>url.searchParams.get('shared')===kind);
    await tab.frameLocator('#editing-body iframe').locator(kind==='whiteboard'?'#canvas':'.tiptap').waitFor();
    assert.equal(await tab.locator('#editing-panel').isVisible(),true);
    await tab.close();
  }
});

test('a failed editor read stays on the requested item and can be retried',async t => {
  const {page}=await room(t,true);
  const opened=page.waitForEvent('popup');
  await page.locator('#editing-items [data-item-id="document"]').click();
  const tab=await opened;
  tab.setDefaultTimeout(5000);
  await tab.locator('#editing-body').getByText('Temporary read failure',{exact:true}).waitFor();
  assert.equal(await tab.locator('#editing-panel').isVisible(),true);
  await tab.locator('#editing-body').getByRole('button',{name:'Retry',exact:true}).click();
  await tab.frameLocator('#editing-body iframe').locator('.tiptap').waitFor();
  assert.equal(new URL(tab.url()).searchParams.get('shared'),'document');
  await tab.close();
  const openingAgain=page.waitForEvent('popup');
  await page.locator('#editing-items [data-item-id="whiteboard"]').click();
  const other=await openingAgain;
  other.setDefaultTimeout(5000);
  await other.locator('#editing-body').getByText('Temporary read failure',{exact:true}).waitFor();
  await other.evaluate(()=>{window.holdReadTest=true;});
  await other.locator('#editing-body').getByRole('button',{name:'Retry',exact:true}).click();
  await other.waitForFunction(()=>window.releaseReadTest);
  await other.getByRole('button',{name:'Back to room',exact:true}).click();
  await other.evaluate(async()=>{window.releaseReadTest();await new Promise(resolve=>setTimeout(resolve,50));});
  assert.equal(await other.locator('#editing-panel').isVisible(),false,'a delayed read does not reopen an editor after Back to room');
  assert.equal(new URL(other.url()).searchParams.has('shared'),false);
});

test('artifact fragment links stay within the artifact, in the panel and a new tab',async t => {
  const {page}=await room(t);
  await page.locator('#artifacts .dock-chip').click();
  const check=async frame => {
    await frame.getByRole('link',{name:'Decision',exact:true}).click();
    await frame.getByRole('heading',{name:'The decision',exact:true}).waitFor();
    assert.ok(await frame.locator('#decision').evaluate(el=>Math.abs(el.getBoundingClientRect().top)<100));
    await frame.getByRole('link',{name:'Encoded heading',exact:true}).click();
    assert.ok(await frame.locator('#café').evaluate(el=>Math.abs(el.getBoundingClientRect().top)<100));
  };
  await check(page.frameLocator('#artifact-body iframe'));
  const opened=page.waitForEvent('popup');
  await page.locator('#artifact-tab').click();
  const tab=await opened;
  await check(tab.frameLocator('iframe'));
  assert.equal(new URL(page.url()).hash,'');
});
