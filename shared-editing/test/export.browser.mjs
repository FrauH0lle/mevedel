import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {handle} from '../host.mjs';
import {editorFixture} from './editor-fixture.mjs';

test('board exports are standalone SVG with positioned lines and opaque white PNG', async t => {
  const {browser} = await editorFixture(t);
  const text = 'R + Plumber API\n\nValidate inputs & authorization\n<Versioned JSON>';
  const content = [
    {id:'label',type:'rectangle',x:0,y:0,width:460,height:180,backgroundColor:'#edf4ef',strokeColor:'#176855'},
    {id:'label-text',type:'text',x:0,y:0,width:0,height:0,fontSize:24,strokeColor:'#176855',text,containerId:'label'},
    {id:'arrow',type:'arrow',x:460,y:90,width:80,height:0,points:[[0,0],[80,0]],strokeColor:'#176855'},
    {id:'note',type:'text',x:540,y:0,width:260,height:180,fontSize:20,text:'Science in R\nSafe results'},
  ];
  const {state} = await handle({action:'create',id:'export-test',kind:'whiteboard',
    actor:'Agent: /root',opId:'create',content});
  const {result:svg} = await handle({action:'export',format:'svg',state});
  const {result:png} = await handle({action:'export',format:'png',state});
  const page = await browser.newPage({viewport:{width:850,height:350}});
  const parsed = await page.evaluate(source => {
    const doc = new DOMParser().parseFromString(source,'image/svg+xml');
    return {errors:doc.querySelectorAll('parsererror').length,namespace:doc.documentElement.namespaceURI,
      lines:[...doc.querySelectorAll('[data-shape="label-text"] text')].map(n=>({text:n.textContent,x:+n.getAttribute('x'),y:+n.getAttribute('y')})),
      fonts:[...doc.querySelectorAll('style')].map(n=>n.textContent.match(/font-family:"([^"]+)"/g)).flat(),
      external:[...doc.querySelectorAll('[href]')].filter(n=>!n.getAttribute('href').startsWith('data:')).length};
  },svg.text);
  assert.equal(parsed.errors,0);
  assert.equal(parsed.namespace,'http://www.w3.org/2000/svg');
  assert.equal(parsed.external,0);
  assert.deepEqual(parsed.fonts,['font-family:"Excalifont"'],'the fonts in use are embedded');
  assert.deepEqual(parsed.lines.map(n=>n.text),text.split('\n'));
  assert.equal(new Set(parsed.lines.map(n=>n.x)).size,1,'centred lines share one anchor');
  assert.deepEqual(parsed.lines.slice(1).map((n,i)=>n.y-parsed.lines[i].y),[30,30,30],'blank lines keep their 1.25 em');
  assert.doesNotMatch(svg.text,/tspan|context-stroke|data-agent|agent-contribution|class="hit"/);
  await page.setContent(svg.text);
  const starts = await page.locator('[data-shape="label-text"] text').evaluateAll(nodes=>
    nodes.filter(n=>n.textContent).map(n=>n.getStartPositionOfChar(0).y));
  const placed = parsed.lines.filter(n=>n.text).map(n=>n.y);
  starts.forEach((y,i)=>assert.ok(Math.abs(y-placed[i])<0.01,'browser respects line and blank-line positions'));
  const corner = await page.evaluate(async data => {
    const img = new Image(); img.src = `data:image/png;base64,${data}`; await img.decode();
    const canvas = document.createElement('canvas'); canvas.width=img.width;canvas.height=img.height;
    const ctx = canvas.getContext('2d');ctx.drawImage(img,0,0);
    return [...ctx.getImageData(0,0,1,1).data];
  },png.data);
  assert.deepEqual(corner,[255,255,255,255]);
  if (process.env.MEVEDEL_EXPORT_SCREENSHOTS) {
    const dir = new URL('../../.scratch/editor-polish/',import.meta.url);
    await mkdir(dir,{recursive:true});
    await writeFile(new URL('export.svg',dir),svg.text);
    await writeFile(new URL('export.png',dir),Buffer.from(png.data,'base64'));
    await page.screenshot({path:new URL('export-browser.png',dir).pathname});
  }
  await page.close();
});
