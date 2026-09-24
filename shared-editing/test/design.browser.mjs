import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {editorFixture} from './editor-fixture.mjs';

const paragraph = text => ({type:'paragraph',content:[{type:'text',text}]});
const content = {type:'doc',content:[
  {type:'heading',attrs:{level:1},content:[{type:'text',text:'A shared place to work'}]},
  paragraph('Keep the conversation close to the thing it describes.'),
  {type:'bulletList',content:['First point','Second point'].map(text => ({type:'listItem',content:[paragraph(text)]}))},
  paragraph('Leave room for the work.'),
]};
async function menu(frame, group, action, scope = '#formatting') {
  await frame.locator(`${scope} summary`).getByText(group,{exact:true}).click();
  await frame.getByRole('button',{name:action,exact:true}).click();
}
async function select(frame, selector = '.tiptap > p') {
  return frame.locator(selector).first().evaluate(node => {
    const editor = node.closest('.tiptap').editor;
    const from = editor.view.posAtDOM(node,0);
    editor.commands.setTextSelection({from,to:from+node.textContent.length});
    editor.commands.focus();
    return node.textContent;
  });
}
async function end(frame) { await frame.locator('.tiptap').evaluate(node => node.editor.commands.focus('end')); }
async function cell(frame,row=1,column=1) {
  await frame.locator('.tiptap tr').nth(row).locator('th,td').nth(column).click();
  await frame.locator('#table-tools').waitFor();
}
async function dimensions(frame,rows,columns) {
  assert.deepEqual(await frame.locator('.tiptap tr').evaluateAll(nodes=>nodes.map(n=>n.children.length)),Array(rows).fill(columns));
}

test('approved editor controls on the production bundle', async t => {
  const {open} = await editorFixture(t);
  await t.test('document menus edit formatting, lists and links through the current selection', async () => {
    const {page,frame} = await open({kind:'document',content});
    const text = await select(frame);
    await menu(frame,'Text','Inline code');
    assert.equal(await frame.locator('.tiptap p code').innerText(),text);
    await menu(frame,'Text','Clear formatting');
    await frame.getByRole('button',{name:'Bold',exact:true}).click();
    await menu(frame,'Text','Strikethrough');
    assert.equal(await frame.locator('.tiptap strong s').innerText(),text);
    await menu(frame,'Text','Clear formatting');
    for (const level of [4,5,6]) {
      await frame.getByLabel('Paragraph style').selectOption(String(level));
      assert.equal(await frame.locator(`.tiptap h${level}`).innerText(),text);
    }
    await frame.getByLabel('Paragraph style').selectOption('0');
    for (const [label,tag] of [['Block quote','blockquote'],['Code block','pre']]) {
      await menu(frame,'Text',label);
      assert.equal(await frame.locator(`.tiptap ${tag}`).innerText(),text);
      await menu(frame,'Text','Clear formatting');
    }
    await select(frame,'.tiptap li:nth-child(2) > p');
    await menu(frame,'Lists','Indent list item');
    assert.equal(await frame.locator('.tiptap li li').innerText(),'Second point');
    await menu(frame,'Lists','Outdent list item');
    await menu(frame,'Lists','Numbered list');
    assert.equal(await frame.locator('.tiptap ol > li').count(),2);
    await menu(frame,'Lists','Bullet list');
    await select(frame);
    await menu(frame,'Insert','Link…');
    await frame.locator('#link-url').fill('javascript:alert(1)');
    await frame.getByRole('button',{name:'Save link',exact:true}).click();
    assert.equal(await frame.locator('.tiptap a').count(),0);
    await frame.locator('#link-url').fill('https://example.com/guide');
    await frame.getByRole('button',{name:'Save link',exact:true}).click();
    assert.equal(await frame.locator('.tiptap a').getAttribute('href'),'https://example.com/guide');
    await frame.locator('.tiptap a').click();
    await frame.getByRole('button',{name:'Edit link',exact:true}).click();
    await frame.locator('#link-url').fill('https://example.com/revised');
    await frame.getByRole('button',{name:'Save link',exact:true}).click();
    assert.equal(await frame.locator('.tiptap a').innerText(),text);
    await frame.locator('#link-tools').getByRole('button',{name:'Remove link',exact:true}).click();
    assert.equal(await frame.locator('.tiptap a').count(),0);
    await end(frame);
    await menu(frame,'Insert','Horizontal divider');
    await menu(frame,'Insert','Line break');
    assert.equal(await frame.locator('.tiptap hr').count(),1);
    assert.ok(await frame.locator('.tiptap > p br').count());
    await page.close();
  });
  await t.test('tables retain content across rows, columns, headers, merging, resizing and undo', async () => {
    const {page,frame} = await open({kind:'document',content});
    await end(frame);
    await menu(frame,'Insert','Insert table');
    await dimensions(frame,3,3);
    assert.equal(await frame.locator('[aria-label="Insert table"]').isDisabled(),true);
    await cell(frame); await page.keyboard.type('42');
    await menu(frame,'Rows','Insert row above','#table-tools');
    await dimensions(frame,4,3);
    await menu(frame,'Rows','Insert row below','#table-tools');
    await dimensions(frame,5,3);
    await cell(frame); await menu(frame,'Rows','Delete row','#table-tools');
    await dimensions(frame,4,3);
    await cell(frame); await menu(frame,'Columns','Insert column left','#table-tools');
    await menu(frame,'Columns','Insert column right','#table-tools');
    await dimensions(frame,4,5);
    await cell(frame); await menu(frame,'Columns','Delete column','#table-tools');
    await dimensions(frame,4,4);
    assert.match(await frame.locator('.tiptap table').innerText(),/42/);
    await menu(frame,'Rows','Toggle header row','#table-tools');
    assert.equal(await frame.locator('.tiptap tr').first().locator('th').count(),0);
    await menu(frame,'Rows','Toggle header row','#table-tools');
    await menu(frame,'Columns','Toggle header column','#table-tools');
    assert.equal(await frame.locator('.tiptap tr > th:first-child').count(),4);
    await menu(frame,'Columns','Toggle header column','#table-tools');
    await cell(frame); await menu(frame,'Cells','Toggle header cell','#table-tools');
    assert.equal(await frame.locator('.tiptap tr').nth(1).locator('th').count(),1);
    await menu(frame,'Cells','Toggle header cell','#table-tools');
    await cell(frame,2,0);
    const boxes=await frame.locator('.tiptap tr').nth(2).locator('td').evaluateAll(nodes=>nodes.slice(0,2).map(n=>{const b=n.getBoundingClientRect();return {x:b.x,y:b.y,width:b.width};}));
    await page.mouse.move(boxes[0].x+10,boxes[0].y+10);await page.mouse.down();
    await page.mouse.move(boxes[1].x+boxes[1].width/2,boxes[1].y+10,{steps:8});await page.mouse.up();
    await frame.locator('.selectedCell').first().waitFor();
    await menu(frame,'Cells','Merge cells','#table-tools');
    assert.equal(await frame.locator('.tiptap td[colspan="2"]').count(),1);
    await menu(frame,'Cells','Split cell','#table-tools');
    await dimensions(frame,4,4);
    const first=frame.locator('.tiptap th').first();await first.scrollIntoViewIfNeeded();
    const b=await first.boundingBox();
    await page.mouse.move(b.x+b.width-1,b.y+b.height/2);await frame.locator('.column-resize-handle').first().waitFor();
    await page.mouse.down();await page.mouse.move(b.x+b.width+50,b.y+b.height/2,{steps:8});await page.mouse.up();
    assert.ok(Number(await first.getAttribute('colwidth'))>b.width);
    await frame.locator('#undo').click();assert.equal(await first.getAttribute('colwidth'),null);
    await frame.locator('#redo').click();assert.ok(Number(await first.getAttribute('colwidth'))>b.width);
    await cell(frame);await frame.getByRole('button',{name:'Delete table',exact:true}).click();
    assert.equal(await frame.locator('.tiptap table').count(),0);
    await frame.locator('#undo').click();await dimensions(frame,4,4);
    await frame.locator('#saved').getByText('Saved on host',{exact:true}).waitFor();
    const persisted=await page.evaluate(()=>window.apply({action:'read'}));
    assert.match(JSON.stringify(persisted.content),/42/);
    await page.close();
  });
  await t.test('image properties and drag handles repaint through undo and preserve proportions', async () => {
    const {page,frame}=await open({kind:'document',content});await end(frame);
    const png=await page.evaluate(()=>{const c=document.createElement('canvas');c.width=320;c.height=160;c.getContext('2d').fillRect(0,0,320,160);return c.toDataURL().split(',')[1];});
    await frame.locator('#image-upload').setInputFiles({name:'diagram.png',mimeType:'image/png',buffer:Buffer.from(png,'base64')});
    const img=frame.locator('.tiptap img');await img.click();
    await frame.getByRole('button',{name:'Image properties',exact:true}).click();
    await frame.locator('#image-alt').fill('Workspace diagram');await frame.locator('#image-width').fill('240');
    assert.equal(await frame.locator('#image-height').inputValue(),'120');
    await frame.getByRole('button',{name:'Save image',exact:true}).click();
    assert.equal(await img.getAttribute('alt'),'Workspace diagram');
    assert.equal(Math.round((await img.boundingBox()).width),240);
    await frame.locator('#undo').click();assert.equal(Math.round((await img.boundingBox()).width),320);
    await frame.locator('#redo').click();assert.equal(Math.round((await img.boundingBox()).width),240);
    const b=await frame.locator('[data-resize-handle="bottom-right"]').boundingBox();
    await page.mouse.move(b.x+b.width/2,b.y+b.height/2);await page.mouse.down();
    await page.mouse.move(b.x+b.width/2+60,b.y+b.height/2+30,{steps:8});await page.mouse.up();
    assert.ok((await img.boundingBox()).width>280);
    await frame.locator('#undo').click();assert.equal(Math.round((await img.boundingBox()).width),240);
    await page.close();
  });
  await t.test('discussion keeps its attached passage when focus or scope changes, and resizes with pointer or keyboard', async () => {
    const {page,frame}=await open({kind:'document',content,viewport:{width:1600,height:1000}});
    const text=await select(frame);await frame.locator('#selection-question').click();
    await frame.locator('#question').fill('Keep my question');
    assert.equal(await frame.locator('.discussion-anchor').innerText(),text);
    await frame.locator('#whole-question').click();assert.equal(await frame.locator('.discussion-anchor').count(),0);
    await frame.locator('#selected-question').click();
    assert.equal(await frame.locator('.discussion-anchor').innerText(),text);
    assert.equal(await frame.locator('#question').inputValue(),'Keep my question');
    const handle=frame.locator('#discussion-resize'),b=await handle.boundingBox();
    await page.mouse.move(b.x+3,b.y+80);await page.mouse.down();await page.mouse.move(b.x-140,b.y+80,{steps:8});await page.mouse.up();
    assert.ok((await frame.locator('#assistant').boundingBox()).width>=495);
    await handle.focus();await page.keyboard.press('End');assert.equal((await frame.locator('#assistant').boundingBox()).width,800);
    await page.keyboard.press('Home');assert.equal((await frame.locator('#assistant').boundingBox()).width,360);
    await frame.locator('#assistant-close').click();assert.equal(await frame.locator('.discussion-anchor').count(),0);
    await frame.locator('#ask-toggle').click();assert.equal(await frame.locator('.discussion-anchor').innerText(),text);
    const draft=await page.evaluate(()=>window.messages.filter(m=>m.type==='draft').at(-1).draft.assistant);
    const restored=await open({kind:'document',content,assistantDraft:draft,viewport:{width:1600,height:1000}});
    assert.equal(await restored.frame.locator('#question').inputValue(),'Keep my question');
    assert.equal(await restored.frame.locator('#selected-question').getAttribute('aria-pressed'),'true');
    await restored.page.close();await page.close();
  });
  await t.test('whiteboard object commands, shortcuts and zoom are exposed and edits stay undoable', async () => {
    const {page,frame}=await open();
    await frame.locator('#scene [data-shape="ellipse"]').click();
    await frame.locator('#object-menu > summary').click();
    await frame.getByRole('button',{name:/^Duplicate/}).click();
    assert.equal(await frame.locator('#scene [data-shape]').count(),2);
    await frame.locator('#undo').click();assert.equal(await frame.locator('#scene [data-shape]').count(),1);
    await frame.locator('#redo').click();assert.equal(await frame.locator('#scene [data-shape]').count(),2);
    await frame.locator('#canvas').focus();await page.keyboard.press('Control+a');
    assert.equal(await frame.locator('#selection [data-resize]').count(),2);
    await page.keyboard.press('Delete');assert.equal(await frame.locator('#scene [data-shape]').count(),0);
    await frame.locator('#undo').click();assert.equal(await frame.locator('#scene [data-shape]').count(),2);
    await frame.getByRole('button',{name:'Zoom in',exact:true}).click();
    await frame.locator('#board-zoom').click();assert.equal(await frame.locator('#board-zoom').innerText(),'100%');
    await frame.getByRole('button',{name:'Shortcuts',exact:true}).click();await frame.locator('#board-shortcuts').waitFor();
    await page.keyboard.press('Escape');assert.equal(await frame.locator('#board-shortcuts').isVisible(),false);
    await page.close();
  });
  await t.test('both palettes fit desktop and phone, and read-only editors expose no editing controls', async () => {
    const directory=new URL('../../.scratch/design-integration/screenshots/',import.meta.url).pathname;
    if(process.env.MEVEDEL_DESIGN_SCREENSHOTS) await mkdir(directory,{recursive:true});
    for(const kind of ['document','whiteboard']) for(const palette of ['cool','warm']) for(const theme of ['light','dark']) for(const width of [1600,390]) {
      const {page,frame}=await open({kind,content:kind==='document'?content:undefined,appearance:{palette,theme,accents:'selective'},viewport:{width,height:900}});
      assert.equal(await frame.locator('html').getAttribute('data-palette'),palette);
      assert.equal(await frame.locator('html').getAttribute('data-theme'),theme);
      assert.ok(await frame.locator('body').evaluate(n=>n.scrollWidth<=innerWidth));
      if(kind==='document') for(const group of ['Text','Lists','Insert']) {
        await frame.locator('#formatting summary').getByText(group,{exact:true}).click();
        const b=await frame.locator('.document-menu[open] .editor-menu').boundingBox();
        assert.ok(b.x>=0&&b.x+b.width<=width);
        await page.keyboard.press('Escape');
      }
      if(process.env.MEVEDEL_DESIGN_SCREENSHOTS) await page.screenshot({path:`${directory}/${kind}-${palette}-${theme}-${width}.png`});
      await page.close();
    }
    for(const kind of ['document','whiteboard']) {
      const {page,frame}=await open({kind,content:kind==='document'?content:undefined,readOnly:true});
      if(kind==='document') {
        assert.equal(await frame.locator('#formatting').isVisible(),false);
        assert.equal(await frame.locator('.tiptap').getAttribute('contenteditable'),'false');
      } else assert.equal(await frame.locator('[data-tool="rect"]').count(),0);
      await page.close();
    }
  });
});
