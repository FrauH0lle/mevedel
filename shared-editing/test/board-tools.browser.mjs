import test from 'node:test';
import assert from 'node:assert/strict';
import {editorFixture, libraryHost} from './editor-fixture.mjs';

const read = (page) => page.evaluate(async () => (await window.apply({action:'read'})).content);
const saved = (frame) => frame.locator('#saved').getByText('Saved on host', {exact:true}).waitFor();
/* Screen position of board point [x, y] in the page. */
async function at(page, frame, x, y) {
  const offset = await page.locator('iframe').boundingBox();
  const [px, py] = await frame.locator('#canvas').evaluate((canvas, [x, y]) => {
    const p = new DOMPoint(x, y).matrixTransform(canvas.getScreenCTM());
    return [p.x, p.y];
  }, [x, y]);
  return [offset.x + px, offset.y + py];
}
async function stroke(page, frame, corners, steps = 18) {
  await page.mouse.move(...await at(page, frame, ...corners[0]));
  await page.mouse.down();
  for (let i = 1; i < corners.length; i++)
    for (let s = 1; s <= steps; s++) {
      const [a, b] = [corners[i - 1], corners[i]];
      await page.mouse.move(...await at(page, frame, a[0] + (b[0] - a[0]) * s / steps + (s % 3 - 1), a[1] + (b[1] - a[1]) * s / steps + (s % 2)));
    }
  await page.mouse.up();
}
const selectedIds = (frame) => frame.locator('#selection [data-selected]').evaluateAll(n => n.map(e => e.dataset.selected).sort());

test('board tools', async (t) => {
  const catalogText = JSON.stringify({type:'excalidrawlib', version:2, libraryItems:[
    {id:'cloud', name:'Cloud', status:'published', elements:[{id:'c', type:'ellipse', x:0, y:0, width:80, height:40}]}]});
  const library = libraryHost([{name:'Clouds', description:'Weather', authors:'Ann', source:'ann/clouds.excalidrawlib'}],
    {'ann/clouds.excalidrawlib': catalogText});
  const {open} = await editorFixture(t, {library});

  await t.test('a hand-drawn rectangle becomes a clean shape; a scribble stays a stroke', async () => {
    const {page, frame} = await open({content:[]});
    await frame.locator('#canvas').focus();
    await page.keyboard.press('Shift+X');
    assert.equal(await frame.locator('[data-tool="autoshape"]').getAttribute('aria-pressed'), 'true');
    await stroke(page, frame, [[100, 100], [300, 100], [300, 220], [100, 220], [100, 102]]);
    await saved(frame);
    let content = await read(page);
    assert.deepEqual(content.map(e => e.type), ['rectangle']);
    const [x, y, w, h] = [content[0].x, content[0].y, content[0].width, content[0].height];
    assert.ok(Math.abs(x - 100) < 6 && Math.abs(y - 100) < 6 && Math.abs(w - 200) < 8 && Math.abs(h - 120) < 8, JSON.stringify(content[0]));
    await frame.locator('[data-tool="autoshape"]').click();
    await stroke(page, frame, [[100, 300], [180, 390], [110, 440], [260, 320], [140, 500], [300, 480]], 8);
    await saved(frame);
    content = await read(page);
    assert.deepEqual(content.map(e => e.type).sort(), ['freedraw', 'rectangle']);
    await page.close();
  });

  await t.test('the library inserts built-in and personal items and browses public libraries', async () => {
    const {page, frame} = await open({content:[{id:'box', type:'rectangle', x:0, y:0, width:100, height:60}]});
    await frame.locator('#library-menu > summary').click();
    await frame.getByRole('button', {name:'Insert Database', exact:true}).click();
    await saved(frame);
    let content = await read(page);
    const cylinder = content.filter(e => e.groupIds?.length);
    assert.equal(cylinder.length, 4, 'the built-in database cylinder is four grouped elements');
    assert.equal(new Set(cylinder.map(e => e.groupIds.at(-1))).size, 1);
    assert.deepEqual(await selectedIds(frame), cylinder.map(e => e.id).sort());
    // Clicking any part selects the whole group.
    await frame.locator('#canvas').click({position:{x:5, y:5}});
    await frame.locator(`[data-shape="${cylinder[0].id}"]`).click({force:true});
    assert.equal((await selectedIds(frame)).length, 4);
    // Selected objects become a personal item, kept by the host.
    await frame.locator('#library-menu > summary').click();
    await frame.getByRole('button', {name:'Add selection', exact:true}).click();
    await frame.getByRole('button', {name:'Remove item from the library', exact:true}).waitFor();
    assert.equal(library.requests.filter(r => r.action === 'library-add').length, 1);
    assert.equal(JSON.parse(library.requests.at(-1).text).libraryItems[0].elements.length, 4);
    await frame.getByRole('button', {name:'Remove item from the library', exact:true}).click();
    await frame.getByText('Select objects and choose Add selection', {exact:false}).waitFor();
    // An .excalidrawlib file imports into the personal library.
    await frame.locator('#library-menu input[type=file]').setInputFiles({name:'clouds.excalidrawlib',
      mimeType:'application/json', buffer:Buffer.from(catalogText)});
    await frame.getByRole('button', {name:'Insert Cloud', exact:true}).waitFor();
    // The public collection is listed and fetched by the host.
    await frame.getByRole('button', {name:'Browse public libraries', exact:true}).click();
    await frame.getByRole('searchbox', {name:'Search public libraries'}).fill('weather');
    await frame.locator('.library-entry').filter({hasText:'Clouds'}).click();
    await frame.getByRole('heading', {name:'Clouds', exact:true}).waitFor();
    assert.deepEqual(library.requests.filter(r => r.action === 'library-fetch').map(r => r.source), ['ann/clouds.excalidrawlib']);
    await frame.getByRole('button', {name:'Insert Cloud', exact:true}).click();
    await saved(frame);
    content = await read(page);
    assert.equal(content.filter(e => e.type === 'ellipse').length, 2, 'a public item inserts as new elements');
    await page.close();
  });

  await t.test('groups, locks, duplicates and arrowheads behave as in Excalidraw', async () => {
    const bind = elementId => ({elementId, fixedPoint:[0.5, 0.5], mode:'orbit'});
    const {page, frame} = await open({content:[
      {id:'a', type:'rectangle', x:0, y:0, width:100, height:60},
      {id:'b', type:'rectangle', x:300, y:0, width:100, height:60},
      {id:'link', type:'arrow', x:0, y:0, width:1, height:0, points:[[0, 0], [1, 0]], startBinding:bind('a'), endBinding:bind('b')},
      {id:'note', type:'text', x:0, y:200, width:0, height:0, text:'Note'},
    ]});
    await frame.getByRole('button', {name:'Fit', exact:true}).click();
    const click = async (x, y, modifier) => {
      if (modifier) await page.keyboard.down(modifier);
      await page.mouse.click(...await at(page, frame, x, y));
      if (modifier) await page.keyboard.up(modifier);
    };
    await click(50, 30); await click(350, 30, 'Shift');
    assert.deepEqual(await selectedIds(frame), ['a', 'b']);
    await page.keyboard.press('Control+g');
    await saved(frame);
    let content = await read(page);
    assert.equal(new Set(content.filter(e => ['a', 'b'].includes(e.id)).map(e => e.groupIds.join())).size, 1);
    await click(200, 150); await click(50, 30);
    assert.deepEqual(await selectedIds(frame), ['a', 'b'], 'a click selects the whole group');
    await page.keyboard.press('Control+d');
    await saved(frame);
    content = await read(page);
    assert.equal(content.length, 6, 'both shapes are copied');
    const copies = content.filter(e => e.type === 'rectangle' && !['a', 'b'].includes(e.id));
    assert.ok(copies.every(e => e.groupIds[0] !== content.find(o => o.id === 'a').groupIds[0]), 'copies form a new group');
    await page.keyboard.press('Delete');
    await click(50, 30);
    await page.keyboard.press('Control+Shift+g');
    await saved(frame);
    assert.ok((await read(page)).filter(e => ['a', 'b'].includes(e.id)).every(e => !e.groupIds), 'ungrouped');
    // A locked element cannot be picked until it is unlocked.
    await click(50, 30);
    await frame.locator('#object-menu > summary').click();
    await frame.getByRole('button', {name:'Lock', exact:true}).click();
    await click(50, 30);
    assert.deepEqual(await selectedIds(frame), []);
    await frame.locator('#object-menu > summary').click();
    await frame.getByRole('button', {name:'Unlock all', exact:true}).click();
    await click(50, 30);
    assert.deepEqual(await selectedIds(frame), ['a']);
    // Arrowheads and fonts come from the style panel.
    await page.mouse.click(...await at(page, frame, 200, 30));
    assert.deepEqual(await selectedIds(frame), ['link']);
    await frame.locator('#properties > summary').click();
    await frame.getByRole('button', {name:'End arrowhead: Triangle', exact:true}).click();
    await frame.getByRole('button', {name:'Start arrowhead: Circle', exact:true}).click();
    await frame.locator('#properties > summary').click();
    await click(15, 210);
    await frame.locator('#properties > summary').click();
    await frame.getByRole('button', {name:'Normal', exact:true}).click();
    await saved(frame);
    content = await read(page);
    const link = content.find(e => e.id === 'link');
    assert.deepEqual([link.startArrowhead, link.endArrowhead], ['circle', 'triangle']);
    assert.equal(content.find(e => e.id === 'note').fontFamily, 6);
    await page.close();
  });
});
