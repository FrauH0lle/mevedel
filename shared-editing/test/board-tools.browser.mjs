import test from 'node:test';
import assert from 'node:assert/strict';
import {editorFixture, libraryHost} from './editor-fixture.mjs';

const read = (page) => page.evaluate(async () => (await window.apply({action:'read'})).content);
const saved = (frame) => frame.locator('#saved').getByText('Saved on host', {exact:true}).waitFor();
/* Autoshape strokes settle after a pause, handing the board back to Select. */
const shaped = async (frame) => {
  await frame.locator('[data-tool="select"][aria-pressed="true"]').waitFor();
  await saved(frame);
};
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
const until = async (check, ms = 5000) => {
  for (const end = Date.now() + ms; Date.now() < end; await new Promise(r => setTimeout(r, 50)))
    if (await check()) return;
  throw new Error('Timed out');
};
const selectedIds = (frame) => frame.locator('#selection').evaluate(n => n.dataset.selected ? n.dataset.selected.split(' ') : []);

test('board tools', async (t) => {
  const catalogText = JSON.stringify({type:'excalidrawlib', version:2, libraryItems:[
    {id:'cloud', name:'Cloud', status:'published', elements:[{id:'c', type:'ellipse', x:0, y:0, width:80, height:40}]}]});
  const library = libraryHost([
    {name:'Clouds', description:'Weather', authors:'Ann', source:'ann/clouds.excalidrawlib', downloads:50, created:'2021-01-01', updated:'2021-02-01'},
    {name:'Arrows', description:'Pointers', authors:'Bo', source:'bo/arrows.excalidrawlib', downloads:5, created:'2024-05-01', updated:''}],
    {'ann/clouds.excalidrawlib': catalogText});
  const {open} = await editorFixture(t, {library});

  await t.test('a hand-drawn rectangle becomes a clean shape; a scribble stays a stroke', async () => {
    const {page, frame} = await open({content:[]});
    await frame.locator('#canvas').focus();
    await page.keyboard.press('Shift+X');
    assert.equal(await frame.locator('[data-tool="autoshape"]').getAttribute('aria-pressed'), 'true');
    await stroke(page, frame, [[100, 100], [300, 100], [300, 220], [100, 220], [100, 102]]);
    await shaped(frame);
    let content = await read(page);
    assert.deepEqual(content.map(e => e.type), ['rectangle']);
    const [x, y, w, h] = [content[0].x, content[0].y, content[0].width, content[0].height];
    assert.ok(Math.abs(x - 100) < 6 && Math.abs(y - 100) < 6 && Math.abs(w - 200) < 8 && Math.abs(h - 120) < 8, JSON.stringify(content[0]));
    await frame.locator('[data-tool="autoshape"]').click();
    await stroke(page, frame, [[100, 300], [180, 390], [110, 440], [260, 320], [140, 500], [300, 480]], 8);
    await shaped(frame);
    content = await read(page);
    assert.deepEqual(content.map(e => e.type).sort(), ['freedraw', 'rectangle']);
    await page.close();
  });

  await t.test('the menu sets the board canvas background, shared and undoable', async () => {
    const {page, frame} = await open({content:[{id:'a', type:'rectangle', x:0, y:0, width:100, height:60}]});
    const background = async () => (await page.evaluate(() => window.apply({action:'read'}))).background;
    await frame.locator('#menu > summary').click();
    await frame.getByRole('button', {name:'Canvas background: Yellow', exact:true}).click();
    await saved(frame);
    assert.equal(await background(), '#fffce8');
    assert.equal(await frame.locator('#board').evaluate(n => getComputedStyle(n, '::before').backgroundColor), 'rgb(255, 252, 232)');
    assert.equal(await frame.getByRole('button', {name:'Canvas background: Yellow', exact:true}).getAttribute('aria-pressed'), 'true');
    await frame.locator('#canvas').focus();
    await page.keyboard.press('Control+z');
    await until(async () => (await background()) === undefined);
    assert.equal(await frame.locator('#board').evaluate(n => n.style.getPropertyValue('--canvas')), '');
    await page.close();
  });

  await t.test('a dark theme shows the light canvas and its content dark, as Excalidraw does', async () => {
    const {page, frame} = await open({content:[{id:'ink', type:'rectangle', x:0, y:0, width:200, height:120,
      backgroundColor:'#1e1e1e', fillStyle:'solid', roughness:0}]});
    // The colour at the middle of the board area and of the shape, as rendered.
    const pixels = async () => {
      const shot = await page.screenshot();
      const [board, ink] = await Promise.all([frame.locator('#board'), frame.locator('#scene [data-shape="ink"]')]
        .map(async (l) => { const b = await l.boundingBox(); return [b.x + 10, b.y + b.height / 2]; }));
      return page.evaluate(async ([png, points]) => {
        const image = await createImageBitmap(await (await fetch(`data:image/png;base64,${png}`)).blob());
        const canvas = new OffscreenCanvas(image.width, image.height), context = canvas.getContext('2d');
        context.drawImage(image, 0, 0);
        return points.map(([x, y]) => [...context.getImageData(Math.round(x), Math.round(y), 1, 1).data.slice(0, 3)]);
      }, [shot.toString('base64'), [board, ink]]);
    };
    const light = (rgb) => rgb.every(v => v > 180), dark = (rgb) => rgb.every(v => v < 80);
    let [board, ink] = await pixels();
    assert.ok(light(board) && dark(ink), JSON.stringify([board, ink]));
    await frame.locator('html').evaluate(root => { root.dataset.theme = 'dark'; });
    [board, ink] = await pixels();
    assert.ok(dark(board) && light(ink), JSON.stringify([board, ink]));
    // Stored colours and exports stay as authored.
    assert.equal((await read(page))[0].backgroundColor, '#1e1e1e');
    await page.close();
  });

  await t.test('an arrow drawn shaft first, head second, binds to the shapes at its ends', async () => {
    const {page, frame} = await open({content:[
      {id:'a', type:'rectangle', x:100, y:100, width:100, height:60},
      {id:'b', type:'rectangle', x:400, y:100, width:100, height:60}]});
    await frame.locator('[data-tool="autoshape"]').click();
    await stroke(page, frame, [[150, 130], [410, 130]]);
    // The head's barbs end left of B: the tip, not the lift-off, picks the target.
    await stroke(page, frame, [[385, 112], [410, 130], [385, 148]], 8);
    await shaped(frame);
    const content = await read(page);
    const arrows = content.filter(e => e.type === 'arrow');
    assert.equal(content.length, 3, JSON.stringify(content.map(e => e.type)));
    assert.equal(arrows.length, 1);
    assert.equal(arrows[0].startBinding?.elementId, 'a');
    assert.equal(arrows[0].endBinding?.elementId, 'b');
    await page.close();
  });

  await t.test('the library inserts built-in and personal items and installs public libraries', async () => {
    const {page, frame} = await open({content:[{id:'box', type:'rectangle', x:0, y:0, width:100, height:60}]});
    await frame.locator('#library-menu > summary').click();
    await frame.getByRole('button', {name:'Insert Database', exact:true}).click();
    await saved(frame);
    let content = await read(page);
    const cylinder = content.filter(e => e.groupIds?.length);
    assert.equal(cylinder.length, 4, 'the built-in database cylinder is four grouped elements');
    assert.equal(new Set(cylinder.map(e => e.groupIds.at(-1))).size, 1);
    // Selected objects become a personal item, kept by the host.
    await frame.locator('#library-menu > summary').click();
    await frame.getByRole('button', {name:'Add selection', exact:true}).click();
    await frame.getByRole('button', {name:'Remove item from the library', exact:true}).waitFor();
    assert.equal(JSON.parse(library.requests.findLast(r => r.action === 'library-add').text).libraryItems[0].elements.length, 4);
    await frame.getByRole('button', {name:'Remove item from the library', exact:true}).click();
    await frame.getByText('Select objects and choose Add selection', {exact:false}).waitFor();
    // An .excalidrawlib file imports into the personal library.
    await frame.locator('#library-menu input[type=file]').setInputFiles({name:'clouds.excalidrawlib',
      mimeType:'application/json', buffer:Buffer.from(catalogText)});
    await frame.getByRole('button', {name:'Insert Cloud', exact:true}).waitFor();
    // The public collection sorts, previews and installs through the host.
    await frame.getByRole('button', {name:'Browse public libraries', exact:true}).click();
    await frame.locator('.library-entry').first().waitFor();
    const names = () => frame.locator('.library-entry strong').allTextContents();
    assert.deepEqual(await names(), ['Clouds', 'Arrows'], 'most downloaded first');
    await frame.getByRole('combobox', {name:'Sort public libraries'}).selectOption('created');
    assert.deepEqual(await names(), ['Arrows', 'Clouds'], 'newest first');
    await frame.getByRole('searchbox', {name:'Search public libraries'}).fill('weather');
    await frame.locator('.library-entry').filter({hasText:'Clouds'}).click();
    await frame.getByRole('heading', {name:'Clouds', exact:true}).waitFor();
    await frame.getByRole('button', {name:'Install library', exact:true}).click();
    await frame.getByRole('button', {name:'Installed', exact:true}).waitFor();
    assert.deepEqual(library.requests.filter(r => r.action === 'library-install').map(r => [r.source, r.name]),
      [['ann/clouds.excalidrawlib', 'Clouds']]);
    await frame.getByRole('button', {name:'← Libraries', exact:true}).click();
    await frame.getByRole('button', {name:'← My library', exact:true}).click();
    await frame.getByRole('heading', {name:'Clouds', exact:true}).waitFor();
    await frame.getByRole('button', {name:'Insert Cloud', exact:true}).last().click();
    await saved(frame);
    content = await read(page);
    assert.equal(content.filter(e => e.type === 'ellipse').length, 2, 'an installed item inserts as new elements');
    await frame.locator('#library-menu > summary').click();
    await frame.getByRole('button', {name:'Remove library Clouds', exact:true}).click();
    await frame.getByRole('heading', {name:'Clouds', exact:true}).waitFor({state:'detached'});
    await page.close();
  });

  await t.test('groups, locks, duplicates and arrowheads behave as in Excalidraw', async () => {
    const bind = elementId => ({elementId, fixedPoint:[0.5, 0.5], mode:'orbit'});
    const {page, frame} = await open({viewport:{width:1280, height:700}, content:[
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
    assert.equal(await frame.locator('#properties').evaluate(e => e.open), true, 'a selection opens Style');
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

  await t.test('handles resize, rotate and bend as in Excalidraw', async () => {
    const bind = elementId => ({elementId, fixedPoint:[0.5, 0.5], mode:'orbit'});
    const {page, frame} = await open({viewport:{width:1280, height:800}, content:[
      {id:'a', type:'rectangle', x:0, y:0, width:100, height:60},
      {id:'b', type:'rectangle', x:400, y:0, width:100, height:60},
      {id:'g1', type:'ellipse', x:300, y:200, width:60, height:60, groupIds:['grp']},
      {id:'g2', type:'diamond', x:380, y:200, width:60, height:60, groupIds:['grp']},
      {id:'link', type:'arrow', x:150, y:30, width:200, height:0, points:[[0, 0], [200, 0]], roundness:{type:2}},
    ]});
    await frame.getByRole('button', {name:'Fit', exact:true}).click();
    const element = async (id) => (await read(page)).find(e => e.id === id);
    const handleAt = async (name) => {
      const box = await frame.locator(`#selection [data-${name.startsWith('point') || name.startsWith('mid') ? name.split(':')[0] : 'handle'}="${name.split(':').at(-1)}"]`).boundingBox();
      return [box.x + box.width / 2, box.y + box.height / 2];
    };
    const dragFrom = async ([x, y], [bx, by], modifier) => {
      if (modifier) await page.keyboard.down(modifier);
      await page.mouse.move(x, y); await page.mouse.down();
      await page.mouse.move((x + bx) / 2, (y + by) / 2); await page.mouse.move(bx, by); await page.mouse.up();
      if (modifier) await page.keyboard.up(modifier);
      await saved(frame);
    };
    // A group has one frame; double-clicking enters it.
    await page.mouse.click(...await at(page, frame, 330, 230));
    assert.deepEqual(await selectedIds(frame), ['g1', 'g2']);
    assert.equal(await frame.locator('#selection rect[stroke-dasharray="5 3"]').count(), 1, 'one outline around the group');
    await page.mouse.dblclick(...await at(page, frame, 410, 230));
    assert.deepEqual(await selectedIds(frame), ['g2'], 'inside the group, elements select alone');
    await page.mouse.click(...await at(page, frame, 330, 230));
    assert.deepEqual(await selectedIds(frame), ['g1']);
    await page.keyboard.press('Escape');
    assert.deepEqual(await selectedIds(frame), ['g1', 'g2'], 'Escape leaves the group selected');
    // Eight resize handles and a rotation handle surround a lone shape.
    await page.keyboard.press('Escape');
    await frame.locator('#properties').evaluate(e => { e.open = false; });
    await page.mouse.click(...await at(page, frame, 50, 30));
    await frame.locator('#properties').evaluate(e => { e.open = false; });
    assert.equal(await frame.locator('#selection [data-handle]').count(), 9);
    const e = await handleAt('e');
    await dragFrom(e, [e[0] + 60, e[1] + 40]);
    let a = await element('a');
    assert.ok(a.width > 130 && a.height === 60 && a.x === 0, `a side handle changes only its side: ${JSON.stringify(a)}`);
    const nw = await handleAt('nw');
    await dragFrom(nw, [nw[0] - 30, nw[1] - 30]);
    a = await element('a');
    assert.ok(a.x < -10 && a.y < -10, 'the opposite corner stays in place');
    // The rotation handle sits above the frame; keep it inside the canvas.
    await frame.getByRole('button', {name:'Zoom out', exact:true}).click();
    const r = await handleAt('rotation'), [cx, cy] = await at(page, frame, a.x + a.width / 2, a.y + a.height / 2);
    await dragFrom(r, [cx + 200, cy], 'Shift');
    a = await element('a');
    assert.ok(Math.abs(a.angle - Math.PI / 2) < 1e-9, `rotation snaps to 15 degrees: ${a.angle}`);
    // Several shapes scale together from their common frame.
    await page.keyboard.press('Escape');
    await frame.locator('#canvas').focus();
    await page.keyboard.press('Control+a');
    const before = await read(page);
    const se = await handleAt('se');
    await dragFrom(se, [se[0] + 100, se[1] + 50]);
    const after = await read(page);
    assert.ok(after.find(e => e.id === 'b').width > before.find(e => e.id === 'b').width, 'members scale with the frame');
    // A lone arrow shows its points; a midpoint drag bends it.
    await page.keyboard.press('Escape');
    const link = await element('link');
    const [mx, my] = await at(page, frame, link.x + link.points[1][0] / 2, link.y);
    await page.mouse.click(mx, my);
    assert.deepEqual(await selectedIds(frame), ['link']);
    assert.equal(await frame.locator('#selection [data-point]').count(), 2);
    await dragFrom(await handleAt('mid:0'), [mx, my - 80]);
    assert.equal((await element('link')).points.length, 3, 'the bend is a new point');
    // Dropping an end on a shape binds it.
    const target = await element('b');
    const tip = await handleAt('point:2'), [bx, by] = await at(page, frame, target.x + target.width / 2, target.y + target.height / 2);
    await dragFrom(tip, [bx, by]);
    assert.equal((await element('link')).endBinding?.elementId, 'b');
    // Arrow type: elbow arrows route orthogonally, curved ones bend.
    await frame.getByRole('button', {name:'Elbow arrow', exact:true}).click();
    await saved(frame);
    const elbow = await element('link');
    assert.equal(elbow.elbowed, true);
    const d = await frame.locator('[data-shape="link"] path.hit').getAttribute('d');
    const corners = d.split('L').map(p => p.replace('M', '').trim().split(' ').map(Number));
    assert.ok(corners.slice(1).every((p, i) => Math.abs(p[0] - corners[i][0]) < 0.6 || Math.abs(p[1] - corners[i][1]) < 0.6),
      `every elbow segment is horizontal or vertical: ${d}`);
    await frame.getByRole('button', {name:'Curved arrow', exact:true}).click();
    await saved(frame);
    const curved = await element('link');
    assert.deepEqual([curved.elbowed, curved.roundness], [undefined, {type:2}]);
    await page.close();
  });

  await t.test('closed lines are picked inside; labels align in their shape', async () => {
    const {page, frame} = await open({viewport:{width:1280, height:700}, content:[
      {id:'hex', type:'line', x:0, y:0, width:200, height:180, backgroundColor:'#be4bdb', fillStyle:'hachure',
        points:[[50, 0], [150, 0], [200, 90], [150, 180], [50, 180], [0, 90], [50, 0]]},
      {id:'box', type:'rectangle', x:300, y:0, width:200, height:120},
      {id:'label', type:'text', x:0, y:0, width:0, height:0, text:'Hi', containerId:'box'},
      {id:'free', type:'text', x:0, y:300, width:0, height:0, text:'Free'},
    ]});
    await frame.getByRole('button', {name:'Fit', exact:true}).click();
    await page.mouse.click(...await at(page, frame, 103, 91));
    assert.deepEqual(await selectedIds(frame), ['hex'], 'a click between the hatch lines picks the line');
    await page.mouse.click(...await at(page, frame, 400, 60));
    const section = (key) => frame.locator(`#properties [data-sec="${key}"]`);
    assert.deepEqual([await section('textAlign').isVisible(), await section('verticalAlign').isVisible()], [true, true]);
    await frame.getByRole('button', {name:'Align right', exact:true}).click();
    await frame.getByRole('button', {name:'Align bottom', exact:true}).click();
    await saved(frame);
    const label = (await read(page)).find(e => e.id === 'label');
    assert.deepEqual([label.textAlign, label.verticalAlign], ['right', 'bottom']);
    await page.mouse.click(...await at(page, frame, 10, 310));
    assert.deepEqual(await selectedIds(frame), ['free']);
    assert.deepEqual([await section('textAlign').isVisible(), await section('verticalAlign').isVisible()], [true, false],
      'free text has no container to align in');
    await page.close();
  });

  await t.test('a pen records pressure for each sample', async () => {
    const {page, frame} = await open({content:[]});
    await frame.locator('[data-tool="freedraw"]').click();
    const cdp = await page.context().newCDPSession(page);
    const [x, y] = await at(page, frame, 100, 100);
    const pen = (type, dx, force) => cdp.send('Input.dispatchMouseEvent', {type, x: x + dx, y: y + dx / 3,
      button: 'left', buttons: type === 'mouseReleased' ? 0 : 1, clickCount: 1, pointerType: 'pen', force});
    await pen('mousePressed', 0, 0.2);
    for (let i = 1; i <= 10; i++) await pen('mouseMoved', i * 10, 0.2 + i * 0.07);
    await pen('mouseReleased', 100, 0);
    await saved(frame);
    const [stroke] = await read(page);
    assert.equal(stroke.simulatePressure, false);
    assert.ok(stroke.pressures.length >= 5 && stroke.pressures.at(-1) > stroke.pressures[0],
      `pressure follows the pen: ${stroke.pressures}`);
    await page.close();
  });

  await t.test('scrolling pans the board; Ctrl scroll zooms at the pointer', async () => {
    const {page, frame} = await open({content:[{id:'a', type:'rectangle', x:0, y:0, width:100, height:60}]});
    const zoom = () => frame.locator('#board-zoom').innerText();
    const before = await zoom();
    const [x, y] = await at(page, frame, 50, 30);
    await page.mouse.move(x, y);
    await page.mouse.wheel(0, 120);
    await until(async () => Math.abs((await at(page, frame, 50, 30))[1] - (y - 120)) < 1);
    assert.equal((await at(page, frame, 50, 30))[0], x);
    assert.equal(await zoom(), before, 'a plain scroll keeps the zoom');
    await page.keyboard.down('Shift');
    await page.mouse.wheel(0, 80);
    await page.keyboard.up('Shift');
    await until(async () => Math.abs((await at(page, frame, 50, 30))[0] - (x - 80)) < 1);
    const [px, py] = await at(page, frame, 50, 30);
    assert.ok(Math.abs(py - (y - 120)) < 1, 'Shift scrolls sideways only');
    // Zooming keeps the board point under the pointer in place.
    await page.mouse.move(px, py);
    await page.keyboard.down('Control');
    await page.mouse.wheel(0, -100);
    await page.keyboard.up('Control');
    await until(async () => (await zoom()) !== before);
    assert.ok(parseInt(await zoom()) > parseInt(before), `zoomed in: ${before} → ${await zoom()}`);
    const [zx, zy] = await at(page, frame, 50, 30);
    assert.ok(Math.abs(zx - px) < 1 && Math.abs(zy - py) < 1, 'the pointer keeps its board point');
    await page.close();
  });

  await t.test('the pen keeps drawing; the lock keeps the shape tools', async () => {
    const {page, frame} = await open({content:[]});
    const pressed = (tool) => frame.locator(`[data-tool="${tool}"]`).getAttribute('aria-pressed');
    const count = async (type) => (await read(page)).filter(e => e.type === type).length;
    await frame.locator('#canvas').focus();
    await page.keyboard.press('p');
    await stroke(page, frame, [[100, 100], [200, 150]], 6);
    await stroke(page, frame, [[100, 200], [200, 250]], 6);
    await until(async () => (await count('freedraw')) === 2);
    assert.equal(await pressed('freedraw'), 'true');
    assert.deepEqual(await selectedIds(frame), []);
    // Unlocked, a new shape hands over to selection with the shape selected.
    await frame.locator('#canvas').focus();
    await page.keyboard.press('r');
    await stroke(page, frame, [[300, 100], [400, 160]], 4);
    await frame.locator('[data-tool="select"][aria-pressed="true"]').waitFor();
    assert.equal((await selectedIds(frame)).length, 1);
    await frame.locator('#canvas').focus();
    await page.keyboard.press('q');
    assert.equal(await frame.locator('#board-tool-lock').getAttribute('aria-pressed'), 'true');
    await page.keyboard.press('r');
    await stroke(page, frame, [[300, 200], [400, 260]], 4);
    await stroke(page, frame, [[300, 300], [400, 360]], 4);
    await until(async () => (await count('rectangle')) === 3);
    assert.equal(await pressed('rectangle'), 'true');
    assert.deepEqual(await selectedIds(frame), []);
    await frame.locator('#board-tool-lock').click();
    assert.equal(await frame.locator('#board-tool-lock').getAttribute('aria-pressed'), 'false');
    await stroke(page, frame, [[300, 400], [400, 460]], 4);
    await frame.locator('[data-tool="select"][aria-pressed="true"]').waitFor();
    await page.close();
  });
});
