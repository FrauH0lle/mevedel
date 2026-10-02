import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {editorFixture} from './editor-fixture.mjs';

// Real image pixels, editor transactions, host validation and exports.
test('shared image transformations', async t => {
  const {open} = await editorFixture(t);
  for (const kind of ['document']) await t.test(kind,async () => {
    const {page,frame} = await open({kind,content:kind==='whiteboard'?[]:undefined,viewport:{width:1280,height:900}});
    const src = await page.evaluate(() => {
      const canvas = document.createElement('canvas');canvas.width=80;canvas.height=40;
      const ctx=canvas.getContext('2d');
      for (const [color,x,y] of [['#ff0000',0,0],['#00ff00',40,0],['#0000ff',0,20],['#ffff00',40,20]]) {
        ctx.fillStyle=color;ctx.fillRect(x,y,40,20);
      }
      return canvas.toDataURL();
    });
    await frame.locator('#image-upload').setInputFiles({name:'quadrants.png',mimeType:'image/png',buffer:Buffer.from(src.split(',')[1],'base64')});
    const image = frame.locator(kind==='whiteboard' ? '#scene image' : '.tiptap img');
    await image.waitFor();await image.click();
    await frame.locator('#image-tools').waitFor();
    const read = async () => page.evaluate(async kind => {
      const {content} = await window.apply({action:'read'});
      return kind==='whiteboard' ? content.find(s=>s.type==='image') : content.content.find(n=>n.type==='image').attrs;
    },kind);
    const saved = () => frame.locator('#saved').getByText('Saved on host',{exact:true}).waitFor();
    const pixels = async source => page.evaluate(async src => {
      const img = new Image();img.src=src;await img.decode();
      const c=document.createElement('canvas');c.width=img.width;c.height=img.height;
      const ctx=c.getContext('2d');ctx.drawImage(img,0,0);
      return {width:img.width,height:img.height,colors:[[1,1],[img.width-2,1],[1,img.height-2],[img.width-2,img.height-2]]
        .map(([x,y])=>[...ctx.getImageData(x,y,1,1).data].slice(0,3).join(','))};
    },source);
    const waitEdit = async rotation => {
      await page.waitForFunction(async ({kind,rotation}) => {
        const {content}=await window.apply({action:'read'});
        const image=kind==='whiteboard' ? content.find(s=>s.type==='image') : content.content.find(n=>n.type==='image')?.attrs;
        return image?.imageEdit?.rotation===rotation;
      },{kind,rotation});
      await saved();return read();
    };
    await saved(); const before = await read();
    await frame.getByRole('button',{name:'Rotate 90°',exact:true}).click();
    let edited = await waitEdit(90);
    assert.equal(edited.src,src,'original retained');
    assert.deepEqual(await pixels(edited.imageEdit.src),{width:40,height:80,colors:['0,0,255','255,0,0','255,255,0','0,255,0']});
    await frame.getByRole('button',{name:'Flip horizontal',exact:true}).click();await saved();
    await page.waitForFunction(async kind => {const c=(await window.apply({action:'read'})).content;return (kind==='whiteboard'?c.find(s=>s.type==='image'):c.content.find(n=>n.type==='image').attrs).imageEdit.flipX;},kind);
    edited=await read();
    assert.deepEqual((await pixels(edited.imageEdit.src)).colors,['255,0,0','0,0,255','0,255,0','255,255,0']);
    await frame.getByRole('button',{name:'Flip vertical',exact:true}).click();await saved();
    await page.waitForFunction(async kind => {const c=(await window.apply({action:'read'})).content;return (kind==='whiteboard'?c.find(s=>s.type==='image'):c.content.find(n=>n.type==='image').attrs).imageEdit.flipY;},kind);
    assert.deepEqual((await pixels((await read()).imageEdit.src)).colors,['0,255,0','255,255,0','255,0,0','0,0,255']);
    await frame.getByRole('button',{name:'Reset image',exact:true}).click();
    await page.waitForFunction(async kind => {const c=(await window.apply({action:'read'})).content;return !(kind==='whiteboard'?c.find(s=>s.type==='image'):c.content.find(n=>n.type==='image').attrs).imageEdit;},kind);
    await saved();
    assert.equal(await image.getAttribute(kind==='whiteboard'?'href':'src'),src);
    await frame.getByRole('button',{name:'Crop image',exact:true}).click();
    const dialog = frame.locator('.image-crop-dialog');await dialog.waitFor();
    await dialog.getByLabel('Crop width (%)',{exact:true}).fill('50');
    await dialog.getByRole('button',{name:'Cancel',exact:true}).click();
    assert.equal((await read()).imageEdit ?? null,null,'cancel does not apply the crop');
    await frame.getByRole('button',{name:'Crop image',exact:true}).click();await dialog.waitFor();
    const stage = await dialog.locator('.crop-stage').boundingBox();
    const handle = await dialog.getByRole('button',{name:'Crop right edge',exact:true}).boundingBox();
    await page.mouse.move(handle.x+handle.width/2,handle.y+handle.height/2);await page.mouse.down();
    await page.mouse.move(stage.x+stage.width/2,handle.y+handle.height/2);await page.mouse.up();
    assert.ok(Math.abs(+(await dialog.getByLabel('Crop width (%)',{exact:true}).inputValue())-50)<1);
    await dialog.getByLabel('Crop width (%)',{exact:true}).fill('50');
    const edge = dialog.getByRole('button',{name:'Crop right edge',exact:true});
    await edge.focus();await page.keyboard.press('ArrowRight');
    assert.equal(await dialog.getByLabel('Crop width (%)',{exact:true}).inputValue(),'51.25');
    await page.keyboard.press('ArrowLeft');
    if(process.env.MEVEDEL_IMAGE_SCREENSHOTS) {
      await mkdir('.scratch/image-tools',{recursive:true});
      await page.screenshot({path:`.scratch/image-tools/${kind}-crop.png`});
    }
    await dialog.getByRole('button',{name:'Apply image',exact:true}).click();await dialog.waitFor({state:'detached'});
    edited = await waitEdit(0);
    edited.imageEdit.crop.forEach((v,i)=>assert.ok(Math.abs(v-[0,0,0.5,1][i])<1e-8));
    assert.deepEqual(await pixels(edited.imageEdit.src),{width:40,height:40,colors:['255,0,0','255,0,0','0,0,255','0,0,255']});
    assert.equal(await image.getAttribute(kind==='whiteboard'?'href':'src'),edited.imageEdit.src);
    await frame.locator('#undo').click();await saved();assert.equal((await read()).imageEdit ?? null,null);
    await frame.locator('#redo').click();await saved();assert.deepEqual((await read()).imageEdit,edited.imageEdit);
    const native = await page.evaluate(async () => (await window.apply({action:'export',format:'native'})).text);
    assert.ok(native.includes(src));assert.ok(native.includes(edited.imageEdit.src));
    for (const format of kind==='whiteboard'?['svg']:['html','markdown']) {
      const exported = await page.evaluate(async format => (await window.apply({action:'export',format})).text,format);
      assert.ok(exported.includes(edited.imageEdit.src),`${format} uses the transformed pixels`);
      assert.ok(!exported.includes(src),`${format} does not reveal cropped-away pixels`);
    }
    // Read-only viewers receive the same appearance without editing controls.
    const nativeContent=JSON.parse(native).content;
    const viewer = await open({kind,content:nativeContent,readOnly:true});
    const viewed=viewer.frame.locator(kind==='whiteboard'?'#scene image':'.tiptap img');
    await viewed.waitFor();assert.equal(await viewed.getAttribute(kind==='whiteboard'?'href':'src'),edited.imageEdit.src);
    assert.equal(await viewer.frame.locator('#image-tools').isVisible(),false);await viewer.page.close();
    // An update while the crop dialog is open must not be overwritten.
    await image.click();await frame.getByRole('button',{name:'Crop image',exact:true}).click();await dialog.waitFor();
    await page.evaluate(async kind => {
      const {content}=await window.apply({action:'read'});
      const before=kind==='whiteboard'?content.find(s=>s.type==='image'):content.content.find(n=>n.type==='image');
      const after=kind==='whiteboard'?{...before,box:[...before.box.slice(0,2),100,100]}:{...before,attrs:{...before.attrs,width:100,height:100}};
      const result=await window.apply({action:'patch',opId:'concurrent-image',changes:[{id:kind==='whiteboard'?before.id:before.attrs.id,before,after}]});
      window.port.postMessage({type:'changed',...result});
    },kind);
    await dialog.getByRole('button',{name:'Apply image',exact:true}).click();
    await dialog.getByRole('alert').getByText(/This image changed/).waitFor();
    await dialog.getByRole('button',{name:'Cancel',exact:true}).click();
    const current = await read();assert.equal(kind==='whiteboard'?current.box[2]:current.width,100);
    assert.equal(current.src,before.src);
    await page.setViewportSize({width:390,height:700});
    if (await frame.locator('#assistant').isVisible()) await frame.locator('#assistant-close').click();
    await image.click();await frame.getByRole('button',{name:'Crop image',exact:true}).click();await dialog.waitFor();
    assert.equal(await dialog.evaluate(el=>el.scrollWidth<=el.clientWidth),true,'crop dialog fits a phone');
    await dialog.getByRole('button',{name:'Reset crop',exact:true}).click();
    assert.equal(await dialog.getByLabel('Crop width (%)',{exact:true}).inputValue(),'100');
    if(process.env.MEVEDEL_IMAGE_SCREENSHOTS)
      await page.screenshot({path:`.scratch/image-tools/${kind}-crop-phone.png`});
    await dialog.getByRole('button',{name:'Cancel',exact:true}).click();
    await page.close();
  });
  await t.test('whiteboard', async () => {
    const {page,frame} = await open({content:[],viewport:{width:1280,height:900}});
    const src = await page.evaluate(() => {
      const canvas = document.createElement('canvas');canvas.width=80;canvas.height=40;
      const ctx=canvas.getContext('2d');
      for (const [color,x,y] of [['#ff0000',0,0],['#00ff00',40,0],['#0000ff',0,20],['#ffff00',40,20]]) {
        ctx.fillStyle=color;ctx.fillRect(x,y,40,20);
      }
      return canvas.toDataURL();
    });
    await frame.locator('#image-upload').setInputFiles({name:'quadrants.png',mimeType:'image/png',buffer:Buffer.from(src.split(',')[1],'base64')});
    const image = frame.locator('#scene image');
    await image.waitFor();await image.click();
    await frame.locator('#image-tools').waitFor();
    const read = async () => page.evaluate(async () => (await window.apply({action:'read'})).content.find(s=>s.type==='image'));
    const saved = () => frame.locator('#saved').getByText('Saved on host',{exact:true}).waitFor();
    // The host's PNG shows what every viewer sees: sample the image's corners.
    const corners = () => page.evaluate(async () => {
      const {data} = await window.apply({action:'export',format:'png'});
      const img = new Image();img.src=`data:image/png;base64,${data}`;await img.decode();
      const c=document.createElement('canvas');c.width=img.width;c.height=img.height;
      const ctx=c.getContext('2d');ctx.drawImage(img,0,0);
      const [w,h]=[img.width-60,img.height-60];
      return {size:[w,h],colors:[[32,32],[28+w,32],[32,28+h],[28+w,28+h]]
        .map(([x,y])=>[...ctx.getImageData(x,y,1,1).data].slice(0,3).join(','))};
    });
    const until = async test => {
      for (let i = 0; !test(await read()); i++) {
        assert.ok(i < 100, 'the image edit is saved');
        await page.waitForTimeout(50);
      }
      await saved();return read();
    };
    await saved();
    const before = await read();
    assert.match(before.fileId,/^[0-9a-f]{40}$/);
    await frame.getByRole('button',{name:'Rotate 90°',exact:true}).click();
    let edited = await until(e => Math.abs(e.angle - Math.PI / 2) < 1e-9);
    assert.equal(edited.fileId,before.fileId,'the original file is retained');
    assert.deepEqual(await corners(),{size:[40,80],colors:['0,0,255','255,0,0','255,255,0','0,255,0']});
    await frame.getByRole('button',{name:'Flip horizontal',exact:true}).click();
    edited = await until(e => e.scale[0] * e.scale[1] === -1);
    assert.deepEqual((await corners()).colors,['255,0,0','0,0,255','0,255,0','255,255,0']);
    await frame.getByRole('button',{name:'Flip vertical',exact:true}).click();
    edited = await until(e => e.scale[0] * e.scale[1] === 1 && e.scale[1] === -1);
    assert.deepEqual((await corners()).colors,['0,255,0','255,255,0','255,0,0','0,0,255']);
    await frame.getByRole('button',{name:'Reset image',exact:true}).click();
    edited = await until(e => !e.angle && e.scale[0] === 1 && e.scale[1] === 1);
    assert.deepEqual(await corners(),{size:[80,40],colors:['255,0,0','0,255,0','0,0,255','255,255,0']});
    await frame.getByRole('button',{name:'Crop image',exact:true}).click();
    const dialog = frame.locator('.image-crop-dialog');await dialog.waitFor();
    await dialog.getByLabel('Crop width (%)',{exact:true}).fill('50');
    await dialog.getByRole('button',{name:'Cancel',exact:true}).click();
    assert.equal((await read()).crop ?? null,null,'cancel does not apply the crop');
    await frame.getByRole('button',{name:'Crop image',exact:true}).click();await dialog.waitFor();
    await dialog.getByLabel('Crop width (%)',{exact:true}).fill('50');
    await dialog.getByRole('button',{name:'Apply image',exact:true}).click();await dialog.waitFor({state:'detached'});
    edited = await until(e => e.crop?.width === 40);
    assert.deepEqual(edited.crop,{x:0,y:0,width:40,height:40,naturalWidth:80,naturalHeight:40});
    assert.deepEqual([edited.width,edited.height],[40,40],'cropping keeps the display scale');
    assert.deepEqual(await corners(),{size:[40,40],colors:['255,0,0','255,0,0','0,0,255','0,0,255']});
    await frame.locator('#undo').click();await saved();assert.equal((await read()).crop ?? null,null);
    await frame.locator('#redo').click();await saved();assert.deepEqual((await read()).crop,edited.crop);
    const native = await page.evaluate(async () => (await window.apply({action:'export',format:'native'})).text);
    assert.ok(native.includes(src),'the Excalidraw file keeps the original, so the crop stays editable');
    const svg = await page.evaluate(async () => (await window.apply({action:'export',format:'svg'})).text);
    assert.ok(!svg.includes(src),'an SVG download does not reveal cropped-away pixels');
    // Read-only viewers receive the same appearance without editing controls.
    const viewer = await open({scene:native,readOnly:true});
    await viewer.frame.locator('#scene image').waitFor();
    assert.match(await viewer.frame.locator('#scene clipPath').first().innerHTML(),/<rect width="40" height="40"/);
    assert.equal(await viewer.frame.locator('#image-tools').isVisible(),false);await viewer.page.close();
    // An update while the crop dialog is open must not be overwritten.
    // The image element spans the uncropped source; click its visible part.
    await image.click({position:{x:4,y:4}});await frame.getByRole('button',{name:'Crop image',exact:true}).click();await dialog.waitFor();
    await page.evaluate(async () => {
      const before=(await window.apply({action:'read'})).content.find(s=>s.type==='image');
      const result=await window.apply({action:'patch',opId:'concurrent-image',changes:[{id:before.id,before,after:{...before,width:100,height:100}}]});
      window.port.postMessage({type:'changed',...result});
    });
    await dialog.getByRole('button',{name:'Apply image',exact:true}).click();
    await dialog.getByRole('alert').getByText(/This image changed/).waitFor();
    await dialog.getByRole('button',{name:'Cancel',exact:true}).click();
    const current = await read();assert.equal(current.width,100);
    assert.equal(current.fileId,before.fileId);
    await page.close();
  });
});
