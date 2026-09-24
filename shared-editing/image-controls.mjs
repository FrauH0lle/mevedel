/* One non-destructive image editor for document nodes and board images. */
import {validateImageContent} from './image.mjs';

const defaults = () => ({crop:[0,0,1,1], rotation:0, flipX:false, flipY:false});
const clamp = (n, lo, hi) => Math.max(lo, Math.min(hi, n));
function size(image, edit) {
  const w = Math.max(1, Math.round(image.naturalWidth * edit.crop[2]));
  const h = Math.max(1, Math.round(image.naturalHeight * edit.crop[3]));
  return edit.rotation % 180 ? [h,w] : [w,h];
}
function paint(canvas, image, edit, maxEdge = 8192) {
  const [width,height] = size(image,edit), scale = Math.min(1,maxEdge/Math.max(width,height));
  canvas.width = Math.max(1,Math.round(width*scale));
  canvas.height = Math.max(1,Math.round(height*scale));
  const ctx = canvas.getContext('2d');
  ctx.translate(canvas.width / 2, canvas.height / 2);
  ctx.scale((edit.flipX ? -1 : 1)*scale, (edit.flipY ? -1 : 1)*scale);
  ctx.rotate(edit.rotation * Math.PI / 180);
  const [x,y,w,h] = edit.crop.map((v,i) => v * (i % 2 ? image.naturalHeight : image.naturalWidth));
  ctx.drawImage(image,x,y,w,h,-w/2,-h/2,w,h);
}
export async function renderImageEdit(snapshot, edit) {
  const image = new Image(); image.src = snapshot.src; await image.decode();
  const next = edit || defaults(), previous = snapshot.imageEdit || defaults();
  const [w,h] = size(image,next), [oldW,oldH] = size(image,previous);
  let scaleX = (snapshot.width || oldW) / oldW, scaleY = (snapshot.height || oldH) / oldH;
  if ((next.rotation - previous.rotation) % 180) [scaleX,scaleY] = [scaleY,scaleX];
  let imageEdit = null;
  if (edit && (edit.rotation || edit.flipX || edit.flipY || edit.crop.some((v,i) => v !== defaults().crop[i]))) {
    const canvas = document.createElement('canvas'); paint(canvas,image,edit);
    imageEdit = {...edit,src:canvas.toDataURL('image/png')};
  }
  validateImageContent({src:snapshot.src,imageEdit});
  return {imageEdit,width:clamp(Math.round(w*scaleX),1,8192),height:clamp(Math.round(h*scaleY),1,8192)};
}

export function imageTools(parent, getImage, commit, report) {
  let busy = false;
  const buttons = [];
  const button = (label, action) => {
    const el = document.createElement('button'); el.type = 'button'; el.textContent = label;
    el.onclick = action; parent.append(el); buttons.push(el); return el;
  };
  const apply = async (snapshot, edit) => {
    if (busy) return false;
    busy = true; buttons.forEach(b => {b.disabled = true;});
    try { await commit(snapshot,await renderImageEdit(snapshot,structuredClone(edit))); return true; }
    finally { busy = false; buttons.forEach(b => {b.disabled = false;}); }
  };
  const change = action => async () => {
    const snapshot = getImage(); if (!snapshot || busy) return;
    const edit = structuredClone(snapshot.imageEdit || defaults()); delete edit.src;
    try { await apply(snapshot,action(edit)); } catch (error) { report(error.message); }
  };
  const rotate = edit => ({...edit,rotation:(edit.rotation+90)%360,flipX:edit.flipY,flipY:edit.flipX});
  button('Crop image', async () => {
    const snapshot = getImage(); if (!snapshot || busy) return;
    try { await cropDialog(snapshot,apply,rotate); } catch (error) { report(error.message); }
  });
  button('Rotate 90°', change(rotate));
  button('Flip horizontal', change(e => ({...e,flipX:!e.flipX})));
  button('Flip vertical', change(e => ({...e,flipY:!e.flipY})));
  button('Reset image', change(() => null));
}

async function cropDialog(snapshot, apply, rotate) {
  const original = new Image(); original.src = snapshot.src; await original.decode();
  let edit = structuredClone(snapshot.imageEdit || defaults()); delete edit.src;
  const dialog = document.createElement('dialog'); dialog.className = 'editor-dialog image-crop-dialog';
  dialog.setAttribute('aria-labelledby','crop-heading');
  dialog.innerHTML = `<form method="dialog"><h2 id="crop-heading">Crop & transform image</h2>
    <p class="field-help">Drag the crop edges or use the percentage fields. Your original image is kept.</p>
    <div class="crop-previews"><figure><figcaption>Original · crop area</figcaption><div class="crop-stage"><div class="crop-window" tabindex="0" aria-label="Move crop area"></div></div></figure>
    <figure><figcaption>Result</figcaption><canvas class="crop-result" aria-label="Edited image preview"></canvas></figure></div>
    <div class="crop-fields">${['Left','Top','Width','Height'].map((label,i) => `<label>${label} (%)<input type="number" data-crop-field="${i}" aria-label="Crop ${label.toLowerCase()} (%)" min="0" max="100" step="0.01" required></label>`).join('')}</div>
    <div class="crop-actions"><button type="button" data-action="rotate">Rotate 90°</button><button type="button" data-action="flipX">Flip horizontal</button><button type="button" data-action="flipY">Flip vertical</button><button type="button" data-action="crop">Reset crop</button><button type="button" data-action="reset">Reset image</button></div>
    <p class="crop-error" role="alert"></p><div class="dialog-actions"><button value="cancel" formnovalidate>Cancel</button><button value="apply" class="primary">Apply image</button></div></form>`;
  const $ = selector => dialog.querySelector(selector), stage = $('.crop-stage'), region = $('.crop-window');
  const fields = [...dialog.querySelectorAll('[data-crop-field]')];
  stage.style.width = `min(100%,${45*original.naturalWidth/original.naturalHeight}dvh)`;
  original.alt = 'Original image'; original.draggable = false; stage.prepend(original);
  for (const [direction,label] of [['nw','top left'],['n','top'],['ne','top right'],['e','right'],['se','bottom right'],['s','bottom'],['sw','bottom left'],['w','left']]) {
    const handle = document.createElement('button'); handle.type = 'button'; handle.dataset.edge = direction;
    handle.setAttribute('aria-label',`Crop ${label} edge`); region.append(handle);
  }
  const limit = crop => {
    const minW = 1/original.naturalWidth, minH = 1/original.naturalHeight;
    const x = clamp(crop[0],0,1-minW), y = clamp(crop[1],0,1-minH);
    return [x,y,clamp(crop[2],minW,1-x),clamp(crop[3],minH,1-y)];
  };
  const update = () => {
    const [x,y,w,h] = edit.crop;
    Object.assign(region.style,{left:`${x*100}%`,top:`${y*100}%`,width:`${w*100}%`,height:`${h*100}%`});
    fields.forEach((field,i) => {field.value = +(edit.crop[i]*100).toFixed(2);});
    for (const key of ['flipX','flipY']) $(`[data-action="${key}"]`).setAttribute('aria-pressed',String(edit[key]));
    paint($('.crop-result'),original,edit,640);
  };
  const move = (crop, edge, dx, dy) => {
    const [x,y,w,h] = crop, minW = 1/original.naturalWidth, minH = 1/original.naturalHeight;
    if (!edge) return [clamp(x+dx,0,1-w),clamp(y+dy,0,1-h),w,h];
    const left = edge.includes('w') ? clamp(x+dx,0,x+w-minW) : x;
    const top = edge.includes('n') ? clamp(y+dy,0,y+h-minH) : y;
    const right = edge.includes('e') ? clamp(x+w+dx,x+minW,1) : x+w;
    const bottom = edge.includes('s') ? clamp(y+h+dy,y+minH,1) : y+h;
    return [left,top,right-left,bottom-top];
  };
  let drag;
  region.onpointerdown = event => {
    if (event.button !== 0) return;
    event.preventDefault(); region.setPointerCapture(event.pointerId);
    drag = {crop:[...edit.crop],edge:event.target.dataset.edge || '',x:event.clientX,y:event.clientY};
  };
  region.onpointermove = event => {
    if (!drag) return;
    const box = stage.getBoundingClientRect();
    edit.crop = move(drag.crop,drag.edge,(event.clientX-drag.x)/box.width,(event.clientY-drag.y)/box.height); update();
  };
  region.onpointerup = () => {drag = null;};
  region.onpointercancel = () => {if (drag) {edit.crop = drag.crop;drag = null;update();}};
  region.onkeydown = event => {
    if (!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown'].includes(event.key)) return;
    event.preventDefault(); const step = event.shiftKey ? 10 : 1;
    edit.crop = move(edit.crop,event.target.dataset.edge || '',
      (event.key === 'ArrowRight' ? step : event.key === 'ArrowLeft' ? -step : 0)/original.naturalWidth,
      (event.key === 'ArrowDown' ? step : event.key === 'ArrowUp' ? -step : 0)/original.naturalHeight);update();
  };
  fields.forEach((field,i) => {field.oninput = () => {
    if (!Number.isFinite(field.valueAsNumber)) return;
    const crop = [...edit.crop];crop[i] = field.valueAsNumber/100;edit.crop = limit(crop);update();
  };});
  for (const action of ['rotate','flipX','flipY','crop','reset']) $(`[data-action="${action}"]`).onclick = () => {
    if (action === 'rotate') edit = rotate(edit);
    else if (action === 'crop') edit.crop = [0,0,1,1];
    else if (action === 'reset') edit = defaults();
    else edit[action] = !edit[action];
    update();
  };
  let applying = false;
  dialog.oncancel = event => {if (applying) event.preventDefault();};
  $('form').onsubmit = async event => {
    if (event.submitter?.value !== 'apply') return;
    event.preventDefault();if (applying) return;
    applying = true;
    const controls = [...dialog.querySelectorAll('button,input')];controls.forEach(el => {el.disabled = true;});
    try { if (await apply(snapshot,edit)) dialog.close(); }
    catch (error) {$('.crop-error').textContent = error.message;}
    finally {applying = false;controls.forEach(el => {el.disabled = false;});}
  };
  dialog.addEventListener('keydown', event => event.stopPropagation());
  dialog.addEventListener('close', () => dialog.remove(),{once:true});
  document.body.append(dialog);dialog.showModal();update();
}
