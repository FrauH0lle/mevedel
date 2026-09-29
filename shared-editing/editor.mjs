/* Packaged editor in an opaque iframe. All host access uses the bound port. */
import * as Y from 'yjs';
import { BoardPresence, BoardPreviews } from './presence.mjs';
import { AssistantPanel } from './assistant.mjs';
import { documentControls } from './document-controls.mjs';
import { imageTools } from './image-controls.mjs';
import { captureContext, readComments } from './context.mjs';
import { Editor, Extension } from '@tiptap/core';
import { Plugin, PluginKey } from '@tiptap/pm/state';
import { Decoration, DecorationSet } from '@tiptap/pm/view';
import Collaboration from '@tiptap/extension-collaboration';
import CollaborationCaret from '@tiptap/extension-collaboration-caret';
import { Awareness, applyAwarenessUpdate, removeAwarenessStates } from 'y-protocols/awareness';
import * as encoding from 'lib0/encoding';
import { ObservableV2 } from 'lib0/observable';
import {
  absolutePositionToRelativePosition,
  relativePositionToAbsolutePosition,
  defaultDeleteFilter,
  ySyncPluginKey,
} from '@tiptap/y-tiptap';
import { extensions, schema, seedEmptyText, selectionPositions, validateDocument } from './document.mjs';
import { restore, encode, inspect, putShape, validateShape, validate } from './model.mjs';
import { validateImage } from './image.mjs';
import { shapeSVG, escape, bounds, extent, shapesInRegion, styleOf, FILLABLE, LINEAR } from './render.mjs';
const $ = (id) => document.getElementById(id),
  remote = Symbol('remote'),
  local = Symbol('local');
const bytes = (text) => Uint8Array.from(atob(text), (c) => c.charCodeAt(0));
const discussionHighlight = new PluginKey('discussionHighlight');
const b64 = (data) => {
  let value = '';
  for (let i = 0; i < data.length; i += 8192)
    value += String.fromCharCode(...data.subarray(i, i + 8192));
  return btoa(value);
};
let port,
  doc,
  item,
  editor,
  undo,
  awareness,
  readOnly = true,
  online = true,
  failed = false,
  revision = 0,
  pending = [],
  updates = [],
  bufferedId = null,
  inflight = false,
  initialized = false;
let tool = 'select',
  selected = new Set(),
  /* Board area [x, y, w, h] from the last box selection, kept with that selection. */
  selectionRegion = null,
  view = [-40, -40, 1000, 650],
  drag = null,
  lastPresence = 0,
  pointing = null,
  presenceTimer = null,
  laserSamples = [],
  boardPresence,
  boardPreviews,
  outgoingPreview = null,
  refresh = () => {};
/* Style of the next drawn shape; the panel edits it alongside the selection. */
const current = {
  stroke: '#242424',
  fill: 'none',
  pattern: 'hachure',
  width: 2,
  dash: 'solid',
  rough: 1,
  edges: 'round',
  opacity: 100,
  fontSize: 24,
};
const drawable = ['rect', 'diamond', 'ellipse', 'cylinder', 'sticky', 'arrow', 'line', 'pen', 'text'];
function applies(key, type) {
  if (key === 'fill' || key === 'pattern') return FILLABLE.includes(type);
  if (key === 'edges') return type === 'rect';
  if (key === 'fontSize') return !LINEAR.includes(type) && type !== 'image';
  if (key === 'width') return !['text', 'image'].includes(type);
  if (key === 'dash' || key === 'rough') return !['text', 'pen', 'image'].includes(type);
  if (key === 'stroke') return type !== 'image';
  return true;
}
const replies = new Map();
let documentSelection = null,
  captureDocumentSelection = () => {},
  assistant,
  comments = [];
let recoveryWarning = '';
let participant = 'You',
  textEditing = null,
  sceneSignature = '';
const agentTargets = new Map();
let seenRevision = null, highlightTimer;
const highlightDuration = 8000;
function status(text, error = false) {
  $('saved').textContent = text;
  $('saved').dataset.error = String(error);
}
function saved() {
  if (failed) return;
  if (recoveryWarning) {
    status(recoveryWarning, true);
    return;
  }
  status(
    pending.length || updates.length
      ? online
        ? 'Saving…'
        : 'Offline · changes pending'
      : online
        ? boardPreviews?.people.size ? 'Live movement · not saved yet' : 'Saved on host'
        : 'Offline · saved copy',
  );
}
function request(args) {
  const reqId = crypto.randomUUID();
  return new Promise((resolve, reject) => {
    replies.set(reqId, { resolve, reject });
    port.postMessage({ type: 'request', reqId, args });
  });
}
function draft() {
  port.postMessage({
    type: 'draft',
    draft: {
      id: item.id,
      kind: item.kind,
      title: doc.getMap('meta').get('title'),
      crdt: b64(encode(doc)),
      pending: updates.length
        ? [...pending, { opId: bufferedId, update: b64(Y.mergeUpdates(updates)) }]
        : pending,
      revision,
      assistant: assistant?.drafts,
    },
  });
}
function flush() {
  // Keep accumulating while an earlier operation awaits acknowledgement.
  // Its identity must stay stable for retries; unsent edits can share one save.
  if (updates.length && !pending.length) {
    pending.push({ opId: bufferedId, update: b64(Y.mergeUpdates(updates)) });
    updates = [];
    bufferedId = null;
    draft();
  }
  saved();
  pump();
}
async function pump() {
  if (!online || failed || inflight || readOnly || !pending.length) return;
  inflight = true;
  const next = pending[0];
  try {
    const result = await request({ action: 'update', ...next });
    revision = Math.max(revision, result.revision);
    pending.shift();
    if (outgoingPreview?.opId === next.opId) {
      outgoingPreview = null;
      if (pointing) presence(pointing.point, pointing.mode);
    }
    draft();
  } catch (error) {
    failed = online;
    outgoingPreview = null;
    if (pointing) presence(pointing.point, pointing.mode);
    status(error.message, true);
  } finally {
    inflight = false;
    saved();
    if (!failed) flush();
  }
}
function refreshAgentHighlights() {
  clearTimeout(highlightTimer);
  const now = performance.now();
  for (const [id, target] of agentTargets)
    if (now - target.started >= highlightDuration) agentTargets.delete(id);
  if (editor) {
    editor.view.dispatch(editor.state.tr.setMeta('addToHistory', false));
    // Decorations can reuse their DOM or be recreated after a toggle. Keep
    // either animation at the edit's actual age, without restarting its timer.
    for (const node of editor.view.dom.querySelectorAll('.agent-contribution'))
      for (const animation of node.getAnimations())
        if (animation.animationName === 'document-contribution')
          animation.currentTime = now - Number(node.dataset.agentStart);
  } else if (doc) draw();
  if (agentTargets.size)
    highlightTimer = setTimeout(refreshAgentHighlights,
      Math.max(1, Math.min(...[...agentTargets.values()].map(t => t.started + highlightDuration - now))));
}
function history(transactions = []) {
  boardPreviews?.reconcile(transactions);
  const previous = new Map(agentTargets);
  agentTargets.clear();
  const seen = new Set();
  for (const tx of transactions)
    for (const change of tx.changes || []) {
      if (!seen.has(change.id) && tx.actor.startsWith('Agent:') && change.after) {
        const prior = previous.get(change.id);
        if (prior?.revision === tx.revision) agentTargets.set(change.id, prior);
        else if (seenRevision !== null && tx.revision > seenRevision)
          agentTargets.set(change.id, {revision: tx.revision, started: performance.now(),
            label: `${tx.actor} · revision ${tx.revision}`});
      }
      seen.add(change.id);
    }
  // Initial history is attribution, not a new edit. Duplicate syncs never
  // restart the local timer, and host/browser wall-clock differences do not matter.
  seenRevision = Math.max(seenRevision ?? 0, ...transactions.map(tx => tx.revision));
  refreshAgentHighlights();
  $('contributions').replaceChildren();
  if (transactions.length) {
    const note = document.createElement('p');
    note.className = 'hint';
    note.textContent = `Retained contributions from revision ${transactions.at(-1).revision}. Older entries expire within count and size limits.`;
    $('contributions').append(note);
  }
  const groups = [];
  for (const tx of transactions) {
    const previous = groups.at(-1);
    if (
      previous &&
      !tx.actor.startsWith('Agent:') &&
      previous[0].actor === tx.actor &&
      previous.at(-1).time - tx.time >= 0 &&
      previous.at(-1).time - tx.time < 5000
    )
      previous.push(tx);
    else groups.push([tx]);
  }
  for (const group of groups) {
    const tx = group[0];
    const row = document.createElement('div');
    row.append(
      document.createTextNode(
        `${tx.actor} · ${group.length > 1 ? `revisions ${group.at(-1).revision}–${tx.revision}` : `revision ${tx.revision}`} `,
      ),
    );
    if (!readOnly && tx.actor.startsWith('Agent:')) {
      const button = document.createElement('button');
      button.textContent = 'Revert';
      button.onclick = async () => {
        try {
          await request({
            action: 'revert',
            transaction: tx.id,
            opId: crypto.randomUUID(),
          });
        } catch (e) {
          status(e.message, true);
        }
      };
      row.append(button);
    }
    $('contributions').append(row);
  }
}
function shapeList() {
  return inspect(doc).content;
}
function transformGeometry(geometry, box) {
  const old = geometry.box;
  return {
    box,
    ...(geometry.points
      ? {
          points: geometry.points.map(([x, y]) => [
            box[0] + (x - old[0]) * (old[2] ? box[2] / old[2] : 1),
            box[1] + (y - old[1]) * (old[3] ? box[3] / old[3] : 1),
          ]),
        }
      : {}),
  };
}
function dragGeometry(d) {
  if (!d?.end || !['move', 'resize'].includes(d.mode)) return [];
  return (d.mode === 'move' ? d.boxes : [[d.id, d.before]]).map(([id, g]) => [
    id, transformGeometry(g, d.mode === 'move'
      ? [g.box[0] + d.end[0] - d.start[0], g.box[1] + d.end[1] - d.start[1], ...g.box.slice(2)]
      : [...g.box.slice(0, 2), Math.max(10, d.end[0] - g.box[0]), Math.max(10, d.end[1] - g.box[1])]),
  ]);
}
function visibleGeometry(id) {
  const geometry = doc.getMap('shapes').get(id).get('geometry');
  const box = boardPreviews?.boxes().get(id);
  return box ? transformGeometry(geometry,box) : geometry;
}
function draw() {
  const live = boardPreviews?.boxes() || new Map();
  const preview = new Map(dragGeometry(drag));
  const shapes = shapeList().map(s => preview.has(s.id) ? {...s, ...preview.get(s.id)}
    : live.has(s.id) ? {...s, ...transformGeometry(s,live.get(s.id))} : s);
  $('canvas').setAttribute('viewBox', view.join(' '));
  const scale = $('canvas').getScreenCTM()?.a || 1;
  if ($('board-zoom')) $('board-zoom').textContent = `${Math.round(scale * 100)}%`;
  const signature = JSON.stringify([shapes, scale, [...agentTargets], $('show-agent').checked]);
  if (signature !== sceneSignature) {
    sceneSignature = signature;
    $('scene').innerHTML = shapes.map((s) => shapeSVG(s, shapes)).join('');
    for (const [index, group] of $('scene').querySelectorAll('[data-shape]').entries()) {
      const shape = shapes[index];
      group.dataset.agent = String(agentTargets.has(shape.id) && $('show-agent').checked);
      const target = agentTargets.get(shape.id);
      if (target) {
        group.style.setProperty('--highlight-delay', `${target.started - performance.now()}ms`);
        const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
        title.textContent = target.label;
        group.append(title);
      }
      if (LINEAR.includes(shape.type)) {
        const hit = group.querySelector('path').cloneNode();
        hit.setAttribute('stroke', 'transparent');
        hit.setAttribute('stroke-width', Math.max(shape.width || 2, 14 / scale));
        hit.style.pointerEvents = 'stroke';
        group.prepend(hit);
      } else if (shape.type === 'text') {
        const hit = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        ['x', 'y', 'width', 'height'].forEach((key, i) => hit.setAttribute(key, shape.box[i]));
        hit.setAttribute('fill', 'transparent');
        group.prepend(hit);
      }
    }
  }
  for (const group of $('scene').querySelectorAll('[data-shape]'))
    {
      group.dataset.editing = String(group.dataset.shape === textEditing);
      group.dataset.livePreview = String(live.has(group.dataset.shape));
    }
  layoutShapeText();
  for (const id of selected) if (!doc.getMap('shapes').has(id)) selected.delete(id);
  const selectedImage = selected.size === 1 && shapes.find(s => selected.has(s.id) && s.type === 'image');
  $('image-tools').hidden = readOnly || !selectedImage;
  $('image-size').textContent = selectedImage ? `${Math.round(selectedImage.box[2])} × ${Math.round(selectedImage.box[3])} px` : '';
  $('selection').innerHTML = shapes
    .filter((s) => selected.has(s.id))
    .map((s) => {
      const [x, y, w, h] = s.box;
      const gap = 4 / scale, handle = 10 / scale;
      return `<rect x="${x - gap}" y="${y - gap}" width="${w + gap * 2}" height="${h + gap * 2}" fill="none" stroke="var(--board-selection)" stroke-width="1.5" vector-effect="non-scaling-stroke"/><rect data-resize="${escape(s.id)}" x="${x + w - handle / 2}" y="${y + h - handle / 2}" width="${handle}" height="${handle}" fill="white" stroke="var(--board-selection)" vector-effect="non-scaling-stroke"/>`;
    })
    .join('') + regionSVG(drag?.mode === 'marquee' ? drag.region : selectionRegion, drag?.mode === 'marquee');
  boardPresence?.animate();
  $('selection-question').disabled = $('comment-selection').disabled = readOnly || !(selected.size || selectionRegion);
  refresh();
}
function regionSVG(region, active) {
  if (!region) return '';
  const [x, y, w, h] = region;
  return `<rect class="selection-region" data-active="${active}" x="${x}" y="${y}" width="${w}" height="${h}" vector-effect="non-scaling-stroke"/>`;
}
/* Normalized, integer region between two board points. */
function regionBetween(a, b) {
  const x = Math.floor(Math.min(a[0], b[0])), y = Math.floor(Math.min(a[1], b[1]));
  return [x, y, Math.ceil(Math.max(a[0], b[0])) - x, Math.ceil(Math.max(a[1], b[1])) - y];
}
/* A box selection selects the shapes it contains, or touches with Alt. */
function marqueeSelection(d) {
  const hits = shapesInRegion(shapeList(), d.region, d.touching).map((s) => s.id);
  return new Set(d.additive ? [...d.base, ...hits] : hits);
}
function world(event) {
  const p = new DOMPoint(event.clientX, event.clientY).matrixTransform(
    $('canvas').getScreenCTM().inverse(),
  );
  return [Math.max(-999000, Math.min(999000, p.x)), Math.max(-999000, Math.min(999000, p.y))];
}
function presence(point, mode = 'cursor') {
  if (readOnly || !online) return;
  const now = performance.now();
  laserSamples = mode === 'laser' ? laserSamples.filter(s => now - s[2] < 550).slice(-63) : [];
  if (mode === 'laser' && (!laserSamples.length || Math.floor(now / 8) !== Math.floor(laserSamples.at(-1)[2] / 8)))
    laserSamples.push([...point, now]);
  else if (mode === 'laser') laserSamples[laserSamples.length - 1] = [...point, now];
  pointing = { point, mode };
  if (mode === 'laser') showPresence({ peer: 'self', name: participant, point, mode,
    trail: laserSamples.map(([x,y,time]) => [x,y,now-time]) });
  // Coalesce a burst, but always deliver its final position even if motion stops.
  if (presenceTimer) return;
  presenceTimer = setTimeout(() => {
    presenceTimer = null;
    lastPresence = performance.now();
    if (pointing && online) port.postMessage({ type: 'presence', ...pointing,
      preview: pointing.mode === 'cursor' && !failed ? outgoingPreview : null,
      trail: pointing.mode === 'laser' ? laserSamples.filter(s => lastPresence - s[2] < 550)
        .map(([x,y,time]) => [x,y,lastPresence-time]) : undefined });
  }, Math.max(0, 50 - (performance.now() - lastPresence)));
}
function stopPointing() {
  clearTimeout(presenceTimer);
  presenceTimer = null;
  if (pointing && online) port.postMessage({ type: 'presence', mode: 'clear', point: null });
  pointing = null;
  laserSamples = [];
  boardPresence?.clear('self');
}
function clearPresence() {
  outgoingPreview = null;
  stopPointing();
  boardPresence?.clear();
  boardPreviews?.clear();
}
function showPresence(data) {
  if (awareness && Number.isSafeInteger(data.clientId) && data.clientId !== doc.clientID) {
    try {
      const update = encoding.createEncoder();
      encoding.writeVarUint(update, 1);
      encoding.writeVarUint(update, data.clientId);
      encoding.writeVarUint(update, data.clock);
      encoding.writeVarString(
        update,
        JSON.stringify(
          data.mode === 'clear'
            ? null
            : {
                cursor: data.cursor,
                user: { name: data.name, color: '#a22458' },
              },
        ),
      );
      applyAwarenessUpdate(awareness, encoding.toUint8Array(update), remote);
    } catch (_) {
      /* Malformed presence cannot affect shared content. */
    }
  }
  boardPresence?.receive(data);
  boardPreviews?.receive(data);
}

function selectTool(value) {
  stopPointing();
  $('menu').open = false;
  tool = value;
  document
    .querySelectorAll('[data-tool]')
    .forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.tool === value)));
  $('canvas').style.cursor =
    value === 'pan' ? 'grab' : value === 'select' ? 'default' : 'crosshair';
  if (drawable.includes(value)) { selected.clear(); selectionRegion = null; }
  draw();
}
function button(parent, label, action) {
  const b = document.createElement('button');
  b.type = 'button';
  b.textContent = label;
  b.onclick = action;
  parent.append(b);
  return b;
}
function layoutShapeText() {
  if (!textEditing) return;
  const shape = shapeList().find(s => s.id === textEditing);
  if (!shape) { $('shape-text').blur(); return; }
  const input = $('shape-text'), [x, y, w, h] = shape.box;
  const matrix = $('canvas').getScreenCTM(), size = styleOf(shape).fontSize;
  const centered = !['text', ...LINEAR].includes(shape.type);
  const height = Math.max(1, input.value.split('\n').length) * size * 1.375;
  const point = new DOMPoint(centered ? x : x + 10,
    centered ? y + (h - height) / 2 : y + size * .4375).matrixTransform(matrix);
  Object.assign(input.style, {
    left: `${point.x}px`, top: `${point.y}px`, width: `${w * matrix.a}px`,
    height: `${height * matrix.a}px`, fontSize: `${size * matrix.a}px`,
    textAlign: centered ? 'center' : 'left', color: styleOf(shape).stroke,
  });
}
function editText(id) {
  if (readOnly) return;
  const shape = doc.getMap('shapes').get(id);
  if (!shape) return;
  const input = $('shape-text');
  textEditing = id;
  undo?.stopCapturing();
  const before = shape.get('text') || '';
  input.value = before;
  draw();
  input.hidden = false;
  input.focus();
  input.select();
  const finish = (save) => {
    if (textEditing !== id) return;
    textEditing = null;
    input.hidden = true;
    const current = doc.getMap('shapes').get(id);
    if (!save && current?.get('text') === input.value)
      doc.transact(() => current.set('text', before), local);
    undo?.stopCapturing();
    draw();
  };
  input.oninput = () => {
    const current = doc.getMap('shapes').get(id);
    if (current) doc.transact(() => current.set('text', input.value), local);
  };
  input.onblur = () => finish(true);
  input.onkeydown = (event) => {
    if (event.key === 'Escape' || (event.key === 'Enter' && (event.ctrlKey || event.metaKey))) {
      event.preventDefault();
      finish(event.key !== 'Escape');
      $('canvas').focus();
    }
  };
}
async function insertImages(files, position) {
  if (readOnly || !files.length) return;
  try {
    const binding = editor && ySyncPluginKey.getState(editor.state).binding;
    const anchor = binding && absolutePositionToRelativePosition(
      position ?? editor.state.selection.from, binding.type, binding.mapping);
    const origin = editor ? null : position || [view[0] + 50, view[1] + 50];
    const images = [];
    for (const file of files) {
      if (file.size > 4 * 1024 * 1024 || !/^image\/(png|jpeg|webp)$/.test(file.type))
        throw new Error('Use a PNG, JPEG, or WebP image up to 4 MB');
      const src = await new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.onload = () => resolve(reader.result);
        reader.onerror = () => reject(new Error('Could not read image'));
        reader.readAsDataURL(file);
      });
      validateImage(src);
      const bitmap = await createImageBitmap(file);
      const scale = Math.min(1, 640 / bitmap.width);
      images.push({src, alt: file.name, width: bitmap.width * scale, height: bitmap.height * scale});
      bitmap.close();
    }
    if (readOnly) return;
    undo.stopCapturing();
    if (editor) {
      const current = ySyncPluginKey.getState(editor.state).binding;
      const at = relativePositionToAbsolutePosition(doc, current.type, anchor, current.mapping);
      if (at === null) throw new Error('The image insertion point was removed; try again');
      const content = images.map(attrs => ({type:'image', attrs:{...attrs, id:crypto.randomUUID()}}));
      validateDocument({...editor.getJSON(), content:[...editor.getJSON().content, ...content]});
      editor.chain().focus().insertContentAt(at, content).run();
    } else {
      const shapes = images.map((image, index) => ({
        id: crypto.randomUUID(), type: 'image', src: image.src,
        box: [origin[0] + index * 24, origin[1] + index * 24, image.width, image.height],
      }));
      doc.transact(() => shapes.forEach(shape => putShape(doc, shape)), local);
      selected = new Set(shapes.map(shape => shape.id));
      selectTool('select');
    }
    undo.stopCapturing();
  } catch (error) {
    status(error.message, true);
  }
}
function imageControls() {
  const surface = editor ? $('document') : $('board');
  if (!readOnly) {
    const input = document.createElement('input');
    input.id = 'image-upload';
    input.type = 'file';
    input.accept = 'image/png,image/jpeg,image/webp';
    input.multiple = true;
    input.hidden = true;
    $('menu').append(input);
    const choose = () => { $('menu').open = false; input.click(); };
    if (!editor) $('board-image').onclick = choose;
    input.onchange = () => { const files = [...input.files]; input.value = ''; insertImages(files); };
  }
  surface.addEventListener('dragover', event => {
    if (!event.dataTransfer?.types.includes('Files')) return;
    event.preventDefault();
    event.dataTransfer.dropEffect = readOnly ? 'none' : 'copy';
    surface.classList.toggle('image-drop-target', !readOnly);
  });
  surface.addEventListener('dragleave', event => {
    if (!surface.contains(event.relatedTarget)) surface.classList.remove('image-drop-target');
  });
  surface.addEventListener('drop', event => {
    const files = [...(event.dataTransfer?.files || [])];
    if (!files.length) return;
    event.preventDefault();
    event.stopPropagation();
    surface.classList.remove('image-drop-target');
    const at = editor ? editor.view.posAtCoords({left:event.clientX, top:event.clientY})?.pos : world(event);
    if (editor && at == null) return;
    insertImages(files, at);
  }, true);
  surface.addEventListener('paste', event => {
    const files = [...(event.clipboardData?.files || [])];
    if (!files.length) return;
    event.preventDefault();
    event.stopPropagation();
    insertImages(files);
  }, true);
}
function board() {
  document.body.dataset.kind = 'whiteboard';
  $('board').hidden = false;
  imageTools($('image-tools'), () => {
    const shape = selected.size === 1 && shapeList().find(s => selected.has(s.id) && s.type === 'image');
    return !readOnly && shape ? {...shape,width:shape.box[2],height:shape.box[3]} : null;
  }, (before, changes) => {
    const current = shapeList().find(s => s.id === before.id);
    if (readOnly || !current || ['src','imageEdit','box'].some(key =>
      JSON.stringify(current[key]) !== JSON.stringify(before[key])))
      throw new Error('This image changed while you were editing it. Reopen the image tools to try again.');
    const after = {...current,imageEdit:changes.imageEdit,
      box:[...current.box.slice(0,2),changes.width,changes.height]};
    const candidate = restore(encode(doc));
    try {putShape(candidate,after);validate(candidate);} finally {candidate.destroy();}
    undo.stopCapturing();
    doc.transact(() => putShape(doc,after),local);
    undo.stopCapturing();
  },message => status(message,true));
  const tools = [
    ['pan', 'Hand', 'H'],
    ['select', 'Selection', 'V'],
    ['rect', 'Rectangle', 'R'],
    ['diamond', 'Diamond', 'D'],
    ['ellipse', 'Ellipse', 'O'],
    ['cylinder', 'Database', 'C'],
    ['sticky', 'Sticky note', 'S'],
    ['arrow', 'Arrow', 'A'],
    ['line', 'Line', 'L'],
    ['pen', 'Draw', 'P'],
    ['text', 'Text', 'T'],
    ['erase', 'Eraser', 'E'],
    ['laser', 'Laser pointer', 'K'],
  ];
  const icons = {
    pan: 'M8 13V6a2 2 0 0 1 4 0v6-8a2 2 0 0 1 4 0v8-6a2 2 0 0 1 4 0v9c0 5-3 7-7 7-3 0-5-2-7-5l-3-4a2 2 0 0 1 3-2l2 2',
    select: 'm5 3 14 10-7 1-3 7Z',
    rect: 'M4 4h16v16H4Z',
    diamond: 'm12 3 9 9-9 9-9-9Z',
    ellipse: 'M21 12a9 7 0 1 0-18 0 9 7 0 1 0 18 0',
    cylinder: 'M4 6c0-5 16-5 16 0s-16 5-16 0v12c0 5 16 5 16 0V6',
    sticky: 'M4 4h16v11l-5 5H4Zm11 16v-5h5',
    arrow: 'M4 20 20 4M10 4h10v10',
    line: 'M4 20 20 4',
    pen: 'M3 18c5-20 4 12 10-5s8-8 8-8',
    text: 'M4 5h16M12 5v15M8 20h8',
    erase: 'm3 15 12-12 7 7-12 12H9Zm6-6 7 7M10 22h12',
    laser: 'm3 21 10-10 3 3L6 24ZM17 7l3-3M14 5V2M21 10h3',
  };
  const commands = document.createElement('div');
  commands.className = 'board-commands';
  $('tools').append(commands);
  const objectMenu = document.createElement('details');
  objectMenu.className = 'popover board-menu';
  objectMenu.id = 'object-menu';
  objectMenu.innerHTML = '<summary>Objects</summary><div class="editor-menu"></div>';
  commands.append(objectMenu);
  const strip = document.createElement('div');
  strip.className = 'drawing-tools';
  strip.setAttribute('role', 'group');
  strip.setAttribute('aria-label', 'Create and select');
  $('tools').append(strip);
  for (const [value, label, key] of tools) {
    if (readOnly && !['select', 'pan'].includes(value)) continue;
    const b = button(strip, '', () => selectTool(value));
    b.dataset.tool = value;
    if (['rect', 'arrow', 'erase'].includes(value)) b.classList.add('tool-group-start');
    b.title = `${label} (${key})`;
    b.setAttribute('aria-label', label);
    b.setAttribute('aria-keyshortcuts', key);
    b.setAttribute('aria-pressed', String(value === 'select'));
    b.innerHTML = `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="${icons[value]}"/></svg><kbd>${key}</kbd>`;
  }
  if (!readOnly) {
    const imageButton = button(strip, '', () => {});
    imageButton.id = 'board-image';
    imageButton.title = 'Insert image (or drop an image on the canvas)';
    imageButton.setAttribute('aria-label', 'Insert image');
    imageButton.className = 'tool-group-start';
    imageButton.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><rect x="3" y="3" width="18" height="18" rx="2"/><circle cx="8" cy="8" r="1.5"/><path d="m3 18 6-6 4 4 3-4 5 6"/></svg>';
  }
  const properties = document.createElement('details');
  properties.id = 'properties';
  properties.className = 'popover';
  properties.innerHTML = '<summary>Style</summary><div class="menu-body"></div>';
  commands.append(properties);
  const panel = properties.lastElementChild;
  const value = (key) => {
    for (const s of shapeList()) if (selected.has(s.id) && applies(key, s.type)) return styleOf(s)[key];
    return current[key];
  };
  const style = (key, v) => {
    current[key] = v;
    doc.transact(() => {
      for (const id of selected) {
        const s = doc.getMap('shapes').get(id);
        if (s && applies(key, s.get('type'))) s.set(key, v);
      }
    }, local);
    refresh();
  };
  const remove = () =>
    doc.transact(() => {
      for (const id of selected) doc.getMap('shapes').delete(id);
    }, local);
  const reorder = (front) => {
    const layers = shapeList().map((s) => s.layer || 0),
      layer = front ? Math.max(...layers) + 1 : Math.min(...layers) - 1;
    doc.transact(() => {
      for (const id of selected) doc.getMap('shapes').get(id)?.set('layer', layer);
    }, local);
  };
  const duplicate = () => {
    // Copies keep their own geometry; a copied connector no longer follows the originals.
    const copies = shapeList()
      .filter((s) => selected.has(s.id))
      .map(({ from: _from, to: _to, ...s }) => ({
        ...s,
        id: crypto.randomUUID(),
        box: [s.box[0] + 10, s.box[1] + 10, s.box[2], s.box[3]],
        ...(s.points ? { points: s.points.map(([x, y]) => [x + 10, y + 10]) } : {}),
      }));
    if (!copies.length) return;
    doc.transact(() => copies.forEach((s) => putShape(doc, s)), local);
    selected = new Set(copies.map((s) => s.id));
    draw();
  };
  if (!readOnly) {
    const icon = (inner) => `<svg viewBox="0 0 24 24" aria-hidden="true">${inner}</svg>`;
    const option = (key, v, label, body) =>
      `<button type="button" class="opt" data-prop="${key}" data-val="${v}" title="${label}" aria-label="${label}">${body}</button>`;
    const swatch = (key, label, color) =>
      `<button type="button" class="swatch${color === 'none' ? ' transparent' : ''}"${color === 'none' ? '' : ` style="background:${color}"`} data-prop="${key}" data-val="${color}" aria-label="${label}: ${color === 'none' ? 'transparent' : color}"></button>`;
    const colors = (key, label, list) =>
      list.map((c) => swatch(key, label, c)).join('') +
      `<label class="swatch custom" title="Custom ${label.toLowerCase()} colour"><input type="color" data-color="${key}" aria-label="Custom ${label.toLowerCase()} colour"></label>`;
    const section = (key, title, body) =>
      `<div class="sec" data-sec="${key}"><h4>${title}</h4><div class="row">${body}</div></div>`;
    panel.innerHTML =
      section('stroke', 'Stroke', colors('stroke', 'Stroke', ['#242424', '#e03131', '#2f9e44', '#1971c2', '#7048e8'])) +
      section('fill', 'Background', colors('fill', 'Background', ['none', '#ffc9c9', '#b2f2bb', '#a5d8ff', '#fff1a8'])) +
      section(
        'pattern',
        'Fill',
        option('pattern', 'hachure', 'Hachure', icon('<rect x="4" y="4" width="16" height="16" rx="2"/><path d="M4 12l8-8M4 19 19 4M10 20 20 10" stroke-width="1.3"/>')) +
          option('pattern', 'cross', 'Cross-hatch', icon('<rect x="4" y="4" width="16" height="16" rx="2"/><path d="M4 12l8-8M4 19 19 4M10 20 20 10M12 4l8 8M5 5l14 14M4 12l8 8" stroke-width="1.3"/>')) +
          option('pattern', 'solid', 'Solid', icon('<rect x="4" y="4" width="16" height="16" rx="2" fill="currentColor"/>')),
      ) +
      section(
        'width',
        'Stroke width',
        [
          [1, 'Thin', 1.5],
          [2, 'Bold', 3],
          [4, 'Extra bold', 5],
        ]
          .map(([v, label, w]) => option('width', v, label, icon(`<path d="M5 12h14" stroke-width="${w}"/>`)))
          .join(''),
      ) +
      section(
        'dash',
        'Stroke style',
        option('dash', 'solid', 'Solid', icon('<path d="M5 12h14"/>')) +
          option('dash', 'dashed', 'Dashed', icon('<path d="M4 12h4M10 12h4M16 12h4"/>')) +
          option('dash', 'dotted', 'Dotted', icon('<path d="M5 12h.01M9.5 12h.01M14 12h.01M18.5 12h.01" stroke-width="2.4"/>')),
      ) +
      section(
        'rough',
        'Sloppiness',
        option('rough', 0, 'Architect', icon('<path d="M4 16 10 8l5 7 5-8"/>')) +
          option('rough', 1, 'Artist', icon('<path d="M4 16c2-3 3.5-8 6-8s2.5 7 5 7 3-6 5-8"/>')) +
          option('rough', 2, 'Cartoonist', icon('<path d="M3.5 16c1.5-2 2-9 5-8.5S9 17 12.5 15.5s1-8 3.5-8.5 2 5 4.5 2"/>')),
      ) +
      section(
        'edges',
        'Edges',
        option('edges', 'sharp', 'Sharp', icon('<path d="M5 19V5h14"/>')) +
          option('edges', 'round', 'Round', icon('<path d="M5 19v-8a6 6 0 0 1 6-6h8"/>')),
      ) +
      section(
        'fontSize',
        'Font size',
        [
          [16, 'S'],
          [24, 'M'],
          [32, 'L'],
          [44, 'XL'],
        ]
          .map(([v, label]) => option('fontSize', v, `Font size ${label}`, label))
          .join(''),
      ) +
      section('opacity', 'Opacity', '<input type="range" min="0" max="100" step="5" aria-label="Opacity"><output>100</output>');
    const caption = document.createElement('p');
    caption.className = 'property-caption';
    panel.prepend(caption);
    panel.onclick = (event) => {
      const b = event.target.closest('[data-prop]');
      if (b) {
        const key = b.dataset.prop;
        undo.stopCapturing();
        style(key, ['width', 'rough', 'fontSize'].includes(key) ? +b.dataset.val : b.dataset.val);
        undo.stopCapturing();
        return;
      }
    };
    for (const input of panel.querySelectorAll('input[data-color]'))
      input.oninput = () => style(input.dataset.color, input.value);
    const range = panel.querySelector('input[type=range]');
    range.oninput = () => style('opacity', +range.value);
    refresh = () => {
      const shapes = shapeList().filter((s) => selected.has(s.id));
      const kinds = new Set(
        shapes.length ? shapes.map((s) => s.type) : drawable.includes(tool) ? [tool] : [],
      );
      const has = (fn) => [...kinds].some(fn);
      const show = {
        stroke: has((k) => applies('stroke', k)),
        fill: has((k) => applies('fill', k)),
        pattern: has((k) => applies('pattern', k)) && value('fill') !== 'none',
        width: has((k) => applies('width', k)),
        dash: has((k) => applies('dash', k)),
        rough: has((k) => applies('rough', k)),
        edges: kinds.has('rect'),
        fontSize: kinds.has('text') || kinds.has('sticky') || shapes.some((s) => s.text),
        opacity: kinds.size > 0,
      };
      properties.firstElementChild.setAttribute('aria-disabled', String(!kinds.size));
      if (!kinds.size) properties.open = false;
      const name = tools.find(([value]) => value === (shapes[0]?.type || tool))?.[1] || 'Image';
      caption.textContent = shapes.length > 1 ? `${shapes.length} objects` : shapes.length ? name : `New ${name.toLowerCase()}`;
      for (const sec of panel.querySelectorAll('.sec')) sec.hidden = !show[sec.dataset.sec];
      for (const b of panel.querySelectorAll('[data-prop]'))
        b.setAttribute('aria-pressed', String(String(value(b.dataset.prop)) === b.dataset.val));
      for (const input of panel.querySelectorAll('input[data-color]')) {
        const v = value(input.dataset.color);
        if (v !== 'none') input.value = v;
      }
      range.value = value('opacity');
      range.nextElementSibling.value = value('opacity');
    };
  }
  properties.hidden = readOnly;
  const canvas = $('canvas');
  const actions = [];
  const all = () => { selected = new Set(shapeList().map(s => s.id)); selectionRegion = null; selectTool('select'); };
  const clear = () => { selected.clear(); selectionRegion = null; selectTool('select'); };
  const move = (dx, dy, step = 1) => doc.transact(() => {
    selectionRegion = null;
    for (const id of selected) {
      const shape = doc.getMap('shapes').get(id), geometry = shape.get('geometry'), box = geometry.box;
      shape.set('geometry', transformGeometry(geometry, [box[0] + dx * step, box[1] + dy * step, ...box.slice(2)]));
    }
  }, local);
  const action = (parent, label, run, enabled, shortcut = '', keepOpen = false) => {
    const b = button(parent, label, event => {
      undo?.stopCapturing();
      if (!keepOpen) { objectMenu.open = false; canvas.focus({preventScroll:true}); }
      run(event);
      undo?.stopCapturing();
    });
    b.setAttribute('aria-label', label);
    if (shortcut) { const key = document.createElement('kbd'); key.textContent = shortcut; b.append(key); }
    actions.push([b, enabled]);
    return b;
  };
  const objectBody = objectMenu.lastElementChild;
  action(objectBody, 'Select all', all, () => shapeList().length > 0, '⌘ / Ctrl A');
  action(objectBody, 'Clear selection', clear, () => selected.size > 0, 'Esc');
  if (!readOnly) {
    action(objectBody, 'Edit text', () => editText([...selected][0]), () => selected.size === 1, 'Enter').classList.add('menu-divider');
    action(objectBody, 'Duplicate', duplicate, () => selected.size > 0);
    const arrange = document.createElement('details');
    arrange.className = 'object-arrange';
    arrange.innerHTML = '<summary>Arrange & move</summary><div></div>';
    objectBody.append(arrange);
    action(arrange.lastElementChild, 'Bring to front', () => reorder(true), () => selected.size > 0);
    action(arrange.lastElementChild, 'Send to back', () => reorder(false), () => selected.size > 0);
    const nudges = document.createElement('div'); nudges.className = 'board-nudges';
    arrange.lastElementChild.append(nudges);
    for (const [label, dx, dy, glyph] of [['Move left',-1,0,'←'],['Move up',0,-1,'↑'],['Move down',0,1,'↓'],['Move right',1,0,'→']]) {
      const b = action(nudges, label, event => move(dx, dy, event.shiftKey ? 10 : 1), () => selected.size > 0, '', true);
      b.textContent = glyph; b.title = `${label} 1px (Shift: 10px)`;
    }
    const help = document.createElement('p'); help.textContent = 'Move 1px. Hold Shift for 10px.'; arrange.lastElementChild.append(help);
    action(objectBody, 'Delete', remove, () => selected.size > 0, 'Del').classList.add('danger');
  }
  const hints = {
    select: 'Drag across empty canvas to box-select (Alt: touched objects). Shift adds. Double-click edits text.',
    pan: 'Drag to move around the canvas. Scroll to zoom.',
    text: 'Click to place text. Ctrl / ⌘ Enter to finish.',
    sticky: 'Click to place a note. Ctrl / ⌘ Enter to finish.',
    arrow: 'Drag between objects to connect them. Connections follow the objects.',
    erase: 'Click an object to erase it. Undo restores it.',
    laser: 'Drag to point. The trail fades without changing the board.',
  };
  const refreshStyle = refresh;
  refresh = () => {
    refreshStyle();
    for (const [b, enabled] of actions) b.disabled = !enabled();
    const label = selected.size ? `${selected.size} object${selected.size === 1 ? '' : 's'} selected` : tools.find(([value]) => value === tool)?.[1];
    if ($('document-stat').textContent !== label) $('document-stat').textContent = label;
    $('hint').textContent = readOnly ? 'View only · Select objects or use the hand to explore.' : hints[tool] || 'Drag to draw. Choose Style to change the next object.';
  };
  for (const menu of [objectMenu, properties]) {
    const summary = menu.firstElementChild;
    summary.addEventListener('click', event => {
      event.preventDefault();
      if (summary.getAttribute('aria-disabled') === 'true') return;
      menu.open = !menu.open;
      if (!menu.open) return;
      (menu === objectMenu ? properties : objectMenu).open = false;
      const body = menu.lastElementChild;
      body.style.marginLeft = '0px';
      const rect = body.getBoundingClientRect();
      body.style.marginLeft = `${Math.max(8 - rect.left, Math.min(0, innerWidth - 8 - rect.right))}px`;
    });
    menu.addEventListener('keydown', event => {
      if (event.key === 'Escape') { event.stopPropagation(); menu.open = false; summary.focus(); }
    });
  }
  const shortcuts = document.createElement('dialog');
  shortcuts.id = 'board-shortcuts'; shortcuts.className = 'editor-dialog';
  shortcuts.setAttribute('aria-labelledby', 'board-shortcuts-title');
  shortcuts.innerHTML = `<form method="dialog"><h2 id="board-shortcuts-title">Whiteboard shortcuts</h2>
    <p>Click the canvas before using shortcuts.</p>
    <div class="shortcut-columns"><section><h3>Tools</h3><dl>${tools.filter(([value]) => !readOnly || ['pan','select'].includes(value)).map(([,label,key]) => `<div><dt>${label}</dt><dd><kbd>${key}</kbd></dd></div>`).join('')}</dl></section>
    <section><h3>Working on the canvas</h3><dl>
    <div><dt>Select all</dt><dd>Ctrl / ⌘ A</dd></div><div><dt>Clear selection</dt><dd>Esc</dd></div>
    <div><dt>Select multiple</dt><dd>Shift + click</dd></div><div><dt>Box-select contained objects</dt><dd>Drag on empty canvas</dd></div><div><dt>Box-select touched objects</dt><dd>Alt + drag</dd></div><div><dt>Add a box to the selection</dt><dd>Shift + drag</dd></div><div><dt>Pan</dt><dd>Middle-button drag</dd></div><div><dt>Zoom at pointer</dt><dd>Scroll</dd></div>
    ${readOnly ? '' : '<div><dt>Edit text</dt><dd>Enter / double-click</dd></div><div><dt>Finish text</dt><dd>Ctrl / ⌘ Enter</dd></div><div><dt>Cancel text</dt><dd>Esc</dd></div><div><dt>Resize</dt><dd>Drag the corner handle</dd></div><div><dt>Move 1px / 10px</dt><dd>Arrows / Shift + arrows</dd></div><div><dt>Delete</dt><dd>Del / Backspace</dd></div><div><dt>Undo / redo</dt><dd>Ctrl / ⌘ Z / Shift Z</dd></div><div><dt>Insert image</dt><dd>Drop / paste an image</dd></div>'}
    </dl></section></div><div class="dialog-actions"><button>Close</button></div></form>`;
  document.body.append(shortcuts);
  button($('tools'), 'Shortcuts', () => shortcuts.showModal()).className = 'board-help';
  const zoom = document.createElement('div');
  zoom.className = 'zoom-tools';
  zoom.setAttribute('role', 'group'); zoom.setAttribute('aria-label', 'Canvas zoom');
  $('tools').append(zoom);
  const zoomBy = factor => {
    const next = Math.max(100, Math.min(100000, view[2] * factor));
    factor = next / view[2];
    view = [view[0] + view[2] * (1 - factor) / 2, view[1] + view[3] * (1 - factor) / 2, next, view[3] * factor];
    draw();
  };
  button(zoom, 'Fit', () => { view = bounds(shapeList()); draw(); }).title = 'Fit all objects';
  button(zoom, '−', () => zoomBy(1.2)).setAttribute('aria-label', 'Zoom out');
  const percentage = button(zoom, '100%', () => zoomBy(canvas.getScreenCTM()?.a || 1));
  percentage.id = 'board-zoom'; percentage.title = 'Reset zoom to 100%'; percentage.setAttribute('aria-label', 'Reset zoom to 100%');
  button(zoom, '+', () => zoomBy(1 / 1.2)).setAttribute('aria-label', 'Zoom in');
  boardPresence = new BoardPresence(canvas, $('presence'));
  boardPreviews = new BoardPreviews(()=>{ draw(); saved(); });
  new ResizeObserver(() => draw()).observe(canvas);
  const hitAt = (event) =>
    document.elementFromPoint(event.clientX, event.clientY)?.closest('[data-shape]')?.dataset.shape;
  canvas.onpointerdown = (event) => {
    if (event.button !== 0 && event.button !== 1) return;
    event.preventDefault();
    canvas.focus();
    canvas.setPointerCapture(event.pointerId);
    undo?.stopCapturing();
    const point = world(event),
      id = hitAt(event),
      resize = event.target.dataset.resize;
    if (tool === 'laser' && !readOnly) {
      drag = { mode: 'laser' };
      presence(point, 'laser');
      return;
    }
    if (tool === 'pan' || event.button === 1) {
      drag = {
        mode: 'pan',
        start: [event.clientX, event.clientY],
        view: view.slice(),
      };
      return;
    }
    if (tool === 'select') {
      if (resize && !readOnly) {
        drag = {
          mode: 'resize',
          id: resize,
          start: point,
          before: visibleGeometry(resize),
        };
        return;
      }
      selectionRegion = null;
      if (!id) {
        // Empty canvas starts a box selection; a click without movement clears.
        drag = { mode: 'marquee', start: point, screen: [event.clientX, event.clientY],
          additive: event.shiftKey, base: new Set(selected), region: null };
        if (!event.shiftKey) selected.clear();
        draw();
        return;
      }
      if (event.shiftKey) {
        selected.has(id) ? selected.delete(id) : selected.add(id);
      } else if (!selected.has(id)) selected = new Set([id]);
      draw();
      if (!readOnly)
        drag = {
          mode: 'move',
          start: point,
          boxes: [...selected].map((key) => [key, visibleGeometry(key)]),
        };
      return;
    }
    if (readOnly) return;
    if (tool === 'erase') {
      if (id) doc.transact(() => doc.getMap('shapes').delete(id), local);
      return;
    }
    const shape = { id: crypto.randomUUID(), type: tool, box: [...point, 1, 1] };
    for (const [key, v] of Object.entries(current)) if (applies(key, tool)) shape[key] = v;
    if (tool === 'sticky' && shape.fill === 'none') {
      shape.fill = '#fff1a8';
      shape.pattern = 'solid';
    }
    drag = { mode: 'draw', start: point, points: [point], from: id, shape };
  };
  canvas.onpointermove = (event) => {
    const point = world(event);
    if (drag && ['move','resize'].includes(drag.mode)) {
      drag.end = point;
      outgoingPreview = {shapes:dragGeometry(drag).slice(0,100).map(([id,g])=>({id,box:g.box}))};
    }
    if (event.pointerType !== 'touch' || drag)
      presence(point, tool === 'laser' ? 'laser' : 'cursor');
    if (!drag) return;
    if (drag.mode === 'marquee') {
      if (Math.hypot(event.clientX - drag.screen[0], event.clientY - drag.screen[1]) < 4 && !drag.region) return;
      drag.region = regionBetween(drag.start, point);
      drag.touching = event.altKey;
      selected = marqueeSelection(drag);
      draw();
      return;
    }
    if (drag.mode === 'pan') {
      const scale = canvas.getScreenCTM().a;
      view = [
        drag.view[0] - (event.clientX - drag.start[0]) / scale,
        drag.view[1] - (event.clientY - drag.start[1]) / scale,
        ...drag.view.slice(2),
      ];
      draw();
      return;
    }
    if (drag.mode === 'draw') {
      drag.end = point;
      if (drag.points.length < 3999) drag.points.push(point);
      const s = drag.shape;
      s.box = [
        Math.min(point[0], drag.start[0]),
        Math.min(point[1], drag.start[1]),
        Math.abs(point[0] - drag.start[0]),
        Math.abs(point[1] - drag.start[1]),
      ];
      if (LINEAR.includes(s.type)) s.points = s.type === 'pen' ? drag.points : [drag.start, point];
      if (s.type === 'pen') {
        const xs = s.points.map((p) => p[0]),
          ys = s.points.map((p) => p[1]),
          x = Math.min(...xs),
          y = Math.min(...ys);
        s.box = [x, y, Math.max(...xs) - x, Math.max(...ys) - y];
      }
      draw();
      $('selection').innerHTML = shapeSVG(s, shapeList());
    } else if (drag.mode === 'move' || drag.mode === 'resize') {
      drag.end = point;
      draw();
    }
  };
  canvas.onpointerup = (event) => {
    if (!drag) return;
    const d = drag;
    drag = null;
    if (event.pointerType === 'touch') stopPointing();
    if (d.mode === 'marquee') {
      if (d.region) {
        d.touching = event.altKey;
        selected = marqueeSelection(d);
        selectionRegion = d.region;
      }
      draw();
      return;
    }
    if (readOnly || d.mode === 'pan' || d.mode === 'laser') return;
    if (d.mode === 'draw') {
      const s = d.shape;
      if (['text', 'sticky'].includes(s.type)) {
        s.box = [...d.start, 180, 100];
        s.text = s.type === 'sticky' ? 'New note' : 'Text';
      } else if (s.box[2] < 3 && s.box[3] < 3) {
        s.box = [...d.start, 150, 90];
        if (['line', 'arrow', 'pen'].includes(s.type))
          s.points = [d.start, [d.start[0] + 150, d.start[1] + 90]];
      }
      if (s.type === 'arrow') {
        if (d.from) s.from = d.from;
        const to = document.elementFromPoint(event.clientX, event.clientY)?.closest('[data-shape]')
          ?.dataset.shape;
        if (to) s.to = to;
      }
      validateShape(s);
      doc.transact(() => putShape(doc, s), local);
      selected = new Set([s.id]);
      selectTool('select');
      if (['text', 'sticky'].includes(s.type)) editText(s.id);
    } else if (d.end) {
      doc.transact(() => {
        for (const [id, geometry] of dragGeometry(d)) {
          const shape = doc.getMap('shapes').get(id);
          if (shape) shape.set('geometry', geometry);
        }
      }, local);
      if (outgoingPreview) outgoingPreview.opId = bufferedId;
      if (event.pointerType === 'touch') {
        if (online && !failed) port.postMessage({type:'presence',mode:'cursor',point:null,preview:outgoingPreview});
      } else presence(world(event));
    }
  };
  canvas.onpointerleave = () => {
    if (!drag) stopPointing();
  };
  canvas.onpointercancel = () => {
    outgoingPreview = null;
    stopPointing();
    if (drag?.mode === 'marquee') selected = drag.base;
    drag = null;
    draw();
  };
  canvas.ondblclick = (event) => {
    const id = hitAt(event);
    if (id && tool === 'select') editText(id);
  };
  canvas.onwheel = (event) => {
    event.preventDefault();
    const point = world(event),
      factor = event.deltaY > 0 ? 1.1 : 1 / 1.1;
    if (view[2] * factor < 100 || view[2] * factor > 100000) return;
    view = [
      point[0] + (view[0] - point[0]) * factor,
      point[1] + (view[1] - point[1]) * factor,
      view[2] * factor,
      view[3] * factor,
    ];
    draw();
  };
  canvas.onkeydown = (event) => {
    if (event.ctrlKey || event.metaKey) {
      if (event.key === 'a') {
        event.preventDefault();
        all();
      }
      return;
    }
    const keys = {
      v: 'select',
      h: 'pan',
      r: 'rect',
      o: 'ellipse',
      d: 'diamond',
      c: 'cylinder',
      a: 'arrow',
      l: 'line',
      p: 'pen',
      t: 'text',
      s: 'sticky',
      e: 'erase',
      k: 'laser',
      1: 'select',
      2: 'rect',
      3: 'diamond',
      4: 'ellipse',
      5: 'arrow',
      6: 'line',
      7: 'pen',
      8: 'text',
      9: 'sticky',
      0: 'erase',
    };
    if (keys[event.key] && (!readOnly || ['select', 'pan'].includes(keys[event.key]))) {
      event.preventDefault();
      selectTool(keys[event.key]);
    }
    if (event.key === 'Escape') {
      clear();
    }
    if (readOnly) return;
    if (event.key === 'Delete' || event.key === 'Backspace') {
      event.preventDefault();
      remove();
    }
    if (event.key === 'Enter' && selected.size === 1) editText([...selected][0]);
    const moves = {
      ArrowLeft: [-1, 0],
      ArrowRight: [1, 0],
      ArrowUp: [0, -1],
      ArrowDown: [0, 1],
    };
    if (moves[event.key]) {
      event.preventDefault();
      move(...moves[event.key], event.shiftKey ? 10 : 1);
    }
  };
  undo = new Y.UndoManager(doc.getMap('shapes'), {
    trackedOrigins: new Set([local]),
  });
  doc.on('update', () => draw());
  draw();
}
function documentEditor() {
  document.body.dataset.kind = 'document';
  $('tools').hidden = true;
  $('document').hidden = false;
  $('formatting').hidden = readOnly;
  awareness = new Awareness(doc);
  const provider = new ObservableV2();
  provider.awareness = awareness;
  const protectedNodes = new Set(Object.keys(schema.nodes));
  undo = new Y.UndoManager(doc.getXmlFragment('document'), {
    trackedOrigins: new Set([ySyncPluginKey]),
    // A surviving block keeps its identity when another writer's text prevents
    // undo from deleting the block itself. IDs are references, not user edits.
    deleteFilter: (item) => item.parentSub !== 'id' && defaultDeleteFilter(item, protectedNodes),
    captureTransaction: (transaction) => transaction.meta.get('addToHistory') !== false,
  });
  editor = new Editor({
    element: $('document'),
    editable: !readOnly,
    extensions: [
      ...extensions,
      Extension.create({
        name: 'agentAttribution',
        addProseMirrorPlugins() {
          return [
            new Plugin({
              props: {
                decorations(state) {
                  const decorations = [];
                  if ($('show-agent').checked)
                    state.doc.forEach((node, offset) => {
                      const target = agentTargets.get(node.attrs.id);
                      if (target)
                        decorations.push(
                          Decoration.node(offset, offset + node.nodeSize, {
                            class: 'agent-contribution',
                            title: target.label,
                            'data-agent-start': String(target.started),
                          }),
                        );
                    });
                  for (const comment of comments) {
                    if (comment.resolved) continue;
                    try {
                      const [from, to] = selectionPositions(doc, comment.range,
                        ySyncPluginKey.getState(state)?.binding);
                      decorations.push(Decoration.inline(from, to, {
                        class: 'comment-anchor', 'data-comment-id': comment.id,
                        title: comment.text,
                      }));
                    } catch { /* A deleted anchor remains visible in the comments list. */ }
                  }
                  return DecorationSet.create(state.doc, decorations);
                },
              },
            }),
            new Plugin({
              key: discussionHighlight,
              state: {
                init: () => ({ range: null, markers: DecorationSet.empty }),
                apply(transaction, previous, _previousState, state) {
                  const update = transaction.getMeta(discussionHighlight);
                  const range = update === undefined ? previous.range : update;
                  // Map through edits in the current transaction, before Yjs updates its binding.
                  // Yjs rebuilds the document for remote edits and undo, so resolve its anchors again.
                  if (update === undefined && !transaction.getMeta(ySyncPluginKey)?.isChangeOrigin)
                    return { range, markers: previous.markers.map(transaction.mapping, transaction.doc) };
                  let markers = DecorationSet.empty;
                  try {
                    if (range) {
                      const [from, to] = selectionPositions(doc, range,
                        ySyncPluginKey.getState(state)?.binding);
                      markers = DecorationSet.create(state.doc, [Decoration.inline(from, to, {
                        class: 'discussion-anchor',
                      })]);
                    }
                  } catch { /* The attached passage was deleted; keep its draft available. */ }
                  return { range, markers };
                },
              },
              props: { decorations: state => discussionHighlight.getState(state).markers },
            }),
          ];
        },
      }),
      Collaboration.configure({
        document: doc,
        field: 'document',
        yUndoOptions: { undoManager: undo },
      }),
      CollaborationCaret.configure({
        provider,
        user: { name: 'You', color: '#6965db' },
      }),
    ],
  });
  editor.view.dom.setAttribute('aria-label', 'Document content');
  const wordCount = () => {
    const text = editor.getText().trim();
    $('document-stat').textContent = `${text ? text.split(/\s+/u).length : 0} words`;
  };
  editor.on('update', wordCount);
  wordCount();
  editor.on('create', () => provider.emit('synced', []));
  let presenceTimer;
  awareness.on('update', (_changes, origin) => {
    if (origin === remote || readOnly) return;
    clearTimeout(presenceTimer);
    presenceTimer = setTimeout(() => {
      if (online)
        port.postMessage({
          type: 'presence',
          mode: 'selection',
          clientId: doc.clientID,
          clock: awareness.meta.get(doc.clientID).clock,
          cursor: awareness.getLocalState()?.cursor || null,
        });
    }, 60);
  });
  documentControls(editor, undo, () => readOnly, message => status(message,true));
  captureDocumentSelection = () => {
    // Capture before focus leaves the editor. DOM selection changes can precede
    // ProseMirror's selection transaction by one browser event-loop turn.
    const selection = document.getSelection();
    if (
      !selection ||
      !editor.view.dom.contains(selection.anchorNode) ||
      !editor.view.dom.contains(selection.focusNode)
    )
      return;
    const anchor = editor.view.posAtDOM(selection.anchorNode, selection.anchorOffset);
    const head = editor.view.posAtDOM(selection.focusNode, selection.focusOffset);
    documentSelection = null;
    if (anchor !== head) {
      const binding = ySyncPluginKey.getState(editor.state).binding;
      const relative = (position) =>
        Y.relativePositionToJSON(
          absolutePositionToRelativePosition(position, binding.type, binding.mapping),
        );
      documentSelection = {
        anchor: relative(anchor),
        head: relative(head),
      };
    }
    $('comment-selection').disabled = $('selection-question').disabled = readOnly || !documentSelection;
    const toolbar = $('selection-actions');
    toolbar.hidden = readOnly || !documentSelection;
    if (documentSelection) {
      const bounds = selection.getRangeAt(0).getBoundingClientRect();
      toolbar.style.left = `${Math.max(8, Math.min(bounds.left, innerWidth - toolbar.offsetWidth - 8))}px`;
      toolbar.style.top = `${Math.max(80, Math.min(bounds.bottom + 6, innerHeight - toolbar.offsetHeight - 8))}px`;
    }
    selected.clear();
    const from = Math.min(anchor, head),
      to = Math.max(anchor, head);
    editor.state.doc.forEach((node, offset) => {
      if (node.attrs.id && offset <= to && offset + node.nodeSize >= from)
        selected.add(node.attrs.id);
    });
  };
  document.addEventListener('selectionchange', captureDocumentSelection);
  $('selection-actions').addEventListener('pointerdown', event => {
    captureDocumentSelection();
    event.preventDefault();
  });
  document.addEventListener('focusin', event => {
    if (!editor.view.dom.contains(event.target) && !$('selection-actions').contains(event.target))
      $('selection-actions').hidden = true;
  });
  $('document').addEventListener('scroll', () => { $('selection-actions').hidden = true; });
  editor.on('blur', captureDocumentSelection);
}
function captureAttachment(scope, previous) {
  const range = scope === 'selection' && editor ? previous?.range || documentSelection : undefined;
  const selection = scope === 'selection' && !editor ? previous?.selection || [...selected] : [];
  if (scope === 'selection' && !range && !selection.length) throw new Error('Select content first');
  const captured = captureContext(doc, { range, selection });
  return { ...captured, range, selection, revision };
}
async function saveBeforeQuestion() {
  if (!online) throw new Error('Disconnected. Your question draft is kept; reconnect before sending.');
  flush();
  const deadline = performance.now() + 10000;
  while (online && !failed && (pending.length || inflight) && performance.now() < deadline)
    await new Promise(resolve => setTimeout(resolve, 50));
  if (!online || failed || pending.length || inflight || updates.length)
    throw new Error('Edits are not saved. Your question draft is kept; retry the save first.');
}
function setComments(value) {
  comments = value;
  assistant?.setComments(editor ? readComments(doc, comments) : []);
  if (editor) editor.view.dispatch(editor.state.tr.setMeta('addToHistory', false));
}
function revealPassage(range) {
  const [from, to] = selectionPositions(doc, range);
  editor.chain().focus().setTextSelection({from, to}).scrollIntoView().run();
}
function setAppearance(appearance) {
  const {theme, palette, accents} = appearance || {};
  const root = document.documentElement;
  if (theme === 'light' || theme === 'dark') root.dataset.theme = theme;
  else delete root.dataset.theme;
  root.dataset.palette = palette === 'warm' ? 'warm' : 'cool';
  root.dataset.accents = accents === 'minimal' ? 'minimal' : 'selective';
}
async function start(event) {
  if (
    initialized ||
    event.source !== parent ||
    event.data?.type !== 'mevedel-editor' ||
    !event.ports[0]
  )
    return;
  initialized = true;
  setAppearance(event.data.appearance);
  port = event.ports[0];
  item = event.data.item;
  document.documentElement.dataset.kind = item.kind;
  $('discussion-title').textContent = item.kind === 'document' ? 'Document discussion' : 'Whiteboard assistant';
  $('assistant').setAttribute('aria-label', $('discussion-title').textContent);
  $('assistant-close').setAttribute('aria-label', 'Close discussion');
  participant = event.data.name || 'You';
  readOnly = event.data.readOnly;
  online = event.data.online !== false;
  revision = item.revision;
  doc = restore(bytes(item.crdt));
  const recovery = event.data.draft;
  if (recovery?.crdt) {
    try {
      Y.applyUpdate(doc, bytes(recovery.crdt), remote);
      pending = Array.isArray(recovery.pending) ? recovery.pending : [];
    } catch (_) {
      status('Recovery data could not be loaded. Keep the original browser data.', true);
      failed = true;
    }
  }
  port.onmessage = ({ data }) => {
    if (data.type === 'appearance') {
      setAppearance(data.appearance);
      return;
    }
    if (data.type === 'reply') {
      const r = replies.get(data.reqId);
      if (r) {
        replies.delete(data.reqId);
        data.error ? r.reject(new Error(data.error)) : r.resolve(data.result);
      }
      return;
    }
    if (data.type === 'changed') {
      if (data.update) Y.applyUpdate(doc, bytes(data.update), remote);
      revision = Math.max(revision, data.revision);
      if (document.activeElement !== $('title')) $('title').value = doc.getMap('meta').get('title');
      if (data.transactions) history(data.transactions);
      if (data.comments) setComments(data.comments);
      return;
    }
    if (data.type === 'sync') {
      Y.applyUpdate(doc, bytes(data.item.crdt), remote);
      revision = data.item.revision;
      readOnly = data.readOnly;
      editor?.setEditable(!readOnly);
      online = true;
      failed = false;
      history(data.item.transactions);
      setComments(data.item.comments || []);
      flush();
      return;
    }
    if (data.type === 'offline') {
      online = false;
      clearPresence();
      if (awareness)
        removeAwarenessStates(
          awareness,
          [...awareness.states.keys()].filter((id) => id !== doc.clientID),
          remote,
        );
      for (const r of replies.values()) r.reject(new Error('Offline · changes pending'));
      replies.clear();
      saved();
      return;
    }
    if (data.type === 'storage-error') {
      recoveryWarning = data.message;
      saved();
      return;
    }
    if (data.type === 'storage-ok') {
      recoveryWarning = '';
      saved();
      return;
    }
    if (data.type === 'closed') {
      awareness?.setLocalStateField('cursor', null);
      clearPresence();
      return;
    }
    if (data.type === 'presence') showPresence(data);
    if (data.type === 'conversation') assistant?.updateConversation(data);
  };
  doc.on('update', (update, origin) => {
    if (origin === remote) return;
    if (item.kind === 'document') seedEmptyText(doc);
    bufferedId ||= crypto.randomUUID();
    updates.push(update);
    draft();
    saved();
  });
  if (item.kind === 'whiteboard') board();
  else documentEditor();
  imageControls();
  document.addEventListener('pointerdown', (event) => {
    document.querySelectorAll('.popover[open]:not(#properties)').forEach((menu) => {
      if (!menu.contains(event.target)) menu.open = false;
    });
  });
  $('title').value = item.title;
  $('title').readOnly = readOnly;
  $('title').onchange = async () => {
    try {
      await request({
        action: 'rename',
        title: $('title').value,
        opId: crypto.randomUUID(),
      });
    } catch (e) {
      status(e.message, true);
    }
  };
  $('undo').textContent = '↶';
  $('undo').setAttribute('aria-label', 'Undo');
  $('redo').textContent = '↷';
  $('redo').setAttribute('aria-label', 'Redo');
  $('show-agent').onchange = refreshAgentHighlights;
  $('undo').disabled = readOnly;
  $('redo').disabled = readOnly;
  $('retry').onclick = async () => {
    try {
      const current = await request({ action: 'read' });
      Y.applyUpdate(doc, bytes(current.crdt), remote);
      revision = current.revision;
      online = true;
      failed = false;
      flush();
    } catch (e) {
      status(e.message, true);
    }
  };
  $('undo').onclick = () => {
    if (!readOnly) {
      editor ? editor.commands.undo() : undo.undo();
    }
  };
  $('redo').onclick = () => {
    if (!readOnly) {
      editor ? editor.commands.redo() : undo.redo();
    }
  };
  document.addEventListener('keydown', (event) => {
    if (
      item.kind === 'whiteboard' &&
      (event.ctrlKey || event.metaKey) &&
      event.key.toLowerCase() === 'z' &&
      document.activeElement === $('canvas')
    ) {
      event.preventDefault();
      (event.shiftKey ? $('redo') : $('undo')).click();
    }
  });
  for (const value of [
    'native',
    ...(item.kind === 'whiteboard' ? ['png', 'svg'] : ['markdown', 'html']),
  ])
    $('export').add(
      new Option(value === 'native' ? 'Editable snapshot' : value.toUpperCase(), value),
    );
  $('download').onclick = async () => {
    try {
      if (pending.length || updates.length)
        throw new Error('Save pending changes first, or download a recovery copy');
      await request({ action: 'export', format: $('export').value });
    } catch (e) {
      status(e.message, true);
    }
  };
  $('recovery').onclick = () => {
    port.postMessage({
      type: 'recovery',
      result: {
        text: JSON.stringify({ format: 'mevedel-editable-1', ...inspect(doc) }),
        mime: 'application/json',
        extension: 'recovery.mevedel.json',
      },
    });
  };
  assistant = new AssistantPanel({
    capture: captureAttachment, request, save: saveBeforeQuestion,
    changed: () => {
      draft();
      if (editor) {
        const attachment = !$('assistant').hidden && assistant.drafts[
          assistant.drafts.view === 'comments' ? 'comment' : 'question'
        ].attachment;
        editor.view.dispatch(editor.state.tr.setMeta('addToHistory', false)
          .setMeta(discussionHighlight, attachment?.range || null));
      }
    },
    reveal: revealPassage, state: () => ({ readOnly, online }), restored: recovery?.assistant,
  });
  assistant.renderDraft();
  if (editor && matchMedia('(min-width:1100px)').matches) {
    if (assistant.draft.attachment) assistant.toggle(true);
    else assistant.begin('whole');
    document.activeElement?.blur();
  } else if (!editor) requestAnimationFrame(() => { view = bounds(shapeList()); draw(); });
  setComments(item.comments || []);
  $('comment-selection').hidden = readOnly || !editor;
  $('selection-question').hidden = readOnly;
  $('comments-tab').hidden = !editor;
  $('ask-toggle').textContent = editor ? 'Discussion' : 'Assistant';
  if (editor) {
    $('document').after($('selection-actions'));
    $('selection-actions').classList.add('document-selection-actions');
    $('selection-actions').hidden = true;
  }
  $('hint').textContent = editor ? 'Select text to comment or ask the assistant.' : 'Select objects to attach them to a question.';
  $('comment-selection').onclick = () => assistant.begin('selection', 'comment');
  $('selection-question').onclick = () => assistant.begin('selection');
  $('document').addEventListener('click', event => {
    const id = event.target.closest('[data-comment-id]')?.dataset.commentId;
    if (!id) return;
    assistant.openComment(id);
  });
  document.addEventListener('keydown', event => {
    if (editor && (event.ctrlKey || event.metaKey) && event.altKey && event.key.toLowerCase() === 'm') {
      event.preventDefault(); captureDocumentSelection(); assistant.begin('selection', 'comment');
    }
  });
  history(item.transactions);
  setInterval(flush, 300);
  setInterval(() => {
    if (pointing && online && performance.now() - lastPresence > 1500)
      presence(pointing.point, pointing.mode);
  }, 1500);
  window.addEventListener('pagehide', clearPresence);
  window.addEventListener('blur', stopPointing);
  document.addEventListener('visibilitychange', () => {
    if (document.hidden) clearPresence();
  });
  saved();
  pump();
}
window.addEventListener('message', (event) => {
  start(event).catch((error) => status(error.message, true));
});
