/* Packaged editor in an opaque iframe. All host access uses the bound port. */
import * as Y from 'yjs';
import { Editor, Extension } from '@tiptap/core';
import { Plugin } from '@tiptap/pm/state';
import { Decoration, DecorationSet } from '@tiptap/pm/view';
import Collaboration from '@tiptap/extension-collaboration';
import CollaborationCaret from '@tiptap/extension-collaboration-caret';
import { Awareness, applyAwarenessUpdate, removeAwarenessStates } from 'y-protocols/awareness';
import * as encoding from 'lib0/encoding';
import { ObservableV2 } from 'lib0/observable';
import {
  absolutePositionToRelativePosition,
  defaultDeleteFilter,
  ySyncPluginKey,
} from '@tiptap/y-tiptap';
import { extensions, schema, seedEmptyText } from './document.mjs';
import { restore, encode, inspect, putShape, validateShape, validateImage } from './model.mjs';
import { shapeSVG, definitions, escape, bounds, styleOf, FILLABLE, LINEAR } from './render.mjs';
const $ = (id) => document.getElementById(id),
  remote = Symbol('remote'),
  local = Symbol('local');
const bytes = (text) => Uint8Array.from(atob(text), (c) => c.charCodeAt(0));
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
  view = [-40, -40, 1000, 650],
  drag = null,
  lastPresence = 0,
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
const replies = new Map(),
  people = new Map();
let documentSelection = null;
let recoveryWarning = '';
let participant = 'You',
  textEditing = null,
  sceneSignature = '';
const agentTargets = new Map();
function status(text, error = false) {
  $('saved').textContent = text;
  $('saved').dataset.error = String(error);
}
function saved() {
  if (recoveryWarning) {
    status(recoveryWarning, true);
    return;
  }
  if (failed) return;
  status(
    pending.length || updates.length
      ? online
        ? 'Saving…'
        : 'Offline · changes pending'
      : online
        ? 'Saved on host'
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
    },
  });
}
function flush() {
  if (updates.length) {
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
    draft();
  } catch (error) {
    failed = online;
    status(error.message, true);
  } finally {
    inflight = false;
    saved();
    if (!failed) pump();
  }
}
function history(transactions = []) {
  agentTargets.clear();
  const seen = new Set();
  for (const tx of transactions)
    for (const change of tx.changes || []) {
      if (!seen.has(change.id) && tx.actor.startsWith('Agent:') && change.after)
        agentTargets.set(change.id, `${tx.actor} · revision ${tx.revision}`);
      seen.add(change.id);
    }
  if (editor) editor.view.dispatch(editor.state.tr.setMeta('addToHistory', false));
  else if (doc) draw();
  $('contributions').replaceChildren();
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
function draw() {
  const shapes = shapeList();
  $('canvas').setAttribute('viewBox', view.join(' '));
  const scale = $('canvas').getScreenCTM()?.a || 1;
  const signature = JSON.stringify([shapes, scale, [...agentTargets], $('show-agent').checked]);
  if (signature !== sceneSignature) {
    sceneSignature = signature;
    $('scene').innerHTML = definitions + shapes.map((s) => shapeSVG(s, shapes)).join('');
    for (const [index, group] of $('scene').querySelectorAll('[data-shape]').entries()) {
      const shape = shapes[index];
      group.dataset.agent = String(agentTargets.has(shape.id) && $('show-agent').checked);
      const label = agentTargets.get(shape.id);
      if (label) {
        const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
        title.textContent = label;
        group.append(title);
      }
      if (LINEAR.includes(shape.type)) {
        const hit = group.querySelector('path').cloneNode();
        hit.removeAttribute('marker-end');
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
  for (const id of selected) if (!doc.getMap('shapes').has(id)) selected.delete(id);
  $('selection').innerHTML = shapes
    .filter((s) => selected.has(s.id))
    .map((s) => {
      const [x, y, w, h] = s.box;
      return `<rect x="${x - 4}" y="${y - 4}" width="${w + 8}" height="${h + 8}" fill="none" stroke="#6965db" stroke-dasharray="5 3"/><rect data-resize="${escape(s.id)}" x="${x + w - 5}" y="${y + h - 5}" width="10" height="10" fill="white" stroke="#6965db"/>`;
    })
    .join('');
  refresh();
}
function world(event) {
  const p = new DOMPoint(event.clientX, event.clientY).matrixTransform(
    $('canvas').getScreenCTM().inverse(),
  );
  return [Math.max(-999000, Math.min(999000, p.x)), Math.max(-999000, Math.min(999000, p.y))];
}
function presence(point, mode = 'cursor') {
  if (mode === 'laser' && !readOnly) showPresence({ peer: 'self', name: participant, point, mode });
  if (readOnly || !online || performance.now() - lastPresence < 50) return;
  lastPresence = performance.now();
  port.postMessage({ type: 'presence', point, mode });
}
function clearPresence() {
  for (const person of people.values()) clearTimeout(person.timer);
  people.clear();
  $('presence').replaceChildren();
}
function showPresence(data) {
  if (data.mode === 'clear') {
    const person = people.get(data.peer);
    if (person) {
      person.group.remove();
      clearTimeout(person.timer);
    }
    people.delete(data.peer);
  }
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
  if (item.kind !== 'whiteboard' || !Array.isArray(data.point)) return;
  let person = people.get(data.peer);
  if (!person) {
    person = {
      group: document.createElementNS('http://www.w3.org/2000/svg', 'g'),
      points: [],
    };
    people.set(data.peer, person);
    $('presence').append(person.group);
  }
  const { group } = person;
  const [x, y] = data.point;
  const scale = 1 / ($('canvas').getScreenCTM()?.a || 1);
  const laser = data.mode === 'laser',
    now = performance.now();
  person.points = laser
    ? [...person.points.filter((p) => now - p.time < 600), { point: data.point, time: now }].slice(
        -32,
      )
    : [];
  group.innerHTML = laser
    ? `<polyline points="${person.points.map((p) => p.point.join(',')).join(' ')}" fill="none" stroke="#e33164" stroke-width="${3 * scale}" stroke-linecap="round" stroke-linejoin="round"/><circle cx="${x}" cy="${y}" r="${5 * scale}" fill="#e33164"/>`
    : `<path d="M0 0 4 17 8 11 15 10Z" transform="translate(${x} ${y}) scale(${scale})" fill="#a22458" stroke="white" stroke-width="1.5"/>`;
  group.innerHTML += `<text x="${x + 14 * scale}" y="${y - 8 * scale}" font-size="${12 * scale}" fill="#a22458">${escape(data.name)}</text>`;
  group.classList.toggle('laser', laser);
  // Restart the fade on a new sample; ordinary cursors move, never leave trails.
  group.getAnimations().forEach((animation) => {
    animation.currentTime = 0;
  });
  clearTimeout(person.timer);
  person.timer = setTimeout(
    () => {
      group.remove();
      people.delete(data.peer);
    },
    laser ? 1500 : 10000,
  );
}
function selectTool(value) {
  $('menu').open = false;
  tool = value;
  document
    .querySelectorAll('[data-tool]')
    .forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.tool === value)));
  $('canvas').style.cursor =
    value === 'pan' ? 'grab' : value === 'select' ? 'default' : 'crosshair';
  if (drawable.includes(value)) selected.clear();
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
function editText(id) {
  if (readOnly) return;
  const shape = doc.getMap('shapes').get(id);
  if (!shape) return;
  const input = $('shape-text'),
    [x, y, w, h] = shape.get('geometry').box;
  const matrix = $('canvas').getScreenCTM(),
    point = new DOMPoint(x, y).matrixTransform(matrix);
  textEditing = id;
  undo?.stopCapturing();
  const before = shape.get('text') || '';
  input.value = before;
  Object.assign(input.style, {
    left: `${Math.max(0, point.x)}px`,
    top: `${Math.max(0, point.y)}px`,
    width: `${Math.max(100, Math.min(w * matrix.a, innerWidth - Math.max(0, point.x)))}px`,
    height: `${Math.max(60, h * matrix.a)}px`,
    fontSize: `${Math.max(16, (shape.get('fontSize') || 16) * matrix.a)}px`,
  });
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
async function insertImage(file) {
  if (readOnly || !file) return;
  try {
    if (file.size > 4 * 1024 * 1024 || !/^image\/(png|jpeg|webp)$/.test(file.type))
      throw new Error('Use a PNG, JPEG, or WebP image up to 4 MB');
    const src = await new Promise((resolve, reject) => {
      const r = new FileReader();
      r.onload = () => resolve(r.result);
      r.onerror = reject;
      r.readAsDataURL(file);
    });
    validateImage(src);
    const image = await createImageBitmap(file),
      scale = Math.min(1, 640 / image.width);
    doc.transact(
      () =>
        putShape(doc, {
          id: crypto.randomUUID(),
          type: 'image',
          box: [view[0] + 50, view[1] + 50, image.width * scale, image.height * scale],
          src,
        }),
      local,
    );
    image.close();
  } catch (e) {
    status(e.message, true);
  }
}
function board() {
  $('board').hidden = false;
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
  const strip = document.createElement('div');
  strip.className = 'drawing-tools';
  $('tools').append(strip);
  for (const [value, label, key] of tools) {
    if (readOnly && !['select', 'pan'].includes(value)) continue;
    const b = button(strip, '', () => selectTool(value));
    b.dataset.tool = value;
    b.title = `${label} (${key})`;
    b.setAttribute('aria-label', label);
    b.setAttribute('aria-keyshortcuts', key);
    b.setAttribute('aria-pressed', String(value === 'select'));
    b.innerHTML = `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="${icons[value]}"/></svg><kbd>${key}</kbd>`;
  }
  const properties = document.createElement('details');
  properties.id = 'properties';
  properties.className = 'popover';
  properties.innerHTML = '<summary>Style</summary><div class="menu-body"></div>';
  $('tools').append(properties);
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
    const action = (act, label, body) =>
      `<button type="button" class="opt" data-act="${act}" title="${label}" aria-label="${label}">${body}</button>`;
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
      section('opacity', 'Opacity', '<input type="range" min="0" max="100" step="5" aria-label="Opacity"><output>100</output>') +
      section(
        'layers',
        'Layers',
        action('back', 'Send to back', icon('<path d="M12 9v12M7.5 16.5l4.5 4.5 4.5-4.5M4 4.5h16M7 8.5h10"/>')) +
          action('front', 'Bring to front', icon('<path d="M12 15V3M7.5 7.5 12 3l4.5 4.5M4 19.5h16M7 15.5h10"/>')),
      ) +
      section(
        'actions',
        'Actions',
        action('dup', 'Duplicate', icon('<rect x="8.5" y="8.5" width="11" height="11" rx="2"/><path d="M15.5 8.5V6a1.5 1.5 0 0 0-1.5-1.5H6A1.5 1.5 0 0 0 4.5 6v8A1.5 1.5 0 0 0 6 15.5h2.5"/>')) +
          action('del', 'Delete', icon('<path d="M4.5 7h15M9.5 7V4.5h5V7m-8 0 1 12.5h9l1-12.5M10 10.5v6M14 10.5v6"/>')),
      );
    panel.onclick = (event) => {
      const b = event.target.closest('[data-prop]');
      if (b) {
        const key = b.dataset.prop;
        style(key, ['width', 'rough', 'fontSize'].includes(key) ? +b.dataset.val : b.dataset.val);
        return;
      }
      const act = event.target.closest('[data-act]')?.dataset.act;
      if (act === 'front' || act === 'back') reorder(act === 'front');
      else if (act === 'dup') duplicate();
      else if (act === 'del') remove();
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
        layers: shapes.length > 0,
        actions: shapes.length > 0,
      };
      properties.hidden = !kinds.size;
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
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = 'image/png,image/jpeg,image/webp';
    input.hidden = true;
    $('tools').append(input);
    button($('menu').querySelector('.menu-body'), 'Insert image…', () => input.click());
    input.onchange = () => {
      const file = input.files[0];
      input.value = '';
      insertImage(file);
    };
    document.addEventListener('paste', (event) => {
      const file = [...(event.clipboardData?.files || [])][0];
      if (file) {
        event.preventDefault();
        insertImage(file);
      }
    });
  }
  properties.hidden = readOnly;
  const zoom = document.createElement('div');
  zoom.className = 'zoom-tools';
  $('tools').append(zoom);
  button(zoom, 'Fit', () => {
    view = bounds(shapeList());
    draw();
  });
  button(zoom, '−', () => {
    view = [view[0], view[1], Math.min(100000, view[2] * 1.2), Math.min(100000, view[3] * 1.2)];
    draw();
  }).setAttribute('aria-label', 'Zoom out');
  button(zoom, '+', () => {
    view = [view[0], view[1], Math.max(100, view[2] / 1.2), Math.max(65, view[3] / 1.2)];
    draw();
  }).setAttribute('aria-label', 'Zoom in');
  const canvas = $('canvas');
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
          before: doc.getMap('shapes').get(resize).get('geometry'),
        };
        return;
      }
      if (id) {
        if (event.shiftKey) {
          selected.has(id) ? selected.delete(id) : selected.add(id);
        } else if (!selected.has(id)) selected = new Set([id]);
      } else selected.clear();
      draw();
      if (id && !readOnly)
        drag = {
          mode: 'move',
          start: point,
          boxes: [...selected].map((key) => [key, doc.getMap('shapes').get(key).get('geometry')]),
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
    if (tool !== 'laser' || drag?.mode === 'laser')
      presence(point, drag?.mode === 'laser' ? 'laser' : 'cursor');
    if (!drag) return;
    if (drag.mode === 'pan') {
      const rect = canvas.getBoundingClientRect();
      view = [
        drag.view[0] - ((event.clientX - drag.start[0]) * drag.view[2]) / rect.width,
        drag.view[1] - ((event.clientY - drag.start[1]) * drag.view[3]) / rect.height,
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
    }
  };
  canvas.onpointerup = (event) => {
    if (!drag) return;
    const d = drag;
    drag = null;
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
      draw();
      if (['text', 'sticky'].includes(s.type)) editText(s.id);
    } else if (d.end) {
      doc.transact(() => {
        if (d.mode === 'move')
          for (const [id, g] of d.boxes) {
            const s = doc.getMap('shapes').get(id);
            if (s)
              s.set(
                'geometry',
                transformGeometry(g, [
                  g.box[0] + d.end[0] - d.start[0],
                  g.box[1] + d.end[1] - d.start[1],
                  ...g.box.slice(2),
                ]),
              );
          }
        else {
          const s = doc.getMap('shapes').get(d.id),
            b = d.before.box;
          if (s)
            s.set(
              'geometry',
              transformGeometry(d.before, [
                ...b.slice(0, 2),
                Math.max(10, d.end[0] - b[0]),
                Math.max(10, d.end[1] - b[1]),
              ]),
            );
        }
      }, local);
    }
  };
  canvas.onpointerleave = () => {
    if (!drag) port.postMessage({ type: 'presence', mode: 'clear', point: null });
  };
  canvas.onpointercancel = () => {
    showPresence({ peer: 'self', mode: 'clear' });
    drag = null;
    port.postMessage({ type: 'presence', mode: 'clear', point: null });
    draw();
  };
  canvas.ondblclick = (event) => {
    const id = hitAt(event);
    if (id) editText(id);
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
        selected = new Set(shapeList().map((s) => s.id));
        draw();
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
      selected.clear();
      selectTool('select');
      draw();
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
      const [dx, dy] = moves[event.key],
        step = event.shiftKey ? 10 : 1;
      doc.transact(() => {
        for (const id of selected) {
          const s = doc.getMap('shapes').get(id),
            g = s.get('geometry'),
            b = g.box;
          s.set(
            'geometry',
            transformGeometry(g, [b[0] + dx * step, b[1] + dy * step, ...b.slice(2)]),
          );
        }
      }, local);
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
                      const label = agentTargets.get(node.attrs.id);
                      if (label)
                        decorations.push(
                          Decoration.node(offset, offset + node.nodeSize, {
                            class: 'agent-contribution',
                            title: label,
                          }),
                        );
                    });
                  return DecorationSet.create(state.doc, decorations);
                },
              },
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
  const styles = document.createElement('select');
  styles.setAttribute('aria-label', 'Paragraph style');
  for (const [label, value] of [
    ['Body', 0],
    ['Title', 1],
    ['Heading', 2],
    ['Subheading', 3],
  ])
    styles.add(new Option(label, value));
  styles.onchange = () => {
    const c = editor.chain().focus();
    (+styles.value ? c.setHeading({ level: +styles.value }) : c.setParagraph()).run();
  };
  $('formatting').append(styles);
  const actions = [
    ['Bold', 'B', 'bold', (c) => c.toggleBold()],
    ['Italic', 'I', 'italic', (c) => c.toggleItalic()],
    ['Underline', 'U', 'underline', (c) => c.toggleUnderline()],
    ['Strike', 'S', 'strike', (c) => c.toggleStrike()],
    ['Bullets', '☷', 'bulletList', (c) => c.toggleBulletList()],
    ['Numbers', '≡', 'orderedList', (c) => c.toggleOrderedList()],
    ['Quote', '❞', 'blockquote', (c) => c.toggleBlockquote()],
    ['Code', '</>', 'codeBlock', (c) => c.toggleCodeBlock()],
    ['Table', '▦', 'table', (c) => c.insertTable({ rows: 3, cols: 3, withHeaderRow: true })],
  ];
  for (const [label, icon, node, run] of actions) {
    const b = button($('formatting'), icon, () => run(editor.chain().focus()).run());
    b.title = label;
    b.setAttribute('aria-label', label);
    b.dataset.format = node;
  }
  editor.on('transaction', () => {
    styles.value = editor.isActive('heading') ? editor.getAttributes('heading').level : 0;
    document
      .querySelectorAll('[data-format]')
      .forEach((b) => b.setAttribute('aria-pressed', String(editor.isActive(b.dataset.format))));
  });
  const link = button($('formatting'), '↗', () => {
    $('link-dialog').showModal();
    $('link-url').focus();
    $('link-dialog').onclose = () => {
      const url = $('link-url').value;
      if ($('link-dialog').returnValue === 'save' && /^(https?:|mailto:)/i.test(url))
        editor.chain().focus().setLink({ href: url }).run();
    };
  });
  link.title = 'Link';
  link.setAttribute('aria-label', 'Link');
  function captureSelection() {
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
    selected.clear();
    const from = Math.min(anchor, head),
      to = Math.max(anchor, head);
    editor.state.doc.forEach((node, offset) => {
      if (node.attrs.id && offset <= to && offset + node.nodeSize >= from)
        selected.add(node.attrs.id);
    });
  }
  document.addEventListener('selectionchange', captureSelection);
  $('ask').addEventListener('pointerdown', captureSelection);
  editor.on('blur', captureSelection);
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
  port = event.ports[0];
  item = event.data.item;
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
  $('show-agent').onchange = () => {
    if (editor) editor.view.dispatch(editor.state.tr.setMeta('addToHistory', false));
    else draw();
  };
  $('ask-toggle').hidden = readOnly;
  $('ask-toggle').onclick = () => {
    const shown = document.querySelector('footer').classList.toggle('asking');
    $('ask-toggle').setAttribute('aria-expanded', String(shown));
    if (shown) $('question').focus();
  };
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
  $('ask').hidden = readOnly;
  $('ask').onsubmit = async (event) => {
    event.preventDefault();
    const submit = $('ask').querySelector('button');
    if (submit.disabled) return;
    submit.disabled = true;
    try {
      flush();
      if (pending.length || inflight) throw new Error('Wait for edits to save before asking');
      let range;
      const selection = $('scope').value === 'selection';
      if (selection && editor) range = documentSelection;
      if (selection && !range && !selected.size)
        throw new Error('Select content first, or choose Whole item');
      await request({
        action: 'ask',
        revision,
        selection: selection && !range ? [...selected] : [],
        range,
        text: $('question').value,
      });
      $('question').value = '';
      status('Question queued');
    } catch (e) {
      status(e.message, true);
    } finally {
      submit.disabled = false;
    }
  };
  history(item.transactions);
  setInterval(flush, 300);
  saved();
  pump();
}
window.addEventListener('message', (event) => {
  start(event).catch((error) => status(error.message, true));
});
