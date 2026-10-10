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
import { restore, encode, inspect, filesOf, putElement, putFile, validateElement, validate, compareOrder, GEOMETRY } from './model.mjs';
import { validateImage } from './image.mjs';
import { sceneSVG, elementSVG, escape, resolveScene, bounds, extent, shapesInRegion } from './render.mjs';
import { CONTAINERS, LINEAR, pointBounds } from './scene.mjs';
import { containerTextBox, containerSizeFor, fontStack, measure, FONT_FILES } from './text.mjs';
import { serializeScene, placeElements } from './excalidraw.mjs';
import { recognize } from './autoshape.mjs';
import { handlePositions, resizeBox, scaleBoxes, rotationDelta, normalizeAngle, rotate } from './transform.mjs';
import { libraryPanel } from './library.mjs';
import { generateKeyBetween, generateNKeysBetween } from 'fractional-indexing';
import Excalifont from './Excalifont.woff2';
import Nunito from './Nunito.woff2';
import ComicShanns from './ComicShanns.woff2';
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
  /* Whether drawing tools stay active after each new shape. */
  toolLocked = false,
  selected = new Set(),
  /* The group a double-click entered, whose members select individually. */
  editingGroup = null,
  autoStyle = () => {},
  /* Board area [x, y, w, h] from the last box selection, kept with that selection. */
  selectionRegion = null,
  view = [-40, -40, 1000, 650],
  drag = null,
  /* Autoshape strokes waiting for a following stroke before recognition. */
  pendingShape = null,
  settlePendingShape = () => {},
  lastPresence = 0,
  pointing = null,
  presenceTimer = null,
  laserSamples = [],
  /* Index of the newest held laser sample; each press skips one to separate strokes. */
  laserInk = 0,
  boardPresence,
  boardPreviews,
  outgoingPreview = null,
  refresh = () => {};
/* Style of the next drawn element in Excalidraw's fields; the panel edits it
   alongside the selection. `roundness` holds the panel's round/sharp choice. */
const current = {
  strokeColor: '#1e1e1e',
  backgroundColor: 'transparent',
  fillStyle: 'hachure',
  strokeWidth: 2,
  strokeStyle: 'solid',
  roughness: 1,
  roundness: 'round',
  opacity: 100,
  fontSize: 20,
  fontFamily: 5,
  textAlign: 'left',
  verticalAlign: 'middle',
  startArrowhead: null,
  endArrowhead: 'arrow',
  arrowType: 'round',
};
const drawable = ['rectangle', 'diamond', 'ellipse', 'stickynote', 'arrow', 'line', 'freedraw', 'autoshape', 'text'];
/* How long a finished autoshape stroke waits for the next one, so an arrow's
   head can follow its shaft as a second stroke. */
const AUTOSHAPE_DELAY = 700;
const SHAPES = ['rectangle', 'diamond', 'ellipse'];
function applies(key, type) {
  if (key === 'backgroundColor') return [...SHAPES, 'stickynote', 'line', 'freedraw', 'autoshape'].includes(type);
  if (key === 'fillStyle') return [...SHAPES, 'line', 'freedraw', 'autoshape'].includes(type);
  if (key === 'roundness') return ['rectangle', 'diamond', 'line', 'image', 'stickynote', 'autoshape'].includes(type);
  if (['fontSize', 'fontFamily'].includes(key)) return ['text', 'stickynote', ...SHAPES].includes(type);
  if (key === 'textAlign') return type === 'text';
  if (['startArrowhead', 'endArrowhead', 'arrowType'].includes(key)) return type === 'arrow';
  if (key === 'strokeWidth') return [...SHAPES, 'arrow', 'line', 'freedraw', 'autoshape'].includes(type);
  if (key === 'strokeStyle') return [...SHAPES, 'arrow', 'line', 'autoshape'].includes(type);
  if (key === 'roughness') return [...SHAPES, 'arrow', 'line', 'stickynote', 'autoshape'].includes(type);
  if (key === 'strokeColor') return !['image', 'frame', 'magicframe'].includes(type);
  return true;
}
/* Excalidraw's roundness for the panel's round/sharp CHOICE on TYPE. */
const roundnessFor = (type, choice) =>
  choice !== 'round' ? null : { type: ['rectangle', 'image', 'iframe', 'embeddable'].includes(type) ? 3 : 2 };
/* Labels are styled through their container. */
const LABEL_STYLES = ['strokeColor', 'fontSize', 'fontFamily', 'opacity'];
const randomSeed = () => Math.floor(Math.random() * 2 ** 31);
const replies = new Map();
let documentSelection = null,
  captureDocumentSelection = () => {},
  assistant,
  comments = [],
  /* Comments with their live anchor status, as the discussion panel shows them. */
  commentStates = [],
  hoverShape = null;
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
let cachedList = null, cachedScene = null;
/* Stored elements in z-order; cached until the next document update. */
function shapeList() {
  return (cachedList ||= inspect(doc).content);
}
function currentScene() {
  return (cachedScene ||= resolveScene(shapeList()));
}
const elementMap = () => doc.getMap('elements');
const geometryOf = (id) => elementMap().get(id)?.get('geometry');
/* The displayed box [x, y, w, h] a geometry occupies, before rotation. */
function geometryBox(g) {
  if (!g.points) return [g.x, g.y, g.width, g.height];
  const [x1, y1, x2, y2] = pointBounds(g.points);
  return [g.x + x1, g.y + y1, x2 - x1, y2 - y1];
}
/* GEOMETRY moved and scaled so its box becomes BOX. */
function transformGeometry(g, [x, y, w, h]) {
  if (!g.points) return { ...g, x, y, width: w, height: h };
  const [x1, y1, x2, y2] = pointBounds(g.points);
  const sx = x2 - x1 ? w / (x2 - x1) : 1, sy = y2 - y1 ? h / (y2 - y1) : 1;
  return { ...g, x, y, width: w, height: h, points: g.points.map(([px, py]) => [(px - x1) * sx, (py - y1) * sy]) };
}
/* Changes [id, changes] a move, transform or point drag D produces now.
   Changes hold geometry and, for text, font size or auto-resizing. */
function dragGeometry(d) {
  if (!d?.end || !['move', 'transform', 'point'].includes(d.mode)) return [];
  if (d.mode === 'move')
    return d.boxes.map(([id, g]) => {
      const box = geometryBox(g);
      return [id, transformGeometry(g, [box[0] + d.end[0] - d.start[0], box[1] + d.end[1] - d.start[1], box[2], box[3]])];
    });
  if (d.mode === 'point') {
    const points = d.base.points.map((p) => p.slice());
    points[d.index] = [d.end[0] - d.base.x, d.end[1] - d.base.y];
    const [x1, y1, x2, y2] = pointBounds(points);
    return [[d.id, { ...d.base, points, width: x2 - x1, height: y2 - y1 }]];
  }
  const end = [d.end[0] - d.offset[0], d.end[1] - d.offset[1]];
  if (d.handle === 'rotation') {
    let delta = rotationDelta(d.center, d.start, d.end);
    const step = Math.PI / 12;
    if (d.snap && !d.single) delta = Math.round(delta / step) * step;
    return d.entries.map(([id, e]) => {
      const b = geometryBox(e.g), c = [b[0] + b[2] / 2, b[1] + b[3] / 2];
      let angle = (e.g.angle || 0) + delta;
      if (d.snap && d.single) angle = Math.round(angle / step) * step;
      const [cx, cy] = d.single ? c : rotate(c, d.center, angle - (e.g.angle || 0));
      return [id, { ...transformGeometry(e.g, [cx - b[2] / 2, cy - b[3] / 2, b[2], b[3]]), angle: normalizeAngle(angle) }];
    });
  }
  if (d.single) {
    const [[id, e]] = d.entries, box = resizeBox(d.frame, d.angle, d.handle, end, d.keepAspect);
    const changes = transformGeometry(e.g, box);
    // Text scales its font from a corner and wraps to a dragged side, as in Excalidraw.
    if (e.type === 'text' && d.handle.length === 2) changes.fontSize = e.fontSize * box[3] / d.frame[3];
    if (e.type === 'text' && d.handle.length === 1) changes.autoResize = false;
    return [[id, changes]];
  }
  const { boxes, scale } = scaleBoxes(new Map(d.entries.map(([id, e]) => [id, geometryBox(e.g)])),
    d.frame, d.handle, end, d.keepAspect);
  return d.entries.map(([id, e]) => [id, { ...transformGeometry(e.g, boxes.get(id)),
    ...(e.type === 'text' ? { fontSize: e.fontSize * scale[1] } : {}) }]);
}
/* Apply drag CHANGES to element ID: geometry as one property, the rest as fields. */
function applyChanges(id, changes) {
  const element = elementMap().get(id);
  if (!element) return;
  const geometry = {};
  for (const [key, value] of Object.entries(changes))
    if (GEOMETRY.includes(key)) geometry[key] = value;
    else element.set(key, value);
  element.set('geometry', geometry);
}
function visibleGeometry(id) {
  const geometry = geometryOf(id);
  const box = boardPreviews?.boxes().get(id);
  return box ? transformGeometry(geometry, box) : geometry;
}
let fontsLoaded = false;
/* Board fonts travel inside the bundle; the editor's sandbox has no network. */
function loadFonts() {
  if (fontsLoaded) return;
  fontsLoaded = true;
  const data = { Excalifont, Nunito, ComicShanns };
  for (const [family, file] of Object.entries(FONT_FILES)) {
    const face = new FontFace(family, `url(data:font/woff2;base64,${data[file]})`);
    document.fonts.add(face);
    face.load().then(() => { sceneSignature = ''; if (doc) draw(); }, () => {});
  }
}
/* The scene as displayed now, with drag and remote previews applied. */
function displayedScene(extra = []) {
  const live = boardPreviews?.boxes() || new Map();
  const preview = new Map(dragGeometry(drag));
  if (!preview.size && !live.size && !extra.length) return currentScene();
  // A dragged connector leaves shapes that stay behind.
  const unbound = (s) => {
    const keys = drag?.loose?.get(s.id);
    if (!keys) return s;
    const copy = { ...s };
    for (const key of keys) delete copy[key];
    return copy;
  };
  return resolveScene([...shapeList().map(s => preview.has(s.id) ? {...unbound(s), ...preview.get(s.id)}
    : live.has(s.id) ? {...s, ...transformGeometry(s, live.get(s.id))} : s), ...extra]);
}
function draw() {
  const live = boardPreviews?.boxes() || new Map();
  const scene = { ...displayedScene(), paper: canvasPaper() };
  const shapes = scene.order;
  $('canvas').setAttribute('viewBox', view.join(' '));
  const background = doc.getMap('meta').get('background') || '';
  if (background) $('board').style.setProperty('--canvas', background);
  else $('board').style.removeProperty('--canvas');
  for (const b of document.querySelectorAll('#canvas-background [data-background]'))
    b.setAttribute('aria-pressed', String(b.dataset.background === background));
  const custom = document.querySelector('#canvas-background .custom');
  custom?.setAttribute('aria-pressed', String(Boolean(background) && !BACKGROUNDS.some(([value]) => value === background)));
  const scale = $('canvas').getScreenCTM()?.a || 1;
  if ($('board-zoom')) $('board-zoom').textContent = `${Math.round(scale * 100)}%`;
  const files = filesOf(doc);
  const signature = JSON.stringify([shapes, Object.keys(files), [...agentTargets], $('show-agent').checked, scene.paper]);
  if (signature !== sceneSignature) {
    sceneSignature = signature;
    $('scene').innerHTML = sceneSVG(scene, files, true);
    for (const group of $('scene').querySelectorAll('[data-shape]')) {
      const id = group.dataset.shape;
      group.dataset.agent = String(agentTargets.has(id) && $('show-agent').checked);
      const target = agentTargets.get(id);
      if (target) {
        group.style.setProperty('--highlight-delay', `${target.started - performance.now()}ms`);
        const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
        title.textContent = target.label;
        group.append(title);
      }
    }
  }
  for (const group of $('scene').querySelectorAll('[data-shape]'))
    {
      group.dataset.editing = String(group.dataset.shape === textEditing);
      group.dataset.livePreview = String(live.has(group.dataset.shape));
    }
  layoutShapeText();
  for (const id of selected) if (!elementMap().has(id)) selected.delete(id);
  const selectedImage = selected.size === 1 && shapes.find(s => selected.has(s.id) && s.type === 'image');
  $('image-tools').hidden = readOnly || !selectedImage;
  $('image-size').textContent = selectedImage ? `${Math.round(selectedImage.width)} × ${Math.round(selectedImage.height)} px` : '';
  $('selection').dataset.selected = [...selected].sort().join(' ');
  // A box selection's area shows while drawn and while a draft carries it.
  $('selection').innerHTML = selectionSVG(scene, scale)
    + regionSVG(drag?.mode === 'marquee' ? drag.region : attachedRegion(), drag?.mode === 'marquee')
    + hoverSVG(tool === 'comment' && !drag && scene.byId.get(hoverShape), scale)
    // Strokes in progress are authored content, drawn as the board will be.
    + `<g class="authored">${(pendingShape?.strokes.map(drawingPreview).join('') || '')
      + (drag?.mode === 'draw' ? drawingPreview(drag) : '')}</g>`;
  drawCommentMarkers(scene, scale);
  boardPresence?.animate();
  $('selection-question').disabled = $('comment-selection').disabled = readOnly || !(selected.size || selectionRegion);
  autoStyle();
  refresh();
}
function hoverSVG(item, scale) {
  if (!item?.box) return '';
  const [x, y, w, h] = item.box, gap = 4 / scale;
  return `<rect class="comment-hover" x="${x - gap}" y="${y - gap}" width="${w + gap * 2}" height="${h + gap * 2}" vector-effect="non-scaling-stroke"/>`;
}
/* The board box a comment refers to: its surviving objects and its area. */
function commentAnchor(comment, scene) {
  const live = scene.order.filter((s) => comment.selection?.includes(s.id));
  const boxes = [...(live.length ? [extent(live, scene)] : []), ...(comment.region ? [comment.region] : [])];
  if (!boxes.length) return null;
  const x = Math.min(...boxes.map((b) => b[0])), y = Math.min(...boxes.map((b) => b[1]));
  return [x, y, Math.max(...boxes.map((b) => b[0] + b[2])) - x, Math.max(...boxes.map((b) => b[1] + b[3])) - y];
}
/* Numbered pins at the top-right corner of each open comment's anchor. */
function drawCommentMarkers(scene, scale) {
  $('comment-markers').innerHTML = commentStates.filter((c) => !c.resolved).map((c, index) => {
    const box = commentAnchor(c, scene);
    if (!box) return '';
    const [x, y, w, h] = box, gap = 4 / scale;
    // A ring turns around the pin while the assistant works on its thread;
    // markers are redrawn often, so the ring keeps its phase from the clock.
    const working = assistant?.working(c.id);
    return `<g class="comment-marker" data-comment-id="${escape(c.id)}" data-status="${escape(c.anchorStatus)}"${working ? ' data-working="true"' : ''}><rect class="comment-outline" x="${x - gap}" y="${y - gap}" width="${w + gap * 2}" height="${h + gap * 2}" vector-effect="non-scaling-stroke"/><g class="comment-pin" role="button" aria-label="Comment ${index + 1} by ${escape(c.actor.replace(/^Guest: /, ''))}${working ? ', assistant working' : ''}" transform="translate(${x + w} ${y}) scale(${1 / scale})">${working ? `<circle class="comment-spin" r="15" style="animation-delay:-${(performance.now() % 900) / 1000}s"/>` : ''}<circle r="11"/><text>${index + 1}</text></g></g>`;
  }).join('');
  // A pin removed under the pointer never reports the pointer leaving, so a
  // peek at a comment resolved or removed elsewhere closes with its marker.
  const peek = $('comment-peek');
  if (!peek.hidden && ![...$('comment-markers').children].some((m) => m.dataset.commentId === peek.dataset.commentId))
    peek.hidden = true;
}
/* Show board BOX whole, clear of the zoom control floating over the canvas foot. */
function frame(box) {
  const c = $('canvas').getBoundingClientRect(), zoom = document.querySelector('.zoom-tools')?.getBoundingClientRect();
  const room = c.height - (zoom ? Math.max(0, c.bottom - zoom.top) : 0);
  if (!c.width || room <= 0) { view = box; return; }
  const [x, y, w, h] = box, scale = Math.min(c.width / w, room / h);
  view = [x - (c.width / scale - w) / 2, y - (room / scale - h) / 2, c.width / scale, c.height / scale];
}
function revealObjects(comment) {
  const scene = currentScene(), box = commentAnchor(comment, scene);
  if (!box) throw new Error('The commented objects were removed');
  selected = new Set(shapeList().filter((s) => comment.selection?.includes(s.id)).map((s) => s.id));
  selectionRegion = comment.region || null;
  frame([box[0] - 30, box[1] - 30, Math.max(100, box[2] + 60), Math.max(100, box[3] + 60)]);
  selectTool('select');
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
  const hits = shapesInRegion(shapeList(), d.region, d.touching, currentScene())
    .filter((s) => !s.locked).flatMap((s) => [...withGroup(s.id)]);
  return new Set(d.additive ? [...d.base, ...hits] : hits);
}
/* The group an element moves with: its outermost group or, inside the
   entered group, the next group inward. Null for an ungrouped element. */
function unitGroup(element) {
  const groups = element?.groupIds || [], at = editingGroup ? groups.indexOf(editingGroup) : -1;
  return at > 0 ? groups[at - 1] : at === 0 ? null : groups.at(-1) ?? null;
}
/* The selectable unit for element ID: its group's members, or itself. */
function withGroup(id) {
  const group = unitGroup(currentScene().byId.get(id));
  return new Set(group ? shapeList().filter((s) => s.groupIds?.includes(group)).map((s) => s.id) : [id]);
}
/* The selection's frame: its units by group, and the box handles act on. */
function selectionFrame(scene) {
  const items = [...selected].map((id) => scene.byId.get(id)).filter(Boolean);
  if (!items.length) return null;
  const units = new Map();
  for (const e of items) {
    const key = unitGroup(e) ?? e.id;
    units.set(key, [...(units.get(key) || []), e]);
  }
  const single = items.length === 1 ? items[0] : null;
  return single ? { units, single, box: geometryBox(single), angle: single.angle || 0 }
    : { units, box: extent(items, scene), angle: 0 };
}
const SELECTION = 'fill="none" stroke="var(--board-selection)" vector-effect="non-scaling-stroke"';
const outlineSVG = ([x, y, w, h], angle, gap, extra = '') =>
  `<rect x="${x - gap}" y="${y - gap}" width="${w + gap * 2}" height="${h + gap * 2}"${angle ? ` transform="rotate(${(angle * 180) / Math.PI} ${x + w / 2} ${y + h / 2})"` : ''} ${SELECTION} ${extra}/>`;
/* Selection outlines and handles, following Excalidraw: one outline per
   element or group, eight resize handles and a rotation handle on the
   frame, and point handles on a lone arrow or line. */
function selectionSVG(scene, scale) {
  const frame = selectionFrame(scene);
  let svg = '';
  if (editingGroup) {
    const members = scene.order.filter((e) => e.groupIds?.includes(editingGroup));
    if (members.length) svg += outlineSVG(extent(members, scene), 0, 8 / scale, 'stroke-dasharray="6 4" stroke-width="1"');
  }
  if (!frame) return svg;
  const gap = 4 / scale;
  for (const members of frame.units.values())
    svg += members.length === 1 && !(frame.single && LINEAR.includes(frame.single.type))
      ? outlineSVG(geometryBox(members[0]), members[0].angle || 0, gap, 'stroke-width="1.5"')
      : members.length > 1 ? outlineSVG(extent(members, scene), 0, gap, 'stroke-width="1" stroke-dasharray="5 3"') : '';
  if (frame.units.size > 1) svg += outlineSVG(frame.box, 0, gap * 2, 'stroke-width="1" stroke-dasharray="3 3"');
  if (readOnly) return svg;
  const line = frame.single && LINEAR.includes(frame.single.type) && !frame.single.angle ? frame.single : null;
  if (line) {
    const r = 5 / scale, path = line.path;
    svg += path.map(([x, y], i) => `<circle data-point="${i}" cx="${x}" cy="${y}" r="${r}" fill="#fff" stroke="var(--board-selection)" stroke-width="1.5" vector-effect="non-scaling-stroke"/>`).join('');
    if (!line.elbowed)
      svg += path.slice(1).map((p, i) => `<circle data-mid="${i}" cx="${(p[0] + path[i][0]) / 2}" cy="${(p[1] + path[i][1]) / 2}" r="${r * 0.8}" fill="var(--board-selection)" fill-opacity=".55" stroke="none"/>`).join('');
    return svg;
  }
  const pad = frame.units.size > 1 ? gap * 2 : gap, [x, y, w, h] = frame.box;
  const size = 8 / scale;
  for (const { name, point: [hx, hy], cursor } of handlePositions([x - pad, y - pad, w + pad * 2, h + pad * 2], frame.angle, scale))
    svg += name === 'rotation'
      ? `<circle data-handle="rotation" cx="${hx}" cy="${hy}" r="${size * 0.6}" fill="#fff" stroke="var(--board-selection)" stroke-width="1.5" vector-effect="non-scaling-stroke" style="cursor:${cursor}"/>`
      : `<rect data-handle="${name}" x="${hx - size / 2}" y="${hy - size / 2}" width="${size}" height="${size}" rx="${size / 4}" fill="#fff" stroke="var(--board-selection)" stroke-width="1.5" vector-effect="non-scaling-stroke" style="cursor:${cursor}"${frame.angle ? ` transform="rotate(${(frame.angle * 180) / Math.PI} ${hx} ${hy})"` : ''}/>`;
  return svg;
}
/* The board area attached to the open question or comment draft. */
function attachedRegion() {
  if (!assistant || $('assistant').hidden) return null;
  const draft = assistant.drafts[assistant.drafts.view === 'comments' ? 'comment' : 'question'];
  return draft?.attachment?.snapshot?.scope === 'selection' ? draft.attachment.region || null : null;
}
/* Bound labels of the elements IDS. */
const labelsOf = (ids) => shapeList().filter((s) => s.type === 'text' && ids.has(s.containerId)).map((s) => s.id);
const topIndex = () => shapeList().reduce((top, s) => (s.index && (!top || s.index > top) ? s.index : top), null);
/* Fresh z-order keys above everything, for N new elements. */
const nextIndices = (n) => generateNKeysBetween(topIndex(), null, n);
/* A new element of TYPE in the current style. */
function styledElement(type, geometry) {
  const element = { id: crypto.randomUUID(), type, ...geometry, seed: randomSeed(), index: nextIndices(1)[0] };
  for (const [key, value] of Object.entries(current)) {
    // Text style reaches labels through their own text elements.
    const textual = ['fontSize', 'fontFamily', 'textAlign'].includes(key);
    if (key === 'verticalAlign') continue;
    if (key === 'arrowType') Object.assign(element, type !== 'arrow' ? {} : value === 'elbow' ? { elbowed: true }
      : value === 'round' ? { roundness: { type: 2 } } : {});
    else if (textual ? type === 'text' : applies(key, type))
      element[key] = key === 'roundness' ? roundnessFor(type, value) : value;
  }
  if (type === 'stickynote' && [undefined, 'transparent'].includes(element.backgroundColor)) element.backgroundColor = '#ffdf6b';
  if (element.roundness === null) delete element.roundness;
  return element;
}
/* SVG for the element being drawn. */
/* The colour the board's canvas is drawn on: its own, or the theme's. */
const canvasPaper = () => doc.getMap('meta').get('background')
  || getComputedStyle(document.documentElement).getPropertyValue('--board-bg').trim() || '#ffffff';
function drawingPreview(d) {
  if (!d.element) return '';
  const scene = { ...resolveScene([d.element]), paper: canvasPaper() };
  return elementSVG(scene.byId.get(d.element.id), scene, filesOf(doc));
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
  // Samples drawn while the button is held carry an ink index, so observers keep them longer.
  const held = mode === 'laser' && drag?.mode === 'laser', last = laserSamples.at(-1);
  laserSamples = mode === 'laser' ? laserSamples.filter(s => now - s[2] < 550).slice(-63) : [];
  if (mode === 'laser' && (!laserSamples.length || Math.floor(now / 8) !== Math.floor(laserSamples.at(-1)[2] / 8)))
    laserSamples.push([...point, now, held ? ++laserInk : 0]);
  else if (mode === 'laser') laserSamples[laserSamples.length - 1] = [...point, now, held ? last[3] || ++laserInk : last[3]];
  pointing = { point, mode };
  if (mode === 'laser') showPresence({ peer: 'self', name: participant, point, mode,
    trail: laserSamples.map(([x,y,time,ink]) => [x,y,now-time,ink]) });
  // Coalesce a burst, but always deliver its final position even if motion stops.
  if (presenceTimer) return;
  presenceTimer = setTimeout(() => {
    presenceTimer = null;
    lastPresence = performance.now();
    if (pointing && online) port.postMessage({ type: 'presence', ...pointing,
      preview: pointing.mode === 'cursor' && !failed ? outgoingPreview : null,
      trail: pointing.mode === 'laser' ? laserSamples.filter(s => lastPresence - s[2] < 550)
        .map(([x,y,time,ink]) => [x,y,lastPresence-time,ink]) : undefined });
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
  // Leaving the tool ends the strokes it was collecting.
  if (value !== 'autoshape') settlePendingShape();
  stopPointing();
  $('menu').open = false;
  tool = value;
  hoverShape = null;
  document
    .querySelectorAll('[data-tool]')
    .forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.tool === value)));
  $('canvas').style.cursor =
    value === 'pan' ? 'grab' : value === 'select' ? 'default' : 'crosshair';
  if (drawable.includes(value)) { selected.clear(); selectionRegion = null; editingGroup = null; }
  draw();
}
/* After a new drawing, selection takes over so a double-click edits it,
   unless the tool is locked; the pen always stays, as strokes come in runs. */
function finishDrawing(ids) {
  if (toolLocked || tool === 'freedraw') { selected.clear(); draw(); }
  else { selected = new Set(ids); selectTool('select'); }
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
  const scene = displayedScene(), item = scene.byId.get(textEditing);
  if (!item) { $('shape-text').blur(); return; }
  const container = item.bound && scene.byId.get(item.containerId);
  const input = $('shape-text'), matrix = $('canvas').getScreenCTM();
  let x = item.x, width = Math.max(item.width, item.fontSize);
  if (container && container.type !== 'arrow') {
    const box = containerTextBox(container, item.fontSize);
    x = container.x + box.x;
    width = box.width;
  }
  const point = new DOMPoint(x, item.y).matrixTransform(matrix);
  Object.assign(input.style, {
    left: `${point.x}px`, top: `${point.y}px`, width: `${(width + 2) * matrix.a}px`,
    height: `${Math.max(item.height, item.fontSize * item.lineHeight) * matrix.a}px`,
    fontSize: `${item.fontSize * matrix.a}px`, lineHeight: String(item.lineHeight),
    fontFamily: fontStack(item.fontFamily), textAlign: item.textAlign, color: item.strokeColor,
    whiteSpace: container ? 'pre-wrap' : 'pre',
  });
}
/* Grow a label's container until the label fits, as Excalidraw does. */
function fitContainer(textId) {
  const scene = currentScene(), label = scene.byId.get(textId);
  const container = label?.bound && scene.byId.get(label.containerId);
  if (!container || container.type === 'arrow') return;
  const box = containerTextBox(container, label.fontSize), g = geometryOf(container.id);
  const grow = (text, room, size) => container.type === 'stickynote' ? text + size - room : containerSizeFor(text, container.type);
  const width = label.width > box.width ? Math.max(g.width, grow(label.width, box.width, g.width)) : g.width;
  const height = label.height > box.height ? Math.max(g.height, grow(label.height, box.height, g.height)) : g.height;
  if (width !== g.width || height !== g.height)
    doc.transact(() => elementMap().get(container.id).set('geometry', { ...g, width, height }), local);
}
/* Edit the text of element ID: a text element, or a container's label,
   created on demand. Empty text is removed when editing finishes. */
function editText(id) {
  if (readOnly) return;
  const element = elementMap().get(id);
  if (!element) return;
  let textId = id, created = false;
  undo?.stopCapturing();
  if (element.get('type') !== 'text') {
    if (!CONTAINERS.includes(element.get('type'))) return;
    textId = shapeList().find(s => s.type === 'text' && s.containerId === id)?.id;
    if (!textId) {
      textId = crypto.randomUUID();
      created = true;
      const g = element.get('geometry');
      doc.transact(() => putElement(doc, { id: textId, type: 'text', x: g.x, y: g.y, width: 0, height: 0,
        text: '', containerId: id, strokeColor: element.get('strokeColor') ?? current.strokeColor,
        fontSize: current.fontSize, fontFamily: current.fontFamily, textAlign: 'center',
        verticalAlign: 'middle', seed: randomSeed(), index: nextIndices(1)[0] }), local);
    }
  }
  const input = $('shape-text');
  textEditing = textId;
  const before = elementMap().get(textId).get('text') || '';
  input.value = before;
  draw();
  input.hidden = false;
  input.focus();
  input.select();
  const finish = (save) => {
    if (textEditing !== textId) return;
    textEditing = null;
    input.hidden = true;
    const text = elementMap().get(textId), unchanged = text?.get('text') === input.value;
    if (!text) { /* removed by another writer */ }
    else if (!save && unchanged)
      doc.transact(() => (created ? elementMap().delete(textId) : text.set('text', before)), local);
    else if (save && !text.get('text').trim()) doc.transact(() => elementMap().delete(textId), local);
    else if (save) fitContainer(textId);
    undo?.stopCapturing();
    draw();
  };
  input.oninput = () => {
    const text = elementMap().get(textId);
    if (!text) return;
    doc.transact(() => {
      text.set('text', input.value);
      // Typed text is the source; the label wraps it for display.
      text.delete('originalText');
      if (!text.get('containerId') && text.get('autoResize') !== false) {
        const size = measure(input.value, text.get('fontFamily') ?? 5, text.get('fontSize') ?? 20, text.get('lineHeight'));
        text.set('geometry', { ...text.get('geometry'), ...size });
      }
    }, local);
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
/* Content-addressed file id, as Excalidraw names image files. */
async function fileIdOf(file) {
  if (!crypto.subtle) return crypto.randomUUID().replace(/-/g, '');
  const digest = new Uint8Array(await crypto.subtle.digest('SHA-1', await file.arrayBuffer()));
  return [...digest].map(b => b.toString(16).padStart(2, '0')).join('');
}
async function insertImages(files, position) {
  if (readOnly || !files.length) return;
  try {
    const binding = editor && ySyncPluginKey.getState(editor.state).binding;
    const anchor = binding && absolutePositionToRelativePosition(
      position ?? editor.state.selection.from, binding.type, binding.mapping);
    // Without a drop point, images arrive at the view's centre, clear of the panels.
    const origin = editor ? null : position || [view[0] + view[2] / 2, view[1] + view[3] / 2];
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
      images.push({src, alt: file.name, width: bitmap.width * scale, height: bitmap.height * scale,
        mimeType: file.type, fileId: editor ? null : await fileIdOf(file)});
      bitmap.close();
    }
    if (readOnly) return;
    undo.stopCapturing();
    if (editor) {
      const current = ySyncPluginKey.getState(editor.state).binding;
      const at = relativePositionToAbsolutePosition(doc, current.type, anchor, current.mapping);
      if (at === null) throw new Error('The image insertion point was removed; try again');
      const content = images.map(({src, alt, width, height}) =>
        ({type:'image', attrs:{src, alt, width, height, id:crypto.randomUUID()}}));
      validateDocument({...editor.getJSON(), content:[...editor.getJSON().content, ...content]});
      editor.chain().focus().insertContentAt(at, content).run();
    } else {
      const indices = nextIndices(images.length);
      const shapes = images.map((image, index) => ({
        id: crypto.randomUUID(), type: 'image', fileId: image.fileId, status: 'saved', scale: [1, 1],
        x: origin[0] + index * 24 - (position ? 0 : image.width / 2), y: origin[1] + index * 24 - (position ? 0 : image.height / 2),
        width: image.width, height: image.height,
        seed: randomSeed(), index: indices[index],
      }));
      doc.transact(() => images.forEach((image, index) => {
        putFile(doc, image.fileId, { mimeType: image.mimeType, dataURL: image.src, created: Date.now() });
        putElement(doc, shapes[index]);
      }), local);
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
const TAU = Math.PI * 2, QUARTER = Math.PI / 2;
const quarterOf = (angle) => Math.round((((angle % TAU) + TAU) % TAU) / QUARTER) % 4;
const naturalSizes = new Map();
async function naturalSize(src) {
  if (!naturalSizes.has(src)) {
    const image = new Image();
    image.src = src;
    await image.decode();
    naturalSizes.set(src, [image.naturalWidth, image.naturalHeight]);
  }
  return naturalSizes.get(src);
}
/* The image dialog's edit for a board image. Excalidraw rotates after
   flipping and the dialog flips after rotating: one flip reverses the turn. */
function boardImageEdit(e, [nw, nh]) {
  const flipX = e.scale[0] < 0, flipY = e.scale[1] < 0, quarter = quarterOf(e.angle);
  const rotation = (flipX === flipY ? quarter * 90 : 360 - quarter * 90) % 360;
  const crop = e.crop ? [e.crop.x / nw, e.crop.y / nh, e.crop.width / nw, e.crop.height / nh] : [0, 0, 1, 1];
  return { crop, rotation, flipX, flipY };
}
/* Excalidraw crop, flip and rotation for a dialog EDIT (null resets), at
   the image's current display scale and centre. */
async function boardImageChanges(snapshot, edit) {
  const [nw, nh] = await naturalSize(snapshot.src), e = snapshot.element;
  const previous = boardImageEdit(e, [nw, nh]), next = edit || { crop: [0, 0, 1, 1], rotation: 0, flipX: false, flipY: false };
  const kx = e.width / (previous.crop[2] * nw), ky = e.height / (previous.crop[3] * nh);
  const width = next.crop[2] * nw * kx, height = next.crop[3] * nh * ky;
  const turn = ((next.flipX === next.flipY ? next.rotation : -next.rotation) * Math.PI) / 180;
  const full = next.crop.every((v, i) => Math.abs(v - [0, 0, 1, 1][i]) < 1e-9);
  return {
    x: e.x + (e.width - width) / 2, y: e.y + (e.height - height) / 2, width, height,
    angle: edit ? e.angle - quarterOf(e.angle) * QUARTER + turn : 0,
    scale: [next.flipX ? -1 : 1, next.flipY ? -1 : 1],
    crop: full ? null : { x: next.crop[0] * nw, y: next.crop[1] * nh, width: next.crop[2] * nw,
      height: next.crop[3] * nh, naturalWidth: nw, naturalHeight: nh },
  };
}
/* Stored geometry for an arrow drawn where it is displayed now, when its
   bindings are about to be dropped. */
function detachedGeometry(id) {
  const item = displayedScene().byId.get(id), g = geometryOf(id);
  if (!item?.path || item.angle) return g;
  const [x1, y1, x2, y2] = pointBounds(item.path), [x, y] = item.path[0];
  return { ...g, x, y, width: x2 - x1, height: y2 - y1, points: item.path.map(([px, py]) => [px - x, py - y]) };
}
/* Bindings of arrows in MOVED whose targets stay where they are: a moved
   connector leaves them, as in Excalidraw. Maps arrow id to binding keys. */
function looseEnds(moved) {
  const loose = new Map();
  for (const id of moved) {
    const arrow = elementMap().get(id);
    if (arrow?.get('type') !== 'arrow') continue;
    const keys = ['startBinding', 'endBinding'].filter((key) => arrow.get(key) && !moved.has(arrow.get(key).elementId));
    if (keys.length) loose.set(id, keys);
  }
  return loose;
}
/* Unbind the loose ends of MOVED arrows, keeping them where they are drawn. */
function detachArrows(moved) {
  for (const [id, keys] of looseEnds(moved)) {
    const arrow = elementMap().get(id);
    arrow.set('geometry', detachedGeometry(id));
    for (const key of keys) arrow.delete(key);
  }
}
const typeNames = {
  rectangle: 'Rectangle', diamond: 'Diamond', ellipse: 'Ellipse', stickynote: 'Sticky note', arrow: 'Arrow',
  line: 'Line', freedraw: 'Drawing', text: 'Text', image: 'Image', frame: 'Frame', magicframe: 'Frame',
  embeddable: 'Embed', iframe: 'Embed', autoshape: 'Shape from drawing',
};
/* Excalidraw's canvas colours, after the theme's own board colour. */
const BACKGROUNDS = [['', 'Theme'], ['#ffffff', 'White'], ['#f8f9fa', 'Grey'], ['#f5faff', 'Blue'],
  ['#fffce8', 'Yellow'], ['#fdf8f6', 'Rose']];
/* The canvas colour is the board's own, shared and undoable like an edit. */
function canvasBackground() {
  const section = $('canvas-background'), row = section.querySelector('.row');
  section.hidden = readOnly;
  if (readOnly) return;
  const set = (value) => doc.transact(() => {
    if (value) doc.getMap('meta').set('background', value);
    else doc.getMap('meta').delete('background');
  }, local);
  row.innerHTML = BACKGROUNDS.map(([value, label]) =>
    `<button type="button" class="swatch" data-background="${value}" style="background:${value || 'var(--board-bg)'}" aria-label="Canvas background: ${label}" title="${label}"></button>`).join('')
    + '<label class="swatch custom" title="Custom canvas colour"><input type="color" aria-label="Custom canvas colour"></label>';
  for (const b of row.querySelectorAll('[data-background]')) b.onclick = () => set(b.dataset.background);
  // A picker commits once it closes, not for every colour it passes through.
  row.querySelector('input').onchange = (event) => set(event.target.value);
}
function board() {
  document.body.dataset.kind = 'whiteboard';
  $('board').hidden = false;
  loadFonts();
  canvasBackground();
  imageTools($('image-tools'), async () => {
    const shape = selected.size === 1 && shapeList().find(s => selected.has(s.id) && s.type === 'image');
    const file = shape && filesOf(doc)[shape.fileId];
    if (readOnly || !file) return null;
    const element = currentScene().byId.get(shape.id);
    return { id: shape.id, src: file.dataURL, element, stored: shape,
      imageEdit: boardImageEdit(element, await naturalSize(file.dataURL)), width: element.width, height: element.height };
  }, (before, changes) => {
    const now = shapeList().find(s => s.id === before.id);
    if (readOnly || !now || JSON.stringify(now) !== JSON.stringify(before.stored))
      throw new Error('This image changed while you were editing it. Reopen the image tools to try again.');
    const after = { ...now, ...changes };
    if (after.crop === null) delete after.crop;
    if (!after.angle) delete after.angle;
    const candidate = restore(encode(doc));
    try { putElement(candidate, after); validate(candidate); } finally { candidate.destroy(); }
    undo.stopCapturing();
    doc.transact(() => putElement(doc, after), local);
    undo.stopCapturing();
  }, message => status(message, true), boardImageChanges);
  const tools = [
    ['pan', 'Hand', 'H'],
    ['select', 'Selection', 'V'],
    ['rectangle', 'Rectangle', 'R'],
    ['diamond', 'Diamond', 'D'],
    ['ellipse', 'Ellipse', 'O'],
    ['stickynote', 'Sticky note', 'N'],
    ['arrow', 'Arrow', 'A'],
    ['line', 'Line', 'L'],
    ['freedraw', 'Draw', 'P'],
    ['autoshape', 'Shape from drawing', 'Shift X'],
    ['text', 'Text', 'T'],
    ['erase', 'Eraser', 'E'],
    ['laser', 'Laser pointer', 'K'],
    ['comment', 'Comment', 'M'],
  ];
  const icons = {
    pan: 'M8 13V6a2 2 0 0 1 4 0v6-8a2 2 0 0 1 4 0v8-6a2 2 0 0 1 4 0v9c0 5-3 7-7 7-3 0-5-2-7-5l-3-4a2 2 0 0 1 3-2l2 2',
    select: 'm5 3 14 10-7 1-3 7Z',
    rectangle: 'M4 4h16v16H4Z',
    diamond: 'm12 3 9 9-9 9-9-9Z',
    ellipse: 'M21 12a9 7 0 1 0-18 0 9 7 0 1 0 18 0',
    stickynote: 'M4 4h16v11l-5 5H4Zm11 16v-5h5',
    arrow: 'M4 20 20 4M10 4h10v10',
    line: 'M4 20 20 4',
    freedraw: 'M3 18c5-20 4 12 10-5s8-8 8-8',
    autoshape: 'M3 19c2-7 5 1 8-5s5-3 7-6M4 4h7v7H4ZM17 14l1 2 2 1-2 1-1 2-1-2-2-1 2-1Z',
    text: 'M4 5h16M12 5v15M8 20h8',
    erase: 'm3 15 12-12 7 7-12 12H9Zm6-6 7 7M10 22h12',
    laser: 'm3 21 10-10 3 3L6 24ZM17 7l3-3M14 5V2M21 10h3',
    comment: 'M4 4h16v12H10l-5 4v-4H4Z',
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
    if (['rectangle', 'arrow', 'erase', 'comment'].includes(value)) b.classList.add('tool-group-start');
    b.title = `${label} (${key})`;
    b.setAttribute('aria-label', label);
    b.setAttribute('aria-keyshortcuts', key.replace(' ', '+'));
    b.setAttribute('aria-pressed', String(value === 'select'));
    b.innerHTML = `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="${icons[value]}"/></svg><kbd>${key.replace('Shift ', '⇧')}</kbd>`;
  }
  if (!readOnly) {
    const imageButton = button(strip, '', () => {});
    imageButton.id = 'board-image';
    imageButton.title = 'Insert image (9, or drop an image on the canvas)';
    imageButton.setAttribute('aria-label', 'Insert image');
    imageButton.setAttribute('aria-keyshortcuts', '9');
    imageButton.className = 'tool-group-start';
    imageButton.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><rect x="3" y="3" width="18" height="18" rx="2"/><circle cx="8" cy="8" r="1.5"/><path d="m3 18 6-6 4 4 3-4 5 6"/></svg>';
    // The lock is this browser's own: it is not part of the board.
    const lock = button(strip, '', () => { toolLocked = !toolLocked; showLock(); });
    const showLock = () => {
      lock.setAttribute('aria-pressed', String(toolLocked));
      lock.innerHTML = `<svg viewBox="0 0 24 24" aria-hidden="true"><rect x="5" y="11" width="14" height="10" rx="2"/><path d="${toolLocked ? 'M8 11V7a4 4 0 0 1 8 0v4' : 'M8 11V7a4 4 0 0 1 7.7-1.5'}"/></svg><kbd>Q</kbd>`;
    };
    lock.id = 'board-tool-lock';
    lock.className = 'tool-group-start';
    lock.title = 'Keep the tool after drawing (Q)';
    lock.setAttribute('aria-label', 'Keep the tool after drawing');
    lock.setAttribute('aria-keyshortcuts', 'Q');
    showLock();
  }
  const properties = document.createElement('details');
  properties.id = 'properties';
  properties.className = 'popover';
  properties.innerHTML = '<summary>Style</summary><div class="menu-body"></div>';
  commands.append(properties);
  const panel = properties.lastElementChild;
  const TEXTUAL = ['fontSize', 'fontFamily', 'textAlign', 'verticalAlign'];
  /* The elements a style KEY changes for the selection: the selected
     elements it applies to, and labels for text and label styles. */
  const styleTargets = (key) => {
    const chosen = shapeList().filter((s) => selected.has(s.id));
    const labels = new Set(labelsOf(selected));
    // Vertical alignment places a label in its container; free text has none.
    const own = key === 'verticalAlign' ? []
      : chosen.filter((s) => (TEXTUAL.includes(key) ? s.type === 'text' : applies(key, s.type)));
    const bound = LABEL_STYLES.includes(key) || TEXTUAL.includes(key)
      ? shapeList().filter((s) => labels.has(s.id)) : [];
    return [...own, ...bound];
  };
  const value = (key) => {
    const target = styleTargets(key)[0];
    if (!target) return current[key];
    const full = currentScene().byId.get(target.id);
    if (key === 'arrowType') return full.elbowed ? 'elbow' : full.roundness ? 'round' : 'sharp';
    return key === 'roundness' ? (full.roundness ? 'round' : 'sharp') : full[key];
  };
  const style = (key, v) => {
    current[key] = v;
    doc.transact(() => {
      for (const target of styleTargets(key)) {
        const element = elementMap().get(target.id);
        if (key === 'roundness') {
          const roundness = roundnessFor(target.type, v);
          roundness ? element.set('roundness', roundness) : element.delete('roundness');
        } else if (key === 'arrowType') {
          v === 'round' ? element.set('roundness', { type: 2 }) : element.delete('roundness');
          v === 'elbow' ? element.set('elbowed', true) : element.delete('elbowed');
          // A straight two-point arrow gains a bend, so a curve is visible to adjust.
          const g = detachedGeometry(target.id);
          if (v === 'round' && g.points?.length === 2) {
            const [[ax, ay], [bx, by]] = g.points, k = 0.15;
            const mid = [(ax + bx) / 2 - (by - ay) * k, (ay + by) / 2 + (bx - ax) * k];
            const points = [g.points[0], mid, g.points[1]], [x1, y1, x2, y2] = pointBounds(points);
            element.set('geometry', { ...g, points, width: x2 - x1, height: y2 - y1 });
          }
          if (v !== 'round' && g.points) element.set('geometry', { ...g, points: [g.points[0], g.points.at(-1)] });
        } else element.set(key, v);
        if (key === 'fontFamily' && element.has('lineHeight')) element.delete('lineHeight');
      }
    }, local);
    refresh();
  };
  const remove = () =>
    doc.transact(() => {
      for (const id of [...selected, ...labelsOf(selected)]) elementMap().delete(id);
    }, local);
  const reorder = (front) => {
    const order = shapeList(), moving = new Set([...selected, ...labelsOf(selected)]);
    const moved = order.filter((s) => moving.has(s.id)), rest = order.filter((s) => !moving.has(s.id));
    if (!moved.length) return;
    doc.transact(() => {
      // Unindexed elements draw on top; give them keys so the move holds.
      const unindexed = rest.filter((s) => !s.index);
      const restKeys = generateNKeysBetween(rest.filter((s) => s.index).at(-1)?.index ?? null, null, unindexed.length);
      unindexed.forEach((s, i) => elementMap().get(s.id).set('index', restKeys[i]));
      const top = restKeys.at(-1) ?? rest.filter((s) => s.index).at(-1)?.index ?? null;
      const bottom = rest.find((s) => s.index)?.index ?? restKeys[0] ?? null;
      const keys = front ? generateNKeysBetween(top, null, moved.length) : generateNKeysBetween(null, bottom, moved.length);
      moved.forEach((s, i) => elementMap().get(s.id).set('index', keys[i]));
    }, local);
  };
  /* Copies of IDS (with their labels) offset by DELTA, keeping references
     among the copies and dropping the rest. */
  const copyElements = (ids, delta) => {
    const sources = shapeList().filter((s) => ids.has(s.id) || (s.type === 'text' && ids.has(s.containerId)));
    const fresh = new Map(sources.map((s) => [s.id, crypto.randomUUID()]));
    const groups = new Map();
    const group = (g) => (groups.has(g) || groups.set(g, crypto.randomUUID().replace(/-/g, '')), groups.get(g));
    const keys = nextIndices(sources.length);
    return sources.map((s, i) => {
      const copy = { ...s, ...(s.type === 'arrow' ? detachedGeometry(s.id) : {}), id: fresh.get(s.id),
        seed: randomSeed(), index: keys[i] };
      copy.x += delta; copy.y += delta;
      if (copy.groupIds) copy.groupIds = copy.groupIds.map(group);
      if (copy.containerId) copy.containerId = fresh.get(copy.containerId) ?? null;
      for (const key of ['startBinding', 'endBinding']) {
        if (!copy[key]) continue;
        if (fresh.has(copy[key].elementId)) copy[key] = { ...copy[key], elementId: fresh.get(copy[key].elementId) };
        else delete copy[key];
      }
      if (copy.containerId === null) delete copy.containerId;
      return copy;
    });
  };
  const duplicate = () => {
    const copies = copyElements(selected, 10);
    if (!copies.length) return;
    doc.transact(() => copies.forEach((s) => putElement(doc, s)), local);
    selected = new Set(copies.filter((s) => !s.containerId).map((s) => s.id));
    draw();
  };
  const regroup = (join) => doc.transact(() => {
    const id = crypto.randomUUID().replace(/-/g, '');
    for (const s of shapeList().filter((e) => selected.has(e.id))) {
      const element = elementMap().get(s.id), groupIds = s.groupIds || [];
      const next = join ? [...groupIds, id] : groupIds.slice(0, -1);
      next.length ? element.set('groupIds', next) : element.delete('groupIds');
    }
  }, local);
  const lock = () => {
    doc.transact(() => { for (const id of selected) elementMap().get(id)?.set('locked', true); }, local);
    selected.clear();
  };
  const unlockAll = () => doc.transact(() => {
    for (const s of shapeList()) if (s.locked) elementMap().get(s.id).delete('locked');
  }, local);
  if (!readOnly) {
    const icon = (inner) => `<svg viewBox="0 0 24 24" aria-hidden="true">${inner}</svg>`;
    const option = (key, v, label, body) =>
      `<button type="button" class="opt" data-prop="${key}" data-val="${v}" title="${label}" aria-label="${label}">${body}</button>`;
    const swatch = (key, label, color) =>
      `<button type="button" class="swatch${color === 'transparent' ? ' transparent' : ''}"${color === 'transparent' ? '' : ` style="background:${color}"`} data-prop="${key}" data-val="${color}" aria-label="${label}: ${color}"></button>`;
    const colors = (key, label, list) =>
      list.map((c) => swatch(key, label, c)).join('') +
      `<label class="swatch custom" title="Custom ${label.toLowerCase()} colour"><input type="color" data-color="${key}" aria-label="Custom ${label.toLowerCase()} colour"></label>`;
    const section = (key, title, body) =>
      `<div class="sec" data-sec="${key}"><h4>${title}</h4><div class="row">${body}</div></div>`;
    const heads = (key) => [['null', 'None', '<path d="M4 12h16"/>'], ['arrow', 'Arrow', '<path d="M4 12h16M14 6l6 6-6 6"/>'],
      ['triangle', 'Triangle', '<path d="M4 12h10"/><path d="m13 6 7 6-7 6Z" fill="currentColor"/>'],
      ['circle', 'Circle', '<path d="M4 12h11"/><circle cx="17" cy="12" r="3" fill="currentColor"/>'],
      ['bar', 'Bar', '<path d="M4 12h16M20 6v12"/>']]
      .map(([v, label, body]) => option(key, v, `${key === 'startArrowhead' ? 'Start' : 'End'} arrowhead: ${label}`,
        icon(key === 'startArrowhead' ? `<g transform="matrix(-1 0 0 1 24 0)">${body}</g>` : body))).join('');
    panel.innerHTML =
      section('strokeColor', 'Stroke', colors('strokeColor', 'Stroke', ['#1e1e1e', '#e03131', '#2f9e44', '#1971c2', '#f08c00'])) +
      section('backgroundColor', 'Background', colors('backgroundColor', 'Background', ['transparent', '#ffc9c9', '#b2f2bb', '#a5d8ff', '#ffec99'])) +
      section(
        'fillStyle',
        'Fill',
        option('fillStyle', 'hachure', 'Hachure', icon('<rect x="4" y="4" width="16" height="16" rx="2"/><path d="M4 12l8-8M4 19 19 4M10 20 20 10" stroke-width="1.3"/>')) +
          option('fillStyle', 'cross-hatch', 'Cross-hatch', icon('<rect x="4" y="4" width="16" height="16" rx="2"/><path d="M4 12l8-8M4 19 19 4M10 20 20 10M12 4l8 8M5 5l14 14M4 12l8 8" stroke-width="1.3"/>')) +
          option('fillStyle', 'solid', 'Solid', icon('<rect x="4" y="4" width="16" height="16" rx="2" fill="currentColor"/>')),
      ) +
      section(
        'strokeWidth',
        'Stroke width',
        [
          [1, 'Thin', 1.5],
          [2, 'Bold', 3],
          [4, 'Extra bold', 5],
        ]
          .map(([v, label, w]) => option('strokeWidth', v, label, icon(`<path d="M5 12h14" stroke-width="${w}"/>`)))
          .join(''),
      ) +
      section(
        'strokeStyle',
        'Stroke style',
        option('strokeStyle', 'solid', 'Solid', icon('<path d="M5 12h14"/>')) +
          option('strokeStyle', 'dashed', 'Dashed', icon('<path d="M4 12h4M10 12h4M16 12h4"/>')) +
          option('strokeStyle', 'dotted', 'Dotted', icon('<path d="M5 12h.01M9.5 12h.01M14 12h.01M18.5 12h.01" stroke-width="2.4"/>')),
      ) +
      section(
        'roughness',
        'Sloppiness',
        option('roughness', 0, 'Architect', icon('<path d="M4 16 10 8l5 7 5-8"/>')) +
          option('roughness', 1, 'Artist', icon('<path d="M4 16c2-3 3.5-8 6-8s2.5 7 5 7 3-6 5-8"/>')) +
          option('roughness', 2, 'Cartoonist', icon('<path d="M3.5 16c1.5-2 2-9 5-8.5S9 17 12.5 15.5s1-8 3.5-8.5 2 5 4.5 2"/>')),
      ) +
      section(
        'roundness',
        'Edges',
        option('roundness', 'sharp', 'Sharp', icon('<path d="M5 19V5h14"/>')) +
          option('roundness', 'round', 'Round', icon('<path d="M5 19v-8a6 6 0 0 1 6-6h8"/>')),
      ) +
      section(
        'arrowType',
        'Arrow type',
        option('arrowType', 'sharp', 'Sharp arrow', icon('<path d="M5 19 19 5M12 5h7v7"/>')) +
          option('arrowType', 'round', 'Curved arrow', icon('<path d="M5 19C5 11 11 6 19 6M14 3l5 3-4 4"/>')) +
          option('arrowType', 'elbow', 'Elbow arrow', icon('<path d="M5 19v-7h14V5M16 8l3-3 3 3" transform="translate(-2 0)"/>')),
      ) +
      section('startArrowhead', 'Start arrowhead', heads('startArrowhead')) +
      section('endArrowhead', 'End arrowhead', heads('endArrowhead')) +
      section(
        'fontFamily',
        'Font',
        [[5, 'Hand-drawn'], [6, 'Normal'], [8, 'Code']]
          .map(([v, label]) => option('fontFamily', v, label, `<span style="font-family:${fontStack(v)}">Aa</span>`))
          .join(''),
      ) +
      section(
        'fontSize',
        'Font size',
        [
          [16, 'S'],
          [20, 'M'],
          [28, 'L'],
          [36, 'XL'],
        ]
          .map(([v, label]) => option('fontSize', v, `Font size ${label}`, label))
          .join(''),
      ) +
      section(
        'textAlign',
        'Text align',
        [['left', 'Left', 'M4 6h16M4 12h10M4 18h13'], ['center', 'Center', 'M4 6h16M7 12h10M5.5 18h13'],
          ['right', 'Right', 'M4 6h16M10 12h10M7 18h13']]
          .map(([v, label, d]) => option('textAlign', v, `Align ${label.toLowerCase()}`, icon(`<path d="${d}"/>`)))
          .join(''),
      ) +
      section(
        'verticalAlign',
        'Vertical align',
        [['top', 'Top', 'M4 4h16'], ['middle', 'Middle', 'M4 12h5M15 12h5'], ['bottom', 'Bottom', 'M4 20h16']]
          .map(([v, label, d]) => option('verticalAlign', v, `Align ${label.toLowerCase()}`,
            icon(`<path d="${d}"/><rect x="9" y="${{ top: 6, middle: 8, bottom: 10 }[v]}" width="6" height="8" rx="1"/>`)))
          .join(''),
      ) +
      section('opacity', 'Opacity', '<input type="range" min="0" max="100" step="5" aria-label="Opacity"><output>100</output>');
    const caption = document.createElement('p');
    caption.className = 'property-caption';
    panel.prepend(caption);
    panel.onclick = (event) => {
      const b = event.target.closest('[data-prop]');
      if (b) {
        const key = b.dataset.prop, raw = b.dataset.val;
        undo.stopCapturing();
        style(key, raw === 'null' ? null : ['strokeWidth', 'roughness', 'fontSize', 'fontFamily'].includes(key) ? +raw : raw);
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
      const labelled = labelsOf(selected).length > 0;
      const show = Object.fromEntries(Object.keys(current).map((key) => [key, has((k) => applies(key, k))]));
      Object.assign(show, {
        fillStyle: show.fillStyle && value('backgroundColor') !== 'transparent',
        fontSize: kinds.has('text') || labelled || (!shapes.length && ['stickynote', 'text'].includes(tool)),
        fontFamily: kinds.has('text') || labelled || (!shapes.length && ['stickynote', 'text'].includes(tool)),
        textAlign: kinds.has('text') || labelled || (!shapes.length && tool === 'text'),
        verticalAlign: labelsOf(new Set(shapes.filter((s) => s.type !== 'arrow').map((s) => s.id))).length > 0,
        opacity: kinds.size > 0,
      });
      properties.firstElementChild.setAttribute('aria-disabled', String(!kinds.size));
      if (!kinds.size) properties.open = false;
      const name = typeNames[shapes[0]?.type || tool] || 'Element';
      caption.textContent = shapes.length > 1 ? `${shapes.length} objects` : shapes.length ? name : `New ${name.toLowerCase()}`;
      for (const sec of panel.querySelectorAll('.sec')) sec.hidden = !show[sec.dataset.sec];
      for (const b of panel.querySelectorAll('[data-prop]'))
        b.setAttribute('aria-pressed', String(String(value(b.dataset.prop)) === b.dataset.val));
      for (const input of panel.querySelectorAll('input[data-color]')) {
        const v = value(input.dataset.color);
        if (/^#[0-9a-f]{6}$/i.test(v)) input.value = v;
      }
      range.value = value('opacity');
      range.nextElementSibling.value = value('opacity');
    };
  }
  properties.hidden = readOnly;
  const canvas = $('canvas');
  const actions = [];
  const all = () => { selected = new Set(shapeList().filter(s => !s.locked && !(s.containerId && s.type === 'text')).map(s => s.id)); selectionRegion = null; selectTool('select'); };
  const clear = () => { selected.clear(); selectionRegion = null; editingGroup = null; selectTool('select'); };
  const move = (dx, dy, step = 1) => doc.transact(() => {
    selectionRegion = null;
    detachArrows(selected);
    for (const id of selected) {
      const shape = elementMap().get(id), geometry = shape.get('geometry');
      shape.set('geometry', { ...geometry, x: geometry.x + dx * step, y: geometry.y + dy * step });
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
    action(objectBody, 'Duplicate', duplicate, () => selected.size > 0, '⌘ / Ctrl D');
    action(objectBody, 'Group', () => regroup(true), () => selected.size > 1, '⌘ / Ctrl G');
    action(objectBody, 'Ungroup', () => regroup(false), () => shapeList().some(s => selected.has(s.id) && s.groupIds?.length), '⌘ / Ctrl ⇧ G');
    action(objectBody, 'Lock', lock, () => selected.size > 0);
    action(objectBody, 'Unlock all', unlockAll, () => shapeList().some(s => s.locked));
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
    pan: 'Drag or scroll to move around the canvas. Ctrl / ⌘ scroll or pinch to zoom.',
    text: 'Click to place text. Ctrl / ⌘ Enter to finish.',
    stickynote: 'Click to place a note. Ctrl / ⌘ Enter to finish.',
    arrow: 'Drag between objects to connect them. Connections follow the objects.',
    autoshape: 'Draw a rectangle, diamond, ellipse, arrow or line by hand; it becomes a clean shape.',
    erase: 'Click an object to erase it. Undo restores it.',
    laser: 'Move to point; hold to draw a line for a few seconds. Nothing changes the board.',
    comment: 'Click an object or drag across an area to comment on it.',
  };
  const refreshStyle = refresh;
  refresh = () => {
    refreshStyle();
    for (const [b, enabled] of actions) b.disabled = !enabled();
    const label = selected.size ? `${selected.size} object${selected.size === 1 ? '' : 's'} selected` : tools.find(([value]) => value === tool)?.[1];
    if ($('document-stat').textContent !== label) $('document-stat').textContent = label;
    $('hint').textContent = readOnly ? 'View only · Select objects or use the hand to explore.' : hints[tool] || 'Drag to draw. Choose Style to change the next object.';
  };
  const library = readOnly ? null : libraryPanel({
    parent: commands, request, report: (message) => status(message, true),
    selection: () => shapeList().filter((s) => selected.has(s.id) || (s.type === 'text' && selected.has(s.containerId))),
    download: (text) => port.postMessage({ type: 'recovery', result: { text, mime: 'application/vnd.excalidrawlib+json', extension: 'excalidrawlib' } }),
    insert: (elements) => {
      const center = [view[0] + view[2] / 2, view[1] + view[3] / 2];
      const placed = placeElements(elements, center, nextIndices(elements.length), randomSeed);
      undo.stopCapturing();
      doc.transact(() => placed.forEach((e) => putElement(doc, e)), local);
      undo.stopCapturing();
      selected = new Set(placed.filter((e) => !e.containerId).map((e) => e.id));
      selectTool('select');
    },
  });
  const menus = [objectMenu, properties, ...(library ? [library] : [])];
  const place = (menu) => {
    const body = menu.lastElementChild;
    body.style.marginLeft = '0px';
    const rect = body.getBoundingClientRect();
    body.style.marginLeft = `${Math.max(8 - rect.left, Math.min(0, innerWidth - 8 - rect.right))}px`;
  };
  // Style opens with a new selection on wide screens, as Excalidraw's panel
  // does, and closes with it; closing it keeps it closed for that selection.
  let styleKey = '', styleAuto = false, styleDismissed = false;
  autoStyle = () => {
    if (drag) return;
    const key = [...selected].sort().join(' ');
    if (key === styleKey) return;
    styleKey = key;
    if (!key) {
      if (styleAuto) properties.open = false;
      styleAuto = styleDismissed = false;
      return;
    }
    if (readOnly || styleDismissed || properties.open || menus.some((m) => m.open)
        || !matchMedia('(min-width: 1100px)').matches) return;
    properties.open = styleAuto = true;
    place(properties);
  };
  for (const menu of menus) {
    const summary = menu.firstElementChild;
    summary.addEventListener('click', event => {
      event.preventDefault();
      if (summary.getAttribute('aria-disabled') === 'true') return;
      menu.open = !menu.open;
      if (menu === properties) { styleAuto = false; styleDismissed = !menu.open && selected.size > 0; }
      if (!menu.open) return;
      for (const other of menus) if (other !== menu) other.open = false;
      place(menu);
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
    <div class="shortcut-columns"><section><h3>Tools</h3><dl>${tools.filter(([value]) => !readOnly || ['pan','select'].includes(value)).map(([,label,key]) => `<div><dt>${label}</dt><dd><kbd>${key}</kbd></dd></div>`).join('')}${readOnly ? '' : '<div><dt>Insert image</dt><dd><kbd>9</kbd></dd></div><div><dt>Keep the tool after drawing</dt><dd><kbd>Q</kbd></dd></div>'}</dl></section>
    <section><h3>Working on the canvas</h3><dl>
    <div><dt>Select all</dt><dd>Ctrl / ⌘ A</dd></div><div><dt>Clear selection</dt><dd>Esc</dd></div>
    <div><dt>Select multiple</dt><dd>Shift + click</dd></div><div><dt>Box-select contained objects</dt><dd>Drag on empty canvas</dd></div><div><dt>Box-select touched objects</dt><dd>Alt + drag</dd></div><div><dt>Add a box to the selection</dt><dd>Shift + drag</dd></div><div><dt>Pan</dt><dd>Scroll / middle-button drag</dd></div><div><dt>Pan sideways</dt><dd>Shift + scroll</dd></div><div><dt>Zoom at pointer</dt><dd>Ctrl / ⌘ scroll / pinch</dd></div>
    ${readOnly ? '' : '<div><dt>Edit text</dt><dd>Enter / double-click</dd></div><div><dt>Finish text</dt><dd>Ctrl / ⌘ Enter</dd></div><div><dt>Cancel text</dt><dd>Esc</dd></div><div><dt>Resize</dt><dd>Drag the corner handle</dd></div><div><dt>Move 1px / 10px</dt><dd>Arrows / Shift + arrows</dd></div><div><dt>Duplicate</dt><dd>Ctrl / ⌘ D</dd></div><div><dt>Group / ungroup</dt><dd>Ctrl / ⌘ G / Shift G</dd></div><div><dt>Delete</dt><dd>Del / Backspace</dd></div><div><dt>Undo / redo</dt><dd>Ctrl / ⌘ Z / Shift Z</dd></div><div><dt>Insert image</dt><dd>Drop / paste an image</dd></div><div><dt>Comment on the selection</dt><dd>Ctrl / ⌘ Alt M</dd></div>'}
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
  button(zoom, 'Fit', () => { frame(bounds(shapeList(), currentScene())); draw(); }).title = 'Fit all objects';
  button(zoom, '−', () => zoomBy(1.2)).setAttribute('aria-label', 'Zoom out');
  const percentage = button(zoom, '100%', () => zoomBy(canvas.getScreenCTM()?.a || 1));
  percentage.id = 'board-zoom'; percentage.title = 'Reset zoom to 100%'; percentage.setAttribute('aria-label', 'Reset zoom to 100%');
  button(zoom, '+', () => zoomBy(1 / 1.2)).setAttribute('aria-label', 'Zoom in');
  boardPresence = new BoardPresence(canvas, $('presence'));
  boardPreviews = new BoardPreviews(()=>{ draw(); saved(); });
  new ResizeObserver(() => draw()).observe(canvas);
  /* The selectable element under EVENT: a label selects its container,
     and locked elements are not selectable. */
  const hitAt = (event) => {
    const id = document.elementFromPoint(event.clientX, event.clientY)?.closest('[data-shape]')?.dataset.shape;
    const item = id && currentScene().byId.get(id);
    if (!item) return undefined;
    const target = item.bound ? currentScene().byId.get(item.containerId) : item;
    return target && !target.locked ? target.id : undefined;
  };
  /* The selectable element under EVENT other than element EXCEPT. */
  const hitAtExcept = (event, except) => {
    for (const node of document.elementsFromPoint(event.clientX, event.clientY)) {
      const id = node.closest?.('#scene [data-shape]')?.dataset.shape;
      const item = id && currentScene().byId.get(id);
      const target = item && (item.bound ? currentScene().byId.get(item.containerId) : item);
      if (target && target.id !== except && !target.locked) return target.id;
    }
    return undefined;
  };
  const bindingTo = (id) => {
    const target = id && currentScene().byId.get(id);
    return target && !['arrow', 'line', 'freedraw'].includes(target.type)
      ? { elementId: id, fixedPoint: [0.5001, 0.5001], mode: 'orbit' } : null;
  };
  canvas.onpointerdown = (event) => {
    if (event.button !== 0 && event.button !== 1) return;
    const pin = event.target.closest?.('.comment-pin');
    if (pin && event.button === 0) {
      event.preventDefault();
      assistant?.openComment(pin.parentNode.dataset.commentId);
      return;
    }
    event.preventDefault();
    canvas.focus();
    canvas.setPointerCapture(event.pointerId);
    undo?.stopCapturing();
    const point = world(event),
      id = hitAt(event),
      handle = event.target.dataset.handle,
      pointHandle = event.target.dataset.point ?? event.target.dataset.mid;
    if (tool === 'laser' && !readOnly) {
      drag = { mode: 'laser' };
      laserInk++;
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
    if (tool === 'comment' && !readOnly) {
      // A click comments on an object; a drag comments on an area and its objects.
      selectionRegion = null;
      selected = id ? withGroup(id) : new Set();
      drag = { mode: 'marquee', comment: true, target: id, start: point,
        screen: [event.clientX, event.clientY], additive: false, base: new Set(selected), region: null };
      draw();
      return;
    }
    if (tool === 'select') {
      const scene = currentScene();
      if (handle && !readOnly) {
        const frame = selectionFrame(scene), ids = [...selected];
        const entries = ids.map((key) => {
          const stored = shapeList().find((s) => s.id === key);
          return [key, { g: visibleGeometry(key), type: stored?.type, fontSize: stored?.fontSize ?? 20 }];
        });
        const [x, y, w, h] = frame.box, center = [x + w / 2, y + h / 2];
        const anchor = handle === 'rotation' ? point
          : rotate([handle.includes('w') ? x : handle.includes('e') ? x + w : x + w / 2,
            handle.includes('n') ? y : handle.includes('s') ? y + h : y + h / 2], center, frame.angle);
        const fixedAspect = entries.some(([, e]) => ['image', 'text'].includes(e.type)) && handle.length === 2;
        drag = { mode: 'transform', handle, start: point, offset: [point[0] - anchor[0], point[1] - anchor[1]],
          entries, frame: frame.box, angle: frame.angle, center, single: Boolean(frame.single), fixedAspect };
        return;
      }
      if (pointHandle !== undefined && !readOnly) {
        const [lineId] = selected, base = detachedGeometry(lineId), index = Number(pointHandle);
        const points = base.points.map((p) => p.slice());
        let at = index;
        if (event.target.dataset.mid !== undefined) {
          // Dragging a segment's midpoint adds a bend there, as in Excalidraw.
          at = index + 1;
          points.splice(at, 0, points[index].map((v, axis) => (v + points[index + 1][axis]) / 2));
        }
        const end = at === 0 ? 'startBinding' : at === points.length - 1 ? 'endBinding' : null;
        drag = { mode: 'point', id: lineId, index: at, start: point, base: { ...base, points },
          loose: end ? new Map([[lineId, [end]]]) : new Map(), end: undefined };
        return;
      }
      selectionRegion = null;
      if (editingGroup && !(id && scene.byId.get(id)?.groupIds?.includes(editingGroup))) editingGroup = null;
      if (!id) {
        // Empty canvas starts a box selection; a click without movement clears.
        drag = { mode: 'marquee', start: point, screen: [event.clientX, event.clientY],
          additive: event.shiftKey, base: new Set(selected), region: null };
        if (!event.shiftKey) selected.clear();
        draw();
        return;
      }
      const unit = withGroup(id);
      if (event.shiftKey) {
        const on = !selected.has(id);
        for (const member of unit) on ? selected.add(member) : selected.delete(member);
      } else if (!selected.has(id)) selected = unit;
      draw();
      if (!readOnly) {
        const loose = looseEnds(selected);
        drag = {
          mode: 'move',
          start: point,
          loose,
          boxes: [...selected].map((key) => [key, loose.has(key) ? detachedGeometry(key) : visibleGeometry(key)]),
        };
      }
      return;
    }
    if (readOnly) return;
    if (tool === 'erase') {
      if (id) doc.transact(() => [id, ...labelsOf(new Set([id]))].forEach((key) => elementMap().delete(key)), local);
      return;
    }
    const stroke = ['freedraw', 'autoshape'].includes(tool);
    const element = styledElement(tool === 'autoshape' ? 'freedraw' : tool, { x: point[0], y: point[1], width: 0, height: 0 });
    if (stroke) Object.assign(element, { points: [[0, 0]], simulatePressure: event.pointerType !== 'pen',
      ...(event.pointerType === 'pen' ? { pressures: [event.pressure] } : {}) });
    if (LINEAR.includes(tool)) element.points = [[0, 0], [0, 0]];
    drag = { mode: 'draw', start: point, points: [point], from: id, element };
  };
  canvas.onpointermove = (event) => {
    const point = world(event);
    if (drag && ['move', 'transform', 'point'].includes(drag.mode)) {
      drag.end = point;
      drag.snap = event.shiftKey;
      drag.keepAspect = event.shiftKey !== Boolean(drag.fixedAspect);
      outgoingPreview = {shapes:dragGeometry(drag).slice(0,100).map(([id,g])=>({id,box:geometryBox(g)}))};
    }
    if (event.pointerType !== 'touch' || drag)
      presence(point, tool === 'laser' ? 'laser' : 'cursor');
    if (!drag && tool === 'comment' && event.pointerType !== 'touch') {
      const hover = hitAt(event) || null;
      if (hover !== hoverShape) { hoverShape = hover; draw(); }
    }
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
      const e = drag.element, [sx, sy] = drag.start;
      if (e.points && !LINEAR.includes(e.type)) {
        // Coalesced events carry every pen sample between frames, with its pressure.
        const samples = event.getCoalescedEvents?.().filter(Boolean) || [];
        for (const sample of samples.length ? samples : [event]) {
          if (e.points.length >= 3999) break;
          const at = world(sample);
          e.points.push([at[0] - sx, at[1] - sy]);
          drag.points.push(at);
          if (e.pressures) e.pressures.push(sample.pressure);
        }
        const [x1, y1, x2, y2] = pointBounds(e.points);
        Object.assign(e, { width: x2 - x1, height: y2 - y1 });
      } else if (LINEAR.includes(e.type)) {
        e.points = [[0, 0], [point[0] - sx, point[1] - sy]];
        Object.assign(e, { width: Math.abs(point[0] - sx), height: Math.abs(point[1] - sy) });
      } else Object.assign(e, { x: Math.min(point[0], sx), y: Math.min(point[1], sy),
        width: Math.abs(point[0] - sx), height: Math.abs(point[1] - sy) });
      draw();
    } else if (['move', 'transform', 'point'].includes(drag.mode)) draw();
  };
  /* The element a finished drawing gesture D creates. */
  const finishedElement = (d, event) => {
    const e = d.element;
    if (['text', 'stickynote'].includes(e.type)) {
      const size = e.type === 'stickynote' ? 200 : 0;
      Object.assign(e, { x: d.start[0], y: d.start[1], width: Math.max(size, e.width), height: Math.max(size, e.height) });
      if (e.type === 'text') Object.assign(e, { text: '', width: 0, height: e.fontSize * 1.25 });
    } else if (e.width < 3 && e.height < 3 && !e.points) Object.assign(e, { width: 150, height: 90 });
    else if (e.width < 3 && e.height < 3 && LINEAR.includes(e.type))
      Object.assign(e, { points: [[0, 0], [150, 90]], width: 150, height: 90 });
    if (e.type === 'arrow') {
      const end = hitAt(event), start = d.from;
      const startBinding = bindingTo(start), endBinding = end !== start ? bindingTo(end) : null;
      if (startBinding) e.startBinding = startBinding;
      if (endBinding) e.endBinding = endBinding;
    }
    return e;
  };
  /* Hold autoshape stroke D for AUTOSHAPE_DELAY; another stroke in that time
     joins it, and the strokes are then recognized as one drawing. */
  const holdStroke = (d) => {
    pendingShape ??= { strokes: [] };
    pendingShape.strokes.push(d);
    clearTimeout(pendingShape.timer);
    pendingShape.timer = setTimeout(settlePendingShape, AUTOSHAPE_DELAY);
    draw();
  };
  settlePendingShape = () => {
    const pending = pendingShape;
    if (!pending) return;
    clearTimeout(pending.timer);
    // A stroke still being drawn joins the group when it ends.
    if (drag?.mode === 'draw') {
      pending.timer = setTimeout(settlePendingShape, AUTOSHAPE_DELAY);
      return;
    }
    pendingShape = null;
    const [first] = pending.strokes, matrix = canvas.getScreenCTM();
    const shape = recognize(pending.strokes.flatMap((s) => s.points), matrix?.a || 1);
    let elements = pending.strokes.map((s) => s.element);
    if (shape.type !== 'freedraw') {
      const { id, seed, index } = first.element;
      let e;
      if (LINEAR.includes(shape.type)) {
        e = styledElement(shape.type, { x: shape.from[0], y: shape.from[1],
          width: Math.abs(shape.to[0] - shape.from[0]), height: Math.abs(shape.to[1] - shape.from[1]),
          points: [[0, 0], [shape.to[0] - shape.from[0], shape.to[1] - shape.from[1]]] });
      } else {
        const [x, y, width, height] = shape.box;
        e = styledElement(shape.type, { x, y, width, height });
      }
      Object.assign(e, { id, seed, index });
      if (e.type === 'arrow' && matrix) {
        // The tip, not where the pen lifted after the head, says what it points at.
        const tip = new DOMPoint(...shape.to).matrixTransform(matrix);
        const end = hitAtExcept({ clientX: tip.x, clientY: tip.y });
        const startBinding = bindingTo(first.from), endBinding = end !== first.from ? bindingTo(end) : null;
        if (startBinding) e.startBinding = startBinding;
        if (endBinding) e.endBinding = endBinding;
      }
      elements = [e];
    }
    elements.forEach(validateElement);
    doc.transact(() => elements.forEach((e) => putElement(doc, e)), local);
    finishDrawing(elements.map((e) => e.id));
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
      if (d.comment && (d.region || d.target)) assistant.begin('selection', 'comment');
      return;
    }
    if (readOnly || d.mode === 'pan' || d.mode === 'laser') return;
    if (d.mode === 'draw' && tool === 'autoshape') holdStroke(d);
    else if (d.mode === 'draw') {
      const e = finishedElement(d, event);
      validateElement(e);
      doc.transact(() => putElement(doc, e), local);
      finishDrawing([e.id]);
      if (['text', 'stickynote'].includes(e.type)) editText(e.id);
    } else if (d.end) {
      doc.transact(() => {
        for (const [id, keys] of d.loose || []) for (const key of keys) elementMap().get(id)?.delete(key);
        for (const [id, changes] of dragGeometry(d)) applyChanges(id, changes);
        // A dragged arrow end binds to the shape it is dropped on.
        const arrow = d.mode === 'point' && elementMap().get(d.id);
        const key = d.loose?.get(d.id)?.[0];
        if (arrow?.get('type') === 'arrow' && key) {
          const binding = bindingTo(hitAtExcept(event, d.id));
          if (binding) arrow.set(key, binding);
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
    if (hoverShape) { hoverShape = null; draw(); }
  };
  const peek = $('comment-peek');
  canvas.addEventListener('pointerover', (event) => {
    const marker = event.target.closest?.('.comment-pin')?.parentNode;
    const comment = marker && commentStates.find((c) => c.id === marker.dataset.commentId);
    if (!comment) return;
    const line = (tag, text) => { const node = document.createElement(tag); node.textContent = text; return node; };
    const replies = comment.replies?.length || 0;
    peek.replaceChildren(line('small', comment.actor.replace(/^Guest: /, '')), line('p', comment.text),
      line('small', [replies ? `${replies} repl${replies === 1 ? 'y' : 'ies'}` : '',
        comment.anchorStatus === 'changed' ? 'Objects changed since posting' : ''].filter(Boolean).join(' · ')));
    peek.dataset.commentId = comment.id;
    peek.hidden = false;
    const pin = marker.querySelector('.comment-pin').getBoundingClientRect(), host = $('board').getBoundingClientRect();
    peek.style.left = `${Math.max(8, Math.min(pin.right + 6 - host.left, host.width - peek.offsetWidth - 8))}px`;
    peek.style.top = `${Math.max(8, Math.min(pin.top - host.top, host.height - peek.offsetHeight - 8))}px`;
  });
  canvas.addEventListener('pointerout', (event) => {
    if (event.target.closest?.('.comment-pin')) peek.hidden = true;
  });
  canvas.onpointercancel = () => {
    outgoingPreview = null;
    stopPointing();
    if (drag?.mode === 'marquee') selected = drag.base;
    drag = null;
    draw();
  };
  canvas.ondblclick = (event) => {
    if (tool !== 'select' || readOnly) return;
    const id = hitAt(event);
    const group = id && unitGroup(currentScene().byId.get(id));
    if (group) {
      // Double-clicking a group enters it; its members then select one by one.
      editingGroup = group;
      selected = withGroup(id);
      draw();
      return;
    }
    if (id) { editText(id); return; }
    // Double-clicking empty canvas starts text there, as in Excalidraw.
    const [x, y] = world(event), text = styledElement('text', { x, y, width: 0, height: current.fontSize * 1.25, text: '' });
    undo?.stopCapturing();
    doc.transact(() => putElement(doc, text), local);
    editText(text.id);
  };
  /* Scrolling pans, as in Excalidraw and Figma; Ctrl or Command zooms at
     the pointer.  A trackpad pinch arrives as a Ctrl wheel with small
     deltas, so zoom follows the delta: a mouse notch is about 10%. */
  canvas.onwheel = (event) => {
    event.preventDefault();
    const unit = event.deltaMode === 1 ? 16 : event.deltaMode === 2 ? canvas.clientHeight : 1;
    let dx = event.deltaX * unit, dy = event.deltaY * unit;
    if (event.ctrlKey || event.metaKey) {
      const point = world(event),
        next = Math.max(100, Math.min(100000, view[2] * Math.exp(Math.max(-100, Math.min(100, dy)) / 1000))),
        factor = next / view[2];
      view = [
        point[0] + (view[0] - point[0]) * factor,
        point[1] + (view[1] - point[1]) * factor,
        next,
        view[3] * factor,
      ];
    } else {
      // A mouse wheel has one axis; Shift turns it sideways.
      if (event.shiftKey && !dx) [dx, dy] = [dy, 0];
      const scale = canvas.getScreenCTM()?.a || 1;
      view = [view[0] + dx / scale, view[1] + dy / scale, view[2], view[3]];
    }
    draw();
  };
  canvas.onkeydown = (event) => {
    if (event.ctrlKey || event.metaKey) {
      const key = event.key.toLowerCase();
      if (key === 'a') {
        event.preventDefault();
        all();
      } else if (!readOnly && key === 'd' && selected.size) {
        event.preventDefault();
        undo.stopCapturing(); duplicate(); undo.stopCapturing();
      } else if (!readOnly && key === 'g') {
        event.preventDefault();
        undo.stopCapturing(); regroup(!event.shiftKey); undo.stopCapturing();
      }
      return;
    }
    const keys = {
      v: 'select',
      h: 'pan',
      r: 'rectangle',
      o: 'ellipse',
      d: 'diamond',
      a: 'arrow',
      l: 'line',
      p: 'freedraw',
      x: 'freedraw',
      X: 'autoshape',
      t: 'text',
      n: 'stickynote',
      e: 'erase',
      k: 'laser',
      m: 'comment',
      1: 'select',
      2: 'rectangle',
      3: 'diamond',
      4: 'ellipse',
      5: 'arrow',
      6: 'line',
      7: 'freedraw',
      8: 'text',
      0: 'erase',
    };
    if (keys[event.key] && (!readOnly || ['select', 'pan'].includes(keys[event.key]))) {
      event.preventDefault();
      selectTool(keys[event.key]);
    }
    if (event.key === '9' && !readOnly) {
      event.preventDefault();
      $('board-image').click();
    }
    if (event.key.toLowerCase() === 'q' && !readOnly) {
      event.preventDefault();
      $('board-tool-lock').click();
    }
    if (event.key === 'Escape') {
      // Escape leaves an entered group with the group selected, then clears.
      if (editingGroup) {
        const group = editingGroup;
        editingGroup = null;
        selected = new Set(shapeList().filter((s) => s.groupIds?.includes(group)).map((s) => s.id));
        draw();
      } else clear();
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
  // Titles change through the host, so the board's own meta is its background.
  undo = new Y.UndoManager([elementMap(), doc.getMap('files'), doc.getMap('meta')], {
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
  const board = scope === 'selection' && !editor;
  // A comment resends the objects of its anchor that still exist.
  const selection = board ? previous?.liveSelection || previous?.selection || [...selected] : [];
  const region = board ? (previous ? previous.region : selectionRegion) || undefined : undefined;
  if (scope === 'selection' && !range && !selection.length && !region) throw new Error('Select content first');
  const captured = captureContext(doc, { range, selection, region });
  return { ...captured, range, selection, region, revision };
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
  commentStates = readComments(doc, comments);
  assistant?.setComments(commentStates);
  if (editor) editor.view.dispatch(editor.state.tr.setMeta('addToHistory', false));
  else draw();
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
  // Registered first, so every later update listener sees fresh elements.
  doc.on('update', () => { cachedList = cachedScene = null; });
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
      // A skipped revision means this diff was computed against a state this
      // editor lacks, such as edits another Emacs committed before handing
      // the item over; read the whole item rather than lose them.
      if (online && data.revision > revision + 1) resync();
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
  // The room confirms and deletes; the editor only asks.
  $('delete-item').hidden = readOnly;
  $('delete-item').textContent = item.kind === 'whiteboard' ? 'Delete whiteboard…' : 'Delete document…';
  $('delete-item').onclick = () => { $('menu').open = false; port.postMessage({ type: 'delete' }); };
  let resyncing = false;
  async function resync() {
    if (resyncing) return;
    resyncing = true;
    try {
      const current = await request({ action: 'read' });
      Y.applyUpdate(doc, bytes(current.crdt), remote);
      revision = Math.max(revision, current.revision);
    } catch (_) {
      // Offline or refused: the next sync on reconnecting reads it again.
    } finally {
      resyncing = false;
    }
  }
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
      new Option(value === 'native' ? (item.kind === 'whiteboard' ? 'Excalidraw file' : 'Editable snapshot') : value.toUpperCase(), value),
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
      result: item.kind === 'whiteboard'
        ? { text: serializeScene(shapeList(), filesOf(doc), { background: doc.getMap('meta').get('background') }),
            mime: 'application/vnd.excalidraw+json',
            extension: 'recovery.excalidraw' }
        : { text: JSON.stringify({ format: 'mevedel-editable-1', ...inspect(doc) }),
            mime: 'application/json', extension: 'recovery.mevedel.json' },
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
      } else if (doc) draw();
    },
    reveal: comment => editor ? revealPassage(comment.range) : revealObjects(comment), state: () => ({ readOnly, online }), restored: recovery?.assistant,
    onConversation: () => { if (!editor && doc) draw(); },
  });
  assistant.renderDraft();
  if (editor && matchMedia('(min-width:1100px)').matches) {
    if (assistant.draft.attachment) assistant.toggle(true);
    else assistant.begin('whole');
    document.activeElement?.blur();
  } else if (!editor) requestAnimationFrame(() => { frame(bounds(shapeList())); draw(); });
  setComments(item.comments || []);
  $('comment-selection').hidden = readOnly;
  $('selection-question').hidden = readOnly;
  $('ask-toggle').textContent = editor ? 'Discussion' : 'Assistant';
  if (editor) {
    $('document').after($('selection-actions'));
    $('selection-actions').classList.add('document-selection-actions');
    $('selection-actions').hidden = true;
  }
  $('hint').textContent = editor ? 'Select text to comment or ask the assistant.' : 'Select objects or drag across an area to comment or ask the assistant.';
  $('comment-selection').onclick = () => assistant.begin('selection', 'comment');
  $('selection-question').onclick = () => assistant.begin('selection');
  $('document').addEventListener('click', event => {
    const id = event.target.closest('[data-comment-id]')?.dataset.commentId;
    if (!id) return;
    assistant.openComment(id);
  });
  document.addEventListener('keydown', event => {
    if (!readOnly && (event.ctrlKey || event.metaKey) && event.altKey && event.key.toLowerCase() === 'm') {
      event.preventDefault(); if (editor) captureDocumentSelection(); assistant.begin('selection', 'comment');
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
