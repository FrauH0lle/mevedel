/* The same context capture is used for browser previews and host acceptance. */
import { inspect, check } from './model.mjs';
import { selectedText } from './document.mjs';
import { shapesInRegion } from './scene.mjs';
import { contentHash } from './view.mjs';

/* A board area [x, y, w, h] in integer board units. */
export function validRegion(region) {
  return Array.isArray(region) && region.length === 4 && region.every(Number.isSafeInteger)
    && region.slice(0, 2).every(v => Math.abs(v) <= 1e6) && region.slice(2).every(v => v >= 1 && v <= 2e6);
}

/* Labels of the given elements: bound text whose container is among IDS. */
const labelsOf = (all, ids) => all.filter(e => e.type === 'text' && ids.includes(e.containerId));
/* Unselected elements that give a board selection its meaning: bound arrow
   targets, labels, and the containers of selected labels. */
export function boardContext(all, selection) {
  const related = new Set(all.filter(e => selection.includes(e.id)).flatMap(e =>
    [e.startBinding?.elementId, e.endBinding?.elementId, e.containerId]).filter(Boolean));
  for (const label of labelsOf(all, selection)) related.add(label.id);
  return all.filter(e => related.has(e.id) && !selection.includes(e.id));
}
/* Unselected elements an area touches, and their labels. */
function nearby(all, region, selection) {
  const touched = shapesInRegion(all, region, true).filter(e => !selection.includes(e.id));
  return [...touched, ...labelsOf(all, touched.map(e => e.id))];
}

/* Labelled objects are named; unlabelled ones are counted by type, since a
   hand-drawn selection can be dozens of strokes that say nothing apart. */
function boardQuote(shapes, region, all) {
  const label = shape => shape.text ?? labelsOf(all, [shape.id]).map(t => t.text).join(' ');
  const objects = shapes.filter(shape => !(shape.containerId && shapes.some(s => s.id === shape.containerId)));
  const lines = [], counts = new Map();
  for (const shape of objects) {
    const text = label(shape);
    if (text) lines.push(`${shape.type}: ${text}`);
    else if (counts.has(shape.type)) lines[counts.get(shape.type)].n += 1;
    else { counts.set(shape.type, lines.length); lines.push({ type: shape.type, n: 1 }); }
  }
  const named = lines.map(l => typeof l === 'string' ? l : l.n > 1 ? `${l.type} ×${l.n}` : l.type);
  return `${region ? `Area ${region[2]} × ${region[3]} at ${region[0]}, ${region[1]}\n` : ''}${objects.length} object${objects.length === 1 ? '' : 's'}${named.map(l => `\n${l}`).join('')}`;
}

/* A compact fingerprint of anchored elements and their labels. */
export function anchorSignature(shapes, all = shapes) {
  const json = JSON.stringify([...shapes, ...labelsOf(all, shapes.map(shape => shape.id))]);
  let hash = 0xcbf29ce484222325n;
  for (let i = 0; i < json.length; i++)
    hash = BigInt.asUintN(64, (hash ^ BigInt(json.charCodeAt(i))) * 0x100000001b3n);
  return hash.toString(16).padStart(16, '0');
}

export function captureContext(doc, { selection = [], range, region } = {}) {
  const { kind, title, content: all } = inspect(doc);
  let content = all, context = [], quote, scope = 'whole';
  if (range) {
    check(kind === 'document', 'Text selections require a document');
    const selected = selectedText(doc, range);
    check(selected.text.trim(), 'The selected passage is empty or no longer available');
    content = { text: selected.text, content: selected.content };
    context = selected.context;
    quote = selected.text;
    scope = 'selection';
  } else if (selection?.length || region) {
    check(kind === 'whiteboard' && Array.isArray(selection) && selection.length <= 200,
      'A document passage is selected by its text, not by objects');
    check(!region || validRegion(region), 'Invalid board area');
    content = all.filter(shape => selection.includes(shape.id));
    check(content.length === selection.length, 'Selected objects are no longer available');
    context = boardContext(all, selection);
    if (region) {
      const known = new Set(context.map(shape => shape.id));
      context = [...context, ...nearby(all, region, selection).filter(shape => !known.has(shape.id))];
    }
    scope = 'selection';
  }
  if (!quote) quote = kind === 'whiteboard'
    ? boardQuote(content, region, all)
    : all.content.map(node => {
      const text = n => n.text || (n.content || []).map(text).join('');
      return text(node);
    }).join('\n');
  const snapshot = { kind, title, scope, content, context, ...(region ? { region } : {}) };
  return { snapshot, quote };
}

/* An editor question carries the hash of the snapshot its sender reviewed
   and fails when content moved on; the snapshot itself never travels back. A room message about the whole item has no reviewed
   snapshot; it asks about the item as currently committed. */
export function checkContext(doc, request) {
  const whole = request.whole === true;
  check(!whole || (!request.range && !request.selection?.length && !request.region),
    'A whole-item question takes no selection');
  const captured = captureContext(doc, whole ? {} : request);
  check(whole || request.expected === contentHash(captured.snapshot),
    'Content changed. Review and refresh the attached context before sending.');
  return captured;
}

/* A board discussion is changed once its objects move, restyle or
   disappear, and removed when neither objects nor an area remain to show. */
function readBoardTarget(shapes, discussion) {
  const target = discussion.target;
  const live = shapes.filter(shape => target.selection.includes(shape.id));
  const anchorStatus = !live.length && !target.region ? 'deleted'
    : live.length === target.selection.length && anchorSignature(live, shapes) === target.signature ? 'current' : 'changed';
  return { ...discussion, liveSelection: live.map(shape => shape.id),
    liveQuote: boardQuote(live, target.region, shapes), anchorStatus };
}

/* Discussions with the live state of their targets.  A discussion about the
   whole item has no anchor and is always current. */
export function readDiscussions(doc, discussions = []) {
  if (!discussions.length) return [];
  const board = doc.getMap('meta').get('kind') === 'whiteboard';
  // Only boards need their shapes; a document resolves each anchor directly.
  const shapes = board ? inspect(doc).content : null;
  return discussions.map(discussion => {
    const target = discussion.target;
    if (target.kind === 'whole') return { ...discussion, liveQuote: '', anchorStatus: 'current' };
    if (board) return readBoardTarget(shapes, discussion);
    try {
      const { text } = selectedText(doc, target.range);
      return { ...discussion, liveQuote: text,
        anchorStatus: !text.trim() ? 'deleted' : text === discussion.quote ? 'current' : 'changed' };
    } catch {
      return { ...discussion, liveQuote: '', anchorStatus: 'deleted' };
    }
  });
}
