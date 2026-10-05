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

function boardQuote(shapes, region, all) {
  const label = shape => shape.text ?? labelsOf(all, [shape.id]).map(t => t.text).join(' ');
  const objects = shapes.filter(shape => !(shape.containerId && shapes.some(s => s.id === shape.containerId)));
  return `${region ? `Area ${region[2]} × ${region[3]} at ${region[0]}, ${region[1]}\n` : ''}${objects.length} object${objects.length === 1 ? '' : 's'}${objects.map(shape => `\n${shape.type}${label(shape) ? ': ' + label(shape) : ''}`).join('')}`;
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
      'Select document text to create a comment');
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

/* A board comment is changed once its objects move, restyle or disappear, and
   removed when neither objects nor an area remain to show. */
function readBoardComment(shapes, comment) {
  const live = shapes.filter(shape => comment.selection.includes(shape.id));
  const anchorStatus = !live.length && !comment.region ? 'deleted'
    : live.length === comment.selection.length && anchorSignature(live, shapes) === comment.signature ? 'current' : 'changed';
  return { ...comment, liveSelection: live.map(shape => shape.id),
    liveQuote: boardQuote(live, comment.region, shapes), anchorStatus };
}

export function readComments(doc, comments = []) {
  if (!comments.length) return [];
  // Only boards need their shapes; a document resolves each anchor directly.
  if (doc.getMap('meta').get('kind') === 'whiteboard') {
    const { content } = inspect(doc);
    return comments.map(comment => readBoardComment(content, comment));
  }
  return comments.map(comment => {
    try {
      const { text } = selectedText(doc, comment.range);
      return { ...comment, liveQuote: text,
        anchorStatus: !text.trim() ? 'deleted' : text === comment.quote ? 'current' : 'changed' };
    } catch {
      return { ...comment, liveQuote: '', anchorStatus: 'deleted' };
    }
  });
}
