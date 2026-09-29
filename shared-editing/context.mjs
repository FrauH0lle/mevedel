/* The same context capture is used for browser previews and host acceptance. */
import { inspect, check, same } from './model.mjs';
import { selectedText } from './document.mjs';
import { shapesInRegion } from './render.mjs';

/* A board area [x, y, w, h] in integer board units. */
export function validRegion(region) {
  return Array.isArray(region) && region.length === 4 && region.every(Number.isSafeInteger)
    && region.slice(0, 2).every(v => Math.abs(v) <= 1e6) && region.slice(2).every(v => v >= 1 && v <= 2e6);
}

/* Unselected shapes an area touches, without embedded image data: the
   question's PNG shows them, and their bytes would crowd out the selection. */
function nearby(all, region, selection) {
  return shapesInRegion(all, region, true).filter(shape => !selection.includes(shape.id))
    .map(({ src, imageEdit, ...shape }) => src ? { ...shape, image: 'shown in the attached PNG' } : shape);
}

function boardQuote(shapes, region) {
  return `${region ? `Area ${region[2]} × ${region[3]} at ${region[0]}, ${region[1]}\n` : ''}${shapes.length} object${shapes.length === 1 ? '' : 's'}${shapes.map(shape => `\n${shape.type}${shape.text ? ': ' + shape.text : ''}`).join('')}`;
}

/* A compact fingerprint of anchored shapes. Image bytes contribute only their
   length, so a comment on a large image stays cheap to check on every change. */
export function anchorSignature(shapes) {
  const json = JSON.stringify(shapes.map(({ src, imageEdit, ...shape }) => ({ ...shape,
    ...(src ? { src: src.length } : {}),
    ...(imageEdit ? { imageEdit: { ...imageEdit, src: imageEdit.src?.length } } : {}) })));
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
    const endpoints = new Set(content.flatMap(shape => [shape.from, shape.to]).filter(Boolean));
    context = all.filter(shape => endpoints.has(shape.id) && !selection.includes(shape.id));
    if (region) {
      const known = new Set(context.map(shape => shape.id));
      context = [...context, ...nearby(all, region, selection).filter(shape => !known.has(shape.id))];
    }
    scope = 'selection';
  }
  if (!quote) quote = kind === 'whiteboard'
    ? boardQuote(content, region)
    : all.content.map(node => {
      const text = n => n.text || (n.content || []).map(text).join('');
      return text(node);
    }).join('\n');
  const snapshot = { kind, title, scope, content, context, ...(region ? { region } : {}) };
  check(new TextEncoder().encode(JSON.stringify(snapshot)).length <= 128 * 1024,
    'Question snapshot is too large; select a smaller portion');
  return { snapshot, quote };
}

export function checkContext(doc, request) {
  const captured = captureContext(doc, request);
  check(request.expected && same(request.expected, captured.snapshot),
    'Content changed. Review and refresh the attached context before sending.');
  return captured;
}

/* A board comment is changed once its objects move, restyle or disappear, and
   removed when neither objects nor an area remain to show. */
function readBoardComment(shapes, comment) {
  const live = shapes.filter(shape => comment.selection.includes(shape.id));
  const anchorStatus = !live.length && !comment.region ? 'deleted'
    : live.length === comment.selection.length && anchorSignature(live) === comment.signature ? 'current' : 'changed';
  return { ...comment, liveSelection: live.map(shape => shape.id),
    liveQuote: boardQuote(live, comment.region), anchorStatus };
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
