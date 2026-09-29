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
    ? `${region ? `Area ${region[2]} × ${region[3]} at ${region[0]}, ${region[1]}\n` : ''}${content.length} object${content.length === 1 ? '' : 's'}${content.map(shape => `\n${shape.type}${shape.text ? ': ' + shape.text : ''}`).join('')}`
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

export function readComments(doc, comments = []) {
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
