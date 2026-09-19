/* The same context capture is used for browser previews and host acceptance. */
import { inspect, check, same } from './model.mjs';
import { selectedText } from './document.mjs';

export function captureContext(doc, { selection = [], range } = {}) {
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
  } else if (selection.length) {
    check(kind === 'whiteboard' && Array.isArray(selection) && selection.length <= 200,
      'Select document text to create a comment');
    content = all.filter(shape => selection.includes(shape.id));
    check(content.length === selection.length, 'Selected objects are no longer available');
    const endpoints = new Set(content.flatMap(shape => [shape.from, shape.to]).filter(Boolean));
    context = all.filter(shape => endpoints.has(shape.id) && !selection.includes(shape.id));
    scope = 'selection';
  }
  if (!quote) quote = kind === 'whiteboard'
    ? `${content.length} object${content.length === 1 ? '' : 's'}\n${content.map(shape => `${shape.type}${shape.text ? ': ' + shape.text : ''}`).join('\n')}`
    : all.content.map(node => {
      const text = n => n.text || (n.content || []).map(text).join('');
      return text(node);
    }).join('\n');
  const snapshot = { kind, title, scope, content, context };
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
