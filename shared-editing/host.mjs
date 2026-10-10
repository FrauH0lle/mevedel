/* Private JSON request handler. Emacs alone owns files, authority, and commits. */
import * as Y from 'yjs';
import { anchorSignature, boardContext, checkContext, readComments } from './context.mjs';
import { readFile } from 'node:fs/promises';
import { initWasm, Resvg } from '@resvg/resvg-wasm';
import {
  create,
  restore,
  encode,
  inspect,
  filesOf,
  applyUpdate,
  patch,
  putElement,
  putFile,
  pruneFiles,
  validate,
  validBackground,
  check,
  identifier,
  LIMIT,
  same,
} from './model.mjs';
import { initializeDocument, patchDocument, restoreDocument, markdown, selectedText } from './document.mjs';
import { boardSVG, documentHTML, usedFonts } from './render.mjs';
import { parseScene, serializeScene, parseLibrary, placeElements } from './excalidraw.mjs';
import { extent } from './scene.mjs';
import { generateNKeysBetween } from 'fractional-indexing';
import { escape } from './render.mjs';
import { FONT_FILES } from './text.mjs';
import { address, commentsView, contentHash, documentImages, element, historyView, line, nodeId,
  nodesOf, overview, restoreImages } from './view.mjs';
const b64 = (bytes) => Buffer.from(bytes).toString('base64');
const bytes = (text) => {
  check(
    typeof text === 'string' &&
      text.length <= LIMIT * 1.4 &&
      /^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/.test(text),
    'Invalid state encoding',
  );
  return Buffer.from(text, 'base64');
};
const resource = (name) => readFile(new URL(`./${name}`, import.meta.url));
const fontFiles = ['font.ttf', ...Object.values(FONT_FILES).map((file) => `${file}.ttf`)];
let renderer;
async function png(svg) {
  renderer ||= Promise.all([resource('resvg.wasm').then(initWasm), ...fontFiles.map(resource)]);
  const [, ...fonts] = await renderer;
  const r = new Resvg(svg, { font: { fontBuffers: fonts, defaultFontFamily: 'Noto Sans' } });
  try {
    const image = r.render();
    try {
      return b64(image.asPng());
    } finally {
      image.free();
    }
  } finally {
    r.free();
  }
}
/* A standalone SVG download embeds the fonts its text uses. Cropped images
   are re-rendered to their visible pixels, so a download never reveals what
   a crop hides; the editable .excalidraw file keeps the original. */
async function exportSVG(elements, files, background) {
  files = { ...files };
  elements = await Promise.all(elements.map(async (e) => {
    const file = e.type === 'image' && e.crop && files[e.fileId];
    if (!file) return e;
    const { x, y, width, height, naturalWidth, naturalHeight } = e.crop;
    const [w, h] = [Math.max(1, Math.round(width)), Math.max(1, Math.round(height))];
    const visible = `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}" viewBox="${x} ${y} ${width} ${height}"><image href="${file.dataURL}" width="${naturalWidth}" height="${naturalHeight}" preserveAspectRatio="none"/></svg>`;
    files[`crop-${e.id}`] = { mimeType: 'image/png', dataURL: `data:image/png;base64,${await png(visible)}` };
    const { crop, ...uncropped } = e;
    return { ...uncropped, fileId: `crop-${e.id}` };
  }));
  const fonts = {};
  for (const family of usedFonts(elements))
    if (FONT_FILES[family])
      fonts[family] = `data:font/ttf;base64,${b64(await resource(`${FONT_FILES[family]}.ttf`))}`;
  return boardSVG(elements, { files, fonts, background });
}
/* Image files a retained contribution can restore must survive pruning. */
const retainedFiles = (transactions) => new Set(transactions.flatMap((tx) =>
  (tx.changes || []).flatMap((c) => [c.before?.fileId, c.after?.fileId]).filter(Boolean)));
/* Items of LIBRARIES ({name, text}) as the model reads them at
   shared://library[/NAME]: a listing of references, or a numbered PNG
   sheet of the first items. */
const SHEET_ITEMS = 60;
async function libraryView(libraries, part, at) {
  check(Array.isArray(libraries) && libraries.length <= 100, 'Invalid libraries');
  check(['list', 'sheet'].includes(part), 'Unknown library view');
  const items = libraries.flatMap(({ name, text }) => parseLibrary(text).map((item) => {
    const [, , width, height] = extent(item.elements);
    return { ref: `${name}/${item.id}`, name: item.name || '', library: name,
      elements: item.elements.length, size: [Math.round(width), Math.round(height)], item };
  }));
  if (part === 'list') {
    if (!items.length) return { text: 'No library items.\n' };
    return { text: `${items.length} library item${items.length === 1 ? '' : 's'}; insert one with SharedEdit insert and its ref. ` +
      `The sheet ${at}/sheet.png numbers items 1-${Math.min(SHEET_ITEMS, items.length)}.\n` +
      items.map(({ ref, name, library, elements, size }, i) =>
        `${i + 1}. ${ref} · ${JSON.stringify(name)} · ${library} · ${elements} element${elements === 1 ? '' : 's'} · ${size[0]}×${size[1]}`).join('\n') + '\n' };
  }
  const sheet = items.slice(0, SHEET_ITEMS), cell = [200, 170], columns = 6;
  const cells = sheet.map(({ item, name }, i) => {
    const [x, y] = [(i % columns) * cell[0], Math.floor(i / columns) * cell[1]];
    const art = boardSVG(item.elements, { maxEdge: 150, maxScale: 2 }).replace('<svg ', `<svg x="${x + 25}" y="${y + 5}" `);
    return `${art}<text x="${x + 100}" y="${y + 160}" text-anchor="middle" font-family="Noto Sans" font-size="13" fill="#1e1e1e">${i + 1}. ${escape(name.slice(0, 26))}</text>`;
  }).join('');
  const width = cell[0] * Math.min(columns, Math.max(1, sheet.length)), height = cell[1] * Math.max(1, Math.ceil(sheet.length / columns));
  return { png: await png(`<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"><rect width="${width}" height="${height}" fill="#ffffff"/>${cells}</svg>`),
    mime: 'image/png' };
}
/* The model names each target by id and the hash it read. A change gives
   `after` (null deletes; without a hash it adds), or `set` and `unset` to
   change fields of an existing element or block. Stale targets fail
   together with their current lines; unrelated edits are unaffected. */
const CHANGE_FIELDS = ['id', 'hash', 'after', 'set', 'unset', 'afterId'];
function resolveChanges(id, kind, content, changes) {
  check(Array.isArray(changes) && changes.length > 0 && changes.length <= 200, 'Expected 1 to 200 changes');
  const nodes = nodesOf(kind, content), images = kind === 'document' ? documentImages(content) : null;
  const noun = kind === 'whiteboard' ? 'element' : 'block', stale = [], seen = new Set();
  const resolved = changes.map((change) => {
    check(change && typeof change === 'object' && identifier(change.id) && !seen.has(change.id),
      'Each change needs a distinct id');
    seen.add(change.id);
    const unknown = Object.keys(change).find((key) => !CHANGE_FIELDS.includes(key));
    check(!unknown, `Unknown change field ${String(unknown).slice(0, 40)}`);
    const current = nodes.find((n) => nodeId(kind, n) === change.id) || null;
    const hash = current && contentHash(current);
    if ((change.hash ?? null) !== hash) {
      stale.push({ id: change.id, hash, line: current && line(id, kind, current) });
      return null;
    }
    const merge = Object.hasOwn(change, 'set') || Object.hasOwn(change, 'unset');
    check(merge !== Object.hasOwn(change, 'after'), `${change.id}: give after, or set and unset`);
    let after = change.after;
    if (merge) {
      check(current, `${change.id}: set and unset change an existing ${noun}`);
      check(change.set === undefined || (change.set && typeof change.set === 'object' && !Array.isArray(change.set)),
        `${change.id}: set is an object of fields`);
      check(change.unset === undefined || (Array.isArray(change.unset) && change.unset.length <= 100 &&
        change.unset.every((key) => typeof key === 'string')), `${change.id}: unset lists field names`);
      after = { ...current, ...(change.set || {}) };
      for (const key of change.unset || []) delete after[key];
    }
    check(after === null || (after && typeof after === 'object' && !Array.isArray(after)),
      `${change.id}: after is an ${noun} object or null`);
    if (after && kind === 'whiteboard') after = { id: change.id, ...after };
    if (after && kind === 'document') after = restoreImages({ ...after, attrs: { ...after.attrs, id: after.attrs?.id ?? change.id } }, images);
    return { id: change.id, before: current, after: after ?? null,
      ...(Object.hasOwn(change, 'afterId') ? { afterId: change.afterId } : {}) };
  });
  if (stale.length)
    throw Object.assign(new Error(`Stale ${stale.map((t) => t.id).join(', ')}; use the current lines below`), {
      code: 'stale', targets: stale });
  return resolved;
}
/* What a question tells the model: the reviewed content as the same hashed
   lines it reads at shared://ID, the discussion it continues, and past the
   budget the address to read on from, so no question is too large to ask. */
const QUESTION_BUDGET = 128 * 1024;
function questionPrompt(state, before, snapshot, discussion) {
  const { kind } = before, id = state.id, noun = kind === 'whiteboard' ? 'ELEMENT' : 'BLOCK';
  const byId = new Map(nodesOf(kind, before.content).map((n) => [nodeId(kind, n), n]));
  const lines = (nodes) => nodes.filter(Boolean).map((n) => line(id, kind, n));
  const scope = snapshot.scope === 'whole' ? `the whole ${kind}` : snapshot.region ? 'an area of it' : 'a selection';
  const head = [`${kind} ${JSON.stringify(before.title)} at ${address(id)}, revision ${state.revision}; the question is about ${scope}.`];
  if (snapshot.region) head.push(`Area: x ${snapshot.region[0]}, y ${snapshot.region[1]}, ${snapshot.region[2]} × ${snapshot.region[3]}`);
  const sections = [];
  if (discussion) sections.push(['Discussion:', discussion.map(({ actor, text }) => `${actor}: ${JSON.stringify(text)}`)]);
  if (snapshot.scope === 'whole') sections.push([`Content (HASH ${noun}-JSON):`, lines(nodesOf(kind, before.content))]);
  else if (kind === 'document') {
    sections.push(['Selected text:', [JSON.stringify(snapshot.content.text)]]);
    sections.push([`Blocks containing it (HASH ${noun}-JSON):`, lines(snapshot.context.map((c) => byId.get(c.id)))]);
  } else {
    sections.push([`Selected (HASH ${noun}-JSON):`, lines(snapshot.content)]);
    if (snapshot.context.length)
      sections.push(['Context: labels, connected and nearby elements:', lines(snapshot.context)]);
  }
  const out = [...head];
  let budget = QUESTION_BUDGET - out.join('\n').length, omitted = 0;
  for (const [title, entries] of sections) {
    if (budget > title.length) { out.push(title); budget -= title.length + 1; }
    for (const entry of entries) {
      if (entry.length < budget) { out.push(entry); budget -= entry.length + 1; }
      else omitted += 1;
    }
  }
  if (omitted) out.push(`${omitted} more line${omitted === 1 ? '' : 's'} omitted; Read ${address(id)}` +
    (discussion ? ` and ${address(id, 'comments')}` : '') + ' for the rest.');
  return out.join('\n');
}
/* What an edit tells the model: the stored lines of everything it changed,
   within a budget, then ids and hashes; the board PNG travels separately. */
const RESULT_BUDGET = 24 * 1024;
function modelResult(state, after, transaction, extra = {}) {
  let budget = RESULT_BUDGET;
  const changed = [], hashes = [];
  for (const change of transaction?.changes || []) {
    if (!change.after) continue;
    const text = line(state.id, after.kind, change.after);
    if (text.length <= budget) { changed.push(text); budget -= text.length; }
    else hashes.push({ id: change.id, hash: contentHash(change.after) });
  }
  const deleted = (transaction?.changes || []).filter((c) => !c.after).map((c) => c.id);
  return { id: state.id, kind: after.kind, title: after.title, revision: state.revision, address: address(state.id),
    ...(after.background ? { background: after.background } : {}),
    ...(transaction ? { contribution: transaction.id } : {}),
    ...(changed.length ? { changed } : {}), ...(hashes.length ? { changedHashes: hashes } : {}),
    ...(deleted.length ? { deleted } : {}), ...extra };
}
function differences(before, after) {
  const a = before.kind === 'whiteboard' ? before.content : before.content.content || [];
  const b = after.kind === 'whiteboard' ? after.content : after.content.content || [];
  const id = (value) => (before.kind === 'whiteboard' ? value.id : value.attrs?.id);
  const ids = new Set([...a.map(id), ...b.map(id)]);
  return [...ids]
    .filter(Boolean)
    .map((key) => {
      const change = {
        id: key,
        before: a.find((n) => id(n) === key) || null,
        after: b.find((n) => id(n) === key) || null,
      };
      if (before.kind === 'document' && change.before && !change.after) {
        change.restoreAfterId =
          a
            .slice(
              0,
              a.findIndex((n) => id(n) === key),
            )
            .reverse()
            .map(id)
            .find((previous) => b.some((n) => id(n) === previous)) || null;
      }
      return change;
    })
    .filter((c) => !same(c.before, c.after));
}
export async function handle(request) {
  const { action } = request;
  const [major, minor] = process.versions.node.split('.').map(Number);
  check(
    major > 22 || (major === 22 && minor >= 4),
    'Shared editing requires Node 22.4 or newer on the Emacs host',
  );
  if (action === 'status') {
    const doc = create('document', 'Runtime check');
    try {
      initializeDocument(doc);
      validate(doc);
      await png(
        '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><text x="1" y="18">M</text></svg>',
      );
      return { result: { available: true } };
    } catch (error) {
      throw new Error(
        'Repair or reinstall the shared-editing helper resources on the Emacs host, then recheck',
        { cause: error },
      );
    } finally {
      doc.destroy();
    }
  }
  if (action === 'library-view') return { result: await libraryView(request.libraries, request.part, request.at) };
  check(
    ['create', 'import', 'read', 'view', 'update', 'patch', 'insert', 'rename', 'background', 'revert', 'restore', 'export', 'comment', 'reply-comment', 'resolve-comment'].includes(action),
    'Unknown editing action',
  );
  let state = request.state,
    doc,
    notes;
  if (action === 'create' || action === 'import') {
    check(identifier(request.id), 'Invalid item identity');
    let content = request.content,
      kind = request.kind,
      title = request.title,
      importedFiles,
      background;
    if (action === 'import') {
      check(['native', 'excalidraw', 'markdown', 'text'].includes(request.format), 'Unsupported import format');
      check(
        typeof request.data === 'string' && Buffer.byteLength(request.data) <= LIMIT,
        'Import is too large',
      );
      if (request.format === 'markdown' || request.format === 'text') {
        kind = 'document';
        title = request.title || 'Imported document';
        content =
          request.format === 'markdown'
            ? markdown.parse(request.data)
            : {
                type: 'doc',
                content: [
                  {
                    type: 'paragraph',
                    content: request.data ? [{ type: 'text', text: request.data }] : [],
                  },
                ],
              };
      } else if (request.format === 'excalidraw') {
        kind = 'whiteboard';
        title = request.title || 'Imported whiteboard';
        ({ elements: content, files: importedFiles, notes, background } = parseScene(request.data));
      } else {
        const source = JSON.parse(request.data);
        check(
          source.format === 'mevedel-editable-1' && source.kind === 'document' &&
            Object.keys(source).every((k) => ['format', 'kind', 'title', 'content'].includes(k)),
          'Unsupported native file',
        );
        ({ kind, title, content } = source);
      }
    }
    doc = create(kind, title || (kind === 'whiteboard' ? 'Whiteboard' : 'Document'));
    try {
      if (kind === 'document') initializeDocument(doc, content);
      else if (content) {
        doc.transact(() => {
          for (const [id, file] of Object.entries(importedFiles || {})) putFile(doc, id, file);
          for (const element of content) putElement(doc, element);
          if (background) doc.getMap('meta').set('background', background);
        });
        validate(doc);
      }
    } catch (error) {
      doc.destroy();
      throw error;
    }
    state = { format: 1, id: request.id, revision: 0, receipts: {}, transactions: [] };
  } else {
    check(
      state?.format === 1 &&
        identifier(state.id) &&
        Number.isSafeInteger(state.revision) &&
        state.revision >= 0,
      'Invalid persisted editor',
    );
    doc = restore(bytes(state.crdt));
  }
  try {
    const before = inspect(doc),
      vector = Y.encodeStateVector(doc);
    if (action === 'export') {
      const { format } = request;
      if (before.kind === 'whiteboard') {
        check(['native', 'svg', 'png'].includes(format), 'Unsupported board export');
        const files = filesOf(doc);
        if (format === 'native')
          return { result: { text: serializeScene(before.content, files, { background: before.background }),
            mime: 'application/vnd.excalidraw+json', extension: 'excalidraw' } };
        return {
          result:
            format === 'svg'
              ? { text: await exportSVG(before.content, files, before.background), mime: 'image/svg+xml', extension: 'svg' }
              : { data: await png(boardSVG(before.content, { files, background: before.background })), mime: 'image/png', extension: 'png' },
        };
      }
      if (format === 'native')
        return {
          result: {
            text: JSON.stringify({ format: 'mevedel-editable-1', ...before }),
            mime: 'application/json',
            extension: 'mevedel.json',
          },
        };
      check(['markdown', 'html'].includes(format), 'Unsupported document export');
      return {
        result: {
          text:
            format === 'markdown'
              ? markdown.serialize(before.content)
              : documentHTML(before.content),
          mime: format === 'html' ? 'text/html' : 'text/markdown',
          extension: format === 'html' ? 'html' : 'md',
        },
      };
    }
    if (action === 'view') {
      const { part } = request;
      const nodes = nodesOf(before.kind, before.content);
      if (part === 'overview') return { result: { text: overview(state.id, state, before, state.comments || []) } };
      if (part === 'comments') return { result: { text: commentsView(state.id, readComments(doc, state.comments)) } };
      if (part === 'history') return { result: { text: historyView(state) } };
      if (part === 'element') {
        const node = nodes.find((n) => nodeId(before.kind, n) === request.element);
        check(node, `No ${before.kind === 'whiteboard' ? 'element' : 'block'} ${request.element}; Read ${address(state.id)} for current ids`);
        return { result: { text: element(state.id, before.kind, node) } };
      }
      if (part === 'png') {
        check(before.kind === 'whiteboard', `A document has no rendering; Read ${address(state.id)}`);
        return { result: { png: await png(boardSVG(before.content, { files: filesOf(doc), background: before.background })), mime: 'image/png' } };
      }
      if (part === 'image') {
        const file = before.kind === 'whiteboard' ? filesOf(doc)[request.image]
          : { dataURL: documentImages(before.content).get(request.image) };
        check(file?.dataURL, `No image ${request.image} in this ${before.kind}`);
        const [, mime, data] = /^data:([^;]+);base64,(.*)$/s.exec(file.dataURL) || [];
        check(data, 'Unsupported image encoding');
        return { result: { png: data, mime } };
      }
      check(false, 'Unknown shared view');
    }
    if (action === 'read') {
      const result = {
        id: state.id,
        revision: state.revision,
        ...before,
        transactions: state.transactions,
        comments: readComments(doc, state.comments),
      };
      if (request.range) result.selection = selectedText(doc, request.range);
      if (request.question) {
        if (request.commentId) {
          const comment = (state.comments || []).find(c => c.id === request.commentId);
          // A board thread sends the objects of its anchor that still exist.
          check(comment && (before.kind === 'whiteboard'
            ? Array.isArray(request.selection) && request.selection.every(id => comment.selection.includes(id))
              && same(request.region ?? null, comment.region ?? null)
            : same(request.range, comment.range)), 'Comment selection is no longer available');
          check(!comment.resolved, 'Reopen this discussion before asking the assistant');
          check(request.commentVersion === (comment.replies?.at(-1)?.id || comment.id),
            'Discussion changed. Review the thread before sending again.');
          result.discussion = [comment, ...(comment.replies || [])].map(({actor, text}) => ({actor, text}));
        }
        const captured = checkContext(doc, request);
        result.snapshot = { id: state.id, revision: state.revision, ...captured.snapshot,
          ...(result.discussion ? {discussion: result.discussion} : {}) };
        result.prompt = questionPrompt(state, before, captured.snapshot, result.discussion);
        result.quote = captured.quote;
      }
      if (request.since !== undefined) {
        check(
          Number.isSafeInteger(request.since) && request.since >= 0,
          'Invalid earlier revision',
        );
        result.transactions = state.transactions.filter((tx) => tx.revision > request.since);
        result.historyTruncated =
          request.since < state.revision &&
          request.since < (state.transactions.at(-1)?.revision || state.revision) - 1;
      }
      if (request.selection?.length) {
        check(
          Array.isArray(request.selection) && request.selection.length <= 200,
          'Invalid selection',
        );
        const nodes = before.kind === 'whiteboard' ? before.content : before.content.content;
        result.content = nodes.filter((n) =>
          request.selection.includes(before.kind === 'whiteboard' ? n.id : n.attrs?.id),
        );
        check(result.content.length === request.selection.length, 'Selection changed');
        if (before.kind === 'whiteboard')
          result.context = boardContext(before.content, request.selection);
      }
      if (request.sync) result.crdt = state.crdt;
      if (request.image && before.kind === 'whiteboard') {
        check(
          request.imageMax === undefined || [512, 1024, 2048].includes(request.imageMax),
          'Invalid snapshot size',
        );
        // An area question shows the whole board inside the area, with a margin
        // so objects at its edge keep their surroundings.
        const [x, y, w, h] = request.region || [], margin = Math.max(24, Math.round(Math.max(w, h) * 0.08));
        const files = filesOf(doc);
        result.png = await png(request.region
          ? boardSVG(before.content, { files, maxEdge: request.imageMax, background: before.background,
            box: [x - margin, y - margin, w + margin * 2, h + margin * 2], maxScale: 4 })
          : boardSVG(result.content, { files, maxEdge: request.imageMax, context: before.content,
            background: before.background }));
      }
      return { result };
    }
    check(identifier(request.opId), 'Invalid operation identity');
    check(
      typeof request.actor === 'string' && request.actor.length <= 100,
      'Invalid editing actor',
    );
    if (Object.hasOwn(state.receipts, request.opId))
      return {
        state,
        result: { id: state.id, revision: state.revision, ...before, ...(request.image && before.kind === 'whiteboard' ? {png: await png(boardSVG(before.content, { files: filesOf(doc), background: before.background }))} : {}), comments: readComments(doc, state.comments), update: b64(encode(doc)),
          model: modelResult(state, before, null) },
      };
    // ponytail: bounded receipt ledger; compact with acknowledged client epochs if long-lived boards reach this ceiling.
    check(
      Object.keys(state.receipts).length < 65536,
      'Editing history limit reached; export and import into a new item',
    );
    let comments = state.comments || [], inserted;
    if (action === 'comment') {
      const board = before.kind === 'whiteboard';
      check(board ? request.selection?.length || request.region : request.range,
        board ? 'Select objects or an area first' : 'Select a document passage first');
      check(comments.length < 200, `This ${before.kind === 'whiteboard' ? 'whiteboard' : 'document'} has reached its 200 comment limit`);
      check(typeof request.text === 'string' && request.text.trim() && request.text.length <= 10000, 'A comment is required (at most 10000 characters)');
      const captured = checkContext(doc, board ? { ...request, range: undefined, selection: request.selection ?? [] } : request);
      const anchor = board
        ? { selection: captured.snapshot.content.map(shape => shape.id),
            ...(request.region ? { region: request.region } : {}),
            signature: anchorSignature(captured.snapshot.content, before.content) }
        : { range: request.range };
      comments = [...comments, { id: request.opId, actor: request.actor, text: request.text,
        ...anchor, quote: captured.quote, created: Date.now(), resolved: false }];
    } else if (action === 'reply-comment') {
      const comment = comments.find(c => c.id === request.commentId);
      check(comment, 'Comment is no longer available');
      check(!comment.resolved, 'Reopen this discussion before replying');
      check(typeof request.text === 'string' && request.text.trim() && request.text.length <= 10000,
        'A reply is required (at most 10000 characters)');
      const replies = comment.replies || [];
      check(replies.length < 200, 'This discussion has reached its 200 reply limit');
      const reply = {id: request.opId, actor: request.actor, text: request.text, created: Date.now()};
      comments = comments.map(c => c.id === comment.id ? {...c, replies: [...replies, reply]} : c);
    } else if (action === 'resolve-comment') {
      check(comments.some(c => c.id === request.commentId), 'Comment is no longer available');
      check(typeof request.resolved === 'boolean', 'Invalid comment status');
      comments = comments.map(c => c.id === request.commentId ? {...c, resolved: request.resolved} : c);
    } else if (action === 'update') applyUpdate(doc, bytes(request.update));
    else if (action === 'insert') {
      check(before.kind === 'whiteboard', 'Library items insert into whiteboards');
      const item = parseLibrary(request.library).find((candidate) => candidate.id === request.item);
      check(item, `No library item ${request.item}; Read shared://library for current refs`);
      check([request.x, request.y].every((v) => Number.isFinite(v) && Math.abs(v) <= 1e6), 'insert needs x and y');
      const [, , width, height] = extent(item.elements);
      const top = before.content.filter((e) => e.index).at(-1)?.index ?? null;
      const files = filesOf(doc);
      // Library items carry no image files; a missing image draws as a placeholder.
      const elements = item.elements.map((e) => (e.fileId && !files[e.fileId] ? { ...e, fileId: null } : e));
      inserted = placeElements(elements, [request.x + width / 2, request.y + height / 2],
        generateNKeysBetween(top, null, elements.length), () => Math.floor(Math.random() * 2 ** 31));
      patch(doc, inserted.map((e) => ({ id: e.id, before: null, after: e })));
    }
    else if (action === 'background') {
      check(before.kind === 'whiteboard', 'Only whiteboards have a canvas background');
      // An empty value returns the board to the room theme's colour.
      if (request.background === '') doc.getMap('meta').delete('background');
      else {
        check(validBackground(request.background), 'Canvas background must be an opaque #rrggbb colour, or empty for the theme');
        doc.getMap('meta').set('background', request.background);
      }
    } else if (action === 'restore') {
      // A saved version's content becomes the item's content as one ordinary
      // attributed change: lineage, comments and history stay, and the
      // restore itself can be reverted.
      const source = restore(bytes(request.target));
      try {
        const target = inspect(source);
        check(target.kind === before.kind, 'A version restores into its own kind of item');
        doc.transact(() => {
          const meta = doc.getMap('meta');
          meta.set('title', target.title);
          if (target.background) meta.set('background', target.background);
          else meta.delete('background');
          if (before.kind === 'whiteboard') {
            for (const [id, file] of Object.entries(filesOf(source))) putFile(doc, id, file);
            const elements = doc.getMap('elements');
            const retained = new Set(target.content.map((element) => element.id));
            for (const id of [...elements.keys()]) if (!retained.has(id)) elements.delete(id);
            for (const e of target.content) putElement(doc, e);
          } else restoreDocument(doc, source);
        });
        validate(doc);
      } finally {
        source.destroy();
      }
    } else if (action === 'rename') {
      check(
        typeof request.title === 'string' && request.title.trim() && request.title.length <= 200,
        'Invalid title',
      );
      doc.getMap('meta').set('title', request.title);
    } else if (action === 'patch' || action === 'revert') {
      let changes = action === 'patch' ? resolveChanges(state.id, before.kind, before.content, request.changes) : null;
      let title, background;
      if (action === 'revert') {
        const tx = state.transactions.find((t) => t.id === request.transaction);
        check(tx, 'Contribution is no longer available to revert');
        changes = tx.changes.map((c) => ({
          id: c.id,
          before: c.after,
          after: c.before,
          ...(Object.hasOwn(c, 'restoreAfterId') ? { afterId: c.restoreAfterId } : {}),
        }));
        if (tx.title) {
          check(before.title === tx.title.after, 'Stale title; rename it explicitly');
          title = tx.title.before;
        }
        if (tx.background) {
          check((before.background ?? null) === tx.background.after, 'The canvas background changed since');
          background = tx.background;
        }
      }
      if (action === 'patch' || changes.length) {
        if (before.kind === 'whiteboard') patch(doc, changes);
        else patchDocument(doc, changes);
      }
      if (title !== undefined) doc.getMap('meta').set('title', title);
      if (background) {
        if (background.before) doc.getMap('meta').set('background', background.before);
        else doc.getMap('meta').delete('background');
      }
    }
    const after = inspect(doc),
      revision = state.revision + 1;
    const changes = differences(before, after);
    const transaction = {
      id: request.opId,
      revision,
      actor: request.actor,
      time: Date.now(),
      changes,
      ...(before.title !== after.title
        ? { title: { before: before.title, after: after.title } }
        : {}),
      ...(before.background !== after.background
        ? { background: { before: before.background ?? null, after: after.background ?? null } }
        : {}),
    };
    const changed = changes.length || transaction.title || transaction.background;
    if (after.kind === 'whiteboard')
      pruneFiles(doc, retainedFiles(changed ? [transaction, ...state.transactions].slice(0, 32) : state.transactions));
    const next = {
      ...state,
      kind: after.kind,
      title: after.title,
      revision,
      crdt: b64(encode(doc)),
      comments,
      receipts: { ...state.receipts, [request.opId]: revision },
      transactions: (changed ? [transaction, ...state.transactions] : [...state.transactions]).slice(0, 32),
    };
    const sizes = next.transactions.map(tx => Buffer.byteLength(JSON.stringify(tx)));
    let size = Buffer.byteLength(JSON.stringify(next));
    let historySize = 2 + sizes.reduce((sum, n) => sum + n, 0) + Math.max(0, sizes.length - 1);
    // Keep the newest revertible change; expire older snapshots before they
    // consume the item's capacity. One large latest change may exceed 4 MiB.
    while (sizes.length > 1 && (historySize > 4 * 1024 * 1024 || size > LIMIT)) {
      const removed = sizes.pop() + 1;
      next.transactions.pop();
      historySize -= removed;
      size -= removed;
    }
    check(size <= LIMIT, 'Shared content is too large');
    return {
      state: next,
      result: {
        id: next.id,
        revision,
        ...after,
        ...(request.image && after.kind === 'whiteboard' ? {png: await png(boardSVG(after.content, { files: filesOf(doc), background: after.background }))} : {}),
        comments: readComments(doc, comments),
        update: b64(Y.encodeStateAsUpdate(doc, vector)),
        transaction: changed ? transaction : null,
        ...(notes?.length ? { notes } : {}),
        ...(inserted ? { inserted: inserted.map((e) => e.id) } : {}),
        model: modelResult(next, after, changed ? transaction : null, {
          ...(notes?.length ? { notes } : {}),
          ...(inserted ? { inserted: inserted.map((e) => e.id) } : {}) }),
      },
    };
  } finally {
    doc.destroy();
  }
}
