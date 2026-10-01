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
  check,
  identifier,
  LIMIT,
  same,
} from './model.mjs';
import { initializeDocument, patchDocument, markdown, selectedText } from './document.mjs';
import { boardSVG, documentHTML, usedFonts } from './render.mjs';
import { parseScene, serializeScene } from './excalidraw.mjs';
import { FONT_FILES } from './text.mjs';
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
async function exportSVG(elements, files) {
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
  return boardSVG(elements, { files, fonts });
}
/* Image files a retained contribution can restore must survive pruning. */
const retainedFiles = (transactions) => new Set(transactions.flatMap((tx) =>
  (tx.changes || []).flatMap((c) => [c.before?.fileId, c.after?.fileId]).filter(Boolean)));
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
  check(
    ['create', 'import', 'read', 'update', 'patch', 'rename', 'revert', 'export', 'comment', 'reply-comment', 'resolve-comment'].includes(action),
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
      importedFiles;
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
        ({ elements: content, files: importedFiles, notes } = parseScene(request.data));
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
          return { result: { text: serializeScene(before.content, files),
            mime: 'application/vnd.excalidraw+json', extension: 'excalidraw' } };
        return {
          result:
            format === 'svg'
              ? { text: await exportSVG(before.content, files), mime: 'image/svg+xml', extension: 'svg' }
              : { data: await png(boardSVG(before.content, { files })), mime: 'image/png', extension: 'png' },
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
        check(Buffer.byteLength(JSON.stringify(result.snapshot)) <= 128 * 1024, 'Question snapshot is too large; start a smaller discussion');
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
          ? boardSVG(before.content, { files, maxEdge: request.imageMax,
            box: [x - margin, y - margin, w + margin * 2, h + margin * 2], maxScale: 4 })
          : boardSVG(result.content, { files, maxEdge: request.imageMax, context: before.content }));
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
        result: { id: state.id, revision: state.revision, ...before, ...(request.image && before.kind === 'whiteboard' ? {png: await png(boardSVG(before.content, { files: filesOf(doc) }))} : {}), comments: readComments(doc, state.comments), update: b64(encode(doc)) },
      };
    // ponytail: bounded receipt ledger; compact with acknowledged client epochs if long-lived boards reach this ceiling.
    check(
      Object.keys(state.receipts).length < 65536,
      'Editing history limit reached; export and import into a new item',
    );
    let comments = state.comments || [];
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
    else if (action === 'rename') {
      check(
        typeof request.title === 'string' && request.title.trim() && request.title.length <= 200,
        'Invalid title',
      );
      doc.getMap('meta').set('title', request.title);
    } else if (action === 'patch' || action === 'revert') {
      let changes = request.changes;
      let title;
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
      }
      if (action === 'patch' || changes.length) {
        if (before.kind === 'whiteboard') patch(doc, changes);
        else patchDocument(doc, changes);
      }
      if (title !== undefined) doc.getMap('meta').set('title', title);
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
    };
    const changed = changes.length || transaction.title;
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
        ...(request.image && after.kind === 'whiteboard' ? {png: await png(boardSVG(after.content, { files: filesOf(doc) }))} : {}),
        comments: readComments(doc, comments),
        update: b64(Y.encodeStateAsUpdate(doc, vector)),
        transaction: changed ? transaction : null,
        ...(notes?.length ? { notes } : {}),
      },
    };
  } finally {
    doc.destroy();
  }
}
