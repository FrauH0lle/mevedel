/* Private JSON request handler. Emacs alone owns files, authority, and commits. */
import * as Y from 'yjs';
import { checkContext, readComments } from './context.mjs';
import { readFile } from 'node:fs/promises';
import { initWasm, Resvg } from '@resvg/resvg-wasm';
import {
  create,
  restore,
  encode,
  inspect,
  applyUpdate,
  patch,
  putShape,
  validate,
  check,
  identifier,
  LIMIT,
  same,
} from './model.mjs';
import { initializeDocument, patchDocument, markdown, selectedText } from './document.mjs';
import { boardSVG, documentHTML } from './render.mjs';
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
let renderer;
async function png(svg) {
  renderer ||= Promise.all([
    readFile(new URL('./resvg.wasm', import.meta.url)).then(initWasm),
    readFile(new URL('./font.ttf', import.meta.url)),
  ]);
  const [, font] = await renderer;
  const r = new Resvg(svg, { font: { fontBuffers: [font], defaultFontFamily: 'Noto Sans' } });
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
    doc;
  if (action === 'create' || action === 'import') {
    check(identifier(request.id), 'Invalid item identity');
    let content = request.content,
      kind = request.kind,
      title = request.title;
    if (action === 'import') {
      check(['native', 'markdown', 'text'].includes(request.format), 'Unsupported import format');
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
      } else {
        const source = JSON.parse(request.data);
        check(
          source.format === 'mevedel-editable-1' &&
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
        check(Array.isArray(content) && content.length <= 2000, 'Invalid board import');
        check(
          new Set(content.map((s) => s.id)).size === content.length,
          'Duplicate shape identity',
        );
        doc.transact(() => {
          for (const shape of content) putShape(doc, shape);
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
      if (format === 'native')
        return {
          result: {
            text: JSON.stringify({ format: 'mevedel-editable-1', ...before }),
            mime: 'application/json',
            extension: 'mevedel.json',
          },
        };
      if (before.kind === 'whiteboard') {
        check(['svg', 'png'].includes(format), 'Unsupported board export');
        const svg = boardSVG(before.content);
        return {
          result:
            format === 'svg'
              ? { text: svg, mime: 'image/svg+xml', extension: 'svg' }
              : { data: await png(svg), mime: 'image/png', extension: 'png' },
        };
      }
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
          check(comment && same(request.range, comment.range), 'Comment selection is no longer available');
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
        if (before.kind === 'whiteboard') {
          const endpoints = new Set(result.content.flatMap((s) => [s.from, s.to]).filter(Boolean));
          result.context = before.content.filter(
            (s) => endpoints.has(s.id) && !request.selection.includes(s.id),
          );
        }
      }
      if (request.sync) result.crdt = state.crdt;
      if (request.image && before.kind === 'whiteboard') {
        check(
          request.imageMax === undefined || [512, 1024, 2048].includes(request.imageMax),
          'Invalid snapshot size',
        );
        result.png = await png(boardSVG(result.content, request.imageMax, before.content));
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
        result: { id: state.id, revision: state.revision, ...before, ...(request.image && before.kind === 'whiteboard' ? {png: await png(boardSVG(before.content))} : {}), comments: readComments(doc, state.comments), update: b64(encode(doc)) },
      };
    // ponytail: bounded receipt ledger; compact with acknowledged client epochs if long-lived boards reach this ceiling.
    check(
      Object.keys(state.receipts).length < 65536,
      'Editing history limit reached; export and import into a new item',
    );
    let comments = state.comments || [];
    if (action === 'comment') {
      check(before.kind === 'document' && request.range, 'Select a document passage first');
      check(comments.length < 200, 'This document has reached its 200 comment limit');
      check(typeof request.text === 'string' && request.text.trim() && request.text.length <= 10000, 'A comment is required (at most 10000 characters)');
      const captured = checkContext(doc, request);
      comments = [...comments, { id: request.opId, actor: request.actor, text: request.text,
        range: request.range, quote: captured.quote, created: Date.now(), resolved: false }];
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
        ...(request.image && after.kind === 'whiteboard' ? {png: await png(boardSVG(after.content))} : {}),
        comments: readComments(doc, comments),
        update: b64(Y.encodeStateAsUpdate(doc, vector)),
        transaction: changed ? transaction : null,
      },
    };
  } finally {
    doc.destroy();
  }
}
