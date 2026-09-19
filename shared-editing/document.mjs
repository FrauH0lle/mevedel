/* A single ProseMirror schema for the browser editor and host-side operations. */
import * as Y from 'yjs';
import { equalityDeep } from 'lib0/function';
import { getSchema } from '@tiptap/core';
import StarterKit from '@tiptap/starter-kit';
import { TableKit } from '@tiptap/extension-table';
import UniqueID, { generateUniqueIds } from '@tiptap/extension-unique-id';
import { MarkdownManager } from '@tiptap/markdown';
import {
  prosemirrorJSONToYXmlFragment,
  yXmlFragmentToProsemirrorJSON,
  initProseMirrorDoc,
  relativePositionToAbsolutePosition,
} from '@tiptap/y-tiptap';

export const extensions = [
  StarterKit.configure({ undoRedo: false, link: { openOnClick: false } }),
  TableKit,
  UniqueID.configure({
    types: [
      'paragraph',
      'heading',
      'blockquote',
      'codeBlock',
      'bulletList',
      'orderedList',
      'table',
      'horizontalRule',
    ],
  }),
];
export const schema = getSchema(extensions);
export const markdown = new MarkdownManager({ extensions });
export const documentJSON = (doc) => yXmlFragmentToProsemirrorJSON(doc.getXmlFragment('document'));
export function selectionPositions(doc, range, parsed) {
  if (!range || JSON.stringify(range).length > 2000) throw new Error('Invalid document selection');
  const root = doc.getXmlFragment('document');
  parsed ||= initProseMirrorDoc(root, schema);
  const positions = [range.anchor, range.head].map((p) =>
    relativePositionToAbsolutePosition(
      doc,
      root,
      Y.createRelativePositionFromJSON(p),
      parsed.mapping,
    ),
  );
  if (positions.some((p) => p === null) || positions[0] === positions[1])
    throw new Error('Selection is no longer available');
  return positions.sort((a, b) => a - b);
}
export function selectedText(doc, range) {
  const parsed = initProseMirrorDoc(doc.getXmlFragment('document'), schema);
  const [from, to] = selectionPositions(doc, range, parsed);
  const context = [];
  parsed.doc.forEach((node, offset) => {
    if (offset <= to && offset + node.nodeSize >= from)
      context.push({ id: node.attrs.id, text: node.textContent.slice(0, 8000) });
  });
  return {
    anchors: range,
    text: parsed.doc.textBetween(from, to, '\n'),
    content: parsed.doc.slice(from, to).toJSON(),
    context,
  };
}
export function validateDocument(json) {
  let count = 0;
  const visit = (node, depth) => {
    if (!node || depth > 48 || ++count > 10000) throw new Error('Document is too complex');
    if (!schema.nodes[node.type]) throw new Error('Unknown document node');
    if (Object.keys(node).some((k) => !['type', 'attrs', 'content', 'marks', 'text'].includes(k)))
      throw new Error('Unknown document property');
    if (node.attrs) {
      const attrs = schema.nodes[node.type].spec.attrs || {};
      for (const key of Object.keys(node.attrs))
        if (!(key in attrs)) throw new Error('Unknown document attribute');
      if (node.attrs.id != null && !/^[a-zA-Z0-9_-]{1,80}$/.test(node.attrs.id))
        throw new Error('Invalid block identity');
      if (
        node.type === 'heading' &&
        (!Number.isInteger(node.attrs.level) || node.attrs.level < 1 || node.attrs.level > 6)
      )
        throw new Error('Invalid heading level');
      for (const key of ['colspan', 'rowspan'])
        if (
          node.attrs[key] != null &&
          (!Number.isInteger(node.attrs[key]) || node.attrs[key] < 1 || node.attrs[key] > 100)
        )
          throw new Error('Invalid table span');
      if (
        node.attrs.colwidth != null &&
        (!Array.isArray(node.attrs.colwidth) ||
          node.attrs.colwidth.length > 100 ||
          node.attrs.colwidth.some((n) => !Number.isInteger(n) || n < 0 || n > 2000))
      )
        throw new Error('Invalid table width');
      if (
        node.attrs.start != null &&
        (!Number.isInteger(node.attrs.start) || node.attrs.start < 1 || node.attrs.start > 1000000)
      )
        throw new Error('Invalid list start');
    }
    for (const mark of node.marks || []) {
      if (Object.keys(mark).some((k) => !['type', 'attrs'].includes(k)))
        throw new Error('Unknown mark property');
      if (!schema.marks[mark.type]) throw new Error('Unknown document mark');
      if (mark.type === 'link' && !/^(https?:|mailto:|#)/i.test(mark.attrs?.href || ''))
        throw new Error('Unsafe document link');
      for (const key of Object.keys(mark.attrs || {}))
        if (!(key in (schema.marks[mark.type].spec.attrs || {})))
          throw new Error('Unknown mark attribute');
    }
    for (const child of node.content || []) visit(child, depth + 1);
  };
  visit(json, 0);
  const node = schema.nodeFromJSON(json);
  node.check();
  if (new TextEncoder().encode(JSON.stringify(json)).length > 1024 * 1024)
    throw new Error('Document is too large');
  const ids = (json.content || []).map((n) => n.attrs?.id);
  if (ids.some((id) => !id)) throw new Error('Missing block identity');
  if (new Set(ids).size !== ids.length) throw new Error('Duplicate block identity');
  return node.toJSON();
}
export function initializeDocument(doc, json = { type: 'doc', content: [{ type: 'paragraph' }] }) {
  if (doc.getXmlFragment('document').length) throw new Error('Document already initialized');
  json = validateDocument(generateUniqueIds(json, extensions));
  prosemirrorJSONToYXmlFragment(schema, json, doc.getXmlFragment('document'));
  seedEmptyText(doc);
}
export function seedEmptyText(doc) {
  // Give simultaneous first writers one shared text identity. Without it,
  // each inserts a separate XmlText and editor normalization replaces one,
  // making that writer's original insertion unavailable to selective undo.
  const seed = (node) => {
    if (node instanceof Y.XmlElement && schema.nodes[node.nodeName]?.isTextblock && !node.length)
      node.insert(0, [new Y.XmlText()]);
    if (node instanceof Y.XmlFragment) node.toArray().forEach(seed);
  };
  seed(doc.getXmlFragment('document'));
}
export function patchDocument(doc, changes) {
  if (!Array.isArray(changes) || !changes.length || changes.length > 200)
    throw new Error('Expected 1 to 200 changes');
  const root = doc.getXmlFragment('document'),
    before = documentJSON(doc);
  const nodes = before.content || [],
    ids = new Set();
  const plans = changes.map((change) => {
    if (!/^[a-zA-Z0-9_-]{1,80}$/.test(change.id) || ids.has(change.id))
      throw new Error('Invalid or duplicate block target');
    ids.add(change.id);
    const index = nodes.findIndex((n) => n.attrs?.id === change.id);
    const current = index < 0 ? null : nodes[index];
    if (!equalityDeep(current, change.before))
      throw Object.assign(new Error(`Stale target: ${change.id}`), {
        code: 'stale',
        targets: [{ id: change.id, current }],
      });
    if (change.after && change.after.attrs?.id !== change.id)
      throw new Error('Cannot change block identity');
    let replacement = null;
    if (change.after) {
      const temp = new Y.Doc();
      try {
        initializeDocument(temp, { type: 'doc', content: [change.after] });
        replacement = temp.getXmlFragment('document').get(0).clone();
      } finally {
        temp.destroy();
      }
    }
    let position = index;
    if (index < 0) {
      position = change.afterId
        ? nodes.findIndex((n) => n.attrs?.id === change.afterId) + 1
        : Object.hasOwn(change, 'afterId')
          ? 0
          : nodes.length;
      if (change.afterId && position === 0) throw new Error('Stale insertion anchor');
    }
    return { index, position, replacement, json: change.after };
  });
  const groups = new Map();
  for (const plan of plans) {
    if (!groups.has(plan.position)) groups.set(plan.position, { insert: [], replace: null });
    const group = groups.get(plan.position);
    if (plan.index >= 0) group.replace = plan;
    else if (plan.replacement) group.insert.push(plan);
  }
  const projected = [];
  for (let i = 0; i <= nodes.length; i++) {
    const group = groups.get(i);
    projected.push(...(group?.insert || []).map((p) => p.json));
    if (group?.replace) {
      if (group.replace.json) projected.push(group.replace.json);
    } else if (i < nodes.length) projected.push(nodes[i]);
  }
  validateDocument({ type: 'doc', content: projected });
  doc.transact(() => {
    for (const [position, group] of [...groups].sort((a, b) => b[0] - a[0])) {
      if (group.replace) {
        root.delete(position, 1);
        if (group.replace.replacement) root.insert(position, [group.replace.replacement]);
      }
      if (group.insert.length)
        root.insert(
          position,
          group.insert.map((p) => p.replacement),
        );
    }
  });
}
