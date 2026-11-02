// The MCP tools, as a registry: each entry is { name, description,
// inputSchema, run }.  RUN receives the validated arguments and a context
// { env, client, connect }; connect(entry, ctx) returns a room whose
// close() releases it (see connections.mjs).  RUN returns the text result,
// or a full MCP tool result { content, isError? }, or throws an Error whose
// message the agent sees.  New tools are new entries.
import { readFile, stat } from 'node:fs/promises';
import path from 'node:path';
import { openEntry } from './connections.mjs';
import { entry, readStore } from './store.mjs';

const nameArgument = {
  type: 'string',
  description: 'Name of the attached artifact, as list_artifacts shows it.',
};

/* Run BODY on the artifact NAME's room; return the origin's reply. */
async function exchange(name, body, ctx) {
  const found = entry(ctx.env, name);
  const room = await (ctx.connect ?? openEntry)(found, ctx);
  try {
    return await room.request({ ...body, id: room.welcome.id });
  } finally {
    room.close();
  }
}

/* Run BODY on the artifact NAME's room; return its result or throw its error. */
async function onRoom(name, body, ctx) {
  const reply = await exchange(name, body, ctx);
  if (reply.error) throw new Error(reply.error);
  return reply.result;
}

const tierOf = (link) => (Buffer.from(link.slice(link.lastIndexOf('.') + 1), 'base64url').length === 48
  ? 'edit' : 'view');

/* SharedEdit's limit for an image brought into a whiteboard or document. */
const IMAGE_MAX_BYTES = 4_700_000;

/* Whether BYTES are a PNG, JPEG, WebP or SVG image. */
function imageFile(bytes) {
  const starts = (...prefix) => prefix.every((b, i) => bytes[i] === b);
  if (starts(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a) || starts(0xff, 0xd8, 0xff)) return true;
  if (bytes.subarray(0, 4).toString('latin1') === 'RIFF'
    && bytes.subarray(8, 12).toString('latin1') === 'WEBP') return true;
  const head = bytes.subarray(0, 4096).toString('utf8').replace(/^﻿/, '');
  return /^\s*(<\?xml[^>]*\?>\s*)?((<!--[\s\S]*?-->|<!DOCTYPE[^>]*>)\s*)*<svg[\s>/]/i.test(head);
}

/* The base64 content of the local image file SOURCE, or throw why not. */
async function imageData(source) {
  if (typeof source !== 'string' || !source) {
    throw new Error('The image action needs source: the path of a local image file');
  }
  let info;
  try { info = await stat(path.resolve(source)); } catch { throw new Error(`File not found: ${source}`); }
  if (info.isDirectory()) throw new Error(`${source} is a directory, not an image file`);
  if (info.size > IMAGE_MAX_BYTES) {
    throw new Error(`${source} is ${info.size} bytes; shared images are limited to ${IMAGE_MAX_BYTES} bytes. `
      + 'Re-render at a lower resolution or save as JPEG or WebP');
  }
  const bytes = await readFile(path.resolve(source));
  if (!imageFile(bytes)) throw new Error(`${source} is not a PNG, JPEG, WebP or SVG image`);
  return bytes.toString('base64');
}

/* The SharedEdit actions edit_artifact offers, with the arguments each passes on. */
const EDIT_ACTIONS = {
  patch: ['changes'],
  image: ['x', 'y', 'width', 'alt', 'afterId'],
  rename: ['title'],
  background: ['background'],
  insert: ['item', 'x', 'y'],
  revert: ['transaction'],
};

/* A PNG picture as MCP image content, when DATA is there. */
const picture = (data, mimeType = 'image/png') => (data ? [{ type: 'image', data, mimeType }] : []);

export const tools = [
  {
    name: 'list_artifacts',
    description: 'List the remote artifacts attached on this machine: whiteboards and documents that '
      + 'live in a mevedel workspace elsewhere (their origin). Shows each one\'s name, title, kind, '
      + 'whether this link may only view or also edit it, and whether its origin is reachable now.',
    inputSchema: { type: 'object', properties: {}, additionalProperties: false },
    async run(_args, ctx) {
      const attached = Object.entries(readStore(ctx.env).artifacts);
      if (!attached.length) {
        return 'No remote artifacts attached. The user attaches one in a terminal: '
          + 'npx mevedel-remote-artifact attach LINK';
      }
      const lines = await Promise.all(attached.map(async ([name, found]) => {
        let { title, kind } = found;
        let reach = 'origin reachable';
        try {
          const room = await (ctx.connect ?? openEntry)(found, ctx);
          ({ title, kind } = room.welcome);
          room.close();
        } catch (error) {
          // The generic error and the goodbyes already say so; others get the prefix.
          reach = error.message.startsWith('Origin unavailable')
            ? `o${error.message.slice(1)}` : `origin unavailable: ${error.message}`;
        }
        return `${name}: ${JSON.stringify(title)} ${kind}, ${tierOf(found.link)} link, ${reach}`;
      }));
      return lines.join('\n');
    },
  },
  {
    name: 'read_artifact',
    description: 'Read a remote whiteboard or document as its origin shows it: a header, then one line '
      + 'per element or block, HASH then its JSON. With history, read its recent contributions '
      + 'instead, newest first: each line gives the contribution id edit_artifact\'s revert names, its '
      + 'revision, who made it, when, and what it changed. Every read is live from the origin.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        history: { type: 'boolean', description: 'Read the recent contributions instead of the content.' },
      },
      required: ['name'],
      additionalProperties: false,
    },
    async run({ name, history }, ctx) {
      return (await onRoom(name, { action: 'read', ...(history === true ? { history } : {}) }, ctx)).text;
    },
  },
  {
    name: 'edit_artifact',
    description: 'Change a remote whiteboard or document, exactly as mevedel\'s SharedEdit does at its '
      + 'origin; needs an edit link. Read it first with read_artifact: each line is HASH then an '
      + 'element\'s or block\'s JSON. patch: changes are {id,hash,set?,unset?,after?,afterId?}; set merges '
      + 'fields and unset removes them, after replaces the whole element or block and null deletes it; a '
      + 'new element has no hash. A stale hash rejects the whole patch and returns the current lines of '
      + 'its targets; unrelated edits survive. Whiteboards hold Excalidraw elements '
      + '{id,type,x,y,width,height,...} with Excalidraw\'s field names; documents hold top-level '
      + 'ProseMirror blocks, each with a unique attrs.id. image adds a local PNG, JPEG, WebP or SVG file '
      + 'of at most 4.7 MB: on a board at x,y, in a document after afterId; width sets its display width '
      + 'and alt describes a document image. rename sets the title; background sets a board\'s canvas '
      + 'colour (#rrggbb, or empty for the theme). insert places a library item of a whiteboard '
      + '(item as LIBRARY/ITEM-ID from whiteboard_libraries) with its top-left at x,y. revert undoes '
      + 'one contribution, named by its id from read_artifact with history, and is refused if its '
      + 'targets changed since. Returns the stored lines of what changed and, for a whiteboard, the '
      + 'board as drawn.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        action: { type: 'string', enum: Object.keys(EDIT_ACTIONS), description: 'Operation.' },
        changes: { type: 'array', items: { type: 'object' },
          description: 'Targeted changes for patch: {id,hash,set,unset,after,afterId}.' },
        title: { type: 'string', description: 'New title for rename.' },
        background: { type: 'string', description: 'Canvas colour for background: #rrggbb, or empty for the room theme.' },
        item: { type: 'string', description: 'Library item for insert: LIBRARY/ITEM-ID from whiteboard_libraries.' },
        source: { type: 'string', description: 'Local image file for image: PNG, JPEG, WebP or SVG, at most 4.7 MB.' },
        x: { type: 'number', description: 'Left edge of an added board image or inserted library item.' },
        y: { type: 'number', description: 'Top edge of an added board image or inserted library item.' },
        width: { type: 'number', description: 'Display width of an added image, 1 to 8192; its height follows.' },
        alt: { type: 'string', description: 'Description of an added document image.' },
        afterId: { type: 'string', description: 'Document block the added image follows; default the end.' },
        transaction: { type: 'string',
          description: 'Contribution id for revert, from read_artifact with history.' },
      },
      required: ['name', 'action'],
      additionalProperties: false,
    },
    async run(args, ctx) {
      const keys = Object.hasOwn(EDIT_ACTIONS, args.action) && EDIT_ACTIONS[args.action];
      if (!keys) throw new Error(`action must be one of ${Object.keys(EDIT_ACTIONS).join(', ')}`);
      const body = { action: args.action };
      for (const key of keys) if (args[key] !== undefined) body[key] = args[key];
      if (args.action === 'image') body.data = await imageData(args.source);
      if (args.action === 'insert' && (typeof args.item !== 'string' || !args.item.includes('/'))) {
        throw new Error('insert needs item as LIBRARY/ITEM-ID from whiteboard_libraries');
      }
      const reply = await exchange(args.name, body, ctx);
      if (reply.code === 'stale') {
        const { error, code, targets, revision } = reply;
        return { content: [{ type: 'text', text: JSON.stringify({ error, code, targets, revision }) }],
          isError: true };
      }
      if (reply.error) throw new Error(reply.error);
      return { content: [{ type: 'text', text: reply.result.text }, ...picture(reply.result.png)] };
    },
  },
  {
    name: 'whiteboard_libraries',
    description: 'Browse the element libraries of the user whose mevedel shares a remote whiteboard '
      + '(their own, installed ones and the built-in one; sharing a whiteboard exposes them). Without '
      + 'library it lists every item as LIBRARY/ITEM-ID, the ref edit_artifact insert takes; library '
      + 'limits the listing to one library; preview returns the picture of the library (or of all of '
      + 'them) instead. Any link may browse; whiteboards only. Libraries cannot be changed from here.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        library: { type: 'string', description: 'Name of one library, as the listing shows it.' },
        preview: { type: 'boolean', description: 'Return the picture of the library instead of the listing.' },
      },
      required: ['name'],
      additionalProperties: false,
    },
    async run({ name, library, preview }, ctx) {
      const result = await onRoom(name, { action: 'library',
        ...(library === undefined ? {} : { library }), ...(preview ? { part: 'sheet' } : {}) }, ctx);
      return result.text !== undefined
        ? { content: [{ type: 'text', text: result.text }] }
        : { content: picture(result.png, result.mime) };
    },
  },
  {
    name: 'view_artifact',
    description: 'Look at a remote whiteboard as drawn at its origin, or at one image embedded in a '
      + 'whiteboard or document: a board image by its fileId, a document image by the key its src '
      + 'shows. Any link may view.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        image: { type: 'string', description: 'Key of an embedded image; omit for the whole board.' },
      },
      required: ['name'],
      additionalProperties: false,
    },
    async run({ name, image }, ctx) {
      const result = await onRoom(name, { action: 'view', ...(image === undefined ? {} : { image }) }, ctx);
      return { content: picture(result.png, result.mime) };
    },
  },
];

/**
 * Call tool NAME with ARGS in context CTX; return an MCP tool result.
 * Return undefined when no such tool exists.
 */
export async function callTool(name, args, ctx) {
  const tool = tools.find((t) => t.name === name);
  if (!tool) return undefined;
  try {
    const value = await tool.run(args ?? {}, ctx);
    return typeof value === 'string' ? { content: [{ type: 'text', text: value }] } : value;
  } catch (error) {
    return { content: [{ type: 'text', text: error.message }], isError: true };
  }
}
