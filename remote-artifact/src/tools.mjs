// The MCP tools, as a registry: each entry is { name, description,
// inputSchema, run }.  RUN receives the validated arguments and a context
// { env, client, connect }; connect(entry, ctx) returns a room whose
// close() releases it (see connections.mjs).  RUN returns the text result,
// or a full MCP tool result { content, isError? }, or throws an Error whose
// message the agent sees.  New tools are new entries.
import { readFile, stat } from 'node:fs/promises';
import path from 'node:path';
import { openEntry } from './connections.mjs';
import { UNAVAILABLE } from './room.mjs';
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

/* Image types read_artifact shows an HTML artifact's file as, by extension. */
const IMAGE_TYPES = { png: 'image/png', jpg: 'image/jpeg', jpeg: 'image/jpeg', gif: 'image/gif',
  webp: 'image/webp' };

/*
 * The hash of FILE of artifact NAME as this server last saw it, from a read,
 * a write or a refusal that carried the content; undefined when unseen.
 * File writes name it, so the origin refuses them when FILE changed since.
 */
function seen(ctx, name, file) {
  ctx.hashes ??= new Map();
  return ctx.hashes.get(`${name}\0${file}`);
}

function see(ctx, name, file, hash) {
  ctx.hashes ??= new Map();
  if (hash === undefined) ctx.hashes.delete(`${name}\0${file}`);
  else ctx.hashes.set(`${name}\0${file}`, hash);
}

/* The hash FILE was read with, or throw that it must be read first. */
function readHash(ctx, name, file) {
  const hash = seen(ctx, name, file);
  if (hash === undefined) throw new Error(`Read ${file} with read_artifact first`);
  return hash;
}

/* The MCP result of the origin's REPLY refusing a change of FILE as stale. */
function stale(ctx, name, file, reply) {
  // Content that came with the refusal counts as read; a hash alone does not.
  if (typeof reply.text === 'string' || typeof reply.data === 'string' || reply.hash === null) {
    see(ctx, name, file, reply.hash ?? null);
  }
  const current = typeof reply.text === 'string' ? `Current content of ${file}:\n${reply.text}`
    : reply.hash === null ? `${file} does not exist now.`
      : `${file} has ${reply.bytes} bytes now; read it again with read_artifact.`;
  return { content: [{ type: 'text', text: `${reply.error}.\n${current}` }], isError: true };
}

/* Write CONTENT to FILE of artifact NAME with HASH; return the MCP result. */
async function writeFile(name, file, content, hash, ctx) {
  const reply = await exchange(name, { action: 'write-file', file, hash, text: content }, ctx);
  if (reply.code === 'stale') return stale(ctx, name, file, reply);
  if (reply.error) throw new Error(reply.error);
  see(ctx, name, file, reply.result.hash);
  return `${reply.result.created ? 'Created' : 'Wrote'} ${file} (${reply.result.bytes} bytes)`;
}

const fileArgument = {
  type: 'string',
  description: 'Path of the file inside the HTML artifact, relative to its directory, such as '
    + 'index.html or assets/site.css.',
};

export const tools = [
  {
    name: 'list_artifacts',
    description: 'List the remote artifacts attached on this machine: whiteboards, documents and HTML '
      + 'artifacts that '
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
          reach = error.message === UNAVAILABLE ? `origin unavailable (revoked, deleted, or offline)`
            : `origin unavailable: ${error.message}`;
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
      + 'revision, who made it, when, and what it changed. For an HTML artifact, read one file: its '
      + 'primary page by default, or file (list_artifact_files lists them); text comes back as is, '
      + 'images as images. The file edit tools need a read first. Every read is live from the origin.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        file: { ...fileArgument, description: `${fileArgument.description} HTML artifacts only.` },
        history: { type: 'boolean', description: 'Read the recent contributions instead of the content.' },
      },
      required: ['name'],
      additionalProperties: false,
    },
    async run({ name, file, history }, ctx) {
      const result = await onRoom(name, { action: 'read', ...(file === undefined ? {} : { file }),
        ...(history === true ? { history } : {}) }, ctx);
      if (result.hash === undefined) return result.text;
      see(ctx, name, result.file, result.hash);
      if (typeof result.text === 'string') return result.text;
      const type = IMAGE_TYPES[path.extname(result.file).slice(1).toLowerCase()];
      return type ? { content: picture(result.data, type) }
        : `${result.file} is binary (${result.bytes} bytes); write_artifact_file writes text only, `
          + 'delete_artifact_file can delete it.';
    },
  },
  {
    name: 'list_artifact_files',
    description: 'List the files of a remote HTML artifact -- its pages and assets -- with their sizes; '
      + 'the primary file is the page the artifact opens on. Any link may list.',
    inputSchema: { type: 'object', properties: { name: nameArgument }, required: ['name'],
      additionalProperties: false },
    async run({ name }, ctx) {
      const { primary, files } = await onRoom(name, { action: 'files' }, ctx);
      return files.map((f) => `${f.file} (${f.bytes} bytes${f.file === primary ? ', primary' : ''})`)
        .join('\n');
    },
  },
  {
    name: 'edit_artifact_file',
    description: 'Replace one exact string in a text file of a remote HTML artifact; needs an edit link '
      + 'and a read_artifact of the file first. old_string must occur exactly once: include enough '
      + 'surrounding text to make it unique. If the file changed at the origin since it was read, '
      + 'nothing is written and the current content is returned to redo the edit on.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        file: fileArgument,
        old_string: { type: 'string', description: 'The exact text to replace; it must occur once.' },
        new_string: { type: 'string', description: 'The text to put in its place.' },
      },
      required: ['name', 'file', 'old_string', 'new_string'],
      additionalProperties: false,
    },
    async run({ name, file, old_string: before, new_string: after }, ctx) {
      const hash = readHash(ctx, name, file);
      if (!before) throw new Error('old_string must not be empty; use write_artifact_file to rewrite a file');
      const reply = await exchange(name, { action: 'read', file }, ctx);
      if (reply.error) throw new Error(reply.error);
      const current = reply.result;
      if (current.hash !== hash) {
        return stale(ctx, name, file, { ...current,
          error: `${file} changed since it was read; redo the change on its current content` });
      }
      if (typeof current.text !== 'string') throw new Error(`${file} is not a text file`);
      const count = current.text.split(before).length - 1;
      if (count === 0) throw new Error(`old_string not found in ${file}`);
      if (count > 1) {
        throw new Error(`old_string occurs ${count} times in ${file}; include more context to make it unique`);
      }
      const index = current.text.indexOf(before);
      const content = current.text.slice(0, index) + after + current.text.slice(index + before.length);
      const result = await writeFile(name, file, content, hash, ctx);
      return typeof result === 'string' ? result.replace(/^Wrote/, 'Edited') : result;
    },
  },
  {
    name: 'write_artifact_file',
    description: 'Write a whole text file of a remote HTML artifact, replacing it or creating it with any '
      + 'missing directories; needs an edit link. Replacing a file needs a read_artifact of it first. If '
      + 'the file changed at the origin since it was read, or a file to create exists, nothing is '
      + 'written and the current content is returned. Files are limited to 16 MiB.',
    inputSchema: {
      type: 'object',
      properties: {
        name: nameArgument,
        file: fileArgument,
        content: { type: 'string', description: 'The file\'s complete new content.' },
      },
      required: ['name', 'file', 'content'],
      additionalProperties: false,
    },
    async run({ name, file, content }, ctx) {
      return writeFile(name, file, content, seen(ctx, name, file) ?? null, ctx);
    },
  },
  {
    name: 'delete_artifact_file',
    description: 'Delete a file of a remote HTML artifact, such as an unused asset; needs an edit link and '
      + 'a read_artifact of the file first. Refused if the file changed since it was read. The '
      + 'artifact\'s primary file, and so the artifact itself, cannot be deleted from here.',
    inputSchema: {
      type: 'object',
      properties: { name: nameArgument, file: fileArgument },
      required: ['name', 'file'],
      additionalProperties: false,
    },
    async run({ name, file }, ctx) {
      const reply = await exchange(name, { action: 'delete-file', file, hash: readHash(ctx, name, file) }, ctx);
      if (reply.code === 'stale') return stale(ctx, name, file, reply);
      if (reply.error) throw new Error(reply.error);
      see(ctx, name, file, undefined);
      return `Deleted ${file}`;
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
