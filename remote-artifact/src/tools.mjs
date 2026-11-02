// The MCP tools, as a registry: each entry is { name, description,
// inputSchema, run }.  RUN receives the validated arguments and a context
// { env, client, connect } and returns the text result or throws an Error
// whose message the agent sees.  New tools are new entries.
import { displayName } from './identity.mjs';
import { UNAVAILABLE, openRoom } from './room.mjs';
import { entry, readStore } from './store.mjs';

const nameArgument = {
  type: 'string',
  description: 'Name of the attached artifact, as list_artifacts shows it.',
};

/* A fresh connection to ENTRY's room. */
function connect(entry, ctx) {
  return openRoom(entry.link, { name: displayName(entry, ctx.env, ctx.client), guestId: entry.guestId });
}

/* Run BODY on the artifact NAME's room; return its result or throw its error. */
async function onRoom(name, body, ctx) {
  const found = entry(ctx.env, name);
  const room = await (ctx.connect ?? connect)(found, ctx);
  try {
    const reply = await room.request({ ...body, id: room.welcome.id });
    if (reply.error) throw new Error(reply.error);
    return reply.result;
  } finally {
    room.close();
  }
}

const tierOf = (link) => (Buffer.from(link.slice(link.lastIndexOf('.') + 1), 'base64url').length === 48
  ? 'edit' : 'view');

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
          const room = await (ctx.connect ?? connect)(found, ctx);
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
      + 'per element or block, HASH then its JSON. Every read is live from the origin.',
    inputSchema: {
      type: 'object',
      properties: { name: nameArgument },
      required: ['name'],
      additionalProperties: false,
    },
    async run({ name }, ctx) {
      return (await onRoom(name, { action: 'read' }, ctx)).text;
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
    return { content: [{ type: 'text', text: await tool.run(args ?? {}, ctx) }] };
  } catch (error) {
    return { content: [{ type: 'text', text: error.message }], isError: true };
  }
}
