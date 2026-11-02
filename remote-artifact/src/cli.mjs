// The terminal commands.  Links are attached here and only here, so a
// link -- a secret -- never passes through a model or a chat transcript.
import { randomBytes } from 'node:crypto';
import { bridge } from './bridge.mjs';
import { defaultName } from './identity.mjs';
import { parseLink } from './link.mjs';
import { serve } from './mcp.mjs';
import { openRoom } from './room.mjs';
import { readStore, storePath, writeStore } from './store.mjs';

const USAGE = `usage: mevedel-remote-artifact attach LINK [--name NAME] [--as DISPLAY-NAME]
       mevedel-remote-artifact detach NAME
       mevedel-remote-artifact mcp

attach  remember an artifact link shared from a mevedel origin, named by the
        artifact's id there unless --name chooses another; --as sets the name
        your edits carry at the origin
detach  forget an attached artifact
mcp     run the MCP server for Claude Code, Codex or another MCP client
`;

class UsageError extends Error {}

function options(args, allowed) {
  const positional = [];
  const flags = {};
  for (let i = 0; i < args.length; i++) {
    if (args[i].startsWith('--')) {
      const flag = args[i].slice(2);
      if (!allowed.includes(flag) || i + 1 >= args.length) throw new UsageError();
      flags[flag] = args[++i];
    } else {
      positional.push(args[i]);
    }
  }
  return { positional, flags };
}

async function attach(args, { env, stdout }) {
  const { positional, flags } = options(args, ['name', 'as']);
  if (positional.length !== 1) throw new UsageError();
  const parsed = parseLink(positional[0]);
  const store = readStore(env);
  const existing = Object.values(store.artifacts).find((e) => e.link === parsed.link);
  const guestId = existing?.guestId ?? randomBytes(12).toString('base64url');
  const as = flags.as ?? existing?.as;
  const room = await openRoom(parsed.link, { name: as || defaultName(env), guestId });
  const { id, kind, title, origin } = room.welcome;
  room.close();
  const name = flags.name ?? id;
  const clash = store.artifacts[name];
  if (clash && clash.link !== parsed.link) {
    throw new Error(`${name} is already attached (${clash.title}); choose another name with --name`);
  }
  for (const [other, e] of Object.entries(store.artifacts)) {
    if (e.link === parsed.link && other !== name) delete store.artifacts[other];
  }
  store.artifacts[name] = { link: parsed.link, id, kind, title, guestId, ...(as ? { as } : {}),
    ...(typeof origin === 'string' && origin ? { origin } : {}) };
  writeStore(env, store);
  stdout.write(`Attached ${name}: ${title} (${kind}, ${parsed.tier} link). Stored in ${storePath(env)}.\n`);
}

function detach(args, { env, stdout }) {
  const { positional } = options(args, []);
  if (positional.length !== 1) throw new UsageError();
  const [name] = positional;
  const store = readStore(env);
  if (!store.artifacts[name]) throw new Error(`No remote artifact named ${name}`);
  delete store.artifacts[name];
  writeStore(env, store);
  stdout.write(`Detached ${name}.\n`);
}

/**
 * Run the command ARGV.  Return its exit status: 0 on success, 1 when it
 * failed, 2 on a usage error.
 */
export async function main(argv, { env = process.env, stdout = process.stdout,
  stderr = process.stderr, stdin = process.stdin } = {}) {
  const [command, ...args] = argv;
  try {
    switch (command) {
      case 'attach': await attach(args, { env, stdout }); return 0;
      case 'detach': detach(args, { env, stdout }); return 0;
      case 'mcp': await serve({ input: stdin, output: stdout, env }); return 0;
      case 'bridge': await bridge({ input: stdin, output: stdout }); return 0;
      default: throw new UsageError();
    }
  } catch (error) {
    if (error instanceof UsageError) {
      stderr.write(USAGE);
      return 2;
    }
    stderr.write(`mevedel-remote-artifact: ${error.message}\n`);
    return 1;
  }
}
