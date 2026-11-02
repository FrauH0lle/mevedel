// The per-user link store: one JSON file in the user's config directory,
// readable only by the user, holding every attached artifact link.  Links
// are secrets; nothing else in the package writes or prints them.
import { chmodSync, mkdirSync, readFileSync, renameSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import path from 'node:path';

/* The store file for environment ENV. */
export function storePath(env = process.env) {
  const base = env.XDG_CONFIG_HOME
    || (process.platform === 'win32' && env.APPDATA)
    || path.join(env.HOME || homedir(), '.config');
  return path.join(base, 'mevedel-remote-artifact', 'links.json');
}

/* The stored attachments: { artifacts: { NAME: ENTRY } }. */
export function readStore(env) {
  try {
    const data = JSON.parse(readFileSync(storePath(env), 'utf8'));
    if (data && typeof data.artifacts === 'object' && data.artifacts) return data;
  } catch (error) {
    if (error.code !== 'ENOENT') throw new Error(`Cannot read ${storePath(env)}: ${error.message}`);
  }
  return { artifacts: {} };
}

/* Replace the stored attachments with DATA, mode 0600. */
export function writeStore(env, data) {
  const file = storePath(env);
  mkdirSync(path.dirname(file), { recursive: true, mode: 0o700 });
  const temporary = `${file}.${process.pid}.tmp`;
  writeFileSync(temporary, `${JSON.stringify(data, null, 2)}\n`, { mode: 0o600 });
  chmodSync(temporary, 0o600);
  renameSync(temporary, file);
}

/* The stored entry NAME, or throw an actionable error. */
export function entry(env, name) {
  if (typeof name !== 'string' || !name) throw new Error('name is required: list_artifacts shows the attached ones');
  const found = readStore(env).artifacts[name];
  if (!found) throw new Error(`No remote artifact named ${name}; list_artifacts shows the attached ones`);
  return found;
}
