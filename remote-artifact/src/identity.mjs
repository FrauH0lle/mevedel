// Who this consumer is at the origin.
import { hostname, userInfo } from 'node:os';

/* The default display name: user, machine and, when known, client. */
export function defaultName(env = process.env, client) {
  let user = env.USER || env.USERNAME;
  if (!user) {
    try { user = userInfo().username; } catch { user = 'someone'; }
  }
  return `${user}@${hostname()}${client ? ` (${client})` : ''}`;
}

/* The display name ENTRY's edits carry, for CLIENT. */
export function displayName(entry, env, client) {
  return entry.as || defaultName(env, client);
}
