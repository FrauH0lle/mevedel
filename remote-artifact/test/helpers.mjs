// Shared test helpers: an isolated config directory and captured output.
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';

/* An environment whose per-user config directory is a fresh temp dir. */
export function tempEnv(t) {
  const dir = mkdtempSync(path.join(tmpdir(), 'mevedel-remote-artifact-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  return { XDG_CONFIG_HOME: dir, HOME: dir, USER: 'tester' };
}

/* A writable sink collecting everything written to it. */
export function sink() {
  const out = { text: '', write(chunk) { out.text += chunk; return true; } };
  return out;
}
