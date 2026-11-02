import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, statSync } from 'node:fs';
import { main } from '../src/cli.mjs';
import { storePath } from '../src/store.mjs';
import { fixture, matches, startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

async function run(env, ...argv) {
  const out = sink();
  const err = sink();
  const code = await main(argv, { env, stdout: out, stderr: err });
  return { code, out: out.text, err: err.text };
}

async function room(t, options) {
  const fake = await startFakeRoom(options);
  t.after(() => fake.close());
  return fake;
}

const stored = (env) => JSON.parse(readFileSync(storePath(env), 'utf8'));

test('attach stores the link once per user, private, named by the origin id', async (t) => {
  const env = tempEnv(t);
  const origin = await room(t);
  const result = await run(env, 'attach', origin.link('edit'), '--as', 'Roland at laptop');
  assert.equal(result.code, 0, result.err);
  assert.match(result.out, /Attached snt-pipeline: SNT analysis pipeline \(whiteboard, edit link\)/);
  // The link is a secret: it is stored, never printed.
  assert.ok(!result.out.includes(origin.roomId) && !result.err.includes(origin.roomId));
  assert.equal(statSync(storePath(env)).mode & 0o777, 0o600);
  const entry = stored(env).artifacts['snt-pipeline'];
  assert.equal(entry.link, origin.link('edit'));
  assert.equal(entry.as, 'Roland at laptop');
  // The welcome names the origin; the entry keeps it for listings.
  assert.equal(entry.origin, fixture('welcome').bindings.$ORIGIN);
  // The origin saw the hello the fixtures record, with the chosen name.
  const [hello] = origin.received;
  assert.ok(matches(fixture('hello').frames[0].frame, hello));
  assert.equal(hello.name, 'Roland at laptop');
  assert.equal(hello.writeToken, origin.writeToken);
});

test('attach stores no origin when the welcome names none', async (t) => {
  const env = tempEnv(t);
  const named = await room(t);
  assert.equal((await run(env, 'attach', named.link())).code, 0);
  assert.equal(stored(env).artifacts['snt-pipeline'].origin, 'snt-app on mevedel-host');
  const older = await room(t, { bindings: { $ORIGIN: undefined } });
  assert.equal((await run(env, 'attach', older.link(), '--name', 'older')).code, 0);
  assert.ok(!Object.hasOwn(stored(env).artifacts.older, 'origin'));
});

test('attach refuses a name collision unless --name chooses another', async (t) => {
  const env = tempEnv(t);
  const first = await room(t);
  const second = await room(t);
  assert.equal((await run(env, 'attach', first.link())).code, 0);
  // The same link again is no collision.
  assert.equal((await run(env, 'attach', first.link())).code, 0);
  const clash = await run(env, 'attach', second.link('view'));
  assert.equal(clash.code, 1);
  assert.match(clash.err, /snt-pipeline is already attached .*--name/);
  assert.equal(stored(env).artifacts['snt-pipeline'].link, first.link());
  const renamed = await run(env, 'attach', second.link('view'), '--name', 'snt-copy');
  assert.equal(renamed.code, 0, renamed.err);
  assert.match(renamed.out, /Attached snt-copy: .*view link/);
  assert.deepEqual(Object.keys(stored(env).artifacts).sort(), ['snt-copy', 'snt-pipeline']);
});

test('attach refuses session and lobby links without sending anything after hello', async (t) => {
  const env = tempEnv(t);
  for (const mode of ['session', 'lobby']) {
    const origin = await room(t, { mode });
    const result = await run(env, 'attach', origin.link());
    assert.equal(result.code, 1);
    assert.match(result.err, /session or lobby link, not an artifact link/);
    // Let anything the client might still send arrive.
    await new Promise((resolve) => setTimeout(resolve, 100));
    assert.deepEqual(origin.received.map((frame) => frame.t), ['hello'], mode);
  }
  assert.throws(() => statSync(storePath(env)));
});

test('attach refuses an owner link without connecting', async (t) => {
  const env = tempEnv(t);
  const origin = await room(t);
  const result = await run(env, 'attach', origin.link('owner'));
  assert.equal(result.code, 1);
  assert.match(result.err, /not an artifact link/);
  assert.equal(origin.connections, 0);
});

test('attach reports an unavailable origin and a protocol mismatch', async (t) => {
  const env = tempEnv(t);
  const absent = await room(t, { mode: 'absent' });
  const gone = await run(env, 'attach', absent.link());
  assert.equal(gone.code, 1);
  assert.match(gone.err, /Origin unavailable \(revoked, deleted, or offline\)/);
  const mismatch = await room(t, { mode: 'mismatch' });
  const old = await run(env, 'attach', mismatch.link());
  assert.equal(old.code, 1);
  assert.match(old.err, /origin speaks protocol 4, this package speaks 3; update one/);
});

test('detach forgets an artifact and refuses an unknown name', async (t) => {
  const env = tempEnv(t);
  const origin = await room(t);
  assert.equal((await run(env, 'attach', origin.link())).code, 0);
  const done = await run(env, 'detach', 'snt-pipeline');
  assert.equal(done.code, 0, done.err);
  assert.match(done.out, /Detached snt-pipeline/);
  assert.deepEqual(stored(env).artifacts, {});
  const unknown = await run(env, 'detach', 'snt-pipeline');
  assert.equal(unknown.code, 1);
  assert.match(unknown.err, /No remote artifact named snt-pipeline/);
});

test('usage errors name the commands', async (t) => {
  const env = tempEnv(t);
  for (const argv of [[], ['attach'], ['detach'], ['frobnicate'], ['attach', 'x', '--name']]) {
    const result = await run(env, ...argv);
    assert.equal(result.code, 2, argv.join(' '));
    assert.match(result.err, /usage: mevedel-remote-artifact/);
  }
});
