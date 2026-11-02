import test from 'node:test';
import assert from 'node:assert/strict';
import { connectionPool } from '../src/connections.mjs';
import { callTool } from '../src/tools.mjs';
import { main } from '../src/cli.mjs';
import { startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/* A stub room recording whether it was really closed. */
function stubOpen() {
  const opened = [];
  const open = async (entry) => {
    const room = { welcome: { id: entry.id }, closedByUs: false, dead: false,
      get closed() { return this.dead || this.closedByUs; },
      close() { this.closedByUs = true; } };
    opened.push(room);
    return room;
  };
  return { open, opened };
}

test('no connection exists until a call needs one', async (t) => {
  const { open, opened } = stubOpen();
  const pool = connectionPool({ open, idleMs: 50 });
  t.after(() => pool.closeAll());
  assert.equal(opened.length, 0);
  const room = await pool.connect({ id: 'a', link: 'l' }, {});
  assert.equal(opened.length, 1);
  assert.equal(room.welcome.id, 'a');
});

test('calls share one connection; releasing a handle does not close it', async (t) => {
  const { open, opened } = stubOpen();
  const pool = connectionPool({ open, idleMs: 1000 });
  t.after(() => pool.closeAll());
  const first = await pool.connect({ id: 'a', link: 'l' }, {});
  first.close();
  const second = await pool.connect({ id: 'a', link: 'l' }, {});
  assert.equal(opened.length, 1);
  assert.equal(opened[0].closedByUs, false);
  second.close();
});

test('an idle connection closes and the next call reconnects', async (t) => {
  const { open, opened } = stubOpen();
  const pool = connectionPool({ open, idleMs: 30 });
  t.after(() => pool.closeAll());
  (await pool.connect({ id: 'a', link: 'l' }, {})).close();
  await sleep(100);
  assert.equal(opened[0].closedByUs, true);
  (await pool.connect({ id: 'a', link: 'l' }, {})).close();
  assert.equal(opened.length, 2);
});

test('a connection in use is not closed as idle', async (t) => {
  const { open, opened } = stubOpen();
  const pool = connectionPool({ open, idleMs: 30 });
  t.after(() => pool.closeAll());
  const held = await pool.connect({ id: 'a', link: 'l' }, {});
  await sleep(100);
  assert.equal(opened[0].closedByUs, false);
  held.close();
  await sleep(100);
  assert.equal(opened[0].closedByUs, true);
});

test('a connection the origin dropped is replaced on the next call', async (t) => {
  const { open, opened } = stubOpen();
  const pool = connectionPool({ open, idleMs: 1000 });
  t.after(() => pool.closeAll());
  (await pool.connect({ id: 'a', link: 'l' }, {})).close();
  opened[0].dead = true;
  (await pool.connect({ id: 'a', link: 'l' }, {})).close();
  assert.equal(opened.length, 2);
});

test('concurrent first calls open one connection', async (t) => {
  const { open, opened } = stubOpen();
  const pool = connectionPool({ open, idleMs: 1000 });
  t.after(() => pool.closeAll());
  await Promise.all([pool.connect({ id: 'a', link: 'l' }, {}), pool.connect({ id: 'a', link: 'l' }, {})]);
  assert.equal(opened.length, 1);
});

test('a failed open is not cached', async (t) => {
  let calls = 0;
  const pool = connectionPool({ idleMs: 1000, open: async () => {
    if (calls++ === 0) throw new Error('boom');
    return { welcome: {}, closed: false, close() {} };
  } });
  t.after(() => pool.closeAll());
  await assert.rejects(pool.connect({ link: 'l' }, {}), /boom/);
  await pool.connect({ link: 'l' }, {});
  assert.equal(calls, 2);
});

test('end to end: one connection serves several calls, then idle close and reconnect', async (t) => {
  const env = tempEnv(t);
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  assert.equal(await main(['attach', origin.link('view')], { env, stdout: sink(), stderr: sink() }), 0);
  const base = origin.connections;
  const pool = connectionPool({ idleMs: 100 });
  t.after(() => pool.closeAll());
  const ctx = { env, connect: pool.connect };
  assert.equal(base, origin.connections);
  for (let i = 0; i < 2; i++) assert.ok(!(await callTool('read_artifact', { name: 'snt-pipeline' }, ctx)).isError);
  assert.equal(origin.connections, base + 1);
  await sleep(400);
  assert.ok(!(await callTool('read_artifact', { name: 'snt-pipeline' }, ctx)).isError);
  assert.equal(origin.connections, base + 2);
});

test('a protocol mismatch reaches the tool result', async (t) => {
  const env = tempEnv(t);
  const good = await startFakeRoom();
  t.after(() => good.close());
  assert.equal(await main(['attach', good.link('view')], { env, stdout: sink(), stderr: sink() }), 0);
  const old = await startFakeRoom({ mode: 'mismatch' });
  t.after(() => old.close());
  const pool = connectionPool({ idleMs: 100 });
  t.after(() => pool.closeAll());
  const result = await callTool('read_artifact', { name: 'snt-pipeline' },
    { env: { ...env }, connect: (entry, ctx) => pool.connect({ ...entry, link: old.link('view') }, ctx) });
  assert.ok(result.isError);
  assert.match(result.content[0].text, /origin speaks protocol 4, this package speaks 3; update one/);
});
