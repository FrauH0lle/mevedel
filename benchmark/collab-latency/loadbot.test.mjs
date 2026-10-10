import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { Guest, checkDelivery } from './loadbot.mjs';

const creds = { key: Buffer.alloc(32, 1), write: Buffer.alloc(16, 2), ws: 'ws://localhost/' };
class Socket extends EventTarget {
  constructor() { super(); queueMicrotask(() => this.dispatchEvent(new Event('open'))); }
  send() {}
  close() { this.dispatchEvent(new CloseEvent('close', { code: 1000 })); }
}
async function receive(bot, frame) {
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const cipher = await crypto.subtle.encrypt({ name: 'AES-GCM', iv }, bot.key,
    new TextEncoder().encode(JSON.stringify(frame)));
  const packet = new Uint8Array(16 + cipher.byteLength);
  packet.set(iv, 4);
  packet.set(new Uint8Array(cipher), 16);
  bot.ws.dispatchEvent(new MessageEvent('message', { data: packet.buffer }));
  await bot.inbound;
}

test('joining waits for the final snapshot, not the welcome', async t => {
  const original = globalThis.WebSocket;
  globalThis.WebSocket = Socket;
  t.after(() => { globalThis.WebSocket = original; });
  const bot = new Guest(0, creds);
  t.after(() => bot.close());
  let ready = false;
  const connected = bot.connect().then(() => { ready = true; });
  while (!bot.ws) await new Promise(resolve => setImmediate(resolve));
  await receive(bot, { t: 'welcome' });
  assert.equal(ready, false);
  await receive(bot, { t: 'snapshot-chunk', records: [], final: false });
  assert.equal(ready, false);
  await receive(bot, { t: 'snapshot-chunk', records: [], final: true });
  await connected;
  assert.equal(ready, true);
});

test('missing snapshots and prompt records time out and remove their handlers', async t => {
  const original = globalThis.WebSocket;
  globalThis.WebSocket = Socket;
  t.after(() => { globalThis.WebSocket = original; });
  const bot = new Guest(0, creds, 20);
  t.after(() => bot.close());
  await assert.rejects(bot.connect(), /Timed out waiting for the final snapshot/);
  assert.equal(bot.waiters.size, 0);
  const handlers = bot.handlers.size;
  await assert.rejects(bot.waitForFrame(() => false, 'prompt'), /Timed out waiting for prompt/);
  assert.equal(bot.handlers.size, handlers);
});

test('disconnect rejects both pending editing requests and record waits', async t => {
  const original = globalThis.WebSocket;
  globalThis.WebSocket = Socket;
  t.after(() => { globalThis.WebSocket = original; });
  const bot = new Guest(0, creds);
  const connected = bot.connect();
  const joining = assert.rejects(connected, /closed 1000/);
  while (!bot.ws) await new Promise(resolve => setImmediate(resolve));
  const request = assert.rejects(bot.request({ action: 'read' }), /closed 1000/);
  bot.ws.close();
  await Promise.all([joining, request]);
  assert.equal(bot.pending.size, 0);
  assert.equal(bot.waiters.size, 0);
});

test('editing timeout cleans up; a successful response cancels its timer', async () => {
  const bot = new Guest(0, creds, 20);
  bot.send = () => Promise.resolve();
  await assert.rejects(bot.request({ action: 'read' }), /Timed out waiting for editing request/);
  assert.equal(bot.pending.size, 0);
  const request = bot.request({ action: 'read' });
  const data = Buffer.from(JSON.stringify({ result: { revision: 4 } })).toString('base64');
  bot.editingFrame({ t: 'editing', reqId: bot.seq, offset: 0, total: data.length, data });
  assert.deepEqual(await request, { revision: 4 });
  assert.equal(bot.pending.size, 0);
});

test('reliable scenarios reject partial observations; presence remains best effort', () => {
  for (const scenario of ['edit', 'prompt']) {
    assert.throws(() => checkDelivery({ scenario, seen: { n: 1 }, expectedSeen: 2 }), /Incomplete/);
    checkDelivery({ scenario, seen: { n: 2 }, expectedSeen: 2 });
  }
  checkDelivery({ scenario: 'presence', seen: { n: 1 }, expectedSeen: 2 });
});

test('invalid workload sizes fail before connecting', () => {
  const script = fileURLToPath(new URL('./loadbot.mjs', import.meta.url));
  for (const options of [ ['--count', '0'], ['--count', '-1'], ['--guests', '0'],
    ['--writers', '5'], ['--interval', '-1'], ['--count', 'nope'] ]) {
    const result = spawnSync(process.execPath, ['--no-experimental-webstorage', script,
      'invalid-link', 'edit', ...options], { encoding: 'utf8' });
    assert.equal(result.status, 1);
    assert.match(result.stderr, /must be positive integers/);
    assert.equal(result.stdout, '');
  }
});

test('a reused board cannot count old matching geometry as this run\'s edit', async () => {
  const original = process.argv;
  let edit;
  try {
    process.argv = ['node', 'loadbot.mjs', 'unused-link', 'edit', '--item', 'existing',
      '--guests', '2', '--writers', '1', '--count', '1', '--interval', '0'];
    ({ edit } = await import('./loadbot.mjs?reused-board'));
  } finally {
    process.argv = original;
  }
  const Y = await import('../../shared-editing/node_modules/yjs/dist/yjs.mjs');
  const { create, putElement } = await import('../../shared-editing/model.mjs');
  const doc = create('whiteboard', 'Existing');
  putElement(doc, { id: 'bot0rect', type: 'rectangle', x: 1, y: 0, width: 100, height: 80 });
  const crdt = Buffer.from(Y.encodeStateAsUpdate(doc)).toString('base64');
  const empty = Buffer.from(Y.encodeStateAsUpdate(doc, Y.encodeStateVector(doc))).toString('base64');
  doc.destroy();
  const bots = Array.from({ length: 2 }, () => ({
    async request(args) {
      if (args.action === 'read') return { crdt };
      assert.equal(args.action, 'update');
      const at = performance.now();
      bots[1].onEvent({ id: 'existing', update: empty }, at);
      // Only this second frame carries the edit made by the current run.
      bots[1].onEvent({ id: 'existing', update: args.update }, at + 100);
      return { revision: 2 };
    },
  }));
  const result = await edit(bots);
  assert.equal(result.seen.n, 1);
  assert.ok(result.seen.p50 >= 100, `stale geometry counted before delivery: ${JSON.stringify(result.seen)}`);
});
