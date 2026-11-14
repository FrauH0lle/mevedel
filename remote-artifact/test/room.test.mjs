import test from 'node:test';
import assert from 'node:assert/strict';
import { openRoom } from '../src/room.mjs';
import { startFakeRoom } from './fake-room.mjs';

test('a malformed reply is dropped and a later goodbye is still handled', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const stderr = t.mock.method(process.stderr, 'write', () => true);
  const room = await openRoom(origin.link(), { name: 'tester', guestId: 'g' });
  // A complete editing reply whose body ("not") is not JSON.
  await origin.raw({ t: 'editing', reqId: 1, offset: 0, total: 4, data: 'bm90' });
  await origin.goodbye('revoked', { back: true });
  await room.drained();
  await assert.rejects(room.request({ action: 'read' }, 1000),
    /the host revoked this artifact link/);
  assert.match(stderr.mock.calls.map((c) => c.arguments[0]).join(''), /dropped a bad frame/);
});

test("a relay replaying an earlier connection's reply cannot answer a later request", async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const first = await openRoom(origin.link(), { name: 'tester', guestId: 'g' });
  const read = await first.request({ action: 'read', id: 'snt-pipeline' });
  assert.match(read.result.text, /revision 2/);
  first.close();
  // What a recording relay kept: the origin's sealed reply to request 1.
  const recorded = origin.sent.filter(({ connection, frame }) =>
    connection === 1 && frame.t === 'editing' && frame.reqId === 1);
  assert.ok(recorded.length > 0);

  const second = await openRoom(origin.link(), { name: 'tester', guestId: 'g' });
  const release = origin.hold();
  // The later connection's request 1, while the relay holds the true answer.
  const pending = second.request({ action: 'read', id: 'snt-pipeline', history: true });
  while (!origin.received.some((frame) => frame.body?.history)) {
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
  for (const { bytes } of recorded) origin.inject(2, bytes);
  await new Promise((resolve) => setTimeout(resolve, 50));
  await second.drained();
  release();
  const history = await pending;
  assert.match(history.result.text, /revision 3/, 'the replayed reply was taken as the answer');
  second.close();
});
