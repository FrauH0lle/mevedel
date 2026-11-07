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
