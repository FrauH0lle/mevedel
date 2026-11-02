// A scripted fake artifact room behind a minimal local relay, for tests.
//
// It speaks just enough WebSocket (RFC 6455) to accept a guest at
// /r/ROOM?role=guest, unseals its frames with the room key the way the
// origin does, and answers from the shared frame fixtures in
// test/fixtures/remote-artifact/ of the repository, so the package is
// tested against the same exchanges the origin's ERT tests check.
import http from 'node:http';
import { createHash, randomBytes, webcrypto } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { isDeepStrictEqual } from 'node:util';
import path from 'node:path';

const fixtures = path.join(path.dirname(fileURLToPath(import.meta.url)),
  '..', '..', 'test', 'fixtures', 'remote-artifact');

/* Fixture NAME as parsed JSON. */
export function fixture(name) {
  return JSON.parse(readFileSync(path.join(fixtures, `${name}.json`), 'utf8'));
}

/* VALUE with every placeholder string replaced from BINDINGS. */
export function bind(value, bindings) {
  if (typeof value === 'string') return Object.hasOwn(bindings, value) ? bindings[value] : value;
  if (Array.isArray(value)) return value.map((v) => bind(v, bindings));
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, bind(v, bindings)]));
  }
  return value;
}

/* Whether ACTUAL matches fixture EXPECTED, a placeholder matching any string. */
export function matches(expected, actual) {
  if (typeof expected === 'string' && expected.startsWith('$')) return typeof actual === 'string';
  if (Array.isArray(expected)) {
    return Array.isArray(actual) && actual.length === expected.length
      && expected.every((v, i) => matches(v, actual[i]));
  }
  if (expected && typeof expected === 'object') {
    return actual && typeof actual === 'object' && !Array.isArray(actual)
      && Object.keys(expected).length === Object.keys(actual).length
      && Object.keys(expected).every((k) => matches(expected[k], actual[k]));
  }
  return expected === actual;
}

const b64url = (bytes) => Buffer.from(bytes).toString('base64url');

/* The editing exchanges the fake room replays, by fixture name. */
const EDITING_FIXTURES = ['read', 'history', 'patch', 'stale-patch', 'view-only', 'revert',
  'read-file', 'write-file', 'write-conflict'];

/* Frames of RFC 6455 from a byte stream. */
function frameReader(onFrame) {
  let buffer = Buffer.alloc(0);
  return (chunk) => {
    buffer = Buffer.concat([buffer, chunk]);
    for (;;) {
      if (buffer.length < 2) return;
      const opcode = buffer[0] & 0x0f;
      const masked = buffer[1] & 0x80;
      let length = buffer[1] & 0x7f;
      let offset = 2;
      if (length === 126) { if (buffer.length < 4) return; length = buffer.readUInt16BE(2); offset = 4; }
      else if (length === 127) { if (buffer.length < 10) return; length = Number(buffer.readBigUInt64BE(2)); offset = 10; }
      const maskOffset = offset;
      if (masked) offset += 4;
      if (buffer.length < offset + length) return;
      const payload = Buffer.from(buffer.subarray(offset, offset + length));
      if (masked) for (let i = 0; i < payload.length; i++) payload[i] ^= buffer[maskOffset + (i & 3)];
      buffer = buffer.subarray(offset + length);
      onFrame(opcode, payload);
    }
  };
}

function wsFrame(opcode, payload) {
  const length = payload.length;
  const head = length < 126 ? Buffer.from([0x80 | opcode, length])
    : length < 65536 ? Buffer.from([0x80 | opcode, 126, length >> 8, length & 0xff])
      : Buffer.concat([Buffer.from([0x80 | opcode, 127]),
        (() => { const b = Buffer.alloc(8); b.writeBigUInt64BE(BigInt(length)); return b; })()]);
  return Buffer.concat([head, payload]);
}

/**
 * Start a fake room.  MODE says what the "origin" is:
 *   artifact  an artifact room replaying the fixtures
 *   session   a session room (a welcome without a room kind)
 *   lobby     a lobby (a lobby listing instead of a welcome)
 *   mismatch  an origin speaking another protocol version
 *   absent    no origin: the relay closes the room with 4004
 *   goodbye   an artifact room that ends itself when a request arrives:
 *             it sends the goodbye fixture with REASON, then the relay
 *             closes the room
 * BINDINGS override the fixtures' example values.
 * KIND overrides the welcome's artifact kind, such as `html'.
 * ON-EDITING, when given, answers an editing request no fixture covers:
 * it is called with the request body and { writable } and returns the
 * reply body, or undefined for the default refusal.
 * Return { url, link(tier), received, connections, goodbye(reason, {back}),
 * close }.  GOODBYE ends the room for its connected consumers as the
 * origin does; unless BACK, later connections find no host.
 */
export async function startFakeRoom({ mode = 'artifact', bindings = {}, kind, readOnly, onEditing, reason } = {}) {
  const roomId = b64url(randomBytes(16));
  const keyBytes = randomBytes(32);
  const writeToken = randomBytes(16);
  const key = await webcrypto.subtle.importKey('raw', keyBytes, 'AES-GCM', false, ['encrypt', 'decrypt']);
  const received = [];
  let connections = 0;
  const sockets = new Set();
  const goodbyes = new Set();
  const examples = (name) => ({ ...fixture(name).bindings, ...bindings });

  const seal = async (frame) => {
    const nonce = webcrypto.getRandomValues(new Uint8Array(12));
    const sealed = new Uint8Array(await webcrypto.subtle.encrypt({ name: 'AES-GCM', iv: nonce }, key,
      new TextEncoder().encode(JSON.stringify(frame))));
    return Buffer.concat([Buffer.from([0, 0, 0, 1]), nonce, sealed]);
  };
  const unseal = async (bytes) => {
    try {
      const plain = await webcrypto.subtle.decrypt({ name: 'AES-GCM', iv: bytes.subarray(4, 16) }, key,
        bytes.subarray(16));
      return JSON.parse(new TextDecoder().decode(plain));
    } catch { return null; }
  };
  // Fixture frames in their wire form: an editing body becomes its chunks.
  const wire = (frame) => {
    if (frame.t !== 'editing' || !('body' in frame)) return [frame];
    const data = Buffer.from(JSON.stringify(frame.body)).toString('base64');
    const chunks = [];
    for (let offset = 0; offset < data.length; offset += 65536) {
      chunks.push({ t: 'editing', reqId: frame.reqId, offset, total: data.length,
        data: data.slice(offset, offset + 65536) });
    }
    return chunks;
  };

  const server = http.createServer((_req, res) => { res.writeHead(404); res.end(); });
  server.on('upgrade', (req, socket) => {
    connections++;
    sockets.add(socket);
    socket.on('close', () => sockets.delete(socket));
    socket.on('error', () => {});
    const url = new URL(req.url, 'http://relay');
    const accept = createHash('sha1')
      .update(`${req.headers['sec-websocket-key']}258EAFA5-E914-47DA-95CA-C5AB0DC85B11`).digest('base64');
    socket.write('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n'
      + `Sec-WebSocket-Accept: ${accept}\r\n\r\n`);
    const closeWith = (code) => {
      const payload = Buffer.alloc(2); payload.writeUInt16BE(code);
      socket.end(wsFrame(8, payload));
    };
    if (mode === 'absent' || url.pathname !== `/r/${roomId}` || url.searchParams.get('role') !== 'guest') {
      closeWith(4004);
      return;
    }
    const send = async (frame) => { for (const f of wire(frame)) socket.write(wsFrame(2, await seal(f))); };
    // The host ends the room: goodbye, then the relay closes it.
    const leave = async (why) => {
      for (const { from, frame: origin } of fixture('goodbye').frames) {
        if (from === 'origin') await send(bind(origin, { ...examples('goodbye'), ...(why ? { $REASON: why } : {}) }));
      }
      socket.write(wsFrame(1, Buffer.from('{"t":"room-closed"}')));
      closeWith(1000);
    };
    goodbyes.add(leave);
    socket.on('close', () => goodbyes.delete(leave));
    const transfers = new Map();
    let writable = false;
    let inbound = Promise.resolve();
    socket.on('data', frameReader((opcode, payload) => {
      if (opcode === 8) { socket.end(); return; }
      if (opcode !== 2) return;
      inbound = inbound.then(async () => {
        const frame = await unseal(payload);
        if (!frame) return;
        if (frame.t === 'editing') {
          const parts = (transfers.get(frame.reqId) ?? '') + frame.data;
          if (parts.length < frame.total) { transfers.set(frame.reqId, parts); return; }
          transfers.delete(frame.reqId);
          const body = JSON.parse(Buffer.from(parts, 'base64').toString('utf8'));
          received.push({ t: 'editing', reqId: frame.reqId, body });
          await answer({ t: 'editing', reqId: frame.reqId, body });
        } else {
          received.push(frame);
          await answer(frame);
        }
      });
    }));

    const answer = async (frame) => {
      if (frame.t === 'hello') {
        if (mode === 'mismatch') { await send({ t: 'error', message: 'protocol mismatch: host speaks 4' }); return; }
        if (mode === 'session') { await send({ t: 'welcome', proto: 3, readOnly: false, recordCount: 0 }); return; }
        if (mode === 'lobby') { await send({ t: 'lobby', project: 'proj', sessions: [], omitted: 0 }); return; }
        const [hello] = fixture('hello').frames;
        writable = frame.writeToken === b64url(writeToken);
        if (!matches({ ...hello.frame, ...(writable ? {} : { writeToken: undefined }) },
          { ...frame, ...(writable ? {} : { writeToken: undefined }) })) return;
        for (const { from, frame: origin } of fixture('welcome').frames) {
          if (from === 'origin') {
            await send({ ...bind(origin, examples('welcome')), ...(kind ? { kind } : {}),
              readOnly: readOnly ?? !writable });
          }
        }
        return;
      }
      if (frame.t === 'editing') {
        if (mode === 'goodbye') {
          await leave(reason);
          return;
        }
        for (const name of EDITING_FIXTURES) {
          const exchange = fixture(name);
          const values = examples(name);
          const request = exchange.frames.find((f) => f.from === 'consumer').frame;
          if ((!exchange.tier || exchange.tier === (writable ? 'edit' : 'view'))
            && isDeepStrictEqual(bind(request, values).body, frame.body)) {
            for (const { from, frame: origin } of exchange.frames) {
              if (from === 'origin') await send({ ...bind(origin, values), reqId: frame.reqId });
            }
            return;
          }
        }
        const body = onEditing?.(frame.body, { writable })
          ?? { error: 'An artifact link does not permit that' };
        await send({ t: 'editing', reqId: frame.reqId, body });
      }
    };
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  const origin = `http://127.0.0.1:${server.address().port}`;
  return {
    origin,
    roomId,
    writeToken: b64url(writeToken),
    received,
    get connections() { return connections; },
    goodbye: async (why, { back = false } = {}) => {
      await Promise.all([...goodbyes].map((leave) => leave(why)));
      if (!back) mode = 'absent';
      await new Promise((resolve) => setTimeout(resolve, 20));
    },
    link: (tier = 'edit') => `${origin}/#${roomId}.${b64url(tier === 'edit'
      ? Buffer.concat([keyBytes, writeToken]) : tier === 'owner'
        ? Buffer.concat([keyBytes, writeToken, randomBytes(16)]) : keyBytes)}`,
    close: () => new Promise((resolve) => {
      for (const socket of sockets) socket.destroy();
      server.close(resolve);
    }),
  };
}
