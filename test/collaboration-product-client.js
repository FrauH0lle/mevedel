/* collaboration-product-client.js -- product loopback assertions */
'use strict';

const net = require('node:net');
const crypto = require('node:crypto');
const fs = require('node:fs');

const host = process.argv[2];
const port = Number(process.argv[3]);
const room = process.argv[4];
const token = process.argv[5];
const readyFile = process.env.MEVEDEL_SPIKE_READY;
const burstFile = process.env.MEVEDEL_SPIKE_BURST;
const lifecycleFile = process.env.MEVEDEL_SPIKE_LIFECYCLE;
const settleFile = process.env.MEVEDEL_SPIKE_SETTLE;
const mutationFile = process.env.MEVEDEL_SPIKE_MUTATION;
const mutationStateFile = process.env.MEVEDEL_SPIKE_MUTATION_STATE;
if (!host || !Number.isInteger(port) || !room || !token) {
  throw new Error('host, port, room, and token are required');
}

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

function assertNoBearer(frame, label) {
  assert(!frame.payload.toString('utf8').includes(token),
    `${label} leaked the room bearer token`);
}

function connect() {
  return new Promise((resolve, reject) => {
    const socket = net.createConnection({host, port});
    const fail = error => { socket.destroy(); reject(error); };
    socket.once('connect', () => { socket.off('error', fail); resolve(socket); });
    socket.once('error', fail);
  });
}

function readHttp(socket) {
  return new Promise((resolve, reject) => {
    let data = Buffer.alloc(0);
    const onData = chunk => {
      data = Buffer.concat([data, chunk]);
      const end = data.indexOf(Buffer.from('\r\n\r\n'));
      if (end !== -1) {
        socket.off('data', onData);
        resolve({
          head: data.subarray(0, end + 4).toString(),
          rest: data.subarray(end + 4),
        });
      }
    };
    socket.on('data', onData);
    socket.once('error', reject);
  });
}

async function http(path) {
  const socket = await connect();
  socket.write(`GET ${path} HTTP/1.1\r\nHost: ${host}:${port}\r\nConnection: close\r\n\r\n`);
  const result = await readHttp(socket);
  let body = result.rest;
  const lengthMatch = result.head.match(/^Content-Length:\s*(\d+)/im);
  const expectedLength = lengthMatch ? Number(lengthMatch[1]) : null;
  await new Promise(resolve => {
    let settled = false;
    const finish = () => { if (!settled) { settled = true; resolve(); } };
    socket.on('data', chunk => {
      body = Buffer.concat([body, chunk]);
      if (expectedLength !== null && body.length >= expectedLength) finish();
    });
    socket.once('close', resolve);
    if (expectedLength !== null && body.length >= expectedLength) finish();
    socket.end();
  });
  socket.destroy();
  return {head: result.head, body: body.toString()};
}

async function rawHttp(path, headers = '') {
  const socket = await connect();
  socket.write(`GET ${path} HTTP/1.1\r\nHost: ${host}:${port}\r\n` +
    `${headers}Connection: close\r\n\r\n`);
  const result = await readHttp(socket);
  socket.destroy();
  return result;
}

async function preUpgradeSlowloris(label, write, timeoutMs) {
  const socket = await connect();
  const started = Date.now();
  await write(socket);
  try { await waitClose(socket, timeoutMs); }
  catch (error) { throw new Error(`${label}: ${error.message}`); }
  return Date.now() - started;
}

function maskedFrame(value, options = {}) {
  const payload = Buffer.isBuffer(value) ? value : Buffer.from(value, 'utf8');
  const fin = options.fin === false ? 0 : 0x80;
  const opcode = options.opcode === undefined ? 1 : options.opcode;
  const key = crypto.randomBytes(4);
  let header;
  if (payload.length < 126) {
    header = Buffer.from([fin | opcode, 0x80 | payload.length]);
  } else if (payload.length < 65536) {
    header = Buffer.from([fin | opcode, 0x80 | 126,
      payload.length >> 8, payload.length & 255]);
  } else {
    throw new Error('test payload too large');
  }
  const masked = Buffer.alloc(payload.length);
  for (let i = 0; i < payload.length; i += 1) masked[i] = payload[i] ^ key[i % 4];
  return Buffer.concat([header, key, masked]);
}

function parseFrames(buffer) {
  const frames = [];
  let offset = 0;
  while (buffer.length - offset >= 2) {
    const first = buffer[offset];
    const second = buffer[offset + 1];
    const masked = (second & 0x80) !== 0;
    let length = second & 0x7f;
    let header = 2;
    if (length === 126) {
      if (buffer.length - offset < 4) break;
      length = buffer.readUInt16BE(offset + 2);
      header = 4;
    } else if (length === 127) {
      throw new Error('test does not need 64-bit frames');
    }
    if (masked) header += 4;
    if (buffer.length - offset < header + length) break;
    let payload = buffer.subarray(offset + header, offset + header + length);
    if (masked) {
      const keyStart = offset + header - 4;
      const key = buffer.subarray(keyStart, keyStart + 4);
      payload = Buffer.from(payload);
      for (let i = 0; i < payload.length; i += 1) payload[i] ^= key[i % 4];
    }
    frames.push({fin: (first & 0x80) !== 0, opcode: first & 0x0f, payload});
    offset += header + length;
  }
  return {frames, rest: buffer.subarray(offset)};
}

class FrameReader {
  constructor(socket, initial = Buffer.alloc(0)) {
    this.socket = socket;
    this.buffer = initial;
    this.frames = [];
    this.waiters = [];
    this.closed = false;
    this.onData = chunk => this.push(chunk);
    socket.on('data', this.onData);
    socket.once('close', () => {
      this.closed = true;
      for (const waiter of this.waiters.splice(0)) waiter.reject(new Error('socket closed'));
    });
    socket.once('error', error => {
      this.closed = true;
      for (const waiter of this.waiters.splice(0)) waiter.reject(error);
    });
    this.push(Buffer.alloc(0));
  }

  stop() {
    this.socket.off('data', this.onData);
  }

  push(chunk) {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    const parsed = parseFrames(this.buffer);
    this.buffer = parsed.rest;
    for (const frame of parsed.frames) {
      const waiter = this.waiters.shift();
      if (waiter) waiter.resolve(frame);
      else this.frames.push(frame);
    }
  }

  next(timeoutMs = 2000) {
    if (this.frames.length) return Promise.resolve(this.frames.shift());
    if (this.closed) return Promise.reject(new Error('socket closed'));
    let waiter;
    const promise = new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        const index = this.waiters.indexOf(waiter);
        if (index !== -1) this.waiters.splice(index, 1);
        reject(new Error('timed out waiting for WebSocket frame'));
      }, timeoutMs);
      waiter = {
        resolve: frame => { clearTimeout(timer); resolve(frame); },
        reject: error => { clearTimeout(timer); reject(error); },
      };
      this.waiters.push(waiter);
    });
    return promise;
  }
}

async function websocket(origin) {
  const socket = await connect();
  const key = crypto.randomBytes(16).toString('base64');
  socket.write(
    `GET /ws HTTP/1.1\r\nHost: ${host}:${port}\r\nUpgrade: websocket\r\n` +
    `Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n` +
    `Sec-WebSocket-Key: ${key}\r\nOrigin: ${origin}\r\n\r\n`,
  );
  return {socket, result: await readHttp(socket)};
}

async function websocket101(origin) {
  for (let attempt = 0; attempt < 40; attempt += 1) {
    const connection = await websocket(origin);
    if (connection.result.head.startsWith('HTTP/1.1 101')) return connection;
    const retry = connection.result.head.startsWith('HTTP/1.1 409');
    connection.socket.destroy();
    if (!retry) throw new Error(`WebSocket admission failed: ${connection.result.head}`);
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error('WebSocket guest slot did not become available');
}

function waitClose(socket, timeoutMs = 2000) {
  return new Promise((resolve, reject) => {
    if (socket.destroyed) {
      resolve();
      return;
    }
    const timer = setTimeout(() => {
      socket.destroy();
      reject(new Error('socket did not close'));
    }, timeoutMs);
    socket.once('close', event => { clearTimeout(timer); resolve(event); });
    socket.once('error', error => { clearTimeout(timer); resolve(error); });
  });
}

async function authenticate(connection, authToken = token) {
  assert(connection.result.head.startsWith('HTTP/1.1 101'), 'WebSocket handshake failed');
  const reader = new FrameReader(connection.socket, connection.result.rest);
  connection.socket.write(maskedFrame(JSON.stringify({
    type: 'auth', version: 1, room, token: authToken,
  })));
  return {socket: connection.socket, reader};
}

async function rejectAuthPayload(payload) {
  const connection = await websocket101(`http://${host}:${port}`);
  const reader = new FrameReader(connection.socket, connection.result.rest);
  connection.socket.write(maskedFrame(payload));
  await waitClose(connection.socket);
  return reader;
}

async function readSnapshot(reader) {
  let begin = false;
  let end = false;
  let text = null;
  while (!end) {
    const frame = await reader.next();
    assert(frame.opcode === 1 && frame.fin, 'snapshot was not one FIN text frame');
    assertNoBearer(frame, 'snapshot frame');
    const message = JSON.parse(frame.payload.toString('utf8'));
    if (Number.isInteger(message.seq)) {
      assert(typeof message['ack-token'] === 'string' && message['ack-token'].length >= 32,
        'sequenced output did not carry an unpredictable acknowledgement token');
      reader.socket.write(maskedFrame(JSON.stringify({
        type: 'ack', seq: message.seq, 'ack-token': message['ack-token'],
      })));
    }
    if (message.type === 'snapshot-begin') begin = true;
    if (message.type === 'snapshot-chunk') {
      for (const record of message.records || []) if (record.text) text = record.text;
    }
    if (message.type === 'snapshot-end') end = true;
  }
  assert(begin && end, 'snapshot markers missing');
  assert(text && text.includes('Grüße from the loopback product spike'),
    'UTF-8 snapshot text was corrupted');
}

async function readRecord(reader, predicate, label) {
  for (;;) {
    let frame;
    try {
      frame = await reader.next(5000);
    } catch (error) {
      throw new Error(`${label}: ${error.message}`);
    }
    assert(frame.opcode === 1 && frame.fin, `${label} was not a text frame`);
    assertNoBearer(frame, label);
    const message = JSON.parse(frame.payload.toString('utf8'));
    if (Number.isInteger(message.seq)) {
      assert(typeof message['ack-token'] === 'string',
        `${label} lacked an acknowledgement token`);
      reader.socket.write(maskedFrame(JSON.stringify({
        type: 'ack', seq: message.seq, 'ack-token': message['ack-token'],
      })));
    }
    if (predicate(message)) return message;
  }
}

async function expectStopped() {
  try {
    await websocket(`http://${host}:${port}`);
  } catch (_) {
    return;
  }
  throw new Error('stale collaboration link remained reachable after stop');
}

(async () => {
  const silentMs = await preUpgradeSlowloris('silent', () => {}, 4000);
  assert(silentMs < 3500,
    `pre-upgrade idle bound was not enforced (${silentMs}ms)`);
  const dripIdleMs = await preUpgradeSlowloris('drip-idle', async socket => {
    socket.write('GET / HTTP/1.1\r\n');
    await new Promise(resolve => setTimeout(resolve, 2500));
  }, 5000);
  assert(dripIdleMs < 4500,
    `pre-upgrade drip idle bound was not enforced (${dripIdleMs}ms)`);
  const dripTotalMs = await preUpgradeSlowloris('drip-total', async socket => {
    for (let i = 0; i < 20 && !socket.destroyed; i += 1) {
      socket.write('X-Drip: x\r\n');
      await new Promise(resolve => setTimeout(resolve, 700));
    }
  }, 13000);
  assert(dripTotalMs > 8500 && dripTotalMs < 12000,
    `pre-upgrade total bound was not enforced (${dripTotalMs}ms)`);
  const byteBoundMs = await preUpgradeSlowloris('bytes', socket => {
    socket.write(`GET / HTTP/1.1\r\nHost: ${host}:${port}\r\n` +
      `X-Fill: ${'x'.repeat(70 * 1024)}`);
  }, 3000);
  assert(byteBoundMs < 2500,
    `pre-upgrade header byte bound was not enforced (${byteBoundMs}ms)`);
  for (let attempt = 0; attempt < 3; attempt += 1) {
    await preUpgradeSlowloris('repeated-bytes', socket => {
      socket.write(`GET / HTTP/1.1\r\nHost: ${host}:${port}\r\n` +
        `X-Fill: ${'x'.repeat(70 * 1024)}`);
    }, 3000);
  }
  const root = await http('/');
  assert(root.head.startsWith('HTTP/1.1 200'), 'viewer route failed');
  assert(root.body.includes('id="transcript"'), 'packaged viewer missing');
  assert(root.head.includes('Content-Security-Policy'), 'viewer CSP missing');
  assert((await http('/viewer.css')).head.startsWith('HTTP/1.1 200'), 'CSS route failed');
  assert((await http('/viewer.js')).head.startsWith('HTTP/1.1 200'), 'JS route failed');
  assert((await http('/not-found')).head.startsWith('HTTP/1.1 404'), 'unknown route accepted');
  const malformedUpgrade = await rawHttp('/ws', `Origin: http://${host}:${port}\r\n`);
  assert(malformedUpgrade.head.startsWith('HTTP/1.1 400'),
    'malformed WebSocket upgrade was not bounded and rejected');

  const wrongOrigin = await websocket('http://evil.invalid');
  assert(wrongOrigin.result.head.startsWith('HTTP/1.1 403'), 'wrong Origin accepted');
  wrongOrigin.socket.destroy();

  const wrongAuth = await authenticate(await websocket101(`http://${host}:${port}`), 'wrong');
  await waitClose(wrongAuth.socket);
  await rejectAuthPayload('not-json');
  await rejectAuthPayload(JSON.stringify({type: 'auth', version: 2, room, token}));
  await rejectAuthPayload(JSON.stringify({type: 'auth', version: 1, room}));

  const pending = [];
  for (let i = 0; i < 5; i += 1) {
    const connection = await websocket(`http://${host}:${port}`);
    assert(connection.result.head.startsWith('HTTP/1.1 101'),
      'unauthenticated handshake was rejected');
    pending.push(connection);
  }

  // The newest pending handshake can authenticate successfully even though
  // the older unauthenticated sockets were repeatedly admitted and evicted.
  const live = await authenticate(pending.pop());
  await readSnapshot(live.reader);
  for (const connection of pending) await waitClose(connection.socket);
  const second = await websocket(`http://${host}:${port}`);
  assert(second.result.head.startsWith('HTTP/1.1 409'), 'second viewer was admitted');
  second.socket.destroy();
  live.socket.write(maskedFrame('heartbeat', {opcode: 9}));
  const pong = await live.reader.next();
  assert(pong.opcode === 10 && pong.payload.toString('utf8') === 'heartbeat',
    'WebSocket ping did not receive a matching pong');
  assert(lifecycleFile, 'product harness lifecycle path missing');
  fs.writeFileSync(lifecycleFile, 'lifecycle-ready\n', {encoding: 'utf8', mode: 0o600});
  await readRecord(live.reader,
    message => message.type === 'record' && message.record
      && message.record.kind === 'user'
      && message.record.text.includes('Ordinary prompt'),
    'ordinary prompt update');
  await readRecord(live.reader,
    message => message.type === 'record' && message.record
      && message.record.kind === 'tool' && message.record.status === 'running',
    'tool start update');
  assert(settleFile, 'product harness settlement path missing');
  fs.writeFileSync(settleFile, 'settle-ready\n', {encoding: 'utf8', mode: 0o600});
  const settled = await readRecord(live.reader,
    message => message.type === 'record' && message.record
      && message.record.kind === 'tool' && message.record.status === 'completed',
    'tool completion update');
  assert(settled.record.result === 'tool settled', 'tool result was not settled');
  // The host is read-only: any later inbound frame closes this guest.
  live.socket.write(maskedFrame(JSON.stringify({type: 'mutate'})));
  await waitClose(live.socket);
  assert(mutationFile && mutationStateFile, 'mutation state paths missing');
  fs.writeFileSync(mutationFile, 'mutation-attempted\n', {encoding: 'utf8', mode: 0o600});
  for (let attempt = 0; attempt < 100; attempt += 1) {
    if (fs.existsSync(mutationStateFile)) break;
    await new Promise(resolve => setTimeout(resolve, 20));
  }
  assert(fs.existsSync(mutationStateFile), 'host did not report mutation state');
  assert(fs.readFileSync(mutationStateFile, 'utf8') === 'unchanged\n',
    'inbound mutation changed host data-buffer state');

  const reconnect = await authenticate(await websocket101(`http://${host}:${port}`));
  await readSnapshot(reconnect.reader);
  reconnect.socket.destroy();

  const forgedWrong = await authenticate(await websocket101(`http://${host}:${port}`));
  // The attacker knows the monotonic first sequence but does not read the
  // output and cannot predict its per-frame token.
  await new Promise(resolve => setTimeout(resolve, 100));
  forgedWrong.socket.write(maskedFrame(JSON.stringify({
    type: 'ack', seq: 1, 'ack-token': 'forged-token',
  })));
  await waitClose(forgedWrong.socket);

  const forgedMissing = await authenticate(await websocket101(`http://${host}:${port}`));
  await new Promise(resolve => setTimeout(resolve, 100));
  forgedMissing.socket.write(maskedFrame(JSON.stringify({
    type: 'ack', seq: 1,
  })));
  await waitClose(forgedMissing.socket);

  const slow = await authenticate(await websocket101(`http://${host}:${port}`));
  await readSnapshot(slow.reader);
  // Leave the real socket open but stop consuming data.  The host must pace
  // and bound its application queue, then close this guest without blocking.
  slow.reader.stop();
  assert(burstFile, 'product harness burst path missing');
  fs.writeFileSync(burstFile, 'burst-ready\n', {encoding: 'utf8', mode: 0o600});
  await waitClose(slow.socket, 5000);
  const slowReconnect = await authenticate(await websocket101(`http://${host}:${port}`));
  await readSnapshot(slowReconnect.reader);
  slowReconnect.socket.destroy();

  const slowloris = await authenticate(await websocket101(`http://${host}:${port}`));
  await readSnapshot(slowloris.reader);
  slowloris.reader.stop();
  // A masked text frame with only one of its four mask bytes received must
  // be closed by the authenticated incomplete-frame idle deadline.
  slowloris.socket.write(Buffer.from([0x81, 0x81, 0x42]));
  await waitClose(slowloris.socket, 7000);

  const ended = await authenticate(await websocket101(`http://${host}:${port}`));
  await readSnapshot(ended.reader);
  assert(readyFile, 'product harness ready path missing');
  fs.writeFileSync(readyFile, 'snapshot-ready\n', {encoding: 'utf8', mode: 0o600});
  let sawEnded = false;
  while (!sawEnded) {
    const frame = await ended.reader.next(5000);
    assertNoBearer(frame, 'ended frame');
    const message = JSON.parse(frame.payload.toString('utf8'));
    if (message.type === 'status' && message.status === 'ended') sawEnded = true;
  }
  assert(sawEnded, 'final ended status was not delivered');
  await waitClose(ended.socket);
  await expectStopped();
  console.log(`product collaboration loopback passed on ${host}:${port}`);
})().catch(error => {
  console.error(error.stack || error);
  process.exitCode = 1;
});
