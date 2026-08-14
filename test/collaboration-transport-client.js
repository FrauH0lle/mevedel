/* collaboration-transport-client.js -- raw loopback transport assertions */
'use strict';

const net = require('node:net');
const crypto = require('node:crypto');

const host = process.argv[2] || '127.0.0.1';
const port = Number(process.argv[3]);
if (!Number.isInteger(port)) throw new Error('port is required');

function connect() {
  return new Promise((resolve, reject) => {
    const socket = net.createConnection({host, port});
    socket.once('connect', () => resolve(socket));
    socket.once('error', reject);
  });
}

function readUntil(socket, marker) {
  return new Promise((resolve, reject) => {
    let data = Buffer.alloc(0);
    const onData = chunk => {
      data = Buffer.concat([data, chunk]);
      const end = data.indexOf(marker);
      if (end !== -1) {
        socket.off('data', onData);
        resolve({head: data.subarray(0, end + marker.length), rest: data.subarray(end + marker.length)});
      }
    };
    socket.on('data', onData);
    socket.once('error', reject);
  });
}

async function http(path) {
  const socket = await connect();
  socket.write(`GET ${path} HTTP/1.1\r\nHost: ${host}:${port}\r\nConnection: close\r\n\r\n`);
  const result = await readUntil(socket, Buffer.from('\r\n\r\n'));
  let body = result.rest;
  await new Promise(resolve => {
    socket.on('data', chunk => { body = Buffer.concat([body, chunk]); });
    socket.once('close', resolve);
    socket.end();
  });
  socket.destroy();
  const status = Number(result.head.toString().match(/^HTTP\/1\.1 (\d+)/)[1]);
  return {status, headers: result.head.toString(), body: body.toString()};
}

function maskedFrame(text, options = {}) {
  const payload = Buffer.isBuffer(text) ? text : Buffer.from(text, 'utf8');
  const fin = options.fin === false ? 0 : 0x80;
  const opcode = options.opcode === undefined ? 1 : options.opcode;
  const key = crypto.randomBytes(4);
  const header = payload.length < 126
    ? Buffer.from([fin | opcode, 0x80 | payload.length])
    : Buffer.from([fin | opcode, 0x80 | 126, payload.length >> 8, payload.length & 255]);
  const masked = Buffer.alloc(payload.length);
  for (let i = 0; i < payload.length; i += 1) masked[i] = payload[i] ^ key[i % 4];
  return Buffer.concat([header, key, masked]);
}

async function websocket(origin) {
  const socket = await connect();
  const key = crypto.randomBytes(16).toString('base64');
  socket.write(
    `GET /ws HTTP/1.1\r\nHost: ${host}:${port}\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: ${key}\r\nOrigin: ${origin}\r\n\r\n`,
  );
  const result = await readUntil(socket, Buffer.from('\r\n\r\n'));
  return {socket, result};
}

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

(async () => {
  const root = await http('/index.html');
  console.log(`viewer status ${root.status}: ${root.body.slice(0, 160)}`);
  assert(root.status === 200, 'viewer route did not return 200');
  assert(root.body.includes('id="transcript"'), 'packaged viewer was not served');
  assert(root.headers.includes('Content-Security-Policy'), 'viewer CSP missing');
  assert((await http('/viewer.css')).status === 200, 'CSS route failed');
  assert((await http('/viewer.js')).status === 200, 'JS route failed');
  assert((await http('/nope')).status === 404, 'unknown route was not rejected');

  const rejected = await websocket('http://evil.invalid');
  assert(rejected.result.head.toString().startsWith('HTTP/1.1 403'), 'wrong Origin was accepted');
  rejected.socket.destroy();

  const accepted = await websocket(`http://127.0.0.1:${port}`);
  assert(accepted.result.head.toString().startsWith('HTTP/1.1 101'), 'WebSocket handshake failed');
  accepted.socket.write(maskedFrame('ping'));
  const frame = await new Promise((resolve, reject) => {
    let data = accepted.result.rest;
    const onData = chunk => {
      data = Buffer.concat([data, chunk]);
      if (data.length >= 2 && data.length >= 2 + data[1]) {
        accepted.socket.off('data', onData);
        resolve(data.subarray(2, 2 + data[1]).toString());
      }
    };
    accepted.socket.on('data', onData);
    accepted.socket.once('error', reject);
  });
  assert(frame === 'pong', 'WebSocket frame exchange failed');
  accepted.socket.destroy();
  console.log(`transport spike passed on ${host}:${port}`);
})().catch(error => {
  console.error(error.stack || error);
  process.exitCode = 1;
});
