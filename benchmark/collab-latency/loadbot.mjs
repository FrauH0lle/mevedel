#!/usr/bin/env node
/* loadbot.mjs -- headless guests that measure mevedel collaboration latency.

   Every simulated guest speaks the viewer's sealed protocol (protocol 3)
   through the real relay, so one process measures send -> seen on a single
   clock.  Run `npm ci --prefix shared-editing` first: edits use the
   editor's own Yjs model.

     node benchmark/collab-latency/loadbot.mjs LINK SCENARIO [options]

   LINK is a session room's full link.  SCENARIO is one of
     edit      writers add/move one rectangle each on a fresh whiteboard
     presence  writers stream cursor positions over editing presence
     prompt    one guest sends a chat prompt, all wait for its record
   Options: --guests N (4) --writers N (1) --count N (40) --interval MS (300)
            --item ID (reuse an existing whiteboard) --text PROMPT (appended to
            the prompt's marker) --json (machine output) */
import * as Y from '../../shared-editing/node_modules/yjs/dist/yjs.mjs';
import { restore, putElement } from '../../shared-editing/model.mjs';

const args = process.argv.slice(2);
const opt = (name, fallback) => {
  const i = args.indexOf(`--${name}`);
  return i < 0 ? fallback : args[i + 1];
};
const [link, scenario] = args;
if (!link || !scenario) {
  console.error('usage: loadbot.mjs LINK edit|presence|prompt [--guests N] [--writers N] [--count N] [--interval MS] [--item ID] [--json]');
  process.exit(2);
}
const guests = Number(opt('guests', 4)), writers = Number(opt('writers', 1));
const count = Number(opt('count', 40)), interval = Number(opt('interval', 300));
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const now = () => performance.now();

/* Link: https://host/#<roomId>.<base64url secret>, tiers as in transport.js. */
function parseLink(text) {
  const url = new URL(text);
  const [roomId, secret] = url.hash.slice(1).split('.');
  const raw = Buffer.from(secret, 'base64url');
  if (raw.length < 48) throw new Error('Need a full or owner link');
  url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
  url.hash = '';
  url.pathname = `/r/${roomId}`;
  url.search = '?role=guest';
  return { ws: url.toString(), key: raw.subarray(0, 32), write: raw.subarray(32, 48),
           owner: raw.length >= 64 ? raw.subarray(48, 64) : null };
}

class Guest {
  constructor(index, creds) {
    this.index = index;
    this.creds = creds;
    this.handlers = new Set();
    this.pending = new Map();
    this.transfers = new Map();
    this.seq = 0;
    this.outbound = Promise.resolve();
    this.inbound = Promise.resolve();
  }
  async connect() {
    this.key = await crypto.subtle.importKey('raw', this.creds.key, 'AES-GCM', false, ['encrypt', 'decrypt']);
    this.ws = new WebSocket(this.creds.ws);
    this.ws.binaryType = 'arraybuffer';
    const welcome = new Promise((resolve, reject) => {
      const off = this.on((f) => { if (f.t === 'welcome') { off(); resolve(f); } });
      this.ws.addEventListener('close', (e) => reject(new Error(`closed ${e.code}`)), { once: true });
    });
    this.ws.addEventListener('message', (event) => {
      if (typeof event.data === 'string') return;
      const at = now();
      this.inbound = this.inbound.then(async () => {
        const bytes = new Uint8Array(event.data);
        const plain = await crypto.subtle.decrypt({ name: 'AES-GCM', iv: bytes.slice(4, 16) },
                                                  this.key, bytes.slice(16));
        const frame = JSON.parse(new TextDecoder().decode(plain));
        for (const handler of this.handlers) handler(frame, at);
      }).catch((error) => console.error(`guest ${this.index}:`, error.message));
    });
    await new Promise((resolve, reject) => {
      this.ws.addEventListener('open', resolve, { once: true });
      this.ws.addEventListener('error', () => reject(new Error('websocket error')), { once: true });
    });
    this.send({ t: 'hello', proto: 3, name: `bot-${this.index}`, guestId: `bot-${this.index}-${process.pid}`,
                writeToken: Buffer.from(this.creds.write).toString('base64url') });
    this.on((frame, at) => this.editingFrame(frame, at));
    return welcome;
  }
  on(handler) {
    this.handlers.add(handler);
    return () => this.handlers.delete(handler);
  }
  send(frame) {
    const text = JSON.stringify(frame);
    this.outbound = this.outbound.then(async () => {
      const iv = crypto.getRandomValues(new Uint8Array(12));
      const sealed = new Uint8Array(await crypto.subtle.encrypt({ name: 'AES-GCM', iv }, this.key,
                                                                new TextEncoder().encode(text)));
      const envelope = new Uint8Array(16 + sealed.length);
      envelope.set(iv, 4);
      envelope.set(sealed, 16);
      this.ws.send(envelope);
    });
    return this.outbound;
  }
  /* Editing replies and events arrive as chunked base64 JSON. */
  editingFrame(frame, at) {
    if (frame.t !== 'editing') return;
    let t = this.transfers.get(frame.reqId);
    if (frame.offset === 0) this.transfers.set(frame.reqId, (t = { parts: [], offset: 0 }));
    if (!t) return;
    t.parts.push(frame.data);
    t.offset += frame.data.length;
    if (t.offset !== frame.total) return;
    this.transfers.delete(frame.reqId);
    const value = JSON.parse(Buffer.from(t.parts.join(''), 'base64').toString('utf8'));
    if (frame.reqId === 'event') this.onEvent?.(value, at);
    else {
      const p = this.pending.get(frame.reqId);
      this.pending.delete(frame.reqId);
      if (value.error) p?.reject(new Error(value.error));
      else p?.resolve(value.result);
    }
  }
  request(args) {
    const reqId = ++this.seq;
    const data = Buffer.from(JSON.stringify(args)).toString('base64');
    return new Promise((resolve, reject) => {
      this.pending.set(reqId, { resolve, reject });
      for (let offset = 0; offset < data.length; offset += 65536)
        this.send({ t: 'editing', reqId, offset, total: data.length, data: data.slice(offset, offset + 65536) });
    });
  }
  close() { this.ws.close(); }
}

function stats(values) {
  if (!values.length) return { n: 0 };
  const s = [...values].sort((a, b) => a - b);
  const q = (p) => s[Math.min(s.length - 1, Math.floor(p * s.length))];
  return { n: s.length, p50: +q(0.5).toFixed(1), p95: +q(0.95).toFixed(1), max: +s.at(-1).toFixed(1),
           mean: +(s.reduce((a, b) => a + b, 0) / s.length).toFixed(1) };
}

async function edit(bots) {
  let id = opt('item');
  if (!id) {
    id = crypto.randomUUID();
    await bots[0].request({ action: 'create', kind: 'whiteboard', id, opId: crypto.randomUUID(), title: `latency ${new Date().toISOString()}` });
  }
  const docs = await Promise.all(bots.map(async (bot) => {
    const result = await bot.request({ action: 'read', id });
    return restore(Buffer.from(result.crdt, 'base64'));
  }));
  const sent = new Map(), ack = [], seen = [], seenBy = new Map();
  bots.forEach((bot, i) => {
    bot.onEvent = (value, at) => {
      if (value.id !== id || !value.update) return;
      Y.applyUpdate(docs[i], Buffer.from(value.update, 'base64'));
      for (let w = 0; w < writers; w++) {
        if (w === i) continue;
        const element = docs[i].getMap('elements').get(`bot${w}rect`);
        const x = element?.get('geometry')?.x;
        const key = `${w}:${x}`, start = sent.get(key);
        if (start === undefined || seenBy.has(`${key}:${i}`)) continue;
        seenBy.set(`${key}:${i}`, true);
        seen.push(at - start);
      }
    };
  });
  await Promise.all(bots.slice(0, writers).map(async (bot, w) => {
    for (let n = 1; n <= count; n++) {
      const doc = docs[w], before = Y.encodeStateVector(doc);
      putElement(doc, { id: `bot${w}rect`, type: 'rectangle', x: n, y: w * 120, width: 100, height: 80 });
      const update = Y.encodeStateAsUpdate(doc, before);
      const start = now();
      sent.set(`${w}:${n}`, start);
      await bot.request({ action: 'update', id, opId: crypto.randomUUID(), update: Buffer.from(update).toString('base64') });
      ack.push(now() - start);
      await sleep(interval);
    }
  }));
  await sleep(2000);
  return { item: id, ack: stats(ack), seen: stats(seen), expectedSeen: writers * count * (guests - 1) };
}

async function presence(bots) {
  const id = opt('item');
  if (!id) throw new Error('presence needs --item ID of an existing whiteboard');
  await Promise.all(bots.map((bot) => bot.request({ action: 'read', id })));
  const sent = new Map(), seen = [];
  bots.forEach((bot) => bot.on((frame, at) => {
    if (frame.t !== 'editing-presence' || frame.id !== id) return;
    // Cursor presence forwards only the point: x is the sequence, y the writer.
    const start = sent.get(`${frame.point?.[1]}:${frame.point?.[0]}`);
    if (start !== undefined) seen.push(at - start);
  }));
  await Promise.all(bots.slice(0, writers).map(async (bot, w) => {
    for (let n = 1; n <= count; n++) {
      sent.set(`${w}:${n}`, now());
      bot.send({ t: 'editing-presence', id, mode: 'cursor', point: [n, w] });
      await sleep(Math.max(interval, 50));
    }
  }));
  await sleep(1000);
  return { seen: stats(seen), expectedSeen: writers * count * (guests - 1) };
}

async function prompt(bots) {
  const seen = [];
  for (let n = 1; n <= count; n++) {
    const marker = `latency-probe-${process.pid}-${n}`;
    const start = now();
    const got = bots.map((bot, i) => new Promise((resolve) => {
      const off = bot.on((frame, at) => {
        if (frame.t === 'record' && JSON.stringify(frame.record).includes(marker)) {
          off();
          if (i) seen.push(at - start);
          resolve();
        }
      });
    }));
    bots[0].send({ t: 'prompt', name: 'bot-0', text: `${marker} ${opt('text', '')}`.trim() });
    await Promise.race([Promise.all(got), sleep(15000)]);
    await sleep(interval);
  }
  return { seen: stats(seen), expectedSeen: count * (guests - 1) };
}

const creds = parseLink(link);
const bots = Array.from({ length: guests }, (_, i) => new Guest(i, creds));
const t0 = now();
await Promise.all(bots.map((bot) => bot.connect()));
const joinMs = now() - t0;
const run = { edit, presence, prompt }[scenario];
if (!run) throw new Error(`unknown scenario ${scenario}`);
const result = { scenario, guests, writers, count, interval, joinMs: +joinMs.toFixed(1), ...(await run(bots)) };
bots.forEach((bot) => bot.close());
console.log(args.includes('--json') ? JSON.stringify(result) : result);
process.exit(0);
