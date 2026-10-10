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
            the prompt's marker) --json (machine output)
   Each writer waits for acknowledgement, then --interval; this is closed-loop
   latency, not a fixed arrival rate. One guest measures joining/acknowledgement
   only and yields no peer observations. Missing reliable deliveries fail. */
import { pathToFileURL } from 'node:url';
import * as Y from '../../shared-editing/node_modules/yjs/dist/yjs.mjs';
import { restore, putElement } from '../../shared-editing/model.mjs';

const args = process.argv.slice(2);
const opt = (name, fallback) => {
  const i = args.indexOf(`--${name}`);
  return i < 0 ? fallback : args[i + 1];
};
const [link, scenario] = args;
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

export class Guest {
  constructor(index, creds, timeout = 15000) {
    this.index = index;
    this.timeout = timeout;
    this.waiters = new Set();
    this.error = null;
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
    let welcomed = false;
    const ready = this.waitForFrame((frame) => {
      if (frame.t === 'welcome') welcomed = true;
      return welcomed && frame.t === 'snapshot-chunk' && frame.final === true;
    }, 'the final snapshot');
    this.ws.addEventListener('close', (event) => this.fail(new Error(`closed ${event.code}`)));
    this.ws.addEventListener('error', () => this.fail(new Error('websocket error')));
    this.ws.addEventListener('open', () => {
      this.send({ t: 'hello', proto: 3, name: `bot-${this.index}`, guestId: `bot-${this.index}-${process.pid}`,
                  writeToken: Buffer.from(this.creds.write).toString('base64url') });
    }, { once: true });
    this.on((frame, at) => this.editingFrame(frame, at));
    this.ws.addEventListener('message', (event) => {
      if (typeof event.data === 'string') return;
      const at = now();
      this.inbound = this.inbound.then(async () => {
        const bytes = new Uint8Array(event.data);
        const plain = await crypto.subtle.decrypt({ name: 'AES-GCM', iv: bytes.slice(4, 16) },
                                                  this.key, bytes.slice(16));
        const frame = JSON.parse(new TextDecoder().decode(plain));
        for (const handler of this.handlers) handler(frame, at);
      }).catch((error) => this.fail(error));
    });
    return ready;
  }
  waitForFrame(predicate, label) {
    return new Promise((resolve, reject) => {
      if (this.error) return reject(this.error);
      const finish = (error, value) => {
        clearTimeout(timer);
        off();
        this.waiters.delete(finish);
        error ? reject(error) : resolve(value);
      };
      const timer = setTimeout(() => finish(new Error(`Timed out waiting for ${label}`)), this.timeout);
      const off = this.on((frame, at) => { if (predicate(frame)) finish(null, { frame, at }); });
      this.waiters.add(finish);
    });
  }
  fail(error) {
    this.error ||= error;
    for (const reject of this.waiters) reject(this.error);
    for (const pending of this.pending.values()) pending.reject(this.error);
  }
  on(handler) {
    this.handlers.add(handler);
    return () => this.handlers.delete(handler);
  }
  send(frame) {
    const text = JSON.stringify(frame);
    this.outbound = this.outbound.then(async () => {
      if (this.error) throw this.error;
      const iv = crypto.getRandomValues(new Uint8Array(12));
      const sealed = new Uint8Array(await crypto.subtle.encrypt({ name: 'AES-GCM', iv }, this.key,
                                                                new TextEncoder().encode(text)));
      const envelope = new Uint8Array(16 + sealed.length);
      envelope.set(iv, 4);
      envelope.set(sealed, 16);
      this.ws.send(envelope);
    });
    this.outbound.catch((error) => this.fail(error));
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
      if (this.error) return reject(this.error);
      const timer = setTimeout(() => finish(new Error(`Timed out waiting for editing request ${reqId}`)), this.timeout);
      const finish = (error, value) => {
        clearTimeout(timer);
        this.pending.delete(reqId);
        error ? reject(error) : resolve(value);
      };
      this.pending.set(reqId, { resolve: value => finish(null, value), reject: finish });
      for (let offset = 0; offset < data.length; offset += 65536)
        this.send({ t: 'editing', reqId, offset, total: data.length, data: data.slice(offset, offset + 65536) });
    });
  }
  close() { this.fail(new Error('Guest closed')); this.ws?.close(); }
}

function stats(values) {
  if (!values.length) return { n: 0 };
  const s = [...values].sort((a, b) => a - b);
  const q = (p) => s[Math.min(s.length - 1, Math.floor(p * s.length))];
  return { n: s.length, p50: +q(0.5).toFixed(1), p95: +q(0.95).toFixed(1), max: +s.at(-1).toFixed(1),
           mean: +(s.reduce((a, b) => a + b, 0) / s.length).toFixed(1) };
}

export async function edit(bots) {
  let id = opt('item');
  if (!id) {
    id = crypto.randomUUID();
    await bots[0].request({ action: 'create', kind: 'whiteboard', id, opId: crypto.randomUUID(), title: `latency ${new Date().toISOString()}` });
  }
  const docs = await Promise.all(bots.map(async (bot) => {
    const result = await bot.request({ action: 'read', id });
    return restore(Buffer.from(result.crdt, 'base64'));
  }));
  // A reused board's old geometry must not count as delivery of a new edit.
  const elementIds = Array.from({ length: writers }, () => crypto.randomUUID());
  const sent = new Map(), ack = [], seen = [], seenBy = new Map();
  bots.forEach((bot, i) => {
    bot.onEvent = (value, at) => {
      if (value.id !== id || !value.update) return;
      Y.applyUpdate(docs[i], Buffer.from(value.update, 'base64'));
      for (let w = 0; w < writers; w++) {
        if (w === i) continue;
        const element = docs[i].getMap('elements').get(elementIds[w]);
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
      putElement(doc, { id: elementIds[w], type: 'rectangle', x: n, y: w * 120, width: 100, height: 80 });
      const update = Y.encodeStateAsUpdate(doc, before);
      const start = now();
      sent.set(`${w}:${n}`, start);
      await bot.request({ action: 'update', id, opId: crypto.randomUUID(), update: Buffer.from(update).toString('base64') });
      ack.push(now() - start);
      await sleep(interval);
    }
  }));
  const expectedSeen = writers * count * (guests - 1), deadline = now() + 15000;
  while (seen.length < expectedSeen && now() < deadline && bots.every(bot => !bot.error))
    await sleep(25);
  return { item: id, ack: stats(ack), seen: stats(seen), expectedSeen };
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
    const got = bots.map((bot, i) => bot.waitForFrame(
      frame => frame.t === 'record' && frame.record?.kind === 'user' &&
        frame.record.text?.includes(marker), `prompt ${n}`,
    ).then(({ at }) => { if (i) seen.push(at - start); }));
    bots[0].send({ t: 'prompt', name: 'bot-0', text: `${marker} ${opt('text', '')}`.trim() });
    await Promise.all(got);
    await sleep(interval);
  }
  return { seen: stats(seen), expectedSeen: count * (guests - 1) };
}

export function checkDelivery(result) {
  if (result.scenario !== 'presence' && result.seen.n !== result.expectedSeen)
    throw new Error(`Incomplete ${result.scenario} delivery: ${result.seen.n}/${result.expectedSeen} observations`);
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  let bots = [];
  try {
    const run = { edit, presence, prompt }[scenario];
    if (!link || !run) throw new Error('usage: loadbot.mjs LINK edit|presence|prompt [options]');
    if (!Number.isInteger(guests) || guests < 1 || !Number.isInteger(writers) || writers < 1 || writers > guests ||
        !Number.isInteger(count) || count < 1 || !Number.isFinite(interval) || interval < 0)
      throw new Error('Guests, writers and count must be positive integers; writers <= guests; interval >= 0');
    const creds = parseLink(link);
    bots = Array.from({ length: guests }, (_, i) => new Guest(i, creds));
    const t0 = now();
    await Promise.all(bots.map((bot) => bot.connect()));
    const joinMs = now() - t0;
    const result = { scenario, guests, writers, count, interval, joinMs: +joinMs.toFixed(1), ...(await run(bots)) };
    for (const bot of bots) if (bot.error) throw bot.error;
    checkDelivery(result);
    console.log(args.includes('--json') ? JSON.stringify(result) : result);
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  } finally {
    bots.forEach((bot) => bot.close());
  }
}
