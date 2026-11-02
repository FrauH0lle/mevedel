// One connection to an artifact room at the origin, through the relay.
//
// Opening a room sends hello and waits for the artifact welcome.  Any other
// welcome -- a session room's or a lobby's -- is refused and the connection
// closed at once, without sending another frame, so a wrong link never
// exposes more than the hello.
//
// An origin that ends the room says goodbye with a reason first; every
// pending and later request fails with what that reason means.
import { parseLink } from './link.mjs';
import { EditingAssembler, PROTOCOL, editingFrames, importKey, seal, unseal } from './protocol.mjs';

export const UNAVAILABLE = 'Origin unavailable (revoked, deleted, or offline)';

const GOODBYES = {
  revoked: 'Origin unavailable: the host revoked this artifact link; ask them for a new one',
  'artifact-deleted': 'Origin unavailable: the artifact was deleted at its origin',
  'origin-stopped': 'Origin unavailable: the origin Emacs stopped; try again once it is back',
};

/* What the origin's goodbye for REASON means to the user. */
export function goodbye(reason) {
  return Object.hasOwn(GOODBYES, reason) ? GOODBYES[reason] : UNAVAILABLE;
}

export class Room {
  #ws;
  #key;
  #outbound = Promise.resolve();
  #inbound = Promise.resolve();
  #assembler = new EditingAssembler();
  #pending = new Map();
  #nextId = 1;
  #closed = null;

  constructor(ws, key, link) {
    this.#ws = ws;
    this.#key = key;
    this.link = link;
    this.welcome = null;
  }

  /* True once the connection ended or was closed. */
  get closed() { return this.#closed !== null; }

  /* Why the connection ended, such as the origin's goodbye, or null. */
  get ended() { return this.#closed; }

  /* Send FRAME sealed, in order. */
  send(frame) {
    this.#outbound = this.#outbound.then(async () => {
      if (this.#ws.readyState === WebSocket.OPEN) this.#ws.send(await seal(this.#key, frame));
    });
    return this.#outbound;
  }

  /**
   * Send editing request BODY and resolve with the origin's reply,
   * { result } or { error }.  Reject when the connection ends first or
   * TIMEOUT-MS pass.
   */
  request(body, timeoutMs = 30000) {
    if (this.#closed) return Promise.reject(new Error(this.#closed));
    const reqId = this.#nextId++;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.#pending.delete(reqId);
        reject(new Error('The origin did not answer in time; try again'));
      }, timeoutMs);
      this.#pending.set(reqId, { resolve: (v) => { clearTimeout(timer); resolve(v); },
        reject: (e) => { clearTimeout(timer); reject(e); } });
      for (const frame of editingFrames(reqId, body)) this.send(frame);
    });
  }

  close() {
    this.end(UNAVAILABLE);
    try { this.#ws.close(); } catch { /* already closed */ }
  }

  /* Handle sealed ENVELOPE from the origin. */
  receive(envelope) {
    this.#inbound = this.#inbound.then(async () => {
      const frame = await unseal(this.#key, envelope);
      if (!frame) return;
      if (frame.t === 'editing' && frame.reqId !== 'event') {
        const body = this.#assembler.add(frame);
        const pending = body !== undefined && this.#pending.get(frame.reqId);
        if (pending) {
          this.#pending.delete(frame.reqId);
          pending.resolve(body);
        }
      }
      if (frame.t === 'bye') this.end(goodbye(frame.reason));
      this.onFrame?.(frame);
    });
  }

  /* Resolve once every frame received so far was handled. */
  drained() {
    return this.#inbound;
  }

  /* Fail every pending request, and every later one, with MESSAGE. */
  end(message) {
    if (this.#closed) return;
    this.#closed = message;
    for (const { reject } of this.#pending.values()) reject(new Error(message));
    this.#pending.clear();
  }
}

/**
 * Open the artifact room LINK names and resolve with a Room once its
 * artifact welcome arrived; ROOM.welcome holds it.  NAME is the display
 * name the origin shows and GUEST-ID a stable id of this attachment.
 */
export async function openRoom(link, { name, guestId, timeoutMs = 10000 } = {}) {
  const parsed = parseLink(link);
  const key = await importKey(parsed.key);
  const ws = new WebSocket(parsed.socketUrl);
  ws.binaryType = 'arraybuffer';
  const room = new Room(ws, key, parsed);
  return new Promise((resolve, reject) => {
    let settled = false;
    const fail = (message) => {
      room.end(message);
      if (!settled) {
        settled = true;
        clearTimeout(timer);
        try { ws.close(); } catch { /* already closed */ }
        reject(new Error(message));
      }
    };
    const timer = setTimeout(() => fail(UNAVAILABLE), timeoutMs);
    room.onFrame = (frame) => {
      if (settled) return;
      if (frame.t === 'error') {
        const version = /speaks (\d+)/.exec(frame.message ?? '');
        fail(version
          ? `origin speaks protocol ${version[1]}, this package speaks ${PROTOCOL}; update one`
          : `The origin refused the connection: ${frame.message}`);
      } else if (frame.t === 'welcome' && frame.room === 'artifact') {
        settled = true;
        clearTimeout(timer);
        room.welcome = frame;
        resolve(room);
      } else if (frame.t === 'welcome' || frame.t === 'lobby') {
        fail('This is a session or lobby link, not an artifact link. Share the artifact at its '
          + 'origin and attach the link that gives.');
      } else if (frame.t === 'bye') {
        fail(goodbye(frame.reason));
      }
    };
    ws.addEventListener('open', () => {
      room.send({ t: 'hello', proto: PROTOCOL, name, guestId,
        ...(parsed.writeToken ? { writeToken: Buffer.from(parsed.writeToken).toString('base64url') } : {}) });
    });
    ws.addEventListener('message', (event) => {
      // Unsealed text frames are relay controls such as room-closed.
      if (typeof event.data !== 'string') room.receive(event.data);
    });
    // A goodbye arrives just before the close; let it be read first.
    ws.addEventListener('close', () => room.drained().then(() => fail(UNAVAILABLE)));
    ws.addEventListener('error', () => room.drained().then(() => fail(UNAVAILABLE)));
  });
}
