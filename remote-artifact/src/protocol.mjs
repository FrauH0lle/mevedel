// The sealed relay protocol, consumer side.
//
// Every frame is JSON sealed with AES-256-GCM under the room key in an
// envelope [4-byte peer id][12-byte nonce][ciphertext || 16-byte tag]; a
// guest writes peer 0 and the relay fills in its own.  Editing requests and
// replies travel as chunked transfers of base64 UTF-8 JSON.

/** The protocol version this package speaks, checked by the origin at hello. */
export const PROTOCOL = 3;

const CHUNK = 65536;

export function importKey(bytes) {
  return crypto.subtle.importKey('raw', bytes, 'AES-GCM', false, ['encrypt', 'decrypt']);
}

export async function seal(key, frame) {
  const nonce = crypto.getRandomValues(new Uint8Array(12));
  const sealed = new Uint8Array(await crypto.subtle.encrypt({ name: 'AES-GCM', iv: nonce }, key,
    new TextEncoder().encode(JSON.stringify(frame))));
  const envelope = new Uint8Array(16 + sealed.length);
  envelope.set(nonce, 4);
  envelope.set(sealed, 16);
  return envelope;
}

/* The frame sealed in ENVELOPE, or null when it does not authenticate. */
export async function unseal(key, envelope) {
  try {
    const bytes = new Uint8Array(envelope);
    const plain = await crypto.subtle.decrypt({ name: 'AES-GCM', iv: bytes.subarray(4, 16) }, key,
      bytes.subarray(16));
    return JSON.parse(new TextDecoder().decode(plain));
  } catch {
    return null;
  }
}

/* The editing frames carrying BODY as request REQ-ID. */
export function editingFrames(reqId, body) {
  const data = Buffer.from(JSON.stringify(body)).toString('base64');
  const frames = [];
  for (let offset = 0; offset < data.length; offset += CHUNK) {
    frames.push({ t: 'editing', reqId, offset, total: data.length, data: data.slice(offset, offset + CHUNK) });
  }
  return frames;
}

/* Reassembles chunked editing transfers; `add' returns a complete body or undefined. */
export class EditingAssembler {
  #transfers = new Map();

  add(frame) {
    const parts = frame.offset === 0 ? '' : this.#transfers.get(frame.reqId);
    if (parts === undefined || parts.length !== frame.offset) {
      this.#transfers.delete(frame.reqId);
      return undefined;
    }
    const joined = parts + frame.data;
    if (joined.length < frame.total) {
      this.#transfers.set(frame.reqId, joined);
      return undefined;
    }
    this.#transfers.delete(frame.reqId);
    return JSON.parse(Buffer.from(joined, 'base64').toString('utf8'));
  }
}
