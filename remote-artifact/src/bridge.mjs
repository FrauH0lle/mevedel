// The machine-facing entry point mevedel drives: newline-delimited JSON on
// stdin and stdout, one request per line.  It is not meant for people; the
// attach/detach commands and the MCP server are.
//
// mevedel keeps its own record of attached links, so a request carries the
// link itself (over the pipe, never on a command line or in output):
//
//   in   {id, link, guestId, name, ref?, body?}
//   out  {id, welcome: {id, kind, title, readOnly}, reply?}  or  {id, error}
//   out  {ref, event}  for an origin's change event
//
// Without BODY a request only connects and answers the welcome (attaching,
// reachability).  With it, BODY is sent to the artifact room pinned to the
// room's own artifact id and REPLY is the origin's answer, {result} or
// {error, ...}.  NAME is the display name the origin attributes edits to;
// each distinct (link, name) has its own pooled connection, opened on first
// use and closed after IDLE-MS without a call.  REF is echoed on the change
// events that connection receives.
import { createInterface } from 'node:readline';
import { connectionPool } from './connections.mjs';
import { EditingAssembler } from './protocol.mjs';
import { openRoom } from './room.mjs';

/* Open ENTRY's room and report its change events to EMIT with ENTRY.ref. */
async function openWatched(entry, emit) {
  const room = await openRoom(entry.link, { name: entry.name, guestId: entry.guestId });
  const events = new EditingAssembler();
  room.onFrame = (frame) => {
    if (frame.t !== 'editing' || frame.reqId !== 'event') return;
    const event = events.add(frame);
    if (event !== undefined && entry.ref !== undefined) emit({ ref: entry.ref, event });
  };
  return room;
}

/**
 * Answer requests from INPUT on OUTPUT until INPUT ends, then close every
 * connection.  IDLE-MS overrides the five-minute idle close.
 */
export async function bridge({ input, output, idleMs }) {
  const write = (message) => output.write(`${JSON.stringify(message)}\n`);
  const pool = connectionPool({
    idleMs,
    open: (entry) => openWatched(entry, write),
    key: (entry) => JSON.stringify([entry.link, entry.name]),
  });
  const handle = async ({ id, link, guestId, name, ref, body }) => {
    const room = await pool.connect({ link, guestId, name, ref }, {});
    try {
      const { id: origin, kind, title, readOnly } = room.welcome;
      const welcome = { id: origin, kind, title, readOnly };
      if (body === undefined) return { id, welcome };
      return { id, welcome, reply: await room.request({ ...body, id: origin }) };
    } finally {
      room.close();
    }
  };
  const pending = new Set();
  for await (const line of createInterface({ input, crlfDelay: Infinity })) {
    if (!line.trim()) continue;
    let request;
    try {
      request = JSON.parse(line);
      if (typeof request !== 'object' || request === null) throw new Error('not an object');
    } catch {
      write({ id: null, error: 'Malformed request' });
      continue;
    }
    const work = handle(request).then(write, (error) => write({ id: request.id, error: error.message }));
    pending.add(work);
    work.finally(() => pending.delete(work));
  }
  await Promise.all(pending);
  pool.closeAll();
}
