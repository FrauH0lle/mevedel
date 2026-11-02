// Room connections for the MCP server: opened on first use, shared by calls,
// closed after an idle period, reopened on demand.
//
// A tool asks ctx.connect for a room and calls close() on it when done.  With
// the pool, close() only releases: the connection stays open until IDLE-MS pass
// with no call using it, so an open client does not keep the origin awake.
import { displayName } from './identity.mjs';
import { UNAVAILABLE, openRoom } from './room.mjs';

export const IDLE_MS = 5 * 60 * 1000;

/* A fresh, unpooled connection to ENTRY's room. */
export function openEntry(entry, ctx) {
  return openRoom(entry.link, { name: displayName(entry, ctx.env, ctx.client), guestId: entry.guestId });
}

/**
 * Return { connect, closeAll }.  CONNECT(entry, ctx) resolves with a handle
 * on the pooled room of entry.link; OPEN opens one (a test seam).  KEY
 * chooses which entries share a connection, by default those of one link.
 */
export function connectionPool({ open = openEntry, idleMs = IDLE_MS, key = (entry) => entry.link } = {}) {
  /* key -> { opening, room, users, timer } */
  const slots = new Map();
  /* key -> the goodbye its last room ended with, until a reconnect works */
  const goodbyes = new Map();

  const drop = (link, slot) => {
    clearTimeout(slot.timer);
    if (slots.get(link) === slot) slots.delete(link);
    slot.opening.then((room) => room.close(), () => {});
  };
  const arm = (link, slot) => {
    clearTimeout(slot.timer);
    if (slot.users > 0) return;
    slot.timer = setTimeout(() => drop(link, slot), idleMs);
    slot.timer.unref?.();
  };

  async function connect(entry, ctx) {
    const link = key(entry);
    let slot = slots.get(link);
    if (slot?.room?.closed) {
      if (slot.room.ended && slot.room.ended !== UNAVAILABLE) goodbyes.set(link, slot.room.ended);
      drop(link, slot);
      slot = undefined;
    }
    if (!slot) {
      slot = { users: 0, timer: undefined, room: undefined };
      const made = slot;
      // A room that said goodbye has no host now; reconnecting can only say
      // so generically, so report the goodbye instead.
      made.opening = Promise.resolve(open(entry, ctx)).then((room) => {
        goodbyes.delete(link);
        return (made.room = room);
      }, (error) => {
        throw goodbyes.has(link) && error.message === UNAVAILABLE ? new Error(goodbyes.get(link)) : error;
      });
      slots.set(link, made);
      made.opening.catch(() => { if (slots.get(link) === made) slots.delete(link); });
    }
    slot.users++;
    clearTimeout(slot.timer);
    const held = slot;
    let room;
    try {
      room = await held.opening;
    } catch (error) {
      held.users--;
      throw error;
    }
    let released = false;
    return new Proxy(room, {
      get: (target, prop) => {
        if (prop === 'close') {
          return () => {
            if (released) return;
            released = true;
            held.users--;
            if (slots.get(link) === held) arm(link, held);
            else if (held.users === 0) room.close();
          };
        }
        const value = Reflect.get(target, prop, target);
        return typeof value === 'function' ? value.bind(target) : value;
      },
    });
  }

  return {
    connect,
    closeAll() {
      for (const [link, slot] of [...slots]) drop(link, slot);
    },
  };
}
