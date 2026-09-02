/* viewer-session.js -- permission mode, new-session and invite controls */
'use strict';

(() => {
  const MODES = ['ask', 'edits', 'full-auto'];

  // Every tier's secret is a prefix of the next, so a holder can derive
  // any tier at or below its own by truncating the secret it already
  // has. Handing on access therefore needs nothing from the host, and
  // handing on more than you hold is not expressible.
  // Module scope, not instance scope: it is read while the instance's
  // own bindings are still being initialised.
  const STORE = 'mevedel-rooms';

  const TIERS = [
    {name: 'view', bytes: 32, what: 'read only'},
    {name: 'full', bytes: 48, what: 'prompt, interrupt, answer'},
    {name: 'owner', bytes: 64, what: 'and change mode, create sessions'},
  ];

  function create({state, send, el, encode, decode}) {
    const button = document.getElementById('new-session-button');
    const sheet = document.getElementById('new-session');
    const nameInput = document.getElementById('new-session-name');
    const promptInput = document.getElementById('new-session-prompt');
    const submit = document.getElementById('new-session-create');
    const lede = document.getElementById('new-session-lede');
    const invites = document.getElementById('invites');
    const inviteButton = document.getElementById('invite-button');
    const inviteSheet = document.getElementById('invite');
    const inviteTiers = document.getElementById('invite-tiers');
    const roomsButton = document.getElementById('rooms-button');
    const roomsSheet = document.getElementById('rooms');
    const roomsList = document.getElementById('rooms-list');

    // Two lists, because they answer different questions. A notice says
    // what just happened -- your request was approved, refused, someone
    // handed you a room -- and dismissing it dismisses the news. The
    // store answers "which rooms can I get back to", and only Forget
    // takes a room out of it. Conflating them made Dismiss quietly
    // destroy the link it looked like it was only hiding.
    let notices = [];
    let requestSequence = 0;
    let secret = null;
    let roomId = null;

    // Only settled, reachable rooms persist: a waiting request belongs
    // to the socket that made it, and a refusal is news, not a room.
    // Rooms die with the host's share, so a stored link outlives
    // nothing -- it only saves the guest from losing one to a reload.
    //
    // A room is stored by its id rather than by a whole link, because
    // the store is per relay origin and one origin can hold several
    // tiers at once: two tabs of one browser on the view and owner
    // links share this key. Keying on the link would give that browser
    // two cards for one room, and letting the last tab write the whole
    // list would have each clobber the other's rooms.
    function stored() {
      try {
        const raw = JSON.parse(localStorage.getItem(STORE) || '[]');
        if (!Array.isArray(raw)) return [];
        return raw.filter(room => room && typeof room.room === 'string'
                          && typeof room.name === 'string'
                          && typeof room.secret === 'string');
      } catch (_error) { return []; }
    }

    // Merged rather than replaced: another tab of this browser may hold
    // rooms this one was never told about.
    function saveRooms(rooms) {
      try {
        localStorage.setItem(STORE, JSON.stringify(rooms));
      } catch (_error) { /* storage unavailable; this page still has them */ }
    }

    function rememberRoom(name, parts) {
      const rooms = stored();
      const existing = rooms.find(room => room.room === parts.roomId);
      if (!existing) {
        rooms.push({room: parts.roomId, name, secret: parts.secret});
      } else if (existing.secret.length < parts.secret.length) {
        // The stronger secret wins -- it is what this browser was
        // actually handed; what a given tab may present is capped when
        // the link is built, not here.
        existing.secret = parts.secret;
        existing.name = name;
      } else {
        return;
      }
      saveRooms(rooms);
    }

    function forgetRoom(id) {
      saveRooms(stored().filter(room => room.room !== id));
      renderRooms();
    }

    // The room a tab is standing in is kept but never listed: it is not
    // somewhere to go.
    function elsewhere() {
      return stored().filter(room => room.room !== roomId);
    }

    // A link is split rather than kept whole so the tier can be capped
    // when it is handed back out.
    function splitLink(link) {
      const hash = String(link || '').indexOf('#');
      if (hash < 0) return null;
      const value = link.slice(hash + 1);
      const dot = value.indexOf('.');
      if (dot <= 0) return null;
      return {roomId: value.slice(0, dot), secret: value.slice(dot + 1)};
    }

    // A tab never presents a link stronger than the one it holds. One
    // browser can hold several tiers, and the owner link an owner tab
    // was offered must not become a full tab's way into that room.
    function roomLink(entry) {
      const bytes = entry.secret && decode(entry.secret);
      if (!bytes) return null;
      const capped = secret && secret.length < bytes.length
        ? bytes.slice(0, secret.length)
        : bytes;
      return `${window.location.origin}${window.location.pathname}`
        + `#${entry.roomId || entry.room}.${encode(capped)}`;
    }

    function modePicker() {
      const picker = el('select', 'ml mode-picker');
      picker.title = 'Permission mode';
      picker.setAttribute('aria-label', 'Permission mode');
      MODES.forEach(mode => {
        const option = el('option', null, mode);
        option.value = mode;
        picker.append(option);
      });
      picker.value = state.mode;
      picker.addEventListener('change', () => {
        // The host answers with a status frame; until it does the strip
        // must keep reporting the mode the session is actually in.
        const wanted = picker.value;
        picker.value = state.mode;
        send({t: 'set-mode', mode: wanted});
      });
      return picker;
    }

    /* -- Requesting a session ------------------------------------------ */

    function open() {
      submit.textContent = state.owner ? 'Create' : 'Ask host';
      lede.textContent = state.owner
        ? 'A separate room for separate work. This room, and everyone in '
          + 'it, stays exactly where it is.'
        : 'The host decides. This room, and everyone in it, stays exactly '
          + 'where it is either way.';
      sheet.showModal();
      nameInput.focus();
    }

    function submitRequest() {
      const name = nameInput.value.trim().replace(/[^A-Za-z0-9_-]/g, '_');
      if (!/[A-Za-z0-9]/.test(name)) return;
      const reqId = ++requestSequence;
      send({t: 'new-session', reqId, name, prompt: promptInput.value.trim()});
      notices.push({reqId, name, status: 'waiting'});
      nameInput.value = '';
      promptInput.value = '';
      render();
    }

    function showResult({reqId, ok, message, link, name}) {
      const notice = notices.find(candidate => candidate.status === 'waiting'
                                  && candidate.reqId === reqId);
      const parts = ok ? splitLink(link) : null;
      const settled = {
        name: name || (notice && notice.name) || 'session',
        status: parts ? 'open' : 'refused',
        message: parts || !ok ? message : 'The host sent an unusable link',
        roomId: parts && parts.roomId,
        secret: parts && parts.secret,
      };
      if (parts) rememberRoom(settled.name, parts);
      if (notice) Object.assign(notice, settled);
      else notices.push(settled);
      render();
    }

    // A room handed over rather than asked for: someone else's request
    // that an owner approved, which this owner may also join.
    function offerRoom({name, link}) {
      const parts = splitLink(link);
      if (!name || !parts) return;
      rememberRoom(name, parts);
      // The same room reached this tab twice -- an offer after its own
      // reply, or a reconnect replaying one -- is one room, not a second
      // announcement of it.
      if (!notices.some(notice => notice.roomId === parts.roomId)) {
        notices.push({name, roomId: parts.roomId, secret: parts.secret,
                      status: 'open'});
      }
      render();
    }

    function render() {
      renderNotices();
      renderRooms();
    }

    function renderNotices() {
      if (!invites) return;
      invites.replaceChildren();
      const shown = notices.filter(notice => notice.roomId !== roomId);
      invites.hidden = shown.length === 0;
      shown.forEach(notice => {
        const card = el('section', 'own-entry invite-entry');
        card.append(el('span', 'rhead', notice.status === 'waiting'
                       ? `${notice.name} · waiting for the host`
                       : (notice.status === 'open'
                          ? `${notice.name} · open`
                          : `${notice.name} · refused`)));
        if (notice.status === 'refused' && notice.message) {
          card.append(el('p', 'own-text', notice.message));
        }
        const actions = el('div', 'invite-actions');
        const link = notice.status === 'open' ? roomLink(notice) : null;
        if (link) {
          // A browser will not let a delayed approval open a tab on its
          // own, so the link stays something to tap.
          actions.append(openButton(link));
          actions.append(copyButton('Copy link', link));
        }
        if (notice.status !== 'waiting') {
          const dismiss = el('button', 'btn quiet', 'Dismiss');
          dismiss.type = 'button';
          dismiss.setAttribute('title', notice.status === 'open'
                               ? 'Hide this notice; the room stays under Rooms'
                               : 'Hide this notice');
          dismiss.addEventListener('click', () => {
            notices = notices.filter(other => other !== notice);
            renderNotices();
          });
          actions.append(dismiss);
        }
        card.append(actions);
        invites.append(card);
      });
    }

    /* -- Rooms this browser can get back to ----------------------------- */

    function openButton(href) {
      const open = el('a', 'btn invite-open', 'Open room ↗');
      open.href = href;
      open.target = '_blank';
      open.rel = 'noopener';
      return open;
    }

    function renderRooms() {
      const rooms = elsewhere();
      if (roomsButton) {
        roomsButton.hidden = rooms.length === 0;
        roomsButton.textContent = `Rooms ${rooms.length}`;
      }
      if (!roomsList) return;
      roomsList.replaceChildren();
      rooms.forEach(room => {
        const row = el('div', 'invite-tier');
        row.append(el('span', 'invite-name', room.name));
        const link = roomLink(room);
        const actions = el('span', 'invite-what');
        row.append(actions);
        if (link) {
          row.append(openButton(link));
          row.append(copyButton('Copy', link));
        }
        const forget = el('button', 'btn quiet', 'Forget');
        forget.type = 'button';
        forget.setAttribute('title', 'Drop this link from this browser');
        forget.addEventListener('click', () => forgetRoom(room.room));
        row.append(forget);
        roomsList.append(row);
      });
    }

    /* -- Inviting into this room --------------------------------------- */

    function copyButton(label, text) {
      const copy = el('button', 'btn quiet', label);
      copy.type = 'button';
      copy.addEventListener('click', () => {
        Promise.resolve(navigator.clipboard && navigator.clipboard.writeText
                        ? navigator.clipboard.writeText(text)
                        : Promise.reject(new Error('no clipboard')))
          .then(() => { copy.textContent = 'Copied'; })
          .catch(() => { copy.textContent = 'Copy failed'; });
      });
      return copy;
    }

    function held() {
      if (!secret) return 0;
      return secret.length;
    }

    function linkFor(bytes) {
      return `${window.location.origin}${window.location.pathname}`
        + `#${roomId}.${encode(secret.slice(0, bytes))}`;
    }

    function openInvite() {
      inviteTiers.replaceChildren();
      TIERS.filter(tier => tier.bytes <= held()).forEach(tier => {
        const row = el('div', 'invite-tier');
        row.append(el('span', 'invite-name', tier.name));
        row.append(el('span', 'invite-what', tier.what));
        row.append(copyButton('Copy', linkFor(tier.bytes)));
        inviteTiers.append(row);
      });
      inviteSheet.showModal();
    }

    function setVisible(visible) {
      button.hidden = !visible;
    }

    // Inviting outlives the composer -- a read-only guest has a link to
    // hand on too -- but not the room: an ended room's link is dead, and
    // offering to pass it on would be a lie.
    function setInviteVisible(visible) {
      if (inviteButton) inviteButton.hidden = !(visible && secret);
      // Rooms outlive this room, so the list stays reachable after it
      // ends; only its own count decides whether the button is there.
      renderRooms();
    }

    // The fragment is wiped from the URL on connect, so the tiers are
    // rebuilt from the credentials the page kept in memory.
    function useCredentials(credentials) {
      roomId = credentials.roomId;
      secret = new Uint8Array([
        ...credentials.keyBytes,
        ...(credentials.writeToken || []),
        ...(credentials.ownerToken || []),
      ]);
      setInviteVisible(true);
      // Nothing can be rendered before this: which room is the current
      // one, and how far a link may be handed out, both come from here.
      render();
    }

    button.addEventListener('click', open);
    sheet.addEventListener('close', () => {
      if (sheet.returnValue === 'create') submitRequest();
    });
    if (inviteButton) inviteButton.addEventListener('click', openInvite);
    if (roomsButton) {
      roomsButton.addEventListener('click', () => {
        renderRooms();
        roomsSheet.showModal();
      });
    }

    return Object.freeze({modePicker, setVisible, setInviteVisible,
                          showResult, offerRoom, useCredentials});
  }

  window.mevedelSessionView = Object.freeze({create});
})();
