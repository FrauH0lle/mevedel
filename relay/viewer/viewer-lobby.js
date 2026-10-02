/* viewer-lobby.js -- a workspace's session list, opened from its lobby link */
'use strict';

(() => {
  // Rough ages read faster than timestamps in a list of conversations.
  function age(seconds, now = Date.now()) {
    if (typeof seconds !== 'number') return '';
    const elapsed = Math.max(0, now / 1000 - seconds);
    if (elapsed < 60) return 'just now';
    if (elapsed < 3600) return `${Math.floor(elapsed / 60)}m ago`;
    if (elapsed < 86400) return `${Math.floor(elapsed / 3600)}h ago`;
    if (elapsed < 172800) return 'yesterday';
    if (elapsed < 604800) return `${Math.floor(elapsed / 86400)}d ago`;
    return new Date(seconds * 1000).toLocaleDateString(
      undefined, {month: 'short', day: 'numeric'});
  }

  // A session link differs from this page's only in its fragment, which
  // the browser treats as an in-page jump, so the page reloads itself to
  // join the new room. Replacing keeps Back from landing on a page whose
  // fragment was already wiped.
  function follow(link) {
    window.location.replace(link);
    window.location.reload();
  }

  function create({state, send, el, notice, sessions, navigate = follow}) {
    const section = document.getElementById('lobby');
    const list = document.getElementById('lobby-list');
    const empty = document.getElementById('lobby-empty');
    const omitted = document.getElementById('lobby-omitted');
    const title = document.getElementById('lobby-title');
    const newButton = document.getElementById('lobby-new');
    const refresh = document.getElementById('lobby-refresh');

    let active = false;
    let requestSequence = 0;
    const opening = new Map();

    function open(row, button) {
      const reqId = ++requestSequence;
      opening.set(reqId, button);
      button.disabled = true;
      button.textContent = 'Opening…';
      send({t: 'open-session', reqId, id: row.id});
    }

    function renderRow(row) {
      const item = el('li', 'lobby-row');
      const main = el('div', 'lobby-main');
      main.append(el('span', 'lobby-name', row.name || 'Untitled'));
      const meta = [age(row.updated)];
      if (row.shared === true) meta.push('shared');
      else if (row.live === true) meta.push('open in Emacs');
      main.append(el('span', 'lobby-meta', meta.filter(Boolean).join(' · ')));
      if (row.preview) main.append(el('p', 'lobby-preview', row.preview));
      item.append(main);
      if (state.writable) {
        const button = el('button', 'btn quiet', 'Open');
        button.type = 'button';
        button.setAttribute('aria-label', `Open ${row.name || 'session'}`);
        button.addEventListener('click', () => open(row, button));
        item.append(button);
      }
      return item;
    }

    function show(frame) {
      if (!active) {
        active = true;
        document.body.dataset.lobby = '';
        section.hidden = false;
        // The lobby is the way back from every room it opens, so it is
        // kept where those rooms list the others.
        sessions.rememberCurrent(`Lobby · ${frame.project || 'project'}`);
      }
      title.textContent = frame.project ? `${frame.project} sessions` : 'Sessions';
      document.title = frame.project ? `${frame.project} · mevedel` : 'mevedel';
      newButton.hidden = !state.owner;
      const rows = Array.isArray(frame.sessions) ? frame.sessions : [];
      list.replaceChildren(...rows.map(renderRow));
      empty.hidden = rows.length > 0;
      const more = typeof frame.omitted === 'number' ? frame.omitted : 0;
      omitted.hidden = more === 0;
      omitted.textContent = more ? `${more} older sessions not shown.` : '';
    }

    function opened(frame) {
      const button = opening.get(frame.reqId);
      if (!button) return;
      opening.delete(frame.reqId);
      if (frame.ok === true && typeof frame.link === 'string') {
        navigate(frame.link);
        return;
      }
      button.disabled = false;
      button.textContent = 'Open';
      notice(typeof frame.message === 'string' ? frame.message
             : 'The session could not be opened.');
    }

    // A session created from the lobby is joined straight away; its
    // refusal is already reported by the request notice.
    function created(frame) {
      if (active && frame.ok === true && typeof frame.link === 'string') {
        navigate(frame.link);
      }
    }

    newButton.addEventListener('click', () => {
      sessions.openNewSession('Starts a separate session in this project.');
    });
    refresh.addEventListener('click', () => send({t: 'lobby-refresh'}));
    // A phone tab returning to the foreground is the moment the list may
    // be stale: sessions were opened or created while it slept.
    document.addEventListener('visibilitychange', () => {
      if (active && document.visibilityState === 'visible') {
        send({t: 'lobby-refresh'});
      }
    });

    return Object.freeze({show, opened, created, active: () => active});
  }

  window.mevedelLobbyView = Object.freeze({create, age});
})();
