/* viewer-lobby.js -- a workspace's sessions, artifacts and files, from its lobby link */
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

  // The viewer's hashchange handler reloads into the room a link names.
  // Replacing keeps Back from landing on a page whose fragment was
  // already wiped.
  function follow(link) {
    window.location.replace(link);
  }

  function create({state, send, el, notice, sessions, files = null, store = null,
                   navigate = follow, confirm = text => window.confirm(text)}) {
    const section = document.getElementById('lobby');
    const tabs = document.getElementById('lobby-tabs');
    const sessionsTab = document.getElementById('lobby-tab-sessions');
    const artifactsTab = document.getElementById('lobby-tab-artifacts');
    const filesTab = document.getElementById('lobby-tab-files');
    const sessionsPane = document.getElementById('lobby-sessions');
    const artifactsPane = document.getElementById('lobby-artifacts');
    const filesPane = document.getElementById('lobby-files');
    const list = document.getElementById('lobby-list');
    const empty = document.getElementById('lobby-empty');
    const omitted = document.getElementById('lobby-omitted');
    const title = document.getElementById('lobby-title');
    const newButton = document.getElementById('lobby-new');
    const uploadButton = document.getElementById('files-upload');
    const refresh = document.getElementById('lobby-refresh');

    let active = false;
    let tab = 'sessions';
    let requestSequence = 0;
    const opening = new Map();
    const deleting = new Map();

    // The header already names the project, and the tabs, when shown,
    // stand in for this heading on screen; it labels the section for
    // assistive technology and heads a view link's tab-less list.
    function retitle() {
      title.textContent = {files: 'Files', artifacts: 'Artifacts'}[tab] || 'Sessions';
      newButton.hidden = !state.owner || tab !== 'sessions';
      uploadButton.hidden = tab !== 'files';
    }

    // Project files are a full-link feature; a view link sees sessions and
    // artifacts only.
    function filesAllowed() {
      return Boolean(files && state.writable);
    }

    function select(next) {
      if (next === 'files' && filesAllowed()) tab = 'files';
      else if (next === 'artifacts' && store) tab = 'artifacts';
      else tab = 'sessions';
      sessionsTab.setAttribute('aria-selected', String(tab === 'sessions'));
      artifactsTab.setAttribute('aria-selected', String(tab === 'artifacts'));
      filesTab.setAttribute('aria-selected', String(tab === 'files'));
      sessionsPane.hidden = tab !== 'sessions';
      artifactsPane.hidden = tab !== 'artifacts';
      filesPane.hidden = tab !== 'files';
      retitle();
      if (tab === 'files') files.show();
      if (tab === 'artifacts') store.refresh();
    }

    function reload() {
      if (tab === 'files') files.refresh();
      else if (tab === 'artifacts') store.refresh();
      else send({t: 'lobby-refresh'});
    }

    function open(row, button) {
      const reqId = ++requestSequence;
      opening.set(reqId, button);
      button.disabled = true;
      button.textContent = 'Opening…';
      send({t: 'open-session', reqId, id: row.id});
    }

    function remove(row, button) {
      const name = row.name || 'Untitled';
      if (!confirm(`Delete ${name}? Its saved conversation is gone for good.`)) return;
      const reqId = ++requestSequence;
      deleting.set(reqId, button);
      button.disabled = true;
      send({t: 'delete-session', reqId, id: row.id});
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
      if (state.owner) {
        const button = el('button', 'btn quiet danger', 'Delete');
        button.type = 'button';
        button.setAttribute('aria-label', `Delete ${row.name || 'session'}`);
        button.addEventListener('click', () => remove(row, button));
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
      const project = typeof frame.project === 'string' && frame.project ? frame.project : null;
      document.title = project ? `${project} · mevedel` : 'mevedel';
      tabs.hidden = !(store || filesAllowed());
      artifactsTab.hidden = !store;
      filesTab.hidden = !filesAllowed();
      retitle();
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

    // A deletion's success arrives as a fresh listing for every guest.
    function deleted(frame) {
      const button = deleting.get(frame.reqId);
      if (!button) return;
      deleting.delete(frame.reqId);
      if (frame.ok === true) return;
      button.disabled = false;
      notice(typeof frame.message === 'string' ? frame.message
             : 'The session could not be deleted.');
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
    refresh.addEventListener('click', reload);
    sessionsTab.addEventListener('click', () => select('sessions'));
    artifactsTab.addEventListener('click', () => select('artifacts'));
    filesTab.addEventListener('click', () => select('files'));
    // A phone tab returning to the foreground is the moment the list may
    // be stale: sessions were opened or created while it slept.
    document.addEventListener('visibilitychange', () => {
      if (active && document.visibilityState === 'visible') reload();
    });

    return Object.freeze({show, opened, deleted, created, active: () => active});
  }

  window.mevedelLobbyView = Object.freeze({create, age});
})();
