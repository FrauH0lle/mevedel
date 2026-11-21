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

  // NAVIGATE opens a room in this tab as a history entry. ON-TAB hears
  // the tab shown, so a reload or Back comes back to it, and INITIAL-TAB
  // names the tab the entry asks for first.
  function create({state, send, el, notice, sessions, files, store, navigate,
                   onTab = () => {}, initialTab = () => null,
                   confirm = text => window.confirm(text)}) {
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
    const schedules = document.getElementById('lobby-schedules');
    const scheduleList = document.getElementById('lobby-schedule-list');
    const scheduleEmpty = document.getElementById('lobby-schedule-empty');
    const title = document.getElementById('lobby-title');
    const newButton = document.getElementById('lobby-new');
    const uploadButton = document.getElementById('files-upload');
    const createButtons = document.getElementById('lobby-store-create');
    const refresh = document.getElementById('lobby-refresh');

    let active = false;
    let tab = 'sessions';
    let requestSequence = 0;
    const opening = new Map();
    const deleting = new Map();
    const renaming = new Map();
    const scheduleActions = new Map();
    // How many browsers are in each live session, by session id.
    let here = new Map();
    let shown = [];

    // The header already names the project, and the tabs, when shown,
    // stand in for this heading on screen; it labels the section for
    // assistive technology and heads a view link's tab-less list.
    function retitle() {
      title.textContent = {files: 'Files', artifacts: 'Artifacts'}[tab] || 'Sessions';
      newButton.hidden = !state.owner || tab !== 'sessions';
      uploadButton.hidden = tab !== 'files';
      createButtons.hidden = tab !== 'artifacts';
    }

    // Project files are a full-link feature; a view link sees sessions and
    // artifacts only.
    function filesAllowed() {
      return Boolean(state.writable);
    }

    function select(next) {
      if (next === 'files' && filesAllowed()) tab = 'files';
      else if (next === 'artifacts') tab = 'artifacts';
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
      onTab(tab);
    }

    function reload() {
      if (tab === 'files') files.refresh();
      else if (tab === 'artifacts') store.refresh();
      else send({t: 'lobby-refresh'});
    }

    function open(row, button) {
      const reqId = ++requestSequence;
      opening.set(reqId, {id: row.id, button});
      button.disabled = true;
      button.textContent = 'Opening…';
      send({t: 'open-session', reqId, id: row.id});
    }

    function remove(row, button) {
      const name = row.name || 'Untitled';
      if (!confirm(`Delete ${name}? Its saved conversation is gone for good.`)) return;
      const reqId = ++requestSequence;
      deleting.set(reqId, {id: row.id, button});
      button.disabled = true;
      send({t: 'delete-session', reqId, id: row.id});
    }

    // Listings and presence updates replace rows before their replies can arrive.
    function restorePending(requests, id, button) {
      for (const request of requests.values()) {
        if (request.id !== id) continue;
        button.disabled = true;
        button.textContent = request.button.textContent;
        request.button = button;
        break;
      }
    }

    function renderRow(row) {
      const item = el('li', 'lobby-row');
      const main = el('div', 'lobby-main');
      main.append(el('span', 'lobby-name', row.name || 'Untitled'));
      const meta = [age(row.updated)];
      if (row.shared === true) meta.push('shared');
      else if (row.live === true) meta.push('open in Emacs');
      if (here.get(row.id)) meta.push(`${here.get(row.id)} here`);
      main.append(el('span', 'lobby-meta', meta.filter(Boolean).join(' · ')));
      if (row.preview) main.append(el('p', 'lobby-preview', row.preview));
      item.append(main);
      if (state.writable) {
        const button = el('button', 'btn quiet', 'Open');
        button.type = 'button';
        button.setAttribute('aria-label', `Open ${row.name || 'session'}`);
        button.addEventListener('click', () => open(row, button));
        restorePending(opening, row.id, button);
        item.append(button);
      }
      if (state.owner) {
        const rename = el('button', 'btn quiet', 'Rename');
        rename.type = 'button';
        rename.setAttribute('aria-label', `Rename ${row.name || 'session'}`);
        rename.addEventListener('click', () => {
          const name = window.prompt('Session name', row.name || '');
          if (!name?.trim()) return;
          const reqId = ++requestSequence;
          renaming.set(reqId, {id: row.id, button: rename});
          rename.disabled = true;
          send({t: 'rename-session', reqId, id: row.id, name: name.trim()});
        });
        item.append(rename);
        restorePending(renaming, row.id, rename);
        const button = el('button', 'btn quiet danger', 'Delete');
        button.type = 'button';
        button.setAttribute('aria-label', `Delete ${row.name || 'session'}`);
        button.addEventListener('click', () => remove(row, button));
        restorePending(deleting, row.id, button);
        item.append(button);
      }
      return item;
    }

    function scheduleRow(row) {
      const item = el('li', 'lobby-row');
      const main = el('div', 'lobby-main');
      const name = row.name || row.id;
      main.append(el('span', 'lobby-name', name));
      main.append(el('span', 'lobby-meta', [row.timing, row.status].filter(Boolean).join(' · ')));
      if (row.nextRun) main.append(el('span', 'lobby-meta', `Next: ${row.nextRun.replace('T', ' ')}`));
      if (row.lastOutcome) main.append(el('span', 'lobby-meta', `Last run: ${row.lastOutcome}`));
      item.append(main);
      if (state.owner && (row.status === 'active' || row.status === 'paused')) {
        const action = row.status === 'active' ? 'pause' : 'resume';
        const label = action === 'pause' ? 'Pause' : 'Resume';
        const button = el('button', 'btn quiet', label);
        button.type = 'button';
        button.dataset.schedule = row.id;
        button.setAttribute('aria-label', `${label} ${name}`);
        button.setAttribute('aria-disabled', String(scheduleActions.has(row.id)));
        button.addEventListener('click', async () => {
          if (scheduleActions.has(row.id)) return;
          const reqId = ++requestSequence;
          scheduleActions.set(row.id, reqId);
          button.setAttribute('aria-disabled', 'true');
          if (!await send({t: 'schedule-action', action, id: row.id, reqId})) {
            scheduleResult({reqId, ok: false, message: 'Schedule action not sent. Reconnect and try again.'});
          }
        });
        item.append(button);
      }
      return item;
    }

    function scheduleResult(frame) {
      for (const [id, reqId] of scheduleActions) {
        if (reqId !== frame.reqId) continue;
        scheduleActions.delete(id);
        scheduleList.querySelector(`[data-schedule="${CSS.escape(id)}"]`)?.setAttribute('aria-disabled', 'false');
        if (frame.ok !== true) notice(frame.message || 'The schedule could not be changed.');
        break;
      }
    }

    // A pushed listing must not drop keyboard focus or scroll a phone to the new button.
    function showRows(target, rows, render) {
      const focused = document.activeElement;
      const row = focused?.parentElement;
      const retain = row?.parentElement === target;
      const index = retain ? Array.from(row.children).indexOf(focused) : -1;
      target.replaceChildren(...rows.map(data => {
        const item = render(data);
        item.dataset.id = data.id;
        return item;
      }));
      if (retain) {
        const replacement = Array.from(target.children).find(item => item.dataset.id === row.dataset.id);
        replacement?.children[index]?.focus({preventScroll: true});
      }
    }

    function show(frame) {
      if (!active) {
        active = true;
        document.body.dataset.lobby = '';
        section.hidden = false;
        // The lobby is the way back from every room it opens, so it is
        // kept where those rooms list the others.
        sessions.rememberCurrent(`Lobby · ${frame.project || 'project'}`, true);
        const wanted = initialTab();
        if (wanted && wanted !== tab) select(wanted);
      }
      const project = typeof frame.project === 'string' && frame.project ? frame.project : null;
      document.title = project ? `${project} · niwa` : 'niwa';
      tabs.hidden = false;
      filesTab.hidden = !filesAllowed();
      retitle();
      const rows = Array.isArray(frame.sessions) ? frame.sessions : [];
      shown = rows;
      showRows(list, rows, renderRow);
      empty.hidden = rows.length > 0;
      const more = typeof frame.omitted === 'number' ? frame.omitted : 0;
      omitted.hidden = more === 0;
      omitted.textContent = more ? `${more} older sessions not shown.` : '';
      schedules.hidden = !Array.isArray(frame.schedules);
      const jobs = Array.isArray(frame.schedules) ? frame.schedules : [];
      showRows(scheduleList, jobs, scheduleRow);
      scheduleEmpty.hidden = jobs.length > 0;
    }

    function opened(frame) {
      const button = opening.get(frame.reqId)?.button;
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
      const button = deleting.get(frame.reqId)?.button;
      if (!button) return;
      deleting.delete(frame.reqId);
      if (frame.ok === true) return;
      button.disabled = false;
      notice(typeof frame.message === 'string' ? frame.message
             : 'The session could not be deleted.');
    }

    function renamed(frame) {
      const button = renaming.get(frame.reqId)?.button;
      if (!button) return;
      renaming.delete(frame.reqId);
      button.disabled = false;
      if (frame.ok !== true) notice(frame.message || 'The session could not be renamed.');
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

    function counts(entries) {
      if (!Array.isArray(entries)) return;
      here = new Map(entries.filter(entry => entry && typeof entry.id === 'string')
        .map(entry => [entry.id, entry.n]));
      if (active) showRows(list, shown, renderRow);
    }

    return Object.freeze({show, opened, deleted, renamed, created, counts, select, scheduleResult,
                          disconnected: () => {
                            for (const requests of [opening, deleting, renaming, scheduleActions]) requests.clear();
                          },
                          tab: () => tab, active: () => active});
  }

  window.niwaLobbyView = Object.freeze({create, age});
})();
