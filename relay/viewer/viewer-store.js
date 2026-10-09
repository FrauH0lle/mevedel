/* viewer-store.js -- the workspace artifact store, in a room or the lobby */
'use strict';

(() => {
  // LIST is the <ul> the rows render into and EMPTY the note shown when the
  // store has none. ROOM is true in a session's room, where artifacts can be
  // attached to the session; the lobby has no session of its own.
  function create({send, el, list, empty, state, room = false, open, notice,
                   navigate = link => window.location.replace(link),
                   ask = text => window.prompt(text)}) {
    let rows = [];
    let requestSequence = 0;
    const pending = new Map();
    const expanded = new Set();
    const versions = new Map();

    function refresh() {
      send({t: 'store-list'});
    }

    function act(action, id, fields = {}) {
      const reqId = ++requestSequence;
      pending.set(reqId, {action, id});
      send({t: 'store-action', reqId, action, id, ...fields});
    }

    function record(row) {
      return {id: `artifact:${row.id}`, artifact: row.artifact, store: row.id,
              size: row.size, missing: row.missing === true};
    }

    function button(label, title, handler, danger = false) {
      const node = el('button', `btn quiet${danger ? ' danger' : ''}`, label);
      node.type = 'button';
      node.title = title;
      node.addEventListener('click', handler);
      return node;
    }

    function renderVersions(row) {
      const holder = el('ol', 'store-versions');
      const known = versions.get(row.id);
      if (!known) {
        holder.append(el('li', 'lobby-meta', 'Loading versions…'));
        return holder;
      }
      known.forEach((version, index) => {
        const item = el('li', 'store-version');
        const age = window.mevedelLobbyView ? window.mevedelLobbyView.age(version.time) : '';
        item.append(el('span', 'lobby-meta',
                       `Version ${version.n}${index === 0 ? ' (current)' : ''} · ${age}`));
        if (index > 0 && state.writable) {
          item.append(button('Restore', `Make version ${version.n} the newest version`,
                             () => act('restore', row.id, {n: version.n})));
        }
        holder.append(item);
      });
      return holder;
    }

    function renderRow(row) {
      const item = el('li', 'lobby-row store-row');
      const main = el('div', 'lobby-main');
      main.append(el('span', 'lobby-name', row.title || row.id));
      const meta = [row.id, row.kind];
      if (row.missing === true) meta.push('file deleted');
      else if (typeof row.size === 'number') {
        meta.push(window.mevedelTranscriptRenderer.formatBytes(row.size));
      }
      meta.push(`${row.versions} version${row.versions === 1 ? '' : 's'}`);
      if (room && row.attached === true) meta.push('in this session');
      main.append(el('span', 'lobby-meta', meta.filter(Boolean).join(' · ')));
      if (expanded.has(row.id)) main.append(renderVersions(row));
      item.append(main);
      if (row.missing !== true) {
        item.append(button('Open', `Open ${row.title || row.id}`, () => open(record(row))));
      }
      item.append(button(expanded.has(row.id) ? 'Hide versions' : 'Versions',
                         'List this artifact\'s versions', () => {
        if (expanded.has(row.id)) expanded.delete(row.id);
        else {
          expanded.add(row.id);
          act('versions', row.id);
        }
        render();
      }));
      if (state.writable) {
        if (room && row.attached !== true) {
          item.append(button('Attach', 'Let this session work on the artifact',
                             () => act('attach', row.id)));
        }
        item.append(button('Duplicate', 'Copy into a new, independent artifact', () => {
          const name = ask(`Name the copy of ${row.id}:`, `${row.id}-copy`);
          if (name) act('duplicate', row.id, {newId: name.trim()});
        }));
        item.append(button('Conversation', 'Continue in this artifact\'s own session',
                           () => act('conversation', row.id)));
      }
      return item;
    }

    function render() {
      list.replaceChildren(...rows.map(renderRow));
      if (empty) empty.hidden = rows.length > 0;
    }

    // The host's listing, sent on request and after every store change.
    function show(frame) {
      rows = Array.isArray(frame.artifacts) ? frame.artifacts.filter(
        row => row && typeof row.id === 'string') : [];
      rows.sort((a, b) => (b.modified || 0) - (a.modified || 0));
      // A changed store invalidates listed versions.
      versions.clear();
      expanded.forEach(id => act('versions', id));
      render();
    }

    function handle(frame) {
      const request = pending.get(frame.reqId);
      if (!request) return;
      pending.delete(frame.reqId);
      if (frame.ok !== true) {
        notice(typeof frame.error === 'string' ? frame.error : 'The artifact action failed.');
        return;
      }
      if (request.action === 'versions' && Array.isArray(frame.versions)) {
        versions.set(request.id, frame.versions);
        render();
      } else if (request.action === 'conversation' && typeof frame.link === 'string') {
        navigate(frame.link);
      } else if (request.action === 'duplicate') {
        notice(`Copied as ${frame.id}.`);
      } else if (request.action === 'restore') {
        notice(`Restored as version ${frame.n}.`);
      }
    }

    return Object.freeze({show, handle, refresh, rows: () => rows});
  }

  window.mevedelStoreView = Object.freeze({create});
})();
