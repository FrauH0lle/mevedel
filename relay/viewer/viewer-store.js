/* viewer-store.js -- the workspace artifact store, in a room or the lobby */
'use strict';

(() => {
  // LIST is the <ul> the rows render into and EMPTY the note shown when the
  // store has none. ROOM is true in a session's room, where artifacts can be
  // attached to the session; the lobby has no session of its own.
  function create({send, el, list, empty, state, room = false, open, notice,
                   creation = null,
                   navigate = link => window.location.replace(link),
                   ask = (text, value) => window.prompt(text, value),
                   confirm = text => window.confirm(text)}) {
    let rows = [];
    let requestSequence = 0;
    const pending = new Map();
    const expanded = new Set();
    const versions = new Map();

    // A placed menu would drift from its row; scrolling closes it.
    let shown = null;
    window.addEventListener('scroll', () => {
      if (shown) shown.hidePopover();
    }, {capture: true, passive: true});

    function refresh() {
      send({t: 'store-list'});
    }

    function act(action, id, fields = {}) {
      const reqId = ++requestSequence;
      const row = rows.find(candidate => candidate.id === id);
      pending.set(reqId, {action, id, title: (row && row.title) || id});
      Promise.resolve(send({t: 'store-action', reqId, action, id, ...fields})).then(sent => {
        if (sent !== false) return;
        pending.delete(reqId);
        notice('Connection lost; nothing changed.');
      });
    }

    function record(row) {
      return {id: `artifact:${row.id}`, artifact: row.artifact, store: row.id,
              size: row.size, missing: row.missing === true, item: row.item === true};
    }

    // A whiteboard or document opens in its editor: in a room directly, from
    // the lobby in the room of its own conversation.
    function openRow(row) {
      if (row.item === true && !room) act('conversation', row.id);
      else open(record(row));
    }

    function button(label, title, handler, danger = false) {
      const node = el('button', `btn quiet${danger ? ' danger' : ''}`, label);
      node.type = 'button';
      node.title = title;
      node.addEventListener('click', handler);
      return node;
    }

    const KINDS = {html: 'HTML', markdown: 'Markdown', image: 'Image', file: 'File',
                   whiteboard: 'Whiteboard', document: 'Document'};

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
                       `Version ${version.n}${index === 0 ? ' (latest saved)' : ''} · ${age}`));
        item.append(button('View', `Preview version ${version.n} without changing the artifact`,
                           () => open({...record(row), version: version.n})));
        if (state.writable) {
          item.append(button('Restore', `Make version ${version.n} the newest version`,
                             () => act('restore', row.id, {n: version.n})));
        }
        holder.append(item);
      });
      return holder;
    }

    // The row's secondary actions, behind one menu so a row stays one line.
    function actions(row) {
      const entries = [[expanded.has(row.id) ? 'Hide versions' : 'Versions',
                        'List this artifact\'s versions', () => {
        if (expanded.has(row.id)) expanded.delete(row.id);
        else {
          expanded.add(row.id);
          act('versions', row.id);
        }
        render();
      }]];
      if (state.writable) {
        if (row.item === true) {
          entries.push(['Save version', 'Keep the current state as a version',
                        () => act('save-version', row.id)]);
        }
        if (room && row.attached !== true) {
          entries.push(['Attach', 'Let this session work on the artifact',
                        () => act('attach', row.id)]);
        }
        entries.push(['Duplicate', 'Copy into a new, independent artifact', () => {
          const name = ask(`Name the copy of ${row.id}:`, `${row.id}-copy`);
          if (name) act('duplicate', row.id, {newId: name.trim()});
        }]);
        // From the lobby, Open already opens an item in its conversation.
        if (room || row.item !== true) {
          entries.push(['Conversation', 'Continue in this artifact\'s own session',
                        () => act('conversation', row.id)]);
        }
        entries.push(['Delete', 'Delete for everyone, with versions, comments and conversation',
                      () => {
          if (confirm(`Delete ${row.title || row.id} for everyone, with its versions, `
                      + 'comments and conversation? This cannot be undone.')) {
            act('delete', row.id);
          }
        }, true]);
      }
      // A popover sits above the lobby and the room's sheet, which would
      // otherwise clip it, and closes on its own on outside clicks and Escape.
      const trigger = el('button', 'btn quiet store-more', '⋯');
      trigger.type = 'button';
      trigger.title = 'More actions';
      trigger.setAttribute('aria-label', `More actions for ${row.title || row.id}`);
      const menu = el('div', 'store-menu');
      menu.popover = 'auto';
      menu.dataset.artifact = row.id;
      trigger.popoverTargetElement = menu;
      // The toggle event follows the opening by a task, so the menu stays
      // invisible until placed rather than flash at the window's corner.
      menu.addEventListener('beforetoggle', event => {
        delete menu.dataset.placed;
        // Track synchronously: a listing can arrive before the toggle task.
        if (event.newState === 'open') shown = menu;
        else if (shown === menu) shown = null;
      });
      menu.addEventListener('toggle', event => {
        if (event.newState === 'open') {
          shown = menu;
          place(trigger, menu);
          menu.dataset.placed = '';
        } else if (shown === menu) shown = null;
      });
      entries.forEach(([label, title, handler, danger]) => menu.append(button(label, title, () => {
        menu.hidePopover();
        handler();
      }, danger)));
      return [trigger, menu];
    }

    // Below the trigger, or above it when the window ends first.
    function place(trigger, menu) {
      const at = trigger.getBoundingClientRect();
      const fits = at.bottom + 6 + menu.offsetHeight <= window.innerHeight;
      menu.style.top = `${fits ? at.bottom + 6 : Math.max(8, at.top - 6 - menu.offsetHeight)}px`;
      menu.style.left = `${Math.max(8, at.right - menu.offsetWidth)}px`;
    }

    function renderRow(row) {
      const item = el('li', 'lobby-row');
      item.dataset.storeId = row.id;
      const main = el('div', 'lobby-main');
      const name = el('span', 'lobby-name', row.title || row.id);
      name.title = row.id;
      main.append(name);
      const meta = [KINDS[row.kind] || row.kind];
      if (row.missing === true) meta.push('file deleted');
      else if (typeof row.size === 'number') {
        meta.push(window.mevedelTranscriptRenderer.formatBytes(row.size));
      }
      meta.push(`${row.versions} version${row.versions === 1 ? '' : 's'}`);
      if (window.mevedelLobbyView) {
        meta.push(window.mevedelLobbyView.age(row.modified));
      }
      if (room && row.attached === true) meta.push('in this session');
      if (!room && Number.isInteger(row.attachedSessions)) {
        meta.push(`${row.attachedSessions} session${row.attachedSessions === 1 ? '' : 's'}`);
      }
      main.append(el('span', 'lobby-meta', meta.filter(Boolean).join(' · ')));
      if (expanded.has(row.id)) main.append(renderVersions(row));
      item.append(main);
      if (row.missing !== true) {
        item.append(button('Open', `Open ${row.title || row.id}`, () => openRow(row)));
      }
      item.append(...actions(row));
      return item;
    }

    // Rendering replaces every row; focus moves to the same control of the
    // same row, or its menu trigger, rather than falling to the page, and an
    // open menu opens again on its new row.
    function render() {
      const active = document.activeElement;
      const owner = active && active.closest ? active.closest('[data-store-id]') : null;
      const openId = shown && shown.dataset.artifact;
      shown = null;
      if (creation) {
        creation.replaceChildren();
        if (state.writable) {
          for (const kind of ['whiteboard', 'document']) {
            creation.append(button(`New ${kind}`, `Create a ${kind} in this project`, () => {
              const title = ask(`Title of the new ${kind}:`);
              if (title && title.trim()) act('create', null, {kind, title: title.trim()});
            }));
          }
        }
      }
      const rendered = rows.map(renderRow);
      list.replaceChildren(...rendered);
      // Refreshing must not dismiss the action the user is choosing. Rebuild
      // its menu from current permissions and retain it only while its row exists.
      if (openId) {
        const index = rows.findIndex(row => row.id === openId);
        if (index >= 0) {
          const children = rendered[index].children;
          children[children.length - 1].showPopover();
        }
      }
      if (empty) empty.hidden = rows.length > 0;
      if (!owner || list.contains(active)) return;
      const again = list.querySelector(`[data-store-id="${CSS.escape(owner.dataset.storeId)}"]`);
      if (!again) return;
      const controls = [...again.querySelectorAll('button')];
      (controls.find(control => control.textContent === active.textContent)
       || again.querySelector('.store-more'))?.focus();
    }

    // The host's listing, sent on request and after every store change.
    // Listed versions stay until their artifact changes; a gone artifact
    // is no longer expanded.
    function show(frame) {
      const before = new Map(rows.map(row => [row.id, `${row.versions}:${row.modified}`]));
      rows = Array.isArray(frame.artifacts) ? frame.artifacts.filter(
        row => row && typeof row.id === 'string') : [];
      rows.sort((a, b) => (b.modified || 0) - (a.modified || 0));
      const now = new Map(rows.map(row => [row.id, `${row.versions}:${row.modified}`]));
      for (const id of [...expanded]) {
        if (!now.has(id)) {
          expanded.delete(id);
          versions.delete(id);
        } else if (before.get(id) !== now.get(id)) {
          versions.delete(id);
          act('versions', id);
        }
      }
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
      } else if (request.action === 'create' && typeof frame.id === 'string') {
        act('conversation', frame.id);
      } else if (request.action === 'conversation' && typeof frame.link === 'string') {
        navigate(frame.link);
      } else if (request.action === 'duplicate') {
        notice(`Copied as ${frame.id}.`);
      } else if (request.action === 'restore') {
        // A whiteboard or document answers once its restore is saved.
        notice(typeof frame.n === 'number' ? `Restored as version ${frame.n}.` : 'Restored.');
      } else if (request.action === 'delete') {
        notice(`Deleted ${request.title}.`);
      } else if (request.action === 'save-version') {
        notice(`Saved as version ${frame.n}.`);
      }
    }

    return Object.freeze({show, handle, refresh, rows: () => rows});
  }

  window.mevedelStoreView = Object.freeze({create});
})();
