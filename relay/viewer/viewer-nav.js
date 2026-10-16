/* viewer-nav.js -- where this tab is, and the ways back and elsewhere */
'use strict';

/* Every page's header names where it is: the project, the room, the page.
   The project opens a menu of the places this browser can reach; a back
   button leads to the page's parent. Browser history follows the same
   moves: opening a room or panel pushes an entry, and Back returns.

   A room's key never enters history. An entry names its room only, and
   the tab keeps each room's link in its own storage, which dies with the
   tab; going back to a room finds the key the tab used there. */
(() => {
  const SHARES = 'mevedel-tab-shares';

  function shares() {
    try {
      const raw = JSON.parse(sessionStorage.getItem(SHARES) || '{}');
      return raw && typeof raw === 'object' && !Array.isArray(raw) ? raw : {};
    } catch (_error) { return {}; }
  }

  function saveShares(map) {
    try { sessionStorage.setItem(SHARES, JSON.stringify(map)); }
    catch (_error) { /* storage unavailable; Back stays in this room */ }
  }

  function entry() {
    const value = window.history.state;
    return value && typeof value === 'object' ? value : {};
  }

  // The link the tab used in the room this history entry names, or ''.
  function historyShare() {
    const room = entry().room;
    return typeof room === 'string' && typeof shares()[room] === 'string'
      ? shares()[room] : '';
  }

  const same = (a, b) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null);

  // PANELS are {node, describe, reopen, close}: DESCRIBE names the open
  // page so REOPEN can show it again after Back, Forward or a reload.
  // SESSIONS knows the rooms this browser holds, LOBBY the lobby page.
  function create({state, el, parse, sessions, lobby, panels = [],
                   openStore = () => {}, openRooms = () => {}}) {
    const roomBack = document.getElementById('room-back');
    const crumbs = ['room-crumbs', 'agent-crumbs', 'artifact-crumbs']
      .map(id => document.getElementById(id)).filter(Boolean);
    const backs = ['agent-close', 'artifact-close', 'editing-close']
      .map(id => document.getElementById(id)).filter(Boolean);
    // Entries this page pushed lie above one of its own; Back from them
    // stays in the page.
    const page = Math.random().toString(36).slice(2);
    let room = null;
    let restored = false;

    function write(next, push, path) {
      const url = path ?? `${window.location.pathname}${window.location.search}`;
      const method = push ? 'pushState' : 'replaceState';
      if (typeof window.history[method] === 'function') window.history[method](next, '', url);
    }

    // The tab joined ROOM-ID with FRAGMENT. The entry keeps an open panel
    // only when a reload came back to the same room.
    function enter(roomId, fragment) {
      room = roomId;
      const map = shares();
      map[roomId] = fragment;
      saveShares(map);
      const now = entry();
      write({...(now.room === roomId ? now : {}), room: roomId}, false);
    }

    // The room ended: its link is dead, and Back must not rejoin it.
    function leave() {
      const map = shares();
      delete map[room];
      saveShares(map);
    }

    // Opens the room LINK names in this tab, as a new history entry.
    // EXTRA rides on the entry, such as the lobby tab to show.
    function navigate(link, extra = {}) {
      const hash = String(link || '').indexOf('#');
      const fragment = hash < 0 ? '' : link.slice(hash + 1);
      const target = parse(`#${fragment}`);
      if (!target) return;
      if (target.roomId === room) {
        if (extra.tab && lobby.active()) lobby.select(extra.tab);
        return;
      }
      if (typeof window.history.pushState !== 'function') {
        window.location.replace(link);
        return;
      }
      const map = shares();
      map[target.roomId] = fragment;
      saveShares(map);
      // The link's own query comes along -- an artifact's conversation link
      // names the item its editor opens -- and this page's does not: an
      // editor tab's item belongs to the room it leaves.
      const query = hash < 0 ? '' : link.slice(0, hash).replace(/^[^?]*/, '');
      write({room: target.roomId, ...extra}, true, `${window.location.pathname}${query}`);
      window.location.reload();
    }

    // Merges FIELDS into this entry, such as the lobby tab on screen.
    function note(fields) {
      write({...entry(), ...fields, room}, false);
    }

    function shown() {
      return panels.find(panel => panel.node && !panel.node.hidden) || null;
    }

    // The entry follows the panels: opening one pushes an entry, so Back
    // closes it; closing one takes its entry back off.
    function sync() {
      const open = shown();
      const described = open ? open.describe() : null;
      const now = entry();
      if (described) {
        if (same(now.panel, described)) return;
        if (now.panel) write({...now, panel: described}, false);
        else write({...now, room, panel: described, page}, true);
      } else if (now.panel) {
        if (now.page === page) window.history.back();
        else {
          // Restored by a reload: the entry below belongs to another page.
          const {panel: _panel, page: _page, ...rest} = now;
          write(rest, false);
        }
      }
    }

    // Shows what entry NEXT describes.
    function settle(next) {
      const open = shown();
      if (!next.panel) {
        if (open) open.close();
      } else if (!open || !same(open.describe(), next.panel)) {
        const panel = panels.find(candidate => candidate.owns(next.panel));
        if (panel) panel.reopen(next.panel);
      }
      if (next.tab && lobby.active()) lobby.select(next.tab);
    }

    window.addEventListener('popstate', event => {
      const next = event.state && typeof event.state === 'object' ? event.state : {};
      if (typeof next.room === 'string' && next.room !== room) {
        window.location.reload();
        return;
      }
      settle(next);
    });

    // After the first welcome or listing, reopens the panel a reload left.
    function restore() {
      if (restored) return;
      restored = true;
      if (entry().panel) settle(entry());
    }

    if (typeof MutationObserver === 'function') {
      const observer = new MutationObserver(sync);
      panels.forEach(panel => panel.node && observer.observe(
        panel.node, {attributes: true, attributeFilter: ['hidden']}));
    }

    /* -- Header -------------------------------------------------------- */

    // The lobby this tab can go back to: a host may say none runs, and an
    // owner's host names it, even where the browser cannot keep it;
    // otherwise it is the one this browser holds.
    function lobbyLink() {
      if (lobby.active() || state.lobby === false) return null;
      if (typeof state.lobby === 'string') return state.lobby;
      return sessions.lobbyLink();
    }

    const menu = el('div', 'store-menu nav-menu');
    menu.popover = 'auto';
    menu.setAttribute('role', 'menu');
    menu.setAttribute('aria-label', 'Go to');
    menu.addEventListener('toggle', event => {
      if (event.newState !== 'open') delete menu.dataset.placed;
    });
    document.body.append(menu);

    function item(label, action, current = false) {
      const button = el('button', 'btn quiet', label);
      button.type = 'button';
      button.setAttribute('role', 'menuitem');
      if (current) button.setAttribute('aria-current', 'page');
      button.addEventListener('click', () => {
        menu.hidePopover();
        action();
      });
      return button;
    }

    function group(title, items) {
      if (!items.length) return [];
      return [el('div', 'nav-menu-head', title), ...items];
    }

    function closePanel() {
      const open = shown();
      if (open) open.close();
    }

    function items() {
      const target = lobbyLink();
      const project = [];
      if (lobby.active()) {
        const tab = lobby.tab();
        project.push(item('Sessions', () => { closePanel(); lobby.select('sessions'); },
                          tab === 'sessions'));
        project.push(item('Artifacts', () => { closePanel(); lobby.select('artifacts'); },
                          tab === 'artifacts'));
        if (state.writable) {
          project.push(item('Files', () => { closePanel(); lobby.select('files'); },
                            tab === 'files'));
        }
      } else {
        if (target) project.push(item('Lobby', () => navigate(target)));
        project.push(item('Project artifacts', () => openStore()));
      }
      const rooms = sessions.rooms().map(entry => item(entry.name, () => navigate(entry.link)));
      const manage = sessions.hasRooms() ? [item('Manage rooms…', () => openRooms())] : [];
      return [...group(`Project · ${state.project || 'this project'}`, project),
              ...group('Rooms', [...rooms, ...manage])];
    }

    function openMenu(trigger) {
      menu.replaceChildren(...items());
      menu.showPopover();
      const at = trigger.getBoundingClientRect?.();
      if (at) {
        menu.style.top = `${at.bottom + 6}px`;
        menu.style.left = `${Math.max(8, Math.min(at.left, window.innerWidth - menu.offsetWidth - 8))}px`;
      }
      menu.dataset.placed = '';
      menu.querySelector?.('button')?.focus();
    }

    function crumb(text, title, action) {
      const button = el('button', 'crumb', text);
      button.type = 'button';
      button.title = title;
      button.addEventListener('click', action);
      return button;
    }

    // Where the open panel's Back leads.
    function parentName() {
      if (lobby.active()) return 'the lobby';
      return state.sessionName || 'the room';
    }

    function render() {
      const project = state.project || 'Project';
      for (const holder of crumbs) {
        const menuButton = crumb(`${project} ▾`, 'Go to…', event => openMenu(event.currentTarget));
        menuButton.setAttribute('aria-haspopup', 'menu');
        const parts = [menuButton];
        if (holder.id !== 'room-crumbs' && !lobby.active()) {
          parts.push(el('span', 'crumb-sep', '›'),
                     crumb(parentName(), `Back to ${parentName()}`, closePanel));
        }
        holder.replaceChildren(...parts);
      }
      for (const back of backs) {
        back.title = `Back to ${parentName()}`;
        back.setAttribute('aria-label', back.title);
      }
      if (roomBack) {
        const target = lobbyLink();
        roomBack.hidden = !target;
        roomBack.title = 'Back to the project lobby';
        roomBack.setAttribute('aria-label', roomBack.title);
      }
    }

    if (roomBack) {
      roomBack.addEventListener('click', () => {
        const target = lobbyLink();
        if (target) navigate(target);
      });
    }

    return Object.freeze({enter, leave, navigate, note, sync, restore, render,
                          entry, openMenu});
  }

  window.mevedelNav = Object.freeze({create, historyShare});
})();
