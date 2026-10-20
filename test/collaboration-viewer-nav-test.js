/* Focused navigation assertions: history entries, panels, header.
 * Run: node test/collaboration-viewer-nav-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const plain = value => JSON.parse(JSON.stringify(value));

function click(node, overrides = {}) {
  const event = {button: 0, currentTarget: node, defaultPrevented: false,
                 preventDefault() { this.defaultPrevented = true; }, ...overrides};
  node.dispatch('click', event);
  return event;
}

// A link's fragment is ROOM.SECRET; only the room id matters here.
function parse(fragment) {
  const match = /^#([A-Za-z0-9_-]+)\.([A-Za-z0-9_-]+)$/.exec(String(fragment));
  return match ? {roomId: match[1]} : null;
}

function build({lobbyActive = false, lobbyLink = null, rooms = [], state = {}} = {}) {
  const ids = ['room-back', 'room-crumbs', 'agent-crumbs', 'artifact-crumbs',
               'agent-close', 'artifact-close', 'editing-close'];
  const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
  nodes['room-crumbs'].id = 'room-crumbs';
  nodes['artifact-crumbs'].id = 'artifact-crumbs';
  nodes['agent-crumbs'].id = 'agent-crumbs';
  const body = new Element('body');
  const document = {body, getElementById: id => nodes[id],
                    createElement: tag => new Element(tag)};
  const tab = new Map();
  const calls = [];
  const history = {
    state: null,
    pushState(next, _title, url) { calls.push(['push', plain(next), url]); this.state = next; },
    replaceState(next, _title, url) { calls.push(['replace', plain(next), url]); this.state = next; },
    back() { calls.push(['back']); },
  };
  const window = {
    history,
    location: {origin: 'https://relay.example', pathname: '/', search: '?shared=board', reloads: 0,
               reload() { this.reloads++; }, replace(link) { this.replaced = link; }},
    listeners: {},
    addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); },
    innerWidth: 1000,
  };
  const sessionStorage = {getItem: key => (tab.has(key) ? tab.get(key) : null),
                          setItem: (key, value) => tab.set(key, value)};
  load('relay/viewer/viewer-nav.js', {window, document, console, sessionStorage, JSON, Math,
                                    URL, URLSearchParams});
  const lobbyCalls = [];
  const lobby = {active: () => lobbyActive, tab: () => 'sessions',
                 select: name => lobbyCalls.push(name)};
  const panel = {node: new Element('section'), page: null, closed: 0};
  panel.node.hidden = true;
  const opened = [];
  const sheets = [];
  const nav = window.mevedelNav.create({
    state: {project: 'proj', sessionName: 'Feature work', writable: true,
            fragment: 'here.secret', ...state},
    el: (tag, className, text) => element(document, tag, className, text),
    parse, lobby,
    sessions: {lobbyLink: () => lobbyLink, rooms: () => rooms, hasRooms: () => rooms.length > 0},
    openStore: () => sheets.push('store'), openRooms: () => sheets.push('rooms'),
    panels: [{
      node: panel.node,
      describe: () => (panel.node.hidden ? null : panel.page),
      owns: page => Boolean(page.artifact),
      reopen: page => { opened.push(plain(page)); panel.page = page; panel.node.hidden = false; },
      close: () => { panel.closed++; panel.node.hidden = true; },
    }],
  });
  const pop = next => {
    history.state = next;
    window.listeners.popstate.forEach(listener => listener({state: next}));
  };
  const menu = body.children.find(child => child.className === 'store-menu nav-menu');
  menu.querySelector = selector => {
    assert.equal(selector, '[role="menuitem"]');
    return menu.children.find(child => child.attributes.role === 'menuitem');
  };
  return {nav, nodes, window, history, calls, tab, panel, opened, pop, lobbyCalls,
          sheets, menu, historyShare: window.mevedelNav.historyShare};
}

const shares = tab => JSON.parse(tab.get('mevedel-tab-shares'));

// Joining a room names it in the entry and keeps its link in the tab only.
{
  const {nav, calls, tab, history, historyShare} = build();
  nav.enter('here', 'here.secret');
  assert.deepEqual(calls, [['replace', {room: 'here'}, '/?shared=board']]);
  assert.deepEqual(shares(tab), {here: 'here.secret'});
  // The entry finds the link the tab used there.
  assert.equal(historyShare(), 'here.secret');
  history.state = {room: 'gone'};
  assert.equal(historyShare(), '');
  // A reload into the same room keeps the panel its entry holds.
  history.state = {room: 'here', panel: {artifact: {id: 'a'}}};
  nav.enter('here', 'here.secret');
  assert.deepEqual(calls.at(-1), ['replace', {room: 'here', panel: {artifact: {id: 'a'}}},
                                  '/?shared=board']);
  // Another room's entry does not leak its panel into this one.
  history.state = {room: 'there', panel: {artifact: {id: 'a'}}};
  nav.enter('here', 'here.secret');
  assert.deepEqual(calls.at(-1)[1], {room: 'here'});
  // An ended room's link is dropped, so Back cannot rejoin it.
  nav.leave();
  assert.deepEqual(shares(tab), {});
}

// Opening another room is a new entry that names it; the key stays in the
// tab, and an editor tab's item stays behind.
{
  const {nav, calls, tab, window, lobbyCalls} = build();
  nav.enter('here', 'here.secret');
  nav.navigate('https://relay.example/#there.key', {tab: 'artifacts'});
  assert.deepEqual(calls.at(-1), ['push', {room: 'there', tab: 'artifacts'}, '/']);
  // An artifact's conversation link names the item its editor opens; the
  // query comes along, and the key still stays out of the URL.
  nav.navigate('https://relay.example/?shared=board#own.key');
  assert.deepEqual(calls.at(-1), ['push', {room: 'own'}, '/?shared=board']);
  assert.equal(shares(tab).there, 'there.key');
  assert.equal(window.location.reloads, 2);
  // A link that names nothing goes nowhere.
  nav.navigate('https://relay.example/');
  assert.equal(window.location.reloads, 2);
  // The room already on screen only changes what it shows.
  nav.navigate('https://relay.example/#here.secret', {tab: 'files'});
  assert.equal(window.location.reloads, 2);
  assert.deepEqual(lobbyCalls, []);
}
{
  const {nav, window, lobbyCalls} = build({lobbyActive: true});
  nav.enter('lobby', 'lobby.secret');
  nav.navigate('https://relay.example/#lobby.secret', {tab: 'files'});
  assert.equal(window.location.reloads, 0);
  assert.deepEqual(lobbyCalls, ['files']);
}

// A panel is a history entry: opening pushes, switching replaces, and
// closing goes back to the room's entry.
{
  const {nav, calls, panel, history} = build();
  nav.enter('here', 'here.secret');
  calls.length = 0;
  panel.page = {artifact: {id: 'a'}};
  panel.node.hidden = false;
  nav.sync();
  assert.equal(calls[0][0], 'push');
  assert.deepEqual(calls[0][1].panel, {artifact: {id: 'a'}});
  // Nothing changed, nothing written.
  nav.sync();
  assert.equal(calls.length, 1);
  panel.page = {artifact: {id: 'b'}};
  nav.sync();
  assert.equal(calls[1][0], 'replace');
  assert.deepEqual(calls[1][1].panel, {artifact: {id: 'b'}});
  panel.node.hidden = true;
  nav.sync();
  assert.deepEqual(calls[2], ['back']);
  // A panel a reload restored lies over another page's entry: closing it
  // rewrites the entry instead of leaving the page.
  history.state = {room: 'here', panel: {artifact: {id: 'a'}}, page: 'earlier'};
  calls.length = 0;
  nav.sync();
  assert.deepEqual(calls, [['replace', {room: 'here'}, '/?shared=board']]);
}

// Back and Forward show what the entry names; another room reloads.
{
  const {nav, panel, opened, pop, window} = build();
  nav.enter('here', 'here.secret');
  pop({room: 'here', panel: {artifact: {id: 'a'}}});
  assert.deepEqual(opened, [{artifact: {id: 'a'}}]);
  assert.equal(panel.node.hidden, false);
  pop({room: 'here'});
  assert.equal(panel.closed, 1);
  // An entry from before navigation existed is this room without a panel.
  pop(null);
  assert.equal(panel.closed, 1);
  pop({room: 'there'});
  assert.equal(window.location.reloads, 1);
}

// A reload reopens the panel its entry names, once.
{
  const {nav, opened, history} = build();
  history.state = {room: 'here', panel: {artifact: {id: 'a'}}};
  nav.enter('here', 'here.secret');
  nav.restore();
  nav.restore();
  assert.deepEqual(opened, [{artifact: {id: 'a'}}]);
}

// The header: the project opens the menu; a panel names its room; the
// room leads back to the lobby only when there is one to go to.
{
  const {nav, nodes, menu, sheets} = build({rooms: [{name: 'draw', link: 'https://r/#d.k'}]});
  nav.render();
  assert.equal(textOf(nodes['room-crumbs']), 'proj ▾');
  assert.equal(textOf(nodes['artifact-crumbs']), 'proj ▾›Feature work');
  assert.equal(nodes['artifact-close'].attributes['aria-label'], 'Back to Feature work');
  assert.equal(nodes['room-back'].hidden, true);
  const trigger = nodes['room-crumbs'].children[0];
  trigger.dispatch('click', {currentTarget: trigger});
  assert.equal(menu.popoverOpen, true);
  assert.deepEqual(menu.children.map(textOf),
                   ['Project · proj', 'Project artifacts', 'Rooms', 'draw', 'Manage rooms…']);
  menu.children.at(-1).dispatch('click');
  assert.equal(menu.popoverOpen, false);
  assert.deepEqual(sheets, ['rooms']);
}
{
  const {nav, nodes, menu, calls} = build({lobbyLink: 'https://r/#lobby.k'});
  nav.enter('here', 'here.secret');
  nav.render();
  assert.equal(nodes['room-back'].hidden, false);
  assert.equal(nodes['room-back'].attributes['aria-label'], 'Back to the project lobby');
  nodes['room-crumbs'].children[0].dispatch('click', {currentTarget: {}});
  assert.deepEqual(menu.children.map(textOf), ['Project · proj', 'Lobby', 'Project artifacts']);
  click(nodes['room-back']);
  assert.deepEqual(calls.at(-1), ['push', {room: 'lobby'}, '/']);
}
// A host that says no lobby runs outranks a link the browser still holds.
{
  const {nav, nodes} = build({lobbyLink: 'https://r/#lobby.k', state: {lobby: false}});
  nav.render();
  assert.equal(nodes['room-back'].hidden, true);
}
// An owner's host names the lobby, which leads back even where the browser
// kept nothing.
{
  const {nav, nodes, calls} = build({state: {lobby: 'https://r/#named.k'}});
  nav.enter('here', 'here.secret');
  nav.render();
  assert.equal(nodes['room-back'].hidden, false);
  click(nodes['room-back']);
  assert.deepEqual(calls.at(-1), ['push', {room: 'named'}, '/']);
}
// In the lobby the project is the page itself: its menu shows its tabs,
// and a panel's way back is the lobby.
{
  const {nav, nodes, menu, lobbyCalls} = build({lobbyActive: true, lobbyLink: 'https://r/#l.k'});
  nav.render();
  assert.equal(nodes['room-back'].hidden, true);
  assert.equal(textOf(nodes['artifact-crumbs']), 'proj ▾');
  assert.equal(nodes['artifact-close'].attributes['aria-label'], 'Back to the lobby');
  nodes['room-crumbs'].children[0].dispatch('click', {currentTarget: {}});
  assert.deepEqual(menu.children.map(textOf), ['Project · proj', 'Sessions', 'Artifacts', 'Files']);
  click(menu.children[2]);
  assert.deepEqual(lobbyCalls, ['artifacts']);
}

// Shared link helpers keep the browser's native alternate navigation intact.
{
  const {window} = build();
  const {link, roomURL} = window.mevedelNav;
  assert.equal(roomURL('here.secret'), 'https://relay.example/#here.secret');
  assert.equal(roomURL('#here.secret', {view: 'artifacts'}),
               'https://relay.example/#here.secret?view=artifacts');
  assert.equal(roomURL('here.secret', {file: 'src/a b.js'}),
               'https://relay.example/#here.secret?file=src%2Fa+b.js');
  assert.equal(roomURL('here.secret?file=old', {tab: 'files'}),
               'https://relay.example/#here.secret?tab=files');
  assert.equal(roomURL('here.secret', {shared: 'board', agent: '/root/child'}),
               'https://relay.example/?shared=board#here.secret?agent=%2Froot%2Fchild');
  const anchor = new Element('a');
  let activations = 0;
  assert.equal(link(anchor, roomURL('there.key'), () => activations++), anchor);
  assert.equal(anchor.href, 'https://relay.example/#there.key');
  for (const alternate of [{button: 1}, {button: 2}, {ctrlKey: true}, {metaKey: true},
                           {shiftKey: true}, {altKey: true}]) {
    assert.equal(click(anchor, alternate).defaultPrevented, false);
  }
  click(anchor, {defaultPrevented: true});
  assert.equal(activations, 0);
  assert.equal(click(anchor).defaultPrevented, true);
  assert.equal(activations, 1);
}

// Menu and header links use the session module's tier-capped destinations
// verbatim, rather than assembling links from stronger stored credentials.
{
  const lobbyLink = 'https://relay.example/#lobby.writer-token';
  const roomLink = 'https://relay.example/#other.view-token';
  const {nav, nodes, menu, window, sheets} = build({
    lobbyLink, rooms: [{name: 'Other', link: roomLink}],
  });
  nav.enter('here', 'here.secret');
  nav.render();
  click(nodes['room-crumbs'].children[0]);
  const destinations = menu.children.filter(node => node.tagName === 'a');
  assert.deepEqual(destinations.map(node => [textOf(node), node.href]), [
    ['Lobby', lobbyLink],
    ['Project artifacts', 'https://relay.example/#here.secret?view=artifacts'],
    ['Other', roomLink],
  ]);
  assert.equal(destinations[0].focused, true, 'opening the menu focuses its first anchor');
  assert.equal(menu.children.at(-1).tagName, 'button', 'Manage rooms remains an action');
  assert.equal(nodes['room-back'].href, lobbyLink);
  assert.equal(click(nodes['room-back'], {ctrlKey: true}).defaultPrevented, false);
  assert.equal(window.location.reloads, 0);
  const store = destinations[1];
  click(store, {button: 1});
  click(store, {metaKey: true});
  assert.deepEqual(sheets, []);
  assert.equal(menu.popoverOpen, true);
  click(store);
  assert.deepEqual(sheets, ['store']);
  assert.equal(menu.popoverOpen, false);
  click(destinations[2]);
  assert.equal(window.location.reloads, 1);
}

// A panel's parent is a room URL with no source editor/panel query attached.
{
  const {nav, nodes, panel} = build();
  nav.render();
  const parent = nodes['artifact-crumbs'].children.at(-1);
  assert.equal(parent.tagName, 'a');
  assert.equal(parent.href, 'https://relay.example/#here.secret');
  panel.node.hidden = false;
  click(parent, {metaKey: true});
  assert.equal(panel.closed, 0);
  click(parent);
  assert.equal(panel.closed, 1);
}

// Lobby menu destinations carry their tab in the URL, not only history state.
{
  const {nav, nodes, menu, lobbyCalls} = build({lobbyActive: true,
    state: {fragment: 'lobby.writer-token'}});
  nav.render();
  click(nodes['room-crumbs'].children[0]);
  assert.deepEqual(menu.children.slice(1).map(node => [node.tagName, node.href]), [
    ['a', 'https://relay.example/#lobby.writer-token?tab=sessions'],
    ['a', 'https://relay.example/#lobby.writer-token?tab=artifacts'],
    ['a', 'https://relay.example/#lobby.writer-token?tab=files'],
  ]);
  assert.equal(menu.children[1].attributes['aria-current'], 'page');
  click(menu.children[2], {ctrlKey: true});
  assert.deepEqual(lobbyCalls, []);
  click(menu.children[3]);
  assert.deepEqual(lobbyCalls, ['files']);
}
{
  const {nav, nodes, menu} = build({lobbyActive: true, state: {writable: false}});
  nav.render();
  click(nodes['room-crumbs'].children[0]);
  assert.deepEqual(menu.children.slice(1).map(textOf), ['Sessions', 'Artifacts']);
}

console.log('viewer navigation passed');
