/* Focused lobby controller assertions.
 * Run: node test/collaboration-viewer-lobby-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

// Frames are built inside the page's own realm; compare their content.
const plain = value => JSON.parse(JSON.stringify(value));

const ids = ['lobby', 'lobby-list', 'lobby-empty', 'lobby-omitted',
             'lobby-title', 'lobby-new', 'lobby-refresh', 'lobby-tabs',
             'lobby-tab-sessions', 'lobby-tab-artifacts', 'lobby-tab-files',
             'lobby-sessions', 'lobby-artifacts', 'lobby-files', 'files-upload',
             'lobby-store-create'];

function build({writable = true, owner = false} = {}) {
  const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
  nodes.lobby.hidden = true;
  const body = new Element('body');
  const document = {
    body,
    title: '',
    visibilityState: 'visible',
    listeners: {},
    addEventListener(type, callback) {
      (this.listeners[type] ||= []).push(callback);
    },
    getElementById: id => nodes[id],
    createElement: tag => new Element(tag),
  };
  const window = {};
  const context = {window, document, console, Date};
  load('relay/viewer/viewer-lobby.js', context);
  const sent = [];
  const notices = [];
  const followed = [];
  const remembered = [];
  const newSession = [];
  const filesCalls = [];
  const confirms = [];
  const confirmAnswer = {value: true};
  const files = {
    show: () => filesCalls.push(['show']),
    refresh: () => filesCalls.push(['refresh']),
  };
  const storeCalls = [];
  const store = {refresh: () => storeCalls.push('refresh')};
  const lobby = window.mevedelLobbyView.create({
    files,
    store,
    state: {writable, owner},
    send: frame => sent.push(frame),
    el: (tag, className, text) => element(document, tag, className, text),
    notice: text => notices.push(text),
    sessions: {
      rememberCurrent: name => remembered.push(name),
      openNewSession: note => newSession.push(note),
    },
    navigate: link => followed.push(link),
    confirm: text => { confirms.push(text); return confirmAnswer.value; },
  });
  return {lobby, nodes, body, document, sent, notices, followed, confirms,
          confirmAnswer, remembered, newSession, filesCalls, storeCalls,
          age: window.mevedelLobbyView.age};
}

const now = Math.floor(Date.now() / 1000);
const listing = {
  t: 'lobby', project: 'mevedel', omitted: 0,
  sessions: [
    {id: 'a', name: 'design', updated: now - 120, preview: 'Sketch the lobby',
     live: true, shared: true},
    {id: 'b', name: 'notes', updated: now - 7200, live: true, shared: false},
    {id: 'c', name: 'old', updated: null, live: false, shared: false},
  ],
};

function openButton(nodes, index) {
  const row = nodes['lobby-list'].children[index];
  return row.children[row.children.length - 1];
}

// The first listing turns the page into the lobby and keeps it among the
// rooms this browser can return to.
{
  const {lobby, nodes, body, document, remembered} = build();
  assert.equal(lobby.active(), false);
  lobby.show(listing);
  assert.equal(lobby.active(), true);
  assert.equal(body.dataset.lobby, '');
  assert.equal(nodes.lobby.hidden, false);
  // The header names the project; the heading names only the list.
  assert.equal(textOf(nodes['lobby-title']), 'Sessions');
  assert.equal(document.title, 'mevedel · mevedel');
  assert.deepEqual(remembered, ['Lobby · mevedel']);
  const rows = nodes['lobby-list'].children.map(row => textOf(row.children[0]));
  assert.equal(rows.length, 3);
  assert.match(rows[0], /design2m ago · sharedSketch the lobby/);
  assert.match(rows[1], /notes2h ago · open in Emacs/);
  assert.match(rows[2], /^old$/);
  assert.equal(nodes['lobby-empty'].hidden, true);
  assert.equal(nodes['lobby-omitted'].hidden, true);
  // A refresh is not a second arrival.
  lobby.show(listing);
  assert.deepEqual(remembered, ['Lobby · mevedel']);
}

// Opening asks the host once and follows the link it returns.
{
  const {lobby, nodes, sent, followed} = build();
  lobby.show(listing);
  const button = openButton(nodes, 1);
  button.dispatch('click');
  assert.deepEqual(plain(sent), [{t: 'open-session', reqId: 1, id: 'b'}]);
  assert.equal(button.disabled, true);
  assert.equal(textOf(button), 'Opening…');
  lobby.opened({t: 'open-session', reqId: 1, ok: true,
                link: 'https://relay.example/#room.secret'});
  assert.deepEqual(followed, ['https://relay.example/#room.secret']);
  // A reply to nothing this page asked for is ignored.
  lobby.opened({t: 'open-session', reqId: 9, ok: true, link: 'x'});
  assert.equal(followed.length, 1);
}

// A refusal leaves the list usable and says why.
{
  const {lobby, nodes, notices, followed} = build();
  lobby.show(listing);
  const button = openButton(nodes, 0);
  button.dispatch('click');
  lobby.opened({t: 'open-session', reqId: 1, ok: false,
                message: 'This session needs a decision in Emacs first'});
  assert.deepEqual(followed, []);
  assert.equal(button.disabled, false);
  assert.equal(textOf(button), 'Open');
  assert.deepEqual(notices, ['This session needs a decision in Emacs first']);
}

// Tiers keep their meaning: a view link lists, a full link opens, and
// only an owner link creates.
{
  const view = build({writable: false});
  view.lobby.show(listing);
  assert.equal(view.nodes['lobby-list'].children[0].children.length, 1);
  assert.equal(view.nodes['lobby-new'].hidden, true);
  const full = build({writable: true});
  full.lobby.show(listing);
  assert.equal(full.nodes['lobby-new'].hidden, true);
  const owner = build({writable: true, owner: true});
  owner.lobby.show(listing);
  assert.equal(owner.nodes['lobby-new'].hidden, false);
  owner.nodes['lobby-new'].dispatch('click');
  assert.equal(owner.newSession.length, 1);
  assert.match(owner.newSession[0], /this project/);
}

// Only an owner deletes, after confirming; the host's fresh listing is
// the success, and a refusal restores the button.
{
  const full = build({writable: true});
  full.lobby.show(listing);
  assert.equal(full.nodes['lobby-list'].children[0].children.length, 2);
  const {lobby, nodes, sent, notices, confirms, confirmAnswer} =
    build({writable: true, owner: true});
  lobby.show(listing);
  const row = nodes['lobby-list'].children[2];
  const button = row.children[row.children.length - 1];
  assert.equal(textOf(button), 'Delete');
  confirmAnswer.value = false;
  button.dispatch('click');
  assert.match(confirms[0], /Delete old\?/);
  assert.deepEqual(sent, []);
  confirmAnswer.value = true;
  button.dispatch('click');
  assert.deepEqual(plain(sent), [{t: 'delete-session', reqId: 1, id: 'c'}]);
  assert.equal(button.disabled, true);
  lobby.deleted({t: 'delete-session', reqId: 1, ok: false,
                 message: 'Close old in Emacs first'});
  assert.equal(button.disabled, false);
  assert.deepEqual(notices, ['Close old in Emacs first']);
  button.dispatch('click');
  lobby.deleted({t: 'delete-session', reqId: 2, ok: true});
  lobby.deleted({t: 'delete-session', reqId: 2, ok: false, message: 'late'});
  assert.deepEqual(notices, ['Close old in Emacs first']);
}

// A session created from the lobby is joined; one created from a room is
// only announced there.
{
  const {lobby, followed} = build({owner: true});
  lobby.created({t: 'new-session', ok: true, link: 'early'});
  assert.deepEqual(followed, []);
  lobby.show(listing);
  lobby.created({t: 'new-session', ok: false, message: 'taken'});
  lobby.created({t: 'new-session', ok: true, link: 'fresh'});
  assert.deepEqual(followed, ['fresh']);
}

// Refresh asks again; an empty or truncated listing says so.
{
  const {lobby, nodes, sent} = build();
  nodes['lobby-refresh'].dispatch('click');
  assert.deepEqual(plain(sent), [{t: 'lobby-refresh'}]);
  lobby.show({t: 'lobby', project: 'p', sessions: [], omitted: 0});
  assert.equal(nodes['lobby-empty'].hidden, false);
  lobby.show({t: 'lobby', project: 'p', sessions: listing.sessions, omitted: 5});
  assert.equal(nodes['lobby-empty'].hidden, true);
  assert.equal(nodes['lobby-omitted'].hidden, false);
  assert.equal(textOf(nodes['lobby-omitted']), '5 older sessions not shown.');
}

// Coming back to the foreground refreshes a lobby, and nothing else.
{
  const {lobby, document, sent} = build();
  const wake = () => document.listeners.visibilitychange.forEach(f => f());
  wake();
  assert.deepEqual(sent, []);
  lobby.show(listing);
  document.visibilityState = 'hidden';
  wake();
  assert.deepEqual(sent, []);
  document.visibilityState = 'visible';
  wake();
  assert.deepEqual(plain(sent), [{t: 'lobby-refresh'}]);
}

// Ages read at a glance.
{
  const {age} = build();
  const at = 1_000_000_000_000;
  assert.equal(age(null, at), '');
  assert.equal(age(at / 1000 - 30, at), 'just now');
  assert.equal(age(at / 1000 - 600, at), '10m ago');
  assert.equal(age(at / 1000 - 3 * 3600, at), '3h ago');
  assert.equal(age(at / 1000 - 30 * 3600, at), 'yesterday');
  assert.equal(age(at / 1000 - 3 * 86400, at), '3d ago');
}

// Project files are a tab for full links; a view link does not see it.
{
  const view = build({writable: false});
  view.lobby.show(listing);
  assert.equal(view.nodes['lobby-tab-files'].hidden, true);
  view.nodes['lobby-tab-files'].dispatch('click');
  assert.deepEqual(view.filesCalls, []);
  assert.equal(view.nodes['lobby-files'].hidden, true);

  const {lobby, nodes, sent, filesCalls, document} = build({owner: true});
  lobby.show(listing);
  assert.equal(nodes['lobby-tabs'].hidden, false);
  assert.equal(nodes['lobby-new'].hidden, false);
  nodes['lobby-tab-files'].dispatch('click');
  assert.deepEqual(filesCalls, [['show']]);
  assert.equal(nodes['lobby-sessions'].hidden, true);
  assert.equal(nodes['lobby-files'].hidden, false);
  assert.equal(nodes['lobby-tab-files'].attributes['aria-selected'], 'true');
  assert.equal(textOf(nodes['lobby-title']), 'Files');
  // Each tab shows only its own action: new sessions belong to the
  // session list, uploads to the files.
  assert.equal(nodes['lobby-new'].hidden, true);
  assert.equal(nodes['files-upload'].hidden, false);
  assert.equal(nodes['lobby-store-create'].hidden, true);
  nodes['lobby-tab-artifacts'].dispatch('click');
  assert.equal(nodes['lobby-store-create'].hidden, false);
  assert.equal(nodes['files-upload'].hidden, true);
  nodes['lobby-tab-files'].dispatch('click');
  // Refresh and a return to the foreground reload the tab on screen.
  nodes['lobby-refresh'].dispatch('click');
  document.listeners.visibilitychange.forEach(f => f());
  assert.deepEqual(filesCalls.slice(2), [['refresh'], ['refresh']]);
  assert.deepEqual(sent, []);
  nodes['lobby-tab-sessions'].dispatch('click');
  assert.equal(nodes['lobby-files'].hidden, true);
  assert.equal(textOf(nodes['lobby-title']), 'Sessions');
  assert.equal(nodes['lobby-new'].hidden, false);
  assert.equal(nodes['files-upload'].hidden, true);
  nodes['lobby-refresh'].dispatch('click');
  assert.deepEqual(plain(sent), [{t: 'lobby-refresh'}]);
}

// The artifact store is a tab for every link, a view link included.
{
  const {lobby, nodes, storeCalls, sent} = build({writable: false});
  lobby.show(listing);
  assert.equal(nodes['lobby-tabs'].hidden, false);
  assert.equal(nodes['lobby-tab-artifacts'].hidden, false);
  assert.equal(nodes['lobby-tab-files'].hidden, true);
  nodes['lobby-tab-artifacts'].dispatch('click');
  assert.deepEqual(storeCalls, ['refresh']);
  assert.equal(nodes['lobby-artifacts'].hidden, false);
  assert.equal(nodes['lobby-sessions'].hidden, true);
  assert.equal(textOf(nodes['lobby-title']), 'Artifacts');
  nodes['lobby-refresh'].dispatch('click');
  assert.deepEqual(storeCalls, ['refresh', 'refresh']);
  assert.deepEqual(sent, []);
  // Files stay a full-link tab.
  nodes['lobby-tab-files'].dispatch('click');
  assert.equal(nodes['lobby-files'].hidden, true);
  assert.equal(textOf(nodes['lobby-title']), 'Sessions');
}

console.log('viewer lobby controller passed');
