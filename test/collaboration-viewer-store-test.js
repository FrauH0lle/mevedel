/* Focused artifact store list assertions.
 * Run: node test/collaboration-viewer-store-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const plain = value => JSON.parse(JSON.stringify(value));

function build({writable = true, room = false, connected = true, prompt = null} = {}) {
  const list = new Element('ul');
  const empty = new Element('p');
  const document = {createElement: tag => new Element(tag)};
  const window = {mevedelTranscriptRenderer: {formatBytes: bytes => `${bytes} B`},
                  addEventListener() {}, prompt};
  load('relay/viewer/viewer-store.js', {window, document, console, Date});
  const sent = [];
  const opened = [];
  const notices = [];
  const followed = [];
  const answer = {value: 'copy'};
  const confirmed = {value: false};
  const store = window.mevedelStoreView.create({
    send: frame => (connected ? sent.push(frame) : Promise.resolve(false)),
    el: (tag, className, text) => element(document, tag, className, text),
    list, empty, state: {writable}, room,
    open: record => opened.push(record),
    notice: text => notices.push(text),
    navigate: link => followed.push(link),
    ...(prompt ? {} : {ask: () => answer.value}),
    confirm: () => confirmed.value,
  });
  return {store, list, empty, sent, opened, notices, followed, answer, confirmed};
}

const listing = {t: 'store-artifacts', artifacts: [
  {id: 'old', title: 'old.md', kind: 'markdown', artifact: 'old/old.md', size: 3,
   modified: 10, versions: 1, missing: false, attached: false},
  {id: 'flow', title: 'index.html', kind: 'html', artifact: 'flow/index.html', size: 9,
   modified: 20, versions: 2, missing: false, attached: true},
]};

// A row shows Open beside its name and the other actions in its menu.
const menu = row => row.children.at(-1);
const controls = row => [...row.children.slice(1, -2), ...menu(row).children];
const buttons = row => controls(row).map(textOf);
const press = (row, label) => {
  menu(row).popoverOpen = true;
  controls(row).find(child => textOf(child) === label).dispatch('click');
};

// Rows list newest first; Open hands the panel a store record.
{
  const {store, list, empty, sent, opened} = build({room: true});
  store.refresh();
  assert.deepEqual(plain(sent), [{t: 'store-list'}]);
  store.show({t: 'store-artifacts', artifacts: []});
  assert.equal(empty.hidden, false);
  store.show(listing);
  assert.equal(empty.hidden, true);
  assert.match(textOf(list.children[0]), /index\.html.*HTML · 9 B · 2 versions · in this session/);
  assert.equal(list.children[0].children[0].children[0].title, 'flow');
  assert.deepEqual(buttons(list.children[0]),
                   ['Open', 'Versions', 'Duplicate', 'Conversation', 'Delete']);
  // Attaching is offered for an artifact the session does not have yet.
  assert.deepEqual(buttons(list.children[1]),
                   ['Open', 'Versions', 'Attach', 'Duplicate', 'Conversation', 'Delete']);
  press(list.children[0], 'Open');
  assert.deepEqual(plain(opened), [{id: 'artifact:flow', artifact: 'flow/index.html',
                                    store: 'flow', size: 9, missing: false, item: false}]);
}

// A view link lists and opens; the lobby never attaches.
{
  const view = build({writable: false});
  view.store.show(listing);
  assert.deepEqual(buttons(view.list.children[0]), ['Open', 'Versions']);
  const lobby = build();
  lobby.store.show(listing);
  assert.deepEqual(buttons(lobby.list.children[1]),
                   ['Open', 'Versions', 'Duplicate', 'Conversation', 'Delete']);
}

// Versions expand inline and restore older ones; actions report back.
{
  const {store, list, sent, notices, followed, answer} = build({room: true});
  store.show(listing);
  const versionsMenu = menu(list.children[0]);
  press(list.children[0], 'Versions');
  assert.equal(versionsMenu.popoverOpen, false);
  assert.deepEqual(plain(sent.at(-1)), {t: 'store-action', reqId: 1, action: 'versions',
                                        id: 'flow'});
  store.handle({t: 'store-action', reqId: 1, ok: true,
                versions: [{n: 2, time: 20, bytes: 9}, {n: 1, time: 10, bytes: 5}]});
  const versions = list.children[0].children[0].children.at(-1);
  assert.match(textOf(versions), /Version 2 \(current\).*Version 1/);
  versions.children[1].children[1].dispatch('click');
  assert.deepEqual(plain(sent.at(-1)), {t: 'store-action', reqId: 2, action: 'restore',
                                        id: 'flow', n: 1});
  store.handle({t: 'store-action', reqId: 2, ok: true, n: 3});
  assert.deepEqual(notices, ['Restored as version 3.']);
  // Duplicating asks for the copy's name; a cancelled prompt sends nothing.
  answer.value = null;
  press(list.children[0], 'Duplicate');
  assert.equal(sent.length, 2);
  answer.value = ' flow-2 ';
  press(list.children[0], 'Duplicate');
  assert.deepEqual(plain(sent.at(-1)), {t: 'store-action', reqId: 3, action: 'duplicate',
                                        id: 'flow', newId: 'flow-2'});
  store.handle({t: 'store-action', reqId: 3, ok: false, error: 'Taken'});
  assert.deepEqual(notices.at(-1), 'Taken');
  // The conversation opens in its own room.
  press(list.children[0], 'Conversation');
  store.handle({t: 'store-action', reqId: 4, ok: true, link: 'room-link'});
  assert.deepEqual(followed, ['room-link']);
  // Deleting asks first; a declined confirmation sends nothing.
  press(list.children[0], 'Delete');
  assert.equal(sent.length, 4);
  const deleting = build({room: true});
  deleting.store.show(listing);
  deleting.confirmed.value = true;
  press(deleting.list.children[0], 'Delete');
  assert.deepEqual(plain(deleting.sent.at(-1)), {t: 'store-action', reqId: 1,
                                                 action: 'delete', id: 'flow'});
  deleting.store.handle({t: 'store-action', reqId: 1, ok: true});
  assert.deepEqual(deleting.notices, ['Deleted index.html.']);
  // Replies nobody asked for are ignored.
  store.handle({t: 'store-action', reqId: 99, ok: true, link: 'other'});
  assert.deepEqual(followed, ['room-link']);
}

// Whiteboards and documents open in their editor and keep manual versions;
// from the lobby they open in their conversation's room.
{
  const board = {t: 'store-artifacts', artifacts: [
    {id: 'plan', title: 'Plan', kind: 'whiteboard', artifact: 'plan/state.json', size: 9,
     modified: 30, versions: 1, missing: false, attached: true, item: true}]};
  const roomView = build({room: true});
  roomView.store.show(board);
  assert.deepEqual(buttons(roomView.list.children[0]),
                   ['Open', 'Versions', 'Save version', 'Duplicate', 'Conversation',
                    'Delete']);
  press(roomView.list.children[0], 'Open');
  assert.equal(roomView.opened[0].item, true);
  press(roomView.list.children[0], 'Save version');
  assert.deepEqual(plain(roomView.sent.at(-1)), {t: 'store-action', reqId: 1,
                                                 action: 'save-version', id: 'plan'});
  roomView.store.handle({t: 'store-action', reqId: 1, ok: true, n: 2});
  assert.deepEqual(roomView.notices, ['Saved as version 2.']);
  const lobbyView = build();
  lobbyView.store.show(board);
  // From the lobby, Open is the item's conversation; the menu does not repeat it.
  assert.deepEqual(buttons(lobbyView.list.children[0]),
                   ['Open', 'Versions', 'Save version', 'Duplicate', 'Delete']);
  press(lobbyView.list.children[0], 'Open');
  assert.deepEqual(lobbyView.opened, []);
  assert.deepEqual(plain(lobbyView.sent.at(-1)), {t: 'store-action', reqId: 1,
                                                  action: 'conversation', id: 'plan'});
  // A view link cannot start the conversation an item would open in.
  const viewer = build({writable: false});
  viewer.store.show(board);
  assert.deepEqual(buttons(viewer.list.children[0]), ['Versions']);
}

// A deleted artifact that was expanded is forgotten, not asked about again;
// the deletion names it although its row is gone; unchanged artifacts keep
// their listed versions.
(async () => {
  const {store, list, sent, notices, confirmed} = build();
  store.show(listing);
  press(list.children[0], 'Versions');
  press(list.children[1], 'Versions');
  store.handle({t: 'store-action', reqId: 1, ok: true, versions: [{n: 2, time: 20}]});
  store.handle({t: 'store-action', reqId: 2, ok: true, versions: [{n: 1, time: 10}]});
  confirmed.value = true;
  press(list.children[0], 'Delete');
  const asked = sent.length;
  store.show({t: 'store-artifacts', artifacts: [listing.artifacts[0]]});
  assert.equal(sent.length, asked);
  assert.match(textOf(list.children[0]), /Version 1/);
  store.handle({t: 'store-action', reqId: 3, ok: true});
  assert.deepEqual(notices, ['Deleted index.html.']);
  // A changed artifact is asked about again.
  store.show({t: 'store-artifacts', artifacts: [{...listing.artifacts[0], versions: 2,
                                                 modified: 30}]});
  assert.deepEqual(plain(sent.at(-1)), {t: 'store-action', reqId: 4, action: 'versions',
                                        id: 'old'});

  // An action that cannot be sent says so and is not left pending.
  const offline = build({connected: false});
  offline.store.show(listing);
  offline.confirmed.value = true;
  press(offline.list.children[0], 'Delete');
  await new Promise(resolve => setImmediate(resolve));
  assert.deepEqual(offline.notices, ['Connection lost; nothing changed.']);

  // Duplicate proposes a name for the copy.
  const proposals = [];
  const copying = build({prompt: (text, value) => proposals.push(value) && null});
  copying.store.show(listing);
  press(copying.list.children[0], 'Duplicate');
  assert.deepEqual(proposals, ['flow-copy']);
  console.log('viewer store passed');
})().catch(error => {
  console.error(error);
  process.exit(1);
});
