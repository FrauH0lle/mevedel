/* Focused artifact store list assertions.
 * Run: node test/collaboration-viewer-store-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const plain = value => JSON.parse(JSON.stringify(value));

function build({writable = true, room = false} = {}) {
  const list = new Element('ul');
  const empty = new Element('p');
  const document = {createElement: tag => new Element(tag)};
  const window = {mevedelTranscriptRenderer: {formatBytes: bytes => `${bytes} B`}};
  load('relay/viewer/viewer-store.js', {window, document, console, Date});
  const sent = [];
  const opened = [];
  const notices = [];
  const followed = [];
  const answer = {value: 'copy'};
  const store = window.mevedelStoreView.create({
    send: frame => sent.push(frame),
    el: (tag, className, text) => element(document, tag, className, text),
    list, empty, state: {writable}, room,
    open: record => opened.push(record),
    notice: text => notices.push(text),
    navigate: link => followed.push(link),
    ask: () => answer.value,
  });
  return {store, list, empty, sent, opened, notices, followed, answer};
}

const listing = {t: 'store-artifacts', artifacts: [
  {id: 'old', title: 'old.md', kind: 'markdown', artifact: 'old/old.md', size: 3,
   modified: 10, versions: 1, missing: false, attached: false},
  {id: 'flow', title: 'index.html', kind: 'html', artifact: 'flow/index.html', size: 9,
   modified: 20, versions: 2, missing: false, attached: true},
]};

const buttons = row => row.children.slice(1).map(textOf);

// Rows list newest first; Open hands the panel a store record.
{
  const {store, list, empty, sent, opened} = build({room: true});
  store.refresh();
  assert.deepEqual(plain(sent), [{t: 'store-list'}]);
  store.show({t: 'store-artifacts', artifacts: []});
  assert.equal(empty.hidden, false);
  store.show(listing);
  assert.equal(empty.hidden, true);
  assert.match(textOf(list.children[0]), /index\.html.*flow · html · 9 B · 2 versions · in this session/);
  assert.deepEqual(buttons(list.children[0]), ['Open', 'Versions', 'Duplicate', 'Conversation']);
  // Attaching is offered for an artifact the session does not have yet.
  assert.deepEqual(buttons(list.children[1]),
                   ['Open', 'Versions', 'Attach', 'Duplicate', 'Conversation']);
  list.children[0].children[1].dispatch('click');
  assert.deepEqual(plain(opened), [{id: 'artifact:flow', artifact: 'flow/index.html',
                                    store: 'flow', size: 9, missing: false}]);
}

// A view link lists and opens; the lobby never attaches.
{
  const view = build({writable: false});
  view.store.show(listing);
  assert.deepEqual(buttons(view.list.children[0]), ['Open', 'Versions']);
  const lobby = build();
  lobby.store.show(listing);
  assert.deepEqual(buttons(lobby.list.children[1]),
                   ['Open', 'Versions', 'Duplicate', 'Conversation']);
}

// Versions expand inline and restore older ones; actions report back.
{
  const {store, list, sent, notices, followed, answer} = build({room: true});
  store.show(listing);
  list.children[0].children[2].dispatch('click');
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
  list.children[0].children[3].dispatch('click');
  assert.equal(sent.length, 2);
  answer.value = ' flow-2 ';
  list.children[0].children[3].dispatch('click');
  assert.deepEqual(plain(sent.at(-1)), {t: 'store-action', reqId: 3, action: 'duplicate',
                                        id: 'flow', newId: 'flow-2'});
  store.handle({t: 'store-action', reqId: 3, ok: false, error: 'Taken'});
  assert.deepEqual(notices.at(-1), 'Taken');
  // The conversation opens in its own room.
  list.children[0].children[4].dispatch('click');
  store.handle({t: 'store-action', reqId: 4, ok: true, link: 'room-link'});
  assert.deepEqual(followed, ['room-link']);
  // Replies nobody asked for are ignored.
  store.handle({t: 'store-action', reqId: 99, ok: true, link: 'other'});
  assert.deepEqual(followed, ['room-link']);
}

console.log('viewer store passed');
