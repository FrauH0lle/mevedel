/* Focused new-session and invite controller assertions.
 * Run: node test/collaboration-viewer-session-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const ids = ['new-session-button', 'new-session', 'new-session-form',
             'new-session-name', 'new-session-prompt', 'new-session-create',
             'new-session-lede', 'invites', 'invite-button', 'invite',
             'invite-tiers', 'rooms-button', 'rooms', 'rooms-list'];

function base64urlEncode(bytes) {
  return Buffer.from(bytes).toString('base64')
    .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function base64urlDecode(text) {
  return new Uint8Array(Buffer.from(String(text).replace(/-/g, '+')
                                    .replace(/_/g, '/'), 'base64'));
}

// A room's secret is the room key, then the write token, then the owner
// token -- each tier a prefix of the next, which is what lets a link be
// capped by truncation.
function secretOf(seed, bytes) {
  return base64urlEncode(new Uint8Array(bytes).fill(seed));
}

const store = new Map();

function build(tierBytes, seed) {
  store.clear();
  if (seed) store.set('mevedel-rooms', JSON.stringify(seed));
  const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
  const document = {
    getElementById: id => nodes[id],
    createElement: tag => new Element(tag),
  };
  const copied = [];
  const window = {
    location: {origin: 'https://relay.example', pathname: '/'},
    localStorage: {
      getItem: key => (store.has(key) ? store.get(key) : null),
      setItem: (key, value) => store.set(key, value),
    },
    navigator: {clipboard: {writeText: async text => { copied.push(text); }}},
  };
  const context = {window, document, console, localStorage: window.localStorage};
  load('relay/viewer/viewer-session.js', context);
  const sent = [];
  const controller = window.mevedelSessionView.create({
    state: {mode: 'ask', owner: tierBytes === 64},
    send: frame => sent.push(frame),
    el: (tag, className, text) => element(document, tag, className, text),
    encode: base64urlEncode,
    decode: base64urlDecode,
  });
  controller.useCredentials({
    roomId: 'here',
    keyBytes: new Uint8Array(32).fill(1),
    writeToken: tierBytes >= 48 ? new Uint8Array(16).fill(1) : null,
    ownerToken: tierBytes >= 64 ? new Uint8Array(16).fill(1) : null,
  });
  return {controller, nodes, sent, copied, document};
}

// What just happened, in the dock.
function cards(nodes) {
  return nodes.invites.children.map(card => textOf(card.children[0]));
}

// Where this browser can get back to, in the Rooms sheet.
function rooms(nodes) {
  return nodes['rooms-list'].children.map(row => textOf(row.children[0]));
}

function findOpen(node) {
  return (node.children || [])
    .flatMap(child => [child, ...(child.children || [])])
    .find(child => child.className === 'btn invite-open');
}

function openLink(nodes, index) {
  return findOpen(nodes.invites.children[index]).href;
}

function roomLink(nodes, index) {
  return findOpen(nodes['rooms-list'].children[index]).href;
}

// An owner tab is offered an owner link: it hears about it once, and
// keeps it whole.
{
  const {controller, nodes} = build(64);
  assert.equal(nodes['rooms-button'].hidden, true);
  controller.offerRoom({name: 'flow',
                        link: `https://relay.example/#other.${secretOf(2, 64)}`});
  assert.deepEqual(cards(nodes), ['flow · open']);
  assert.deepEqual(rooms(nodes), ['flow']);
  assert.equal(nodes['rooms-button'].hidden, false);
  assert.equal(textOf(nodes['rooms-button']), 'Rooms 1');
  assert.equal(openLink(nodes, 0),
               `https://relay.example/#other.${secretOf(2, 64)}`);
  // What the browser was handed is what is stored.
  assert.equal(JSON.parse(store.get('mevedel-rooms'))[0].secret,
               secretOf(2, 64));
  // The same room twice is one room, not a second announcement of it.
  controller.offerRoom({name: 'flow',
                        link: `https://relay.example/#other.${secretOf(2, 64)}`});
  assert.equal(nodes.invites.children.length, 1);
  assert.equal(nodes['rooms-list'].children.length, 1);

  // Dismiss drops the news, never the room: the whole point of the
  // separate list is that hiding a notice cannot destroy a link.
  const card = nodes.invites.children[0];
  card.children[card.children.length - 1].children[2].dispatch('click');
  assert.deepEqual(cards(nodes), []);
  assert.deepEqual(rooms(nodes), ['flow']);
  assert.equal(JSON.parse(store.get('mevedel-rooms')).length, 1);

  // Forget is the one thing that does drop it.
  const row = nodes['rooms-list'].children[0];
  row.children[row.children.length - 1].dispatch('click');
  assert.deepEqual(rooms(nodes), []);
  assert.deepEqual(JSON.parse(store.get('mevedel-rooms')), []);
  assert.equal(nodes['rooms-button'].hidden, true);
}

// A full-control tab in the same browser reads that stored owner link
// and must not be able to use it: one origin can hold several tiers,
// and a tab may never present a link stronger than its own.
const ownerSeed = [{room: 'other', name: 'flow', secret: secretOf(2, 64)}];
{
  const {nodes} = build(48, ownerSeed);
  // A reload restores rooms, not news: the approval is not fresh any
  // more, but the room is still somewhere to go.
  assert.deepEqual(cards(nodes), []);
  assert.deepEqual(rooms(nodes), ['flow']);
  assert.equal(roomLink(nodes, 0),
               `https://relay.example/#other.${secretOf(2, 48)}`);
  // Capping is presentation: the browser keeps what it was handed, so
  // the owner tab beside this one still gets its own tier.
  assert.equal(JSON.parse(store.get('mevedel-rooms'))[0].secret,
               secretOf(2, 64));
}

// A view tab caps the same link all the way down to read-only.
{
  const {nodes} = build(32, ownerSeed);
  assert.equal(roomLink(nodes, 0),
               `https://relay.example/#other.${secretOf(2, 32)}`);
  // A view link can still be handed on -- its own tier and no more.
  nodes['invite-button'].dispatch('click');
  assert.deepEqual(nodes['invite-tiers'].children.map(
    row => textOf(row.children[0])), ['view']);
}

// The room you are standing in is not a room to go to.
{
  const {controller, nodes} = build(64);
  controller.offerRoom({name: 'this one',
                        link: `https://relay.example/#here.${secretOf(3, 64)}`});
  assert.deepEqual(cards(nodes), []);
  assert.equal(nodes.invites.hidden, true);
  assert.deepEqual(rooms(nodes), []);
  assert.equal(nodes['rooms-button'].hidden, true);
  // It is still remembered, for the tabs that are not standing in it.
  assert.equal(JSON.parse(store.get('mevedel-rooms')).length, 1);
}

// Two tabs share one key, so a write merges rather than replaces: one
// tab's rooms must not vanish because another tab saved its own.
{
  const {controller} = build(
    64, [{room: 'elsewhere', name: 'theirs', secret: secretOf(4, 48)}]);
  controller.offerRoom({name: 'mine',
                        link: `https://relay.example/#other.${secretOf(5, 64)}`});
  assert.deepEqual(JSON.parse(store.get('mevedel-rooms'))
                   .map(room => room.room).sort(),
                   ['elsewhere', 'other']);
}

// A refusal is news, not a room: it shows, and it is not kept.
{
  const {controller, nodes} = build(64);
  nodes['new-session-button'].dispatch('click');
  nodes['new-session-name'].value = 'flow';
  nodes['new-session-prompt'].value = '';
  nodes['new-session'].close('create');
  controller.showResult({reqId: 1, ok: false, name: 'flow',
                         message: 'The host declined'});
  assert.deepEqual(cards(nodes), ['flow · refused']);
  assert.deepEqual(rooms(nodes), []);
  assert.equal(store.get('mevedel-rooms'), undefined);
}

// A second request refused while the first waits settles its own notice.
{
  const {controller, nodes, sent} = build(48);
  nodes['new-session-button'].dispatch('click');
  nodes['new-session-name'].value = 'same name';
  nodes['new-session-prompt'].value = '';
  nodes['new-session'].close('create');
  nodes['new-session-name'].value = 'same_name';
  nodes['new-session'].close('create');
  assert.equal(sent[1].name, 'same_name');
  controller.showResult({reqId: 2, ok: false, name: 'same_name',
                         message: 'Another request is still waiting'});
  assert.deepEqual(cards(nodes),
                   ['same_name · waiting for the host', 'same_name · refused']);
}

console.log('viewer session controller passed');
