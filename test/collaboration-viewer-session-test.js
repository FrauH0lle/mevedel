/* Focused new-session and invite controller assertions.
 * Run: node test/collaboration-viewer-session-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const ids = ['new-session-button', 'new-session', 'new-session-form',
             'new-session-name', 'new-session-prompt', 'new-session-create',
             'new-session-model', 'new-session-model-label',
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

function build(tierBytes, seed, models = []) {
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
  load('relay/viewer/attachments.js', context);
  load('relay/viewer/viewer-session.js', context);
  const sent = [];
  const controller = window.mevedelSessionView.create({
    state: {mode: 'ask', owner: tierBytes === 64, models},
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
  controller.setWorkspace('ws');
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

  // A row is its name and one group of actions, so the layout can give the
  // actions their own column instead of stacking them in one cell.
  const row = nodes['rooms-list'].children[0];
  assert.deepEqual(row.children.map(child => child.className), ['invite-name', 'room-actions']);
  const actions = row.children[1];
  assert.deepEqual(actions.children.map(textOf), ['Open room ↗', 'Copy', 'Forget']);
  // Forget is the one thing that does drop it.
  actions.children.at(-1).dispatch('click');
  assert.deepEqual(rooms(nodes), []);
  assert.deepEqual(JSON.parse(store.get('mevedel-rooms')), []);
  assert.equal(nodes['rooms-button'].hidden, true);
}

// A full-control tab in the same browser reads that stored owner link
// and must not be able to use it: one origin can hold several tiers,
// and a tab may never present a link stronger than its own.
const ownerSeed = [{room: 'other', name: 'flow', secret: secretOf(2, 64),
                    workspace: 'ws'}];
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
    64, [{room: 'elsewhere', name: 'theirs', secret: secretOf(4, 48),
          workspace: 'ws'}]);
  controller.offerRoom({name: 'mine',
                        link: `https://relay.example/#other.${secretOf(5, 64)}`});
  assert.deepEqual(JSON.parse(store.get('mevedel-rooms'))
                   .map(room => room.room).sort(),
                   ['elsewhere', 'other']);
}

// One relay serves every host and project: a tab lists only the rooms
// of its own workspace, and keeps nothing before its host names one.
{
  const {controller, nodes} = build(64, [
    {room: 'remote', name: 'Lobby · general', secret: secretOf(6, 64),
     workspace: 'other-ws'},
    {room: 'local', name: 'draw_test', secret: secretOf(7, 64), workspace: 'ws'},
    {room: 'untagged', name: 'old', secret: secretOf(8, 64)},
  ]);
  assert.deepEqual(rooms(nodes), ['draw_test']);
  controller.setWorkspace('other-ws');
  assert.deepEqual(rooms(nodes), ['Lobby · general']);
  controller.setWorkspace(undefined);
  assert.deepEqual(rooms(nodes), []);
  controller.offerRoom({name: 'unplaced',
                        link: `https://relay.example/#nowhere.${secretOf(9, 64)}`});
  assert.equal(JSON.parse(store.get('mevedel-rooms')).length, 3);
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

// A name is optional: without one the host names the session, and the
// room is kept under the name the host reports. A typed name still needs
// a letter or digit.
{
  const {controller, nodes, sent} = build(64);
  nodes['new-session-button'].dispatch('click');
  nodes['new-session-name'].value = '   ';
  nodes['new-session-prompt'].value = 'Draw the pipeline';
  nodes['new-session'].close('create');
  assert.equal(sent.length, 1);
  assert.equal('name' in sent[0], false);
  assert.equal(sent[0].prompt, 'Draw the pipeline');
  assert.deepEqual(cards(nodes), ['New session · waiting for the host']);
  controller.showResult({reqId: 1, ok: true, name: '2026-10-05T17-00-abc',
                         link: `https://relay.example/#fresh.${secretOf(3, 64)}`});
  assert.deepEqual(cards(nodes), ['2026-10-05T17-00-abc · open']);
  assert.deepEqual(rooms(nodes), ['2026-10-05T17-00-abc']);
  nodes['new-session-name'].value = '///';
  nodes['new-session'].close('create');
  assert.equal(sent.length, 1, 'a typed name without a letter or digit is not sent');
}

// A lobby keeps itself among the rooms, so the rooms it opens list it;
// the tab standing in it does not.
{
  const {controller, nodes} = build(48);
  controller.rememberCurrent('Lobby · mevedel');
  assert.deepEqual(JSON.parse(store.get('mevedel-rooms')),
                   [{room: 'here', name: 'Lobby · mevedel',
                     secret: secretOf(1, 48), workspace: 'ws'}]);
  assert.deepEqual(rooms(nodes), []);
}

// The room a tab stands in is kept under the name its host gives it now,
// so a session titled after its first prompt is listed by that title.
{
  const {controller} = build(48);
  controller.rememberCurrent('2026-10-05T17-00-abc');
  controller.rememberCurrent('Pipeline diagram');
  assert.deepEqual(JSON.parse(store.get('mevedel-rooms')),
                   [{room: 'here', name: 'Pipeline diagram',
                     secret: secretOf(1, 48), workspace: 'ws'}]);
}

// The lobby opens the same sheet with its own explanation.
{
  const {controller, nodes} = build(64);
  controller.openNewSession('Starts a separate session in this project.');
  assert.equal(nodes['new-session'].open, true);
  assert.equal(textOf(nodes['new-session-lede']),
               'Starts a separate session in this project.');
  nodes['new-session'].close('cancel');
  nodes['new-session-button'].dispatch('click');
  assert.match(textOf(nodes['new-session-lede']), /separate room/);
}

console.log('viewer session controller passed');

// The picked model travels with the request; Default leaves it to the
// host, and a host that offers none shows no picker at all.
{
  const {nodes, sent} = build(64, null, ['Codex:gpt-6-astra']);
  nodes['new-session-button'].dispatch('click');
  assert.equal(nodes['new-session-model'].hidden, false);
  assert.equal(nodes['new-session-model-label'].hidden, false);
  nodes['new-session-name'].value = 'picked';
  nodes['new-session-prompt'].value = '';
  nodes['new-session-model'].value = 'Codex:gpt-6-astra';
  nodes['new-session'].close('create');
  nodes['new-session-button'].dispatch('click');
  // The choice survives reopening while the host still offers it.
  assert.equal(nodes['new-session-model'].value, 'Codex:gpt-6-astra');
  nodes['new-session-name'].value = 'default';
  nodes['new-session-prompt'].value = '';
  nodes['new-session-model'].value = '';
  nodes['new-session'].close('create');
  assert.equal(sent[0].model, 'Codex:gpt-6-astra');
  assert.equal('model' in sent[1], false);
}
{
  const {nodes, sent} = build(64);
  nodes['new-session-model'].value = 'Codex:gpt-6-astra';
  nodes['new-session-button'].dispatch('click');
  assert.equal(nodes['new-session-model'].hidden, true);
  assert.equal(nodes['new-session-model-label'].hidden, true);
  nodes['new-session-name'].value = 'none';
  nodes['new-session-prompt'].value = '';
  nodes['new-session'].close('create');
  assert.equal('model' in sent[0], false);
}
