/* collaboration-viewer-test.js -- deterministic viewer protocol assertions */
'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

class Element {
  constructor(tag) {
    this.tagName = tag;
    this.children = [];
    this.listeners = {};
    this.textContent = '';
    this.className = '';
    this.dataset = {};
    this.hidden = false;
    this.scrollHeight = 100;
  }
  append(...children) { this.children.push(...children); }
  replaceChildren(...children) { this.children = children; }
  remove() {
    if (this.parent) this.parent.children = this.parent.children.filter(x => x !== this);
  }
  addEventListener(type, callback) {
    (this.listeners[type] ||= []).push(callback);
  }
  dispatch(type, event = {}) {
    for (const callback of this.listeners[type] || []) callback(event);
  }
}

class Socket {
  static OPEN = 1;
  constructor() { this.listeners = {}; this.readyState = Socket.OPEN; this.sent = []; }
  addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); }
  send(value) { this.sent.push(value); }
  close() { this.readyState = 3; this.dispatch('close', {code: 1006}); }
  dispatch(type, event = {}) {
    for (const callback of this.listeners[type] || []) callback(event);
  }
}

const nodes = Object.fromEntries(['transcript', 'connection', 'notice', 'live-button']
  .map(id => [id, new Element('div')]));
nodes.transcript.scrollHeight = 100;
const sockets = [];
let timer;
const window = {
  location: {href: 'http://127.0.0.1:1/index.html#room.secret', hash: '#room.secret',
             pathname: '/index.html', search: ''},
  history: {replaceState(_state, _title, path) { window.replaced = path; }},
  innerHeight: 20,
  scrollY: 0,
  listeners: {},
  addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); },
  setTimeout(callback) { timer = callback; return 1; },
  scrollTo(options) { this.scrollY = options.top; },
};
const document = {
  getElementById(id) { return nodes[id]; },
  createElement(tag) { return new Element(tag); },
  documentElement: {scrollHeight: 100},
};
nodes.transcript.append = Element.prototype.append;
class TestWebSocket extends Socket {
  constructor() { super(); sockets.push(this); }
}
const context = {document, window, WebSocket: TestWebSocket, URL, setTimeout: window.setTimeout,
                 console};
vm.runInNewContext(fs.readFileSync('collaboration/viewer.js', 'utf8'), context);

const credentials = context.window.mevedelViewer.parseFragment('#room.token');
assert.equal(credentials.room, 'room');
assert.equal(credentials.token, 'token');
assert.equal(window.replaced, '/index.html');
assert.equal(sockets.length, 1);
const first = sockets[0];
first.dispatch('open');
assert.deepEqual(JSON.parse(first.sent[0]), {type: 'auth', version: 1, room: 'room', token: 'secret'});
const send = (message) => first.dispatch('message', {data: JSON.stringify(message)});
send({type: 'snapshot-begin', snapshot: 's1', seq: 1, 'ack-token': 'a'});
send({type: 'snapshot-chunk', snapshot: 's1', records: [
  {id: 'assistant', kind: 'assistant', revision: 0, text: '<b>inert</b>'},
  {id: 'tool', kind: 'tool', revision: 0, name: 'Bash', status: 'completed',
   summary: 'Bash', result: 'large', truncated: true},
]});
send({type: 'record', record: {id: 'assistant', kind: 'assistant', revision: 1,
                                text: 'stream replacement'}, seq: 2, 'ack-token': 'b'});
send({type: 'snapshot-end', snapshot: 's1', seq: 3, 'ack-token': 'c'});
assert.equal(nodes.transcript.children.length, 2);
assert.equal(nodes.transcript.children[0].children[1].textContent, 'stream replacement');
assert.match(nodes.transcript.children[1].children[1].children[0].textContent, /truncated/);
assert.equal(nodes.transcript.children[0].children[1].tagName, 'pre');
assert.equal(first.sent.filter(x => JSON.parse(x).type === 'ack').length, 3);
send({type: 'record', record: {id: 'assistant', kind: 'assistant', revision: 2,
                                text: 'newest'}});
assert.equal(nodes.transcript.children.length, 2);
assert.equal(nodes.transcript.children[0].children[1].textContent, 'newest');
first.dispatch('close', {code: 1006});
assert.equal(typeof timer, 'function');
timer();
assert.equal(sockets.length, 2);
assert.equal(typeof timer, 'function');
sockets[1].dispatch('close', {code: 1013});
assert.equal(typeof timer, 'function');
timer();
assert.equal(sockets.length, 3);
timer = null;
sockets[2].dispatch('close', {code: 1008});
assert.equal(nodes.connection.textContent, 'Link rejected');
assert.equal(timer, null);
// A terminal authentication rejection must not schedule another reconnect.
send({type: 'status', status: 'ended'});
assert.equal(nodes.connection.textContent, 'Session ended');
console.log('viewer protocol passed');
