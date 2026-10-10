/* Focused presence assertions: reported pages and who else is here.
 * Run: node test/collaboration-viewer-presence-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const plain = value => JSON.parse(JSON.stringify(value));

function build() {
  const holders = [new Element('span'), new Element('span')];
  const document = {
    hidden: false,
    listeners: {},
    addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); },
    querySelectorAll: selector => (selector === '[data-presence]' ? holders : []),
    createElement: tag => new Element(tag),
  };
  const window = {};
  load('relay/viewer/viewer-presence.js', {window, document});
  const sent = [];
  const current = {page: null};
  const presence = window.mevedelPresence.create({
    send: frame => sent.push(frame),
    el: (tag, className, text) => element(document, tag, className, text),
    page: () => current.page,
  });
  return {presence, document, holders, sent, current};
}

// The hello carries the page; a report goes out only when it changed.
{
  const {presence, document, sent, current} = build();
  assert.deepEqual(plain(presence.hello()), {page: null, active: true});
  presence.report();
  assert.deepEqual(sent, []);
  current.page = 'board';
  presence.report();
  presence.report();
  assert.deepEqual(plain(sent), [{t: 'viewing', page: 'board', active: true}]);
  // A hidden tab is away.
  document.hidden = true;
  document.listeners.visibilitychange.forEach(callback => callback());
  assert.deepEqual(plain(sent.at(-1)), {t: 'viewing', page: 'board', active: false});
  // A new connection's hello resets what was reported.
  assert.deepEqual(plain(presence.hello()), {page: 'board', active: false});
  presence.report();
  assert.equal(sent.length, 2);
}

// Every holder shows the same people; away ones are marked, a crowd is cut.
{
  const {presence, holders} = build();
  presence.show({people: [{name: 'Ann Lee', active: true}, {name: 'bob', active: false}]});
  for (const holder of holders) {
    assert.equal(holder.hidden, false);
    assert.deepEqual(holder.children.map(textOf), ['AL', 'B']);
    assert.deepEqual(holder.children.map(chip => chip.className),
                     ['presence-chip', 'presence-chip away']);
    assert.equal(holder.attributes['aria-label'], 'Also here: Ann Lee, bob (away)');
  }
  presence.show({people: ['A', 'B', 'C', 'D', 'E', 'F'].map(name => ({name, active: true}))});
  assert.deepEqual(holders[0].children.map(textOf), ['A', 'B', 'C', 'D', '+2']);
  presence.show({people: []});
  assert.equal(holders[0].hidden, true);
  assert.equal(holders[0].children.length, 0);
}

console.log('viewer presence passed');
