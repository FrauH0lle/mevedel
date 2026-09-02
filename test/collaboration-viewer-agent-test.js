/* Focused retained-agent viewer controller assertions.
 * Run: node test/collaboration-viewer-agent-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const ids = ['agents', 'agents-done-list', 'agent-panel', 'agent-title',
             'agent-meta', 'agent-close', 'agent-transcript'];
const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
nodes.agents.hidden = true;
nodes['agent-panel'].hidden = true;
// The roster reports its counts into the shared session summary line,
// which viewer.js owns; here it is just recorded.
let summary = null;
const document = {
  getElementById: id => nodes[id],
  createElement: tag => new Element(tag),
};
const sent = [];
let poll = null;
const window = {mevedelTranscriptRenderer: {
  renderRecord(record) {
    return element(document, 'article', record.kind, record.text || '');
  },
}};
const context = {
  window, document, console,
  setInterval(callback) { poll = callback; return 1; },
  clearInterval() { poll = null; },
};
load('relay/viewer/viewer-agent.js', context);
const controller = window.mevedelAgentView.create({
  send(frame) { sent.push(frame); return Promise.resolve(true); },
  el: (tag, className, text) => element(document, tag, className, text),
  directiveLabel: () => '',
  openArtifact: () => {},
  summarize: (key, text, warning) => { summary = {key, text, warning}; },
});

controller.show([
  {path: '/root/blocked', role: 'explorer', status: 'blocked'},
  {path: '/root/worker', role: 'worker', status: 'running'},
]);
assert.equal(nodes.agents.hidden, false);
assert.equal(nodes.agents.children.length, 2);
assert.match(nodes.agents.children[0].className, /stuck/);
assert.match(textOf(nodes.agents), /explorer · blocked/);
// No settled agents yet, and an agent that is not running is a warning
// the collapsed summary has to carry.
assert.equal(nodes['agents-done-list'].children.length, 0);
assert.deepEqual(summary, {key: 'agents', text: '2 agents', warning: true});

nodes.agents.children[1].dispatch('click');
assert.equal(nodes['agent-panel'].hidden, false);
assert.equal(textOf(nodes['agent-title']), '/root/worker');
assert.deepEqual({...sent[0]},
                 {t: 'fetch-agent', reqId: 1, path: '/root/worker'});
controller.handle({reqId: 1, digest: 'digest-1', final: false,
                   records: [{id: 'u1', kind: 'user', text: 'Investigate'}]});
controller.handle({reqId: 1, digest: 'digest-1', final: true,
                   records: [{id: 'a1', kind: 'assistant', text: 'Found it'}]});
assert.match(textOf(nodes['agent-transcript']), /InvestigateFound it/);

poll();
assert.deepEqual({...sent[1]}, {t: 'fetch-agent', reqId: 2,
                               path: '/root/worker', known: 'digest-1'});
controller.handle({reqId: 2, digest: 'digest-1', unchanged: true});
controller.handle({reqId: 1, digest: 'stale', final: true, records: []});
assert.match(textOf(nodes['agent-transcript']), /InvestigateFound it/);

controller.show([{path: '/root/worker', role: 'worker', status: 'waiting'}]);
assert.match(textOf(nodes['agent-meta']), /waiting/);
assert.equal(sent[2].reqId, 3);
controller.show([]);
assert.equal(nodes.agents.hidden, true);
assert.equal(textOf(nodes['agent-meta']), 'settled');

controller.handle({reqId: 3, error: 'No longer available'});
assert.match(textOf(nodes['agent-transcript']), /No longer available/);
nodes['agent-close'].dispatch('click');
assert.equal(nodes['agent-panel'].hidden, true);
assert.equal(nodes['agent-transcript'].children.length, 0);
assert.equal(poll, null);

// A settled agent leaves the strip for the finished list, where its row
// still opens the transcript panel with its terminal outcome.
controller.show([
  {path: '/root/worker', role: 'worker', status: 'running'},
  {path: '/root/blocked', role: 'explorer', status: 'errored'},
  {path: '/root/settled', role: 'explorer', status: 'done'},
  {path: '/root/interrupted', role: 'worker', status: 'interrupted'},
  {path: '/root/unknown', role: 'worker', status: 'unknown'},
]);
assert.equal(nodes.agents.hidden, false);
assert.equal(nodes.agents.children.length, 1);
assert.deepEqual(summary,
                 {key: 'agents', text: '1 agent · 3 finished', warning: false});
assert.equal(nodes['agents-done-list'].children.length, 3);
assert.match(textOf(nodes['agents-done-list']), /explorer · errored/);
nodes['agents-done-list'].children[1].children[0].dispatch('click');
assert.equal(nodes['agent-panel'].hidden, false);
assert.equal(textOf(nodes['agent-title']), '/root/settled');
assert.equal(textOf(nodes['agent-meta']), 'explorer · done');
assert.equal(sent.at(-1).path, '/root/settled');
// A roster keeping the watched settled agent keeps its outcome meta;
// an emptied roster clears both surfaces and falls back to "settled".
controller.show([]);
assert.equal(nodes.agents.hidden, true);
assert.equal(nodes['agents-done-list'].children.length, 0);
assert.deepEqual(summary, {key: 'agents', text: '', warning: false});
assert.equal(textOf(nodes['agent-meta']), 'settled');
nodes['agent-close'].dispatch('click');
assert.equal(poll, null);

console.log('viewer agent passed');
