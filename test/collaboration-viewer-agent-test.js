/* Focused retained-agent viewer controller assertions.
 * Run: node test/collaboration-viewer-agent-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const ids = ['agent-panel', 'agent-title',
             'agent-meta', 'agent-close', 'agent-latest', 'agent-transcript'];
const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
nodes['agent-panel'].hidden = true;
const document = {
  getElementById: id => nodes[id],
  createElement: tag => new Element(tag),
};
const sent = [];
const notices = [];
let poll = null;
const window = {};
const context = {
  window, document, console,
  setInterval(callback) { poll = callback; return 1; },
  clearInterval() { poll = null; },
};
load('relay/viewer/renderer.js', context);
load('relay/viewer/viewer-agent.js', context);
const controller = window.mevedelAgentView.create({
  send(frame) { sent.push(frame); return Promise.resolve(true); },
  el: (tag, className, text) => element(document, tag, className, text),
  directiveLabel: () => '',
  openArtifact: () => {},
  flash: message => notices.push(message),
});

controller.show([
  {path: '/root/blocked', role: 'explorer', status: 'blocked'},
  {path: '/root/worker', role: 'worker', status: 'running'},
]);
// The unified work panel and durable notices open this inspection surface.
controller.open({path: '/root/worker'});
assert.equal(nodes['agent-panel'].hidden, false);
assert.equal(textOf(nodes['agent-title']), '/root/worker');
assert.deepEqual({...sent[0]},
                 {t: 'fetch-agent', reqId: 1, path: '/root/worker'});
controller.handle({reqId: 1, digest: 'digest-1', final: false,
                   records: [{id: 'u1', kind: 'user', text: 'Investigate'}]});
controller.handle({reqId: 1, digest: 'digest-1', final: true,
                   records: [{id: 'a1', kind: 'assistant', text: 'Found it'}]});
assert.match(textOf(nodes['agent-transcript']), /Investigate.*Found it/s);

poll();
assert.deepEqual({...sent[1]}, {t: 'fetch-agent', reqId: 2,
                               path: '/root/worker', known: 'digest-1'});
controller.handle({reqId: 2, digest: 'digest-1', unchanged: true});
controller.handle({reqId: 1, digest: 'stale', final: true, records: []});
assert.match(textOf(nodes['agent-transcript']), /Investigate.*Found it/s);

controller.show([{path: '/root/worker', role: 'worker', status: 'waiting'}]);
assert.match(textOf(nodes['agent-meta']), /waiting/);
assert.equal(sent[2].reqId, 3);
controller.show([]);
assert.equal(textOf(nodes['agent-meta']), 'settled');

controller.handle({reqId: 3, error: 'No longer available'});
assert.match(textOf(nodes['agent-transcript']), /No longer available/);
nodes['agent-close'].dispatch('click');
assert.equal(nodes['agent-panel'].hidden, true);
assert.equal(nodes['agent-transcript'].children.length, 0);
assert.equal(poll, null);

// A settled agent remains inspectable from its durable final notice.
controller.show([
  {path: '/root/worker', role: 'worker', status: 'running'},
  {path: '/root/blocked', role: 'explorer', status: 'errored'},
  {path: '/root/settled', role: 'explorer', status: 'done'},
  {path: '/root/interrupted', role: 'worker', status: 'interrupted'},
  {path: '/root/unknown', role: 'worker', status: 'unknown'},
]);
controller.open({path: '/root/settled'});
assert.equal(nodes['agent-panel'].hidden, false);
assert.equal(textOf(nodes['agent-title']), '/root/settled');
assert.equal(textOf(nodes['agent-meta']), 'explorer · done');
assert.equal(sent.at(-1).path, '/root/settled');
// A roster keeping the watched settled agent keeps its outcome meta;
// an emptied roster clears both surfaces and falls back to "settled".
controller.show([]);
assert.equal(textOf(nodes['agent-meta']), 'settled');
const skill = {id: 'skill', kind: 'tool', name: 'ToolCall', status: 'completed',
  presentation: {id: 'root', name: 'Skill', detail: 'dashboard', collapsed: true,
    body: 'Dashboard', attachments: [{id: 'attachment:base', name: 'Skill dependency',
      detail: 'base', body: 'Base instructions', collapsed: true}]}};
controller.handle({reqId: sent.at(-1).reqId, digest: 'skill-1', final: true, records: [skill]});
let skillTurn = nodes['agent-transcript'].children[0];
skillTurn.disclosures.get('root').open = true;
skillTurn.disclosures.get('root/attachment:base').open = true;
poll();
controller.handle({reqId: sent.at(-1).reqId, digest: 'skill-2', final: true, records: [skill]});
skillTurn = nodes['agent-transcript'].children[0];
assert.equal(skillTurn.disclosures.get('root/attachment:base').open, true);
nodes['agent-close'].dispatch('click');
assert.equal(poll, null);
controller.show([{path: '/root/model-check', status: 'waiting',
  backend: 'Child provider', model: 'child-model', effort: 'high'}]);
controller.open({path: '/root/model-check'});
assert.match(textOf(nodes['agent-meta']), /Child provider.*child-model:high/);
controller.show([{path: '/root/model-check', status: 'done', model: 'recovered'}]);
assert.match(textOf(nodes['agent-meta']), /recovered/);
assert.equal(controller.describe().agent.status, 'done');
assert.equal(controller.describe().agent.model, 'recovered');
nodes['agent-close'].dispatch('click');

// Source navigation loads the host-located immutable child archive only once;
// Latest explicitly returns to polling the current segment.
controller.open({path: '/root/model-check', segment: 2});
assert.equal(sent.at(-1).segment, 2);
assert.match(textOf(nodes['agent-meta']), /Segment 2/);
assert.equal(nodes['agent-latest'].hidden, false);
assert.equal(poll, null);
assert.equal(controller.describe().agent.segment, 2);
controller.handle({reqId: sent.at(-1).reqId, digest: 'archive-two', final: true,
  records: [{id: 'archive-answer', kind: 'assistant', text: 'Earlier child answer'}]});
assert.match(textOf(nodes['agent-transcript']), /Earlier child answer/);
nodes['agent-latest'].dispatch('click');
assert.equal(Object.hasOwn(sent.at(-1), 'segment'), false);
assert.equal(nodes['agent-latest'].hidden, true);
assert.equal(typeof poll, 'function');
nodes['agent-close'].dispatch('click');

// A host-located but unprojected source fails visibly once, rather than
// retaining a hidden auto-scroll request across later live polls.
let revealAttempts = 0;
window.mevedelTranscriptRenderer = {...window.mevedelTranscriptRenderer,
  revealTool() { revealAttempts++; return false; }};
controller.open({path: '/root/model-check', revealTool: 'omitted-source'});
controller.handle({reqId: sent.at(-1).reqId, digest: 'omitted-one', final: true, records: []});
assert.match(notices.at(-1) || '', /located source.*not.*browser/i);
poll();
controller.handle({reqId: sent.at(-1).reqId, digest: 'omitted-two', final: true, records: []});
assert.equal(revealAttempts, 1);
assert.equal(notices.length, 1);
nodes['agent-close'].dispatch('click');
console.log('viewer agent passed');
