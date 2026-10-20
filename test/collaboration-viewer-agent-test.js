/* Focused retained-agent viewer controller assertions.
 * Run: node test/collaboration-viewer-agent-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');
const {navigation, nativeActivations} = require('./collaboration-viewer-content-links-support');

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
const artifacts = [];
let connected = true;
let poll = null;
const window = {};
const context = {
  window, document, console,
  setInterval(callback) { poll = callback; return 1; },
  clearInterval() { poll = null; },
};
load('relay/viewer/renderer.js', context);
load('relay/viewer/viewer-agent.js', context);
const href = navigation(context);
const controller = window.mevedelAgentView.create({
  href,
  send(frame) { sent.push(frame); return Promise.resolve(connected); },
  el: (tag, className, text) => element(document, tag, className, text),
  directiveLabel: () => '',
  openArtifact: record => artifacts.push(record),
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
// Nested agent notices retain native new-tab navigation and ordinary inspection.
controller.open({path: '/root/worker'});
controller.handle({reqId: sent.at(-1).reqId, final: true, records: [
  {id: 'notice-child', kind: 'agent-notice', sender: '/root/worker/child',
    outcome: 'completed', mailType: 'result', text: 'Done'},
]});
const notice = nodes['agent-transcript'].children[0].children[1].children[1];
const link = notice.children.at(-1);
const sentBeforeLink = sent.length;
nativeActivations(link, () => assert.equal(sent.length, sentBeforeLink));
link.dispatch('click');
assert.equal(sent.at(-1).path, '/root/worker/child');
nodes['agent-close'].dispatch('click');

// Child artifact anchors carry their exact transcript context, not a store
// alias. Both ordinary opening and native navigation see that same record.
const childRecord = {id: 'tool-child-secondary', kind: 'tool', artifact: 'child/second.html',
  store: 'child', title: 'Secondary file', size: 7};
controller.open({path: '/root/worker', segment: 3});
controller.handle({reqId: sent.at(-1).reqId, final: true, records: [childRecord]});
const descendants = node => [node, ...node.children.flatMap(descendants)];
const artifactLink = descendants(nodes['agent-transcript']).find(node => node.className.includes('artifact-open'));
const destination = JSON.parse(decodeURIComponent(artifactLink.href.split('destination=')[1]));
assert.deepEqual(destination.artifact.agent, {path: '/root/worker', segment: 3});
assert.equal(destination.artifact.id, childRecord.id);
nativeActivations(artifactLink, () => assert.equal(artifacts.length, 0));
artifactLink.dispatch('click');
assert.equal(artifacts.at(-1).id, childRecord.id);
assert.deepEqual({...artifacts.at(-1).agent}, {path: '/root/worker', segment: 3});
assert.equal(childRecord.agent, undefined, 'rendering does not mutate host records');
controller.close();
artifacts.length = 0;

const requested = {id: childRecord.id, agent: {path: '/root/worker', segment: 3}, version: 2};
controller.resolveArtifact(requested);
const resolvingReq = sent.at(-1).reqId;
assert.deepEqual({...sent.at(-1)}, {t: 'fetch-agent', reqId: resolvingReq, path: '/root/worker', segment: 3});
assert.equal(poll, null);
assert.equal(controller.describe().artifact, requested, 'pending reload retains the intended artifact');
controller.handle({reqId: resolvingReq - 1, final: true, records: [childRecord]});
assert.equal(artifacts.length, 0);
controller.handle({reqId: resolvingReq, final: false,
  records: [{...childRecord, id: 'different', artifact: 'child/index.html'}]});
assert.equal(artifacts.length, 0, 'no artifact fetch before the complete transcript');
controller.handle({reqId: resolvingReq, final: true, records: [childRecord]});
assert.equal(artifacts.length, 1);
assert.deepEqual({...artifacts[0], agent: {...artifacts[0].agent}}, {...childRecord, agent: requested.agent, version: 2});
assert.equal(controller.describe(), null, 'resolution clears before the open callback');
assert.equal(nodes['agent-panel'].hidden, true);

for (const response of [
  {final: true, records: [{...childRecord, id: 'different'}]},
  {final: true, records: [{...childRecord, missing: true}]},
  {error: 'Agent archive refused'},
  {unchanged: true},
]) {
  controller.resolveArtifact(requested);
  const reqId = sent.at(-1).reqId;
  controller.handle({reqId, ...response});
  assert.equal(artifacts.length, 1, 'missing/refused context never falls back to another file');
  assert.match(textOf(nodes['agent-transcript']), /unavailable|refused|no records/);
  assert.equal(controller.describe().artifact, requested, 'failed reload retains its destination');
  controller.handle({reqId, final: true, records: [childRecord]});
  assert.equal(artifacts.length, 1, 'late success cannot overwrite a failed resolution');
}
controller.resolveArtifact(requested);
const cancelledReq = sent.at(-1).reqId;
controller.cancelArtifactResolution();
controller.handle({reqId: cancelledReq, final: true, records: [childRecord]});
assert.equal(artifacts.length, 1);
assert.equal(nodes['agent-panel'].hidden, true);
controller.open({path: '/root/worker'});
controller.cancelArtifactResolution();
assert.equal(nodes['agent-panel'].hidden, false, 'artifact cancellation leaves ordinary inspection intact');
controller.close();
controller.resolveArtifact(requested);
const supersededReq = sent.at(-1).reqId;
controller.open({path: '/root/another'});
controller.handle({reqId: supersededReq, final: true, records: [childRecord]});
assert.equal(artifacts.length, 1);
assert.equal(controller.describe().agent.path, '/root/another');
controller.close();
// The host deliberately sends no first reply while hydrating a cold live
// transcript. Existing live polling must retry; archives remain one-shot.
const coldRequested = {id: childRecord.id, agent: {path: '/root/cold'}};
controller.resolveArtifact(coldRequested);
const coldReq = sent.at(-1).reqId;
assert.equal(typeof poll, 'function', 'cold live artifact resolution keeps polling');
poll();
assert.notEqual(sent.at(-1).reqId, coldReq);
assert.equal(sent.at(-1).path, '/root/cold');
controller.handle({reqId: sent.at(-1).reqId, final: true, records: [childRecord]});
assert.equal(artifacts.length, 2);
assert.equal(poll, null, 'successful live resolution stops polling');
controller.resolveArtifact(coldRequested);
controller.handle({reqId: sent.at(-1).reqId, error: 'Unavailable'});
assert.equal(poll, null, 'failed live resolution stops polling');
assert.equal(controller.describe().artifact, coldRequested);
controller.close();
const beforeInvalid = sent.length;
controller.resolveArtifact({id: childRecord.id});
assert.equal(sent.length, beforeInvalid);
assert.match(textOf(nodes['agent-transcript']), /no valid agent context/);
controller.close();
connected = false;
controller.resolveArtifact(requested);
new Promise(resolve => setImmediate(resolve)).then(() => {
  assert.match(textOf(nodes['agent-transcript']), /Connection lost/);
  assert.equal(controller.describe().artifact, requested);
  assert.equal(artifacts.length, 2);
  controller.close();
  console.log('viewer agent passed');
}).catch(error => { console.error(error); process.exitCode = 1; });
