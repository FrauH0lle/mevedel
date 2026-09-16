/* Exercises the real browser renderer with host presentation records. */
'use strict';
const assert = require('node:assert/strict');
const {Element, load, textOf} = require('./collaboration-viewer-dom');
const window = {};
const document = {createElement(tag) {
  const node = new Element(tag);
  node.dataset = {};
  node.open = false;
  return node;
}};
load('relay/viewer/renderer.js', {window, document});
const renderer = window.mevedelTranscriptRenderer;
const record = {id: 'tool-envelope', kind: 'tool', name: 'ToolCall', status: 'completed',
  result: '<system-reminder>Raw dependency</system-reminder>',
  presentation: {id: 'root', name: 'Skill', detail: 'artifact-dashboard',
    header: 'Skill: artifact-dashboard (attached artifact)', status: 'completed',
    format: 'markdown', body: '# Dashboard\nReadable instructions', collapsed: true,
    attachments: [{id: 'attachment:artifact', name: 'Skill dependency', detail: 'artifact',
      format: 'markdown', body: '# Artifact\nDelivered dependency', collapsed: true}]}};
const first = renderer.renderRecord(record);
assert.match(textOf(first), /Skill/);
assert.match(textOf(first), /artifact-dashboard/);
assert.match(textOf(first), /Readable instructions/);
assert.doesNotMatch(textOf(first), /system-reminder|Raw dependency/);
assert.equal(first.disclosures.get('root').open, false);
assert.equal(first.disclosures.get('root/attachment:artifact').open, false);
first.disclosures.get('root').open = true;
first.disclosures.get('root/attachment:artifact').open = true;
const next = renderer.renderRecord(record, undefined, undefined, first);
assert.equal(next.disclosures.get('root').open, true);
assert.equal(next.disclosures.get('root/attachment:artifact').open, true);

const script = {...record, presentation: {id: 'root', name: 'ToolCall', status: 'completed',
  body: '', children: [
    {id: 'env/1', name: 'Read', detail: 'a.el', batch: '0', body: 'A', collapsed: true},
    {id: 'env/2', name: 'Read', detail: 'b.el', batch: '0', body: 'Error: missing', status: 'failed', collapsed: false},
    {id: 'returned', name: 'Returned', body: 'Done', collapsed: true},
  ]}};
const tree = renderer.renderRecord(script);
assert.match(textOf(tree), /Parallel/);
assert.equal(tree.disclosures.get('root/env/2').open, true);
tree.disclosures.get('root/env/2').open = false;
const refresh = renderer.renderRecord(script, undefined, undefined, tree);
assert.equal(refresh.disclosures.get('root/env/2').open, false);
assert.match(textOf(refresh), /Returned/);
// A replaced child keeps its explicit closed state, while a newly arriving
// sibling uses the host's default. Removed disclosures do not leak back in.
const changed = {...script, presentation: {...script.presentation, children: [
  {...script.presentation.children[1], body: 'Recovered'},
  {id: 'new', name: 'Read', body: 'New result', collapsed: false},
]}};
const changedTurn = renderer.renderRecord(changed, undefined, undefined, refresh);
assert.equal(changedTurn.disclosures.get('root/env/2').open, false);
assert.equal(changedTurn.disclosures.get('root/new').open, true);
assert.equal(changedTurn.disclosures.has('root/env/1'), false);
assert.match(textOf(changedTurn), /Recovered/);
console.log('Tool presentation renderer passed');
