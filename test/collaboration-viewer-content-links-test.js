/* Focused content renderer anchor semantics; browser routing is tested separately. */
'use strict';

const assert = require('node:assert/strict');
const {Element, load} = require('./collaboration-viewer-dom');
const {navigation, nativeActivations} = require('./collaboration-viewer-content-links-support');

const document = {createElement: tag => new Element(tag)};
const window = {};
const context = {window, document, console};
const href = navigation(context);
load('relay/viewer/renderer.js', context);
const renderer = window.mevedelTranscriptRenderer;
const opened = [];
const destinations = [];
const target = value => { destinations.push(JSON.parse(JSON.stringify(value))); return href(value); };
const render = record => renderer.renderRecord(record, null,
  value => opened.push(value), null, null, value => opened.push(value), target);
const content = turn => turn.children[1].children[1];

// Keep opaque transcript IDs and archived segment metadata intact. The renderer
// delegates URL construction instead of knowing room credentials or route syntax.
const record = {id: 'tool-call-7-artifact-a1b2c3d4e5f6', kind: 'tool',
  artifact: 'review.html', title: 'Review', segment: 3};
const card = content(render(record));
const open = card.children.at(-1);
assert.deepEqual(destinations.at(-1), {artifact: record});
nativeActivations(open, () => assert.equal(opened.length, 0));
open.dispatch('click');
assert.equal(opened[0], record);

const gone = content(render({...record, missing: true})).children.at(-1);
assert.equal(gone.tagName, 'button');
assert.equal(gone.disabled, true);
assert.equal(gone.href, undefined);

const notice = content(render({id: 'agent-notice-1', kind: 'agent-notice',
  sender: '/root/reviewer', outcome: 'completed', mailType: 'result', text: 'Reviewed'}));
const transcript = notice.children.at(-1);
assert.deepEqual(destinations.at(-1), {agent: {path: '/root/reviewer'}});
nativeActivations(transcript, () => assert.equal(opened.length, 1));
transcript.dispatch('click');
assert.equal(opened[1], '/root/reviewer');

console.log('viewer content links passed');
