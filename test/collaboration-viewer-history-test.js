/* Canonical chat boundaries retain lazy segment disclosures. */
'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
class Node {
  constructor(tag, className = '', textContent = '') {
    Object.assign(this, {tag, className, textContent, children: [], listeners: {}});
  }
  append(...nodes) { for (const node of nodes) { node.remove(); node.parent = this; this.children.push(node); } }
  replaceChildren(...nodes) { this.children.forEach(node => { node.parent = null; }); this.children = []; this.append(...nodes); }
  remove() { if (this.parent) this.parent.children = this.parent.children.filter(node => node !== this); this.parent = null; }
  setAttribute() {}
  addEventListener(type, fn) { this.listeners[type] = fn; }
}
const root = new Node('section');
const window = {mevedelTranscriptRenderer: {markContinuations() {}}};
vm.runInNewContext(fs.readFileSync('relay/viewer/viewer-history.js', 'utf8'), {
  window, document: {getElementById: () => root},
});
const sent = [];
const view = window.mevedelHistoryView.create({
  send: frame => { sent.push(frame); return true; }, el: (...args) => new Node(...args),
  renderRecord: record => new Node('p', '', record.text), onArtifacts() {},
});
const frame = {t: 'history-index', currentSegment: 4, final: true, records: [
  {kind: 'chat', number: 1, firstSegment: 1, lastSegment: 2, title: 'Earlier work'},
  {kind: 'chat', number: 2, firstSegment: 3, lastSegment: 4, title: 'Current work'},
  {id: 'artifact', artifact: 'diagram.html'},
]};
view.connection(true);
view.handle(frame);
assert.equal(root.children.length, 2, 'group compactions within clear-separated chats');
assert.match(root.children[0].children[0].textContent, /Earlier work/);
assert.match(root.children[1].children[0].textContent, /Current work/);
assert.match(root.children[1].children.at(-1).textContent, /Live conversation/);
assert.equal(sent.length, 0, 'chat headings do not fetch transcripts');
const segment = root.children[0].children[1];
segment.open = true;
segment.listeners.toggle();
assert.equal(sent[0].segment, 1);
view.handle({t: 'history', reqId: sent[0].reqId, segment: 1, final: true, records: [{text: 'Archived answer'}]});
view.handle({...frame, records: frame.records.map(row => row.number === 2 ? {...row, title: 'Renamed work'} : row)});
assert.equal(root.children[0].children[1], segment, 'updates retain disclosures and loaded bodies');
assert.equal(segment.open, true);
assert.match(root.children[1].children[0].textContent, /Renamed work/);
assert.equal(view.artifacts().length, 1, 'chat descriptors never become artifact entries');
console.log('viewer history assertions passed');
