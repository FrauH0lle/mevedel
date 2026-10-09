/* Artifact panel wiring for comments: the panel inlines the picker into its
 * frame, hears only that frame, and "Show in chat" closes the panel that
 * covers the conversation before revealing the comment's turn.
 * Run: node test/collaboration-viewer-artifact-comments-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

class Box extends Element {
  constructor(tag) {
    super(tag);
    this.style = {};
    this.offsetHeight = 120;
  }
  getBoundingClientRect() { return {left: 0, top: 0, width: 800, height: 600}; }
  remove() { if (this.parent) this.parent.children = this.parent.children.filter(x => x !== this); }
}

const ids = ['artifacts-box', 'artifacts-summary', 'artifacts', 'artifact-panel',
             'artifact-title', 'artifact-meta', 'artifact-tab', 'artifact-download',
             'artifact-close', 'artifact-body', 'artifact-comment'];
const nodes = Object.fromEntries(ids.map(id => [id, new Box('div')]));
nodes['artifact-panel'].hidden = true;
nodes['artifact-comment'].hidden = true;
const listeners = {};
const document = {
  getElementById: id => nodes[id],
  createElement: tag => new Box(tag),
};
const window = {
  mevedelTranscriptRenderer: {
    formatBytes: size => `${size} B`,
    renderMarkdown: text => element(document, 'article', 'prose', text),
  },
};
const context = {
  window, document, console, TextDecoder, TextEncoder, Uint8Array, atob,
  URL, Blob: class {}, crypto: require('node:crypto').webcrypto,
  localStorage: {getItem: () => null, setItem: () => {}},
  addEventListener: (type, callback) => (listeners[type] ||= []).push(callback),
};
load('relay/viewer/viewer-artifact-comments.js', context);
load('relay/viewer/viewer-artifact.js', context);

const revealed = [];
const sentFrames = [];
let busy = false;
const controller = window.mevedelArtifactView.create({
  send: frame => { sentFrames.push(frame); return Promise.resolve(true); },
  el: (tag, className, text) => element(document, tag, className, text),
  flash: () => {}, summarize: () => {},
  reveal: id => revealed.push({id, panelHidden: nodes['artifact-panel'].hidden}),
  canComment: () => true,
  busy: () => busy,
});

controller.open({id: 'tool-1', artifact: 'page/page.html', store: 'page'});
const html = '<p id="lead">Hello</p>';
controller.handle({reqId: 1, mime: 'text/html', size: html.length,
                   data: Buffer.from(html).toString('base64'), final: true});
const frame = nodes['artifact-body'].children[0];
// Only the panel frame carries the picker, ahead of the artifact.
assert.match(frame.srcdoc, /mevedelArtifactCommentRuntime[^]*<p id="lead">Hello<\/p>$/);
assert.equal(nodes['artifact-comment'].hidden, false);
const posted = [];
frame.contentWindow = {postMessage: data => posted.push(data)};

// Opening the artifact lists its stored comments; the store's broadcast
// and the transcript's reply become one answered marker.
assert.deepEqual({...sentFrames.at(-1)}, {t: 'artifact-comment', reqId: 1, action: 'list', id: 'tool-1'});
const anchor = {kind: 'word', selector: '#lead', label: 'word "Hello"', quote: 'Hello', start: 0};
controller.storedComments({artifact: 'page', comments: [
  {id: 'c1', actor: 'Alice', text: 'Wave', anchor, resolved: false, replies: []},
  {id: 'c2', actor: 'Bob', text: 'Done', anchor, resolved: true, replies: []}]});
controller.storedComments({artifact: 'other', comments: []});
controller.render([
  {id: 'tool-1', kind: 'tool', artifact: 'page.html'},
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', artifact: 'page.html', questionId: 'c1', commentId: 'c1', text: 'Wave'}},
  {id: 'a1', kind: 'assistant', text: 'Waved.'},
]);
const markers = posted.filter(message => message.t === 'comment-markers').at(-1);
assert.deepEqual(JSON.parse(JSON.stringify(markers.markers)),
                 [{id: 'c1', n: 1, anchor, state: 'answered'}]);

// A delivered request spins its marker while the session's running turn is
// that request, through its first reply until the turn ends; a queued one
// spins too, a later user turn is someone else's work, and a turn that ended
// without a reply leaves the thread merely sent.
const lastMarker = () => posted.filter(message => message.t === 'comment-markers').at(-1).markers[0];
const delivered = [
  {id: 'tool-1', kind: 'tool', artifact: 'page.html'},
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', artifact: 'page.html', questionId: 'c1', commentId: 'c1', text: 'Wave'}},
];
controller.render(delivered);
assert.equal(lastMarker().state, 'sent');
busy = true;
controller.activity();
assert.equal(lastMarker().state, 'working');
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'}]);
assert.equal(lastMarker().state, 'working');
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'},
                   {id: 'u2', kind: 'user', text: 'Something else'}]);
assert.equal(lastMarker().state, 'answered');
busy = false;
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'}]);
assert.equal(lastMarker().state, 'answered');
busy = true;
controller.queue([{id: 9, shared: {kind: 'artifact', commentId: 'c1'}}]);
assert.equal(lastMarker().state, 'working');
controller.queue([]);
busy = false;
controller.render(delivered);
assert.equal(lastMarker().state, 'sent');
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'}]);

// A marker message from any other window opens nothing.
const message = data => listeners.message.forEach(listener => listener(data));
message({source: {}, data: {mevedelComment: 'open', id: 'c1',
                            rect: {left: 1, top: 1, width: 1, height: 1}}});
assert.equal(nodes['artifact-body'].children.length, 1);
message({source: frame.contentWindow,
         data: {mevedelComment: 'open', id: 'c1', rect: {left: 1, top: 1, width: 1, height: 1}}});
const card = nodes['artifact-body'].children.find(child => child.className === 'artifact-comment-card');
assert.match(textOf(card), /Alice · Answered/);
assert.match(textOf(card), /Waved\./);

// A thread answered in another session says where.
controller.storedComments({artifact: 'page', comments: [
  {id: 'c1', actor: 'Alice', text: 'Wave', anchor, resolved: false, replies: []},
  {id: 'c9', actor: 'Bob', text: 'Elsewhere', anchor: {kind: 'word', selector: '#lead',
                                                     label: 'Lead', quote: 'Hello', start: 0},
   resolved: false, replies: [], session: 's2', sessionName: 'Design chat'}]});
message({source: frame.contentWindow,
         data: {mevedelComment: 'open', id: 'c9', rect: {left: 1, top: 1, width: 1, height: 1}}});
const answeredCard = nodes['artifact-body'].children.find(
  child => child.className === 'artifact-comment-card');
assert.match(textOf(answeredCard), /answered in Design chat/);

const find = (node, text) => node.textContent === text ? node
  : node.children.map(child => typeof child === 'string' ? null : find(child, text)).find(Boolean);
find(card, 'Show in chat').dispatch('click');
assert.deepEqual(revealed, [{id: 'u1', panelHidden: true}],
                 'the panel closes before the turn is revealed');

// A room message about the whole artifact names it by its store id; the
// host decides whether the artifact still takes messages.
controller.discuss('page', 'Tighten the intro');
assert.deepEqual(JSON.parse(JSON.stringify(sentFrames.at(-1))),
                 {t: 'artifact-comment', reqId: sentFrames.at(-1).reqId, action: 'ask',
                  id: 'artifact:page', questionId: sentFrames.at(-1).questionId,
                  text: 'Tighten the intro'});



console.log('viewer artifact comments passed');
