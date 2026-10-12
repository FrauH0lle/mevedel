/* Artifact panel wiring for discussions: the panel inlines the picker into
 * its frame, hears only that frame, and "Show in chat" closes the panel that
 * covers the conversation before revealing the discussion's turn.
 * Run: node test/collaboration-viewer-artifact-discussions-test.js
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
             'artifact-close', 'artifact-body', 'artifact-discussion', 'artifact-general'];
const nodes = Object.fromEntries(ids.map(id => [id, new Box('div')]));
nodes['artifact-panel'].hidden = true;
nodes['artifact-discussion'].hidden = true;
nodes['artifact-general'].hidden = true;
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
load('relay/viewer/viewer-artifact-discussions.js', context);
load('relay/viewer/viewer-artifact.js', context);

const revealed = [];
const sentFrames = [];
let busy = false;
const controller = window.mevedelArtifactView.create({
  send: frame => { sentFrames.push(frame); return Promise.resolve(true); },
  el: (tag, className, text) => element(document, tag, className, text),
  flash: () => {}, summarize: () => {},
  reveal: id => revealed.push({id, panelHidden: nodes['artifact-panel'].hidden}),
  canDiscuss: () => true,
  busy: () => busy,
});

controller.open({id: 'tool-1', artifact: 'page/page.html', store: 'page'});
const html = '<p id="lead">Hello</p>';
controller.handle({reqId: 1, mime: 'text/html', size: html.length,
                   data: Buffer.from(html).toString('base64'), final: true});
const frame = nodes['artifact-body'].children[0];
// Only the panel frame carries the picker, ahead of the artifact.
assert.match(frame.srcdoc, /mevedelArtifactDiscussionRuntime[^]*<p id="lead">Hello<\/p>$/);
assert.equal(nodes['artifact-discussion'].hidden, false);
const posted = [];
frame.contentWindow = {postMessage: data => posted.push(data)};

// Opening the artifact lists its stored discussions; the store's broadcast
// and the transcript's reply become one answered marker.  The general
// discussion about the whole artifact has no marker.
assert.deepEqual({...sentFrames.at(-1)}, {t: 'discussion', reqId: 1, action: 'list', item: 'tool-1'});
const anchor = {kind: 'word', selector: '#lead', label: 'word "Hello"', quote: 'Hello', start: 0};
const discussion = (id, actor, text, extra = {}) =>
  ({id, target: anchor, quote: 'Hello', resolved: false,
    messages: [{id: `${id}-m`, actor, text}], ...extra});
controller.storedDiscussions({item: 'artifact:page', discussions: [
  {id: 'general', target: {kind: 'whole'}, quote: '', resolved: false,
   messages: [{id: 'g1', actor: 'Alice', text: 'Overall?'}]},
  discussion('c1', 'Alice', 'Wave'),
  discussion('c2', 'Bob', 'Done', {resolved: true})]});
controller.storedDiscussions({item: 'artifact:other', discussions: []});
controller.render([
  {id: 'tool-1', kind: 'tool', artifact: 'page.html'},
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', title: 'page.html', questionId: 'c1-m', discussionId: 'c1', text: 'Wave'}},
  {id: 'a1', kind: 'assistant', text: 'Waved.'},
]);
const markers = posted.filter(message => message.t === 'discussion-markers').at(-1);
assert.deepEqual(JSON.parse(JSON.stringify(markers.markers)),
                 [{id: 'c1', n: 1, anchor, state: 'answered'}]);

// A delivered request spins its marker while the session's running turn is
// that request, through its first reply until the turn ends; a queued one
// spins too, a later user turn is someone else's work, and a turn that ended
// without a reply leaves the thread merely sent.
const lastMarker = () => posted.filter(message => message.t === 'discussion-markers').at(-1).markers[0];
const delivered = [
  {id: 'tool-1', kind: 'tool', artifact: 'page.html'},
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', title: 'page.html', questionId: 'c1-m', discussionId: 'c1', text: 'Wave'}},
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
controller.queue([{id: 9, shared: {kind: 'artifact', discussionId: 'c1'}}]);
assert.equal(lastMarker().state, 'working');
controller.queue([]);
busy = false;
controller.render(delivered);
assert.equal(lastMarker().state, 'sent');
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'}]);

// A marker message from any other window opens nothing.
const message = data => listeners.message.forEach(listener => listener(data));
message({source: {}, data: {mevedelDiscussion: 'open', id: 'c1',
                            rect: {left: 1, top: 1, width: 1, height: 1}}});
assert.equal(nodes['artifact-body'].children.length, 1);
message({source: frame.contentWindow,
         data: {mevedelDiscussion: 'open', id: 'c1', rect: {left: 1, top: 1, width: 1, height: 1}}});
const card = nodes['artifact-body'].children.find(child => child.className === 'artifact-discussion-card');
assert.match(textOf(card), /Alice · Answered/);
assert.match(textOf(card), /Waved\./);

// A selection sent alone reads as such in its thread.
controller.storedDiscussions({item: 'artifact:page', discussions: [
  discussion('c1', 'Alice', 'Wave'),
  {id: 'c5', target: anchor, quote: 'Hello', resolved: false,
   messages: [{id: 'c5-m', actor: 'Alice', text: '', alone: true}]}]});
message({source: frame.contentWindow,
         data: {mevedelDiscussion: 'open', id: 'c5', rect: {left: 1, top: 1, width: 1, height: 1}}});
assert.match(textOf(nodes['artifact-body'].children.find(
  child => child.className === 'artifact-discussion-card')), /Sent the selection as the request/);

// A discussion answered in another session says where.
controller.storedDiscussions({item: 'artifact:page', discussions: [
  discussion('c1', 'Alice', 'Wave'),
  discussion('c9', 'Bob', 'Elsewhere', {session: 's2', sessionName: 'Design chat'})]});
message({source: frame.contentWindow,
         data: {mevedelDiscussion: 'open', id: 'c9', rect: {left: 1, top: 1, width: 1, height: 1}}});
const answeredCard = nodes['artifact-body'].children.find(
  child => child.className === 'artifact-discussion-card');
assert.match(textOf(answeredCard), /answered in Design chat/);
controller.storedDiscussions({item: 'artifact:page', discussions: [
  discussion('c9', 'Bob', 'Elsewhere', {session: 's3', sessionName: 'Artifact chat'})]});
assert.match(textOf(answeredCard), /answered in Artifact chat/,
             'an open thread follows a changed answering session');

const findNode = (node, text) => node.textContent === text ? node
  : node.children.map(child => typeof child === 'string' ? null : findNode(child, text)).find(Boolean);
// The general discussion about the whole artifact opens from its own button,
// lists its messages and the latest answer, writes to the whole artifact and
// stays open.
const wholeButton = nodes['artifact-general'];
assert.equal(wholeButton.hidden, false);
controller.storedDiscussions({item: 'artifact:page', discussions: [
  {id: 'general', target: {kind: 'whole'}, quote: '', resolved: false,
   messages: [{id: 'g1', actor: 'Alice', text: 'Overall?'},
              {id: 'g2', actor: 'Bob', text: 'Shorter, please'}]}]});
assert.equal(wholeButton.textContent, 'Whole artifact · 2');
controller.render([{id: 'u9', kind: 'user', guest: 'Bob', shared: {kind: 'artifact', title: 'page.html',
  questionId: 'g2', discussionId: 'general', messageId: 'g2', text: 'Shorter, please'}},
                   {id: 'a9', kind: 'assistant', text: 'Shortened.'}]);
wholeButton.dispatch('click');
const wholeCard = nodes['artifact-body'].children.find(child => child.className === 'artifact-discussion-card');
assert.equal(wholeCard.dataset.discussion, 'general');
assert.match(textOf(wholeCard), /Whole artifact/);
assert.match(textOf(wholeCard), /Alice: Overall\?[^]*Bob: Shorter, please/);
assert.match(textOf(wholeCard), /Shortened\./);
assert.equal(findNode(wholeCard, 'Resolve'), undefined, 'the general discussion stays open');
const wholeForm = wholeCard.children.find(child => child.className === 'artifact-discussion-reply-form');
wholeForm.children.find(child => child.className === 'artifact-discussion-input').value = 'Also the footer';
wholeForm.dispatch('submit', {preventDefault() {}});
assert.deepEqual(JSON.parse(JSON.stringify(sentFrames.at(-1))),
                 {t: 'discussion', reqId: sentFrames.at(-1).reqId, action: 'post', item: 'tool-1',
                  target: {kind: 'whole'}, messageId: sentFrames.at(-1).messageId,
                  text: 'Also the footer', toAssistant: true});

const find = (node, text) => node.textContent === text ? node
  : node.children.map(child => typeof child === 'string' ? null : find(child, text)).find(Boolean);
find(card, 'Show in chat').dispatch('click');
assert.deepEqual(revealed, [{id: 'u1', panelHidden: true}],
                 'the panel closes before the turn is revealed');

// A room message about the whole artifact goes to its general discussion,
// named by store id; the host decides whether the artifact takes messages.
controller.discuss('page', 'Tighten the intro');
assert.deepEqual(JSON.parse(JSON.stringify(sentFrames.at(-1))),
                 {t: 'discussion', reqId: sentFrames.at(-1).reqId, action: 'post',
                  item: 'artifact:page', target: {kind: 'whole'},
                  messageId: sentFrames.at(-1).messageId, text: 'Tighten the intro',
                  toAssistant: true});



console.log('viewer artifact discussions passed');
