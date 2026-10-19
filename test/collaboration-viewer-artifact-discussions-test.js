/* Artifact panel wiring for discussions: the panel inlines the picker into
 * its frame, hears only that frame, gives the discussions sidebar the
 * artifact's discussions, and "Show in chat" closes the panel that covers the
 * conversation before revealing the discussion's turn.  The sidebar itself is
 * a stand-in here; test/collaboration-discussions.browser.mjs drives it.
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
    const classes = new Set();
    this.classList = {toggle: (name, on) => (on ? classes.add(name) : classes.delete(name)),
                      contains: name => classes.has(name)};
  }
  getBoundingClientRect() { return {left: 0, top: 0, width: 800, height: 600}; }
  remove() { if (this.parent) this.parent.children = this.parent.children.filter(x => x !== this); }
  insertBefore(node, reference) {
    if (!reference) return this.append(node);
    node.parent = this;
    this.children.splice(this.children.indexOf(reference), 0, node);
  }
}

const ids = ['artifacts-box', 'artifacts-summary', 'artifacts', 'artifact-panel',
             'artifact-title', 'artifact-meta', 'artifact-tab', 'artifact-download',
             'artifact-close', 'artifact-body', 'artifact-discussion', 'artifact-discussions',
             'artifact-side', 'artifact-discussion-sidebar', 'artifact-discussion-count',
             'artifact-side-close', 'artifact-peek'];
const nodes = Object.fromEntries(ids.map(id => [id, new Box('div')]));
nodes['artifact-panel'].hidden = true;
nodes['artifact-discussion'].hidden = true;
nodes['artifact-discussions'].hidden = true;
nodes['artifact-side'].hidden = true;
const listeners = {};
const document = {
  getElementById: id => nodes[id],
  createElement: tag => new Box(tag),
};
// The controller runs in its own realm; compare its values as plain data.
const plain = value => JSON.parse(JSON.stringify(value));
// The sidebar's interface as the controller uses it.
const sidebar = {updates: [], shown: []};
const window = {
  mevedelDiscussionSidebar: {create(options) {
    sidebar.options = options;
    return {
      // Each render reports back, as the sidebar does.
      update(next = {}) { sidebar.updates.push(next); Object.assign(sidebar, next); options.onRender(); },
      show(view) { sidebar.shown.push(view); sidebar.view = view; options.onRender(); },
      view: () => sidebar.view || {kind: 'list'},
      unread: () => false, unreadCount: () => 0,
      reset() { sidebar.view = {kind: 'list'}; },
      notice(text) { sidebar.notice = text; }, focus() {},
    };
  }},
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
assert.deepEqual({...sentFrames.at(-1)},
                 {t: 'discussion', reqId: sentFrames.at(-1).reqId, action: 'list', item: 'tool-1'});
const anchor = {kind: 'word', selector: '#lead', label: 'word "Hello"', quote: 'Hello', start: 0};
const discussion = (id, actor, text, extra = {}) =>
  ({id, target: anchor, quote: 'Hello', resolved: false,
    messages: [{id: `${id}-m`, actor, text}], ...extra});
assert.equal(nodes['artifact-discussions'].hidden, false, 'the sidebar toggle shows with the frame');
controller.storedDiscussions({item: 'artifact:page', discussions: [
  {id: 'general', target: {kind: 'whole'}, quote: '', resolved: false,
   messages: [{id: 'g1', actor: 'Alice', text: 'Overall?'}]},
  discussion('c1', 'Alice', 'Wave'),
  discussion('c2', 'Bob', 'Done', {resolved: true})]});
controller.storedDiscussions({item: 'artifact:other', discussions: []});
controller.render([
  {id: 'tool-1', kind: 'tool', artifact: 'page.html'},
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', itemId: 'artifact:page', title: 'page.html', questionId: 'c1-m', discussionId: 'c1', text: 'Wave'}},
  {id: 'a1', kind: 'assistant', text: 'Waved.'},
]);
const markers = posted.filter(message => message.t === 'discussion-markers').at(-1);
assert.deepEqual(JSON.parse(JSON.stringify(markers.markers)),
                 [{id: 'c1', n: 1, anchor, state: 'answered', unread: false}]);
// The sidebar lists the whole artifact first, then every anchored
// discussion, resolved ones included, with the transcript about this item.
const listed = id => sidebar.discussions.find(entry => entry.id === id);
assert.deepEqual(plain(sidebar.discussions.map(entry => [entry.id, entry.pin, entry.resolved])),
                 [['general', null, false], ['c1', 1, false], ['c2', null, true]]);
assert.deepEqual(plain(sidebar.records.map(record => record.id)), ['u1', 'a1']);
assert.equal(listed('c1').title, 'word "Hello"');
assert.equal(nodes['artifact-discussion-count'].textContent, '1');

// A delivered request spins its marker while the session's running turn is
// that request, through its first reply until the turn ends; a queued one
// spins too, a later user turn is someone else's work, and a turn that ended
// without a reply leaves the thread merely sent.
const lastMarker = () => posted.filter(message => message.t === 'discussion-markers').at(-1).markers[0];
const delivered = [
  {id: 'tool-1', kind: 'tool', artifact: 'page.html'},
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', itemId: 'artifact:page', title: 'page.html', questionId: 'c1-m', discussionId: 'c1', text: 'Wave'}},
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
controller.queue([{id: 9, shared: {kind: 'artifact', itemId: 'artifact:page', discussionId: 'c1'}}]);
assert.equal(lastMarker().state, 'working');
controller.queue([]);
busy = false;
controller.render(delivered);
assert.equal(lastMarker().state, 'sent');
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'}]);
// The thread's newest request decides: a later unanswered one is merely sent,
// and a reply to another item's request answers nothing here.
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'},
                   {id: 'u3', kind: 'user', shared: {kind: 'artifact', itemId: 'artifact:page',
                                                     discussionId: 'c1', questionId: 'c1-n'}},
                   {id: 'u4', kind: 'user', shared: {kind: 'artifact', itemId: 'artifact:other',
                                                     discussionId: 'c1', questionId: 'x'}},
                   {id: 'a4', kind: 'assistant', text: 'Other answer.'}]);
assert.equal(lastMarker().state, 'sent');
controller.render([...delivered, {id: 'a1', kind: 'assistant', text: 'Waved.'}]);

// A marker message from any other window opens nothing.
const message = data => listeners.message.forEach(listener => listener(data));
message({source: {}, data: {mevedelDiscussion: 'open', id: 'c1',
                            rect: {left: 1, top: 1, width: 1, height: 1}}});
assert.equal(sidebar.shown.length, 0);
assert.equal(nodes['artifact-side'].hidden, true);
message({source: frame.contentWindow,
         data: {mevedelDiscussion: 'open', id: 'c1', rect: {left: 1, top: 1, width: 1, height: 1}}});
assert.deepEqual(plain(sidebar.shown.at(-1)), {kind: 'thread', id: 'c1'});
assert.equal(nodes['artifact-side'].hidden, false, 'a marker opens its discussion in the sidebar');
assert.equal(listed('c1').assistantState, 'answered');

// A selection sent alone reads as such in its thread.
controller.storedDiscussions({item: 'artifact:page', discussions: [
  discussion('c1', 'Alice', 'Wave'),
  {id: 'c5', target: anchor, quote: 'Hello', resolved: false,
   messages: [{id: 'c5-m', actor: 'Alice', text: '', alone: true}]}]});
assert.equal(listed('c5').messages[0].alone, true);

// A discussion answered in another session says where.
controller.storedDiscussions({item: 'artifact:page', discussions: [
  discussion('c1', 'Alice', 'Wave'),
  discussion('c9', 'Bob', 'Elsewhere', {session: 's2', sessionName: 'Design chat'})]});
assert.equal(sidebar.options.extras(listed('c9')).status, 'Answered in Design chat');

// The discussion about the whole artifact comes first, writes to the whole
// artifact and stays open.
controller.storedDiscussions({item: 'artifact:page', discussions: [
  {id: 'general', target: {kind: 'whole'}, quote: '', resolved: false,
   messages: [{id: 'g1', actor: 'Guest: Alice', text: 'Overall?'},
              {id: 'g2', actor: 'Bob', text: 'Shorter, please'}]},
  discussion('c1', 'Alice', 'Wave')]});
assert.deepEqual(plain(sidebar.discussions[0].messages.map(entry => [entry.actor, entry.text])),
                 [['Alice', 'Overall?'], ['Bob', 'Shorter, please']]);
assert.deepEqual(plain(sidebar.options.menu(sidebar.discussions[0]).map(entry => entry.label)), [],
                 'the general discussion stays open');
sidebar.options.submit('general', {text: 'Also the footer', toAssistant: true});
assert.deepEqual(JSON.parse(JSON.stringify(sentFrames.at(-1))),
                 {t: 'discussion', reqId: sentFrames.at(-1).reqId, action: 'post', item: 'tool-1',
                  target: {kind: 'whole'}, messageId: sentFrames.at(-1).messageId,
                  text: 'Also the footer', toAssistant: true});

// Show in chat waits in the discussion's menu.
controller.render([
  {id: 'u1', kind: 'user', guest: 'Alice',
   shared: {kind: 'artifact', itemId: 'artifact:page', title: 'page.html', questionId: 'c1-m', discussionId: 'c1', text: 'Wave'}},
  {id: 'a1', kind: 'assistant', text: 'Waved.'}]);
const entries = sidebar.options.menu(listed('c1'));
assert.deepEqual(plain(entries.map(entry => entry.label)), ['Show in chat', 'Resolve']);
entries[0].run();
assert.deepEqual(revealed, [{id: 'u1', panelHidden: true}],
                 'the panel closes before the turn is revealed');
assert.equal(nodes['artifact-side'].hidden, true, 'closing the panel closes its sidebar');

// A room message about the whole artifact goes to its general discussion,
// named by store id; the host decides whether the artifact takes messages.
controller.discuss('page', 'Tighten the intro');
assert.deepEqual(JSON.parse(JSON.stringify(sentFrames.at(-1))),
                 {t: 'discussion', reqId: sentFrames.at(-1).reqId, action: 'post',
                  item: 'artifact:page', target: {kind: 'whole'},
                  messageId: sentFrames.at(-1).messageId, text: 'Tighten the intro',
                  toAssistant: true});



console.log('viewer artifact discussions passed');
