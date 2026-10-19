/* Canonical artifact conversation retention and bounded polling.
 * Run: node test/collaboration-artifact-history-test.js
 */
'use strict';
const assert = require('node:assert/strict');
const {Element, element, load} = require('./collaboration-viewer-dom');

async function main() {
  const timers = new Map();
  let timerId = 0, busy = false, delayedSend = false, releaseSend, sendFailure = null, writable = false;
  const opened = [];
  const frames = [], updates = [];
  const sidebar = {view: {kind: 'list'}};
  const document = {createElement: tag => new Element(tag)};
  const window = {mevedelDiscussionSidebar: {create(options) {
    sidebar.options = options;
    return {update(next = {}) { updates.push(next); Object.assign(sidebar, next); options.onRender(); },
      view: () => sidebar.view, show(view) { sidebar.view = view; options.onRender(); },
      reset() { sidebar.view = {kind: 'list'}; }, unread: () => false, unreadCount: () => 0,
      notice(text) { sidebar.notice = text; }, focus() {}};
  }}};
  load('relay/viewer/viewer-artifact-discussions.js', {
    window, document, console, TextEncoder, Uint8Array,
    setTimeout(fn) { const id = ++timerId; timers.set(id, fn); return id; },
    clearTimeout(id) { timers.delete(id); },
    crypto: require('node:crypto').webcrypto, addEventListener() {},
  });
  const side = new Element('div'); side.hidden = true;
  const controller = window.mevedelArtifactDiscussions.create({
    send(frame) {
      frames.push(frame);
      if (sendFailure === 'throw') throw new Error('Sealing failed');
      if (sendFailure === 'reject') return Promise.reject(new Error('Transport failed'));
      if (sendFailure === 'false') return Promise.resolve(false);
      return delayedSend && frame.action === 'history'
        ? new Promise(resolve => { releaseSend = resolve; }) : Promise.resolve(true);
    },
    el: (tag, cls, text) => element(document, tag, cls, text),
    side, sideRoot: new Element('div'), canDiscuss: () => writable, busy: () => busy,
    openConversation: id => opened.push(id),
  });
  const frame = new Element('iframe'); frame.contentWindow = {postMessage() {}};
  const flush = () => new Promise(resolve => setImmediate(resolve));
  const answer = async (request, fields) => {
    controller.handle({reqId: request.reqId, ...fields}); await flush();
  };
  controller.attach(frame, 'artifact:page', 'page');
  await flush();
  const first = frames.find(f => f.action === 'history');
  assert.ok(first, 'opening an artifact must request item-scoped canonical history');
  const history = [
    {id: 'history:s:u1', kind: 'user', shared: {kind: 'artifact', itemId: 'artifact:page',
      questionId: 'q1', discussionId: 'general', messageId: 'm1', text: 'Review'}},
    {id: 'history:s:a1', kind: 'assistant', text: 'Archived answer'},
  ];
  await answer(first, {conversation: history, conversationDigest: 'digest-1',
                       conversationTruncated: false});
  assert.deepEqual(JSON.parse(JSON.stringify(sidebar.records)), history,
                   'a lobby with no live transcript still displays the answer');
  // Archival metadata and unrelated busy work must not erase or animate it.
  controller.records([{id: 'catalog', artifact: 'page/index.html'},
                      {id: 'room', kind: 'user', text: 'Unrelated'}]);
  busy = true; controller.activity();
  assert.equal(sidebar.records.at(-1).text, 'Archived answer');
  assert.equal(controller.discussions()[0].working, false);
  controller.records([{id: 'room-summary', kind: 'assistant', text: 'Unrelated summary'},
                      {id: 'catalog-tool', kind: 'tool', artifact: 'page/index.html'}]);
  assert.deepEqual(Array.from(sidebar.records, r => r.id), history.map(r => r.id),
                   'leading room records never become part of the last archived item turn');
  // A preserved live tail replaces the entire archived question, not half of it.
  controller.records([{id: 'live:u', kind: 'user', shared: history[0].shared},
                      {id: 'live:a', kind: 'assistant', text: 'Latest answer'}]);
  assert.deepEqual(Array.from(sidebar.records, r => r.id), ['live:u', 'live:a']);
  controller.records([]);
  const tick = [...timers.values()][0]; timers.clear(); tick(); await flush();
  const poll = frames.at(-1);
  assert.equal(poll.action, 'history'); assert.equal(poll.digest, 'digest-1');
  await answer(poll, {conversationUnchanged: true, conversationDigest: 'digest-1'});
  assert.equal(sidebar.records.at(-1).text, 'Archived answer');
  sidebar.view = {kind: 'thread', id: 'other'}; sidebar.options.onRender();
  const switched = frames.at(-1);
  assert.equal(switched.discussionId, 'other');
  await answer(switched, {conversation: history, conversationDigest: 'digest-2'});
  assert.equal(updates.at(-1).settled, false, 'thread switching must not settle existing unread marks');
  sidebar.view = {kind: 'list'}; sidebar.options.onRender();
  const returning = frames.at(-1);
  await answer(returning, {conversation: history, conversationDigest: 'digest-1'});
  assert.equal(updates.at(-1).settled, false, 'returning to the list preserves news in other discussions');
  const failPoll = [...timers.values()][0]; timers.clear(); failPoll(); await flush();
  await answer(frames.at(-1), {error: 'Archive unavailable'});
  assert.match(sidebar.notice, /Item history unavailable: Archive unavailable/);
  assert.equal(sidebar.records.at(-1).text, 'Archived answer', 'failed polls retain known answers');
  const retry = [...timers.values()][0]; timers.clear(); retry(); await flush();
  await answer(frames.at(-1), {conversationUnchanged: true, conversationDigest: 'digest-1'});
  assert.equal(sidebar.notice, '', 'successful retry clears the unavailable notice');
  controller.detach();
  assert.equal(timers.size, 0, 'closing the panel cancels history polling');
  controller.attach(frame, 'artifact:page', 'page'); await flush();
  assert.equal(frames.at(-1).action, 'list');
  const reopen = frames.filter(f => f.action === 'history').at(-1);
  controller.records([{id: 'new-user', kind: 'user', shared: history[0].shared},
                      {id: 'new-answer', kind: 'assistant', text: 'New while opening'}]);
  await answer(reopen, {conversation: history, conversationDigest: 'digest-1'});
  assert.equal(updates.at(-1).settled, false, 'delayed initial history cannot mark newer live answers seen');
  controller.records([]);
  assert.equal(sidebar.records.at(-1).text, 'Archived answer', 'reopening restores archived answers');
  controller.detach();
  controller.attach(frame, 'artifact:page', 'page'); await flush();
  const decisionRequest = frames.filter(f => f.action === 'history').at(-1);
  const beforeDecision = frames.length;
  await answer(decisionRequest, {conversation: history, conversationDigest: 'decision-1',
                                conversationDecision: {pending: true, title: 'Architecture review'}});
  let extras = sidebar.options.extras(controller.discussions()[0]);
  assert.equal(extras.status, 'Waiting for a decision in Architecture review');
  assert.equal(extras.actions?.length || 0, 0, 'view links retain the current read-only Conversation boundary');
  assert.equal(opened.length, 0, 'a pending interaction never opens or shares its conversation automatically');
  assert.equal(frames.length, beforeDecision);
  writable = true; controller.activity();
  extras = sidebar.options.extras(controller.discussions()[0]);
  assert.equal(extras.actions[0].label, 'Open conversation');
  extras.actions[0].run();
  assert.deepEqual(opened, ['page'], 'explicit opening uses the existing store Conversation callback');
  writable = false;
  const decisionPoll = [...timers.values()][0]; timers.clear(); decisionPoll(); await flush();
  await answer(frames.at(-1), {conversation: history, conversationDigest: 'decision-2', conversationDecision: null});
  assert.doesNotMatch(sidebar.options.extras(controller.discussions()[0]).status, /Waiting for a decision/);
  controller.detach();
  // Real transport sealing can still be pending when the panel closes.
  // Cancellation must already have a rejection owner, before send settles.
  const unhandled = [];
  const onUnhandled = error => unhandled.push(error);
  process.on('unhandledRejection', onUnhandled);
  try {
    delayedSend = true;
    controller.attach(frame, 'artifact:page', 'page');
    controller.detach();
    await flush();
    assert.equal(unhandled.length, 0, 'closing during async send must not leave an unowned history rejection');
    releaseSend(true);
    await flush();
    assert.equal(unhandled.length, 0);
  } finally {
    process.removeListener('unhandledRejection', onUnhandled);
    controller.detach();
  }
  delayedSend = false;
  for (const [failure, message] of [['throw', /Sealing failed/], ['reject', /Transport failed/],
                                  ['false', /Connection lost/]]) {
    sendFailure = failure;
    await assert.rejects(controller.discuss('artifact:page', 'Review again'), message);
  }
  console.log('artifact history passed');
}
main().catch(error => { console.error(error); process.exitCode = 1; });
