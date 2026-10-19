/* Focused task presentation: node test/collaboration-viewer-task-test.js */
'use strict';
const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');
const list = new Element('ul');
const document = {getElementById: id => id === 'tasks-list' ? list : null,
  createElement: tag => new Element(tag)};
const window = {};
load('relay/viewer/viewer-task.js', {window, document});
const controller = window.mevedelTaskView.create({
  el: (tag, cls, text) => element(document, tag, cls, text), summarize() {},
});
controller.show({t: 'tasks', total: 2, completed: 0, omitted: 0, omittedActive: 0, tasks: [
  {id: 1, subject: 'Fix propagation', activeForm: 'Fixing propagation',
    status: 'in-progress', owner: '/root/worker-1', note: 'Checking evidence'},
  {id: 2, subject: 'Verify', status: 'pending', blockedBy: [1]},
]});
assert.match(textOf(list.children[0]), /#1.*Fixing propagation/);
assert.match(textOf(list.children[0]), /Checking evidence/);
assert.match(textOf(list.children[1]), /#2.*Verify.*blocked by #1/);
assert.equal(list.children[0].children.find(el => el.className === 'task-subject').title,
             'Fix propagation');
console.log('viewer task passed');
