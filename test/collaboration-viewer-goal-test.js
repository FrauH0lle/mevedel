/* Focused Goal inspection: node test/collaboration-viewer-goal-test.js */
'use strict';
const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');
const nodes = Object.fromEntries(['goal-sheet', 'goal-content', 'modeline']
  .map(id => [id, new Element('div')]));
nodes['goal-sheet'].showModal = function() { this.open = true; };
nodes['goal-sheet'].close = function() { this.open = false; };
const document = {getElementById: id => nodes[id], createElement: tag => new Element(tag)};
const window = {};
load('relay/viewer/viewer-goal.js', {window, document});
const controller = window.mevedelGoalView.create({
  el: (tag, cls, text) => element(document, tag, cls, text),
  onChange() { nodes.modeline.replaceChildren(); controller.renderChip(nodes.modeline); },
});
const goal = {id: 'goal-1', objective: 'Finish <script>review</script>', status: 'paused',
  reason: 'User paused', turns: 3, tokenBudget: 1000, tokensUsed: 400,
  incomplete: true, seconds: 25};
controller.show({t: 'goal', goal});
assert.match(textOf(nodes.modeline), /Goal paused.*3 turns.*≥400\/1000/);
nodes.modeline.children[0].dispatch('click');
assert.equal(nodes['goal-sheet'].open, true);
assert.match(textOf(nodes['goal-content']), /Finish <script>review<\/script>/);
assert.match(textOf(nodes['goal-content']), /lower bound.*Goal changes stay in Emacs/s);
controller.show({t: 'goal', goal: {...goal, status: 'complete', tokenBudget: null, incomplete: false}});
assert.match(textOf(nodes['goal-content']), /complete.*400\/unbounded/s);
assert.doesNotMatch(textOf(nodes['goal-content']), /lower bound/);
controller.show({t: 'goal', goal: null});
assert.equal(nodes.modeline.children.length, 0);
assert.equal(nodes['goal-sheet'].open, false);
console.log('viewer goal passed');
