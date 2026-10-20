/* Use the real navigation click policy with an injected, credential-free URL fixture. */
'use strict';

const assert = require('node:assert/strict');
const {load} = require('./collaboration-viewer-dom');

function navigation(context) {
  load('relay/viewer/viewer-nav.js', context);
  return destination => `https://viewer.invalid/#fixture?destination=${encodeURIComponent(JSON.stringify(destination))}`;
}

function nativeActivations(node, unchanged) {
  assert.equal(node.tagName.toLowerCase(), 'a');
  assert.match(node.href, /^https:\/\/viewer\.invalid\/#fixture\?destination=/);
  for (const event of [{button: 1}, {button: 0, ctrlKey: true},
    {button: 0, metaKey: true}, {button: 0, shiftKey: true}, {button: 0, altKey: true}]) {
    let prevented = false;
    node.dispatch('click', {...event, preventDefault() { prevented = true; }});
    assert.equal(prevented, false);
    unchanged();
  }
}

module.exports = {navigation, nativeActivations};
