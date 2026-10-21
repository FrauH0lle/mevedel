/* Run the generated Markdown bundle in a real DOM, then adapt its sanitized
 * HTML for the lightweight DOMs used by deterministic viewer tests.
 */
'use strict';

const fs = require('node:fs');
const {createRequire} = require('node:module');
const {resolve} = require('node:path');
const requireEditing = createRequire(resolve(__dirname, '../shared-editing/package.json'));
const {JSDOM} = requireEditing('jsdom');
let browser;

function installMarkdown(context, Element) {
  if (!browser) {
    browser = new JSDOM('', {runScripts: 'outside-only'});
    browser.window.eval(fs.readFileSync(resolve(__dirname, '../relay/viewer/markdown.js'), 'utf8'));
  }
  context.window.mevedelMarkdown = browser.window.mevedelMarkdown;
  if (Object.getOwnPropertyDescriptor(Element.prototype, 'innerHTML')) return;

  function convert(node) {
    if (node.nodeType === 3) return node.data;
    const result = new Element(node.localName);
    for (const {name, value} of node.attributes) {
      result.setAttribute(name, value);
      if (name === 'class') result.className = value;
      if (name === 'href') result.href = value;
    }
    // The fake DOM stores leaf text separately from mixed text/element children.
    if (!node.children.length) result.textContent = node.textContent;
    else result.append(...[...node.childNodes].map(convert));
    return result;
  }

  Object.defineProperty(Element.prototype, 'innerHTML', {set(html) {
    const template = browser.window.document.createElement('template');
    template.innerHTML = html;
    this.textContent = '';
    this.replaceChildren();
    this.append(...[...template.content.childNodes].map(convert));
  }});
  if (!Object.getOwnPropertyDescriptor(Element.prototype, 'parentElement')) {
    Object.defineProperty(Element.prototype, 'parentElement', {get() { return this.parent; }});
  }
  if (!Element.prototype.replaceWith) Element.prototype.replaceWith = function (other) {
    const index = this.parent.children.indexOf(this);
    other.parent = this.parent;
    this.parent.children[index] = other;
  };
  const querySelectorAll = Element.prototype.querySelectorAll;
  Element.prototype.querySelectorAll = function (selector) {
    if (selector !== 'pre > code' && querySelectorAll) return querySelectorAll.call(this, selector);
    const found = [];
    for (const child of this.children) if (typeof child !== 'string') {
      if (selector === 'pre > code' && this.tagName === 'pre' && child.tagName === 'code') found.push(child);
      found.push(...child.querySelectorAll(selector));
    }
    return found;
  };
}

module.exports = {installMarkdown};
