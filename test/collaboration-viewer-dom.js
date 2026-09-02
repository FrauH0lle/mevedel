'use strict';

const fs = require('node:fs');
const vm = require('node:vm');

class Element {
  constructor(tag) {
    this.tagName = tag;
    this.children = [];
    this.listeners = {};
    this.attributes = {};
    this.className = '';
    this.textContent = '';
    this.hidden = false;
    this.scrollHeight = 100;
    this.scrollTop = 0;
    this.clientHeight = 100;
  }
  append(...children) {
    children.forEach(child => {
      if (child && typeof child !== 'string') child.parent = this;
    });
    this.children.push(...children);
  }
  replaceChildren(...children) {
    this.children = [];
    this.append(...children);
  }
  setAttribute(name, value) { this.attributes[name] = value; }
  addEventListener(type, callback) {
    (this.listeners[type] ||= []).push(callback);
  }
  dispatch(type, event = {}) {
    (this.listeners[type] || []).forEach(callback => callback(event));
  }
  click() { this.clicked = true; }
  focus() { this.focused = true; }
  // <dialog> is the whole modal, so the stub carries its three moving
  // parts: open state, a return value, and the close event.
  showModal() { this.open = true; }
  close(value) {
    this.open = false;
    if (value !== undefined) this.returnValue = value;
    this.dispatch('close');
  }
}

function element(document, tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (typeof text === 'string') node.textContent = text;
  return node;
}

function textOf(node) {
  if (typeof node === 'string') return node;
  return (node.textContent || '') + node.children.map(textOf).join('');
}

function load(path, context) {
  vm.runInNewContext(fs.readFileSync(path, 'utf8'), context);
}

module.exports = {Element, element, load, textOf};
