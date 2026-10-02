/* Focused session-artifact viewer controller assertions.
 * Run: node test/collaboration-viewer-artifact-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const {Element, element, load, textOf} = require('./collaboration-viewer-dom');

const ids = ['artifacts-box', 'artifacts-summary', 'artifacts',
             'artifact-panel', 'artifact-title', 'artifact-meta',
             'artifact-tab', 'artifact-download', 'artifact-delete', 'artifact-close',
             'artifact-body'];
const nodes = Object.fromEntries(ids.map(id => [id, new Element('div')]));
nodes['artifacts-box'].hidden = true;
nodes['artifact-panel'].hidden = true;
nodes['artifact-tab'].hidden = true;
nodes['artifact-download'].hidden = true;
nodes['artifact-delete'].hidden = true;
let confirmAnswer = true;
const created = [];
const revoked = [];
const links = [];
const opened = [];
const flashes = [];
let blockPopup = false;
const document = {
  getElementById: id => nodes[id],
  createElement(tag) {
    const node = new Element(tag);
    if (tag === 'a') links.push(node);
    return node;
  },
};
class TestURL extends URL {}
TestURL.createObjectURL = () => {
  const url = `blob:artifact-${created.length + 1}`;
  created.push(url);
  return url;
};
TestURL.revokeObjectURL = url => revoked.push(url);
class TestBlob {
  constructor(parts, options = {}) { this.parts = parts; this.type = options.type; }
}
const window = {
  confirm: () => confirmAnswer,
  mevedelTranscriptRenderer: {
    formatBytes: size => `${size} B`,
    renderMarkdown: text => element(document, 'article', 'prose', text),
  },
  open() {
    if (blockPopup) return null;
    const tab = {document: {
      title: '', body: new Element('body'),
      createElement: tag => new Element(tag),
    }};
    opened.push(tab);
    return tab;
  },
};
const context = {
  window, document, console, URL: TestURL, Blob: TestBlob,
  TextDecoder, Uint8Array, atob,
};
load('relay/viewer/viewer-artifact.js', context);
const sent = [];
let summary;
const controller = window.mevedelArtifactView.create({
  send(frame) { sent.push(frame); return Promise.resolve(true); },
  el: (tag, className, text) => element(document, tag, className, text),
  flash: message => flashes.push(message),
  summarize: (key, text) => { summary = {key, text}; },
  canDelete: () => true,
});

controller.render([
  {id: 'old', artifact: 'mockup.html', size: 10},
  {id: 'new', artifact: 'mockup.html', size: 20},
  {id: 'gone', artifact: 'gone.txt', missing: true},
]);
assert.equal(nodes['artifacts-box'].hidden, false);
assert.equal(nodes['artifacts-summary'].textContent, '2 artifacts');
assert.deepEqual(summary, {key: 'artifacts', text: '2 artifacts'});
assert.equal(nodes.artifacts.children.length, 2);
assert.match(textOf(nodes.artifacts), /mockup\.html20 B/);
assert.equal(nodes.artifacts.children[1].disabled, true);
assert.match(textOf(nodes.artifacts.children[1]), /deleted/);

nodes.artifacts.children[0].dispatch('click');
assert.deepEqual({...sent[0]}, {t: 'artifact-get', reqId: 1, id: 'new'});
assert.equal(nodes['artifact-panel'].hidden, false);
const html = '<h1>Mockup</h1><script>document.title=1</script>';
const encoded = Buffer.from(html).toString('base64');
controller.handle({reqId: 1, mime: 'text/html', size: html.length,
                   data: encoded.slice(0, 8), final: false});
assert.match(textOf(nodes['artifact-meta']), /Loading… \d+%/);
controller.handle({reqId: 1, mime: 'text/html', size: html.length,
                   data: encoded.slice(8), final: true});
const frame = nodes['artifact-body'].children[0];
assert.equal(frame.tagName, 'iframe');
assert.equal(frame.attributes.sandbox, 'allow-scripts');
assert.match(frame.srcdoc, /^<meta http-equiv="Content-Security-Policy"/);
assert.match(frame.srcdoc, /default-src 'none'/);
assert.match(frame.srcdoc, /<h1>Mockup<\/h1>/);
// The viewer theme is baked into the frame prelude and follows toggles by
// postMessage, since the opaque-origin frame cannot read the viewer's root.
assert.match(frame.srcdoc, /<script>[^]*set\(null\)[^]*<\/script><h1>Mockup/);
assert.match(frame.srcdoc, /addEventListener\("message"/);
const posted = [];
frame.contentWindow = {postMessage: (data, origin) => posted.push({data, origin})};
controller.setTheme('dark');
assert.equal(posted.length, 1);
assert.deepEqual({...posted[0].data}, {t: 'theme', theme: 'dark'});
assert.equal(posted[0].origin, '*');
controller.setTheme('system');
assert.deepEqual({...posted[1].data}, {t: 'theme', theme: null});
assert.equal(nodes['artifact-tab'].hidden, false);
assert.equal(nodes['artifact-download'].hidden, false);

nodes['artifact-tab'].dispatch('click');
assert.equal(opened[0].document.title, 'mockup.html');
assert.equal(opened[0].document.body.children[0].attributes.sandbox,
             'allow-scripts');
controller.setTheme('dark');
nodes['artifact-tab'].dispatch('click');
assert.match(opened[1].document.body.children[0].srcdoc, /set\("dark"\)/);
controller.setTheme(null);
blockPopup = true;
nodes['artifact-tab'].dispatch('click');
assert.match(flashes[0], /Popup blocked/);
blockPopup = false;
nodes['artifact-download'].dispatch('click');
assert.equal(links[0].download, 'mockup.html');
assert.equal(links[0].clicked, true);

controller.open({id: 'markdown', artifact: 'notes.md'});
assert.deepEqual({...sent[1]},
                 {t: 'artifact-get', reqId: 2, id: 'markdown'});
controller.handle({reqId: 1, mime: 'text/html', size: html.length,
                   data: encoded, final: true});
const markdown = '# Notes';
controller.handle({reqId: 2, mime: 'text/markdown', size: markdown.length,
                   data: Buffer.from(markdown).toString('base64'), final: true});
assert.match(nodes['artifact-body'].children[0].className, /artifact-prose/);
assert.match(textOf(nodes['artifact-body']), /# Notes/);
assert.equal(nodes['artifact-tab'].hidden, true);

controller.open({id: 'refused', artifact: 'gone.txt'});
controller.handle({reqId: 3, error: 'Deleted on the host'});
assert.match(textOf(nodes['artifact-body']), /Deleted on the host/);
nodes['artifact-close'].dispatch('click');
assert.equal(nodes['artifact-panel'].hidden, true);
assert.deepEqual(revoked, created);

(async () => {
// Writable links delete a loaded artifact; the host answers by request id.
controller.open({id: 'markdown', artifact: 'notes.md'});
assert.equal(nodes['artifact-delete'].hidden, true, 'nothing to delete while loading');
controller.handle({reqId: 4, mime: 'text/markdown', size: markdown.length,
                   data: Buffer.from(markdown).toString('base64'), final: true});
assert.equal(nodes['artifact-delete'].hidden, false);
const before = sent.length;
confirmAnswer = false;
nodes['artifact-delete'].dispatch('click');
await new Promise(resolve => setImmediate(resolve));
assert.equal(sent.length, before, 'a declined confirmation sends nothing');
confirmAnswer = true;
nodes['artifact-delete'].dispatch('click');
await new Promise(resolve => setImmediate(resolve));
assert.deepEqual({...sent.at(-1)}, {t: 'artifact-delete', reqId: 1, id: 'markdown'});
controller.handleDelete({reqId: 1, error: 'This artifact is no longer on the host'});
assert.equal(flashes.at(-1), 'This artifact is no longer on the host');
assert.equal(nodes['artifact-panel'].hidden, false);
nodes['artifact-delete'].dispatch('click');
await new Promise(resolve => setImmediate(resolve));
controller.handleDelete({reqId: 2, ok: true});
assert.equal(flashes.at(-1), 'notes.md deleted.');
assert.equal(nodes['artifact-panel'].hidden, true);
controller.handleDelete({reqId: 2, ok: true});
assert.equal(flashes.at(-1), 'notes.md deleted.', 'a stale reply is ignored');

controller.render([]);
assert.equal(nodes['artifacts-box'].hidden, true);
assert.deepEqual(summary, {key: 'artifacts', text: ''});

console.log('viewer artifact passed');
})().catch(error => { console.error(error); process.exitCode = 1; });
