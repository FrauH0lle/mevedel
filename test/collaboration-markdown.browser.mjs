/* Markdown parsing and sanitization in the actual file preview, with viewer CSS.
 *
 * Only the Markdown bundle, artifact controller, and renderer run; the host
 * transfer is supplied at the controller boundary, without a relay or Emacs.
 * Run: node --test test/collaboration-markdown.browser.mjs
 * Requires `npm ci --prefix shared-editing` and Playwright Chromium.
 */
import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {resolve} from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const {chromium} = require('playwright');
const fixturePath = 'test/fixtures/collaboration-nested-lists.md';
const markdown = await readFile(resolve(root, fixturePath), 'utf8');
const authors = [
  'Roland Goers roland.goers@swisstph.ch',
  'Myroslava Volosko myroslava.volosko@swisstph.ch',
];

async function openFile(page, source = markdown, file = fixturePath) {
  await page.route('http://localhost:8765/**', async route => {
    const path = new URL(route.request().url()).pathname;
    if (path === '/') {
      const html = await readFile(resolve(root, 'relay/viewer/index.html'), 'utf8');
      // Keep production markup and styles, but not unrelated session startup.
      const body = html.replace(/<script\b[^>]*src="([^"]+)"[^>]*><\/script>/g,
        (tag, src) => ['/markdown.js', '/renderer.js', '/viewer-artifact.js'].includes(src) ? tag : '');
      await route.fulfill({contentType: 'text/html', body});
    } else if (/\.(css|js)$/.test(path)) {
      await route.fulfill({
        contentType: path.endsWith('.css') ? 'text/css' : 'text/javascript',
        body: await readFile(resolve(root, 'relay/viewer', `.${path}`)),
      });
    } else {
      await route.fulfill({status: 204, body: ''});
    }
  });
  await page.goto('http://localhost:8765/');
  return page.evaluate(({path, data, size}) => {
    const sent = [];
    const controller = window.mevedelArtifactView.create({
      send: frame => { sent.push(frame); return Promise.resolve(true); },
      el: (tag, className, text) => {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (typeof text === 'string') node.textContent = text;
        return node;
      },
      flash: () => {},
    });
    controller.openFile(path);
    controller.handle({t: 'file', reqId: sent[0].reqId, name: path,
                       mime: 'text/markdown', size, data, final: true});
    return {sent, page: controller.describe()};
  }, {path: file, data: Buffer.from(source).toString('base64'),
      size: Buffer.byteLength(source)});
}

for (const width of [375, 1280]) {
  test(`project Markdown keeps nested authors and inline code at ${width}px`, async t => {
    const browser = await chromium.launch({headless: true});
    t.after(() => browser.close());
    const page = await browser.newPage({viewport: {width, height: 800}});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    const opened = await openFile(page);
    assert.deepEqual(opened.sent, [{t: 'file-get', path: fixturePath, reqId: 1}]);
    assert.deepEqual(opened.page, {file: fixturePath});
    assert.equal(await page.locator('#artifact-panel').isVisible(), true);

    const prose = page.locator('#artifact-body > .prose.artifact-prose');
    const top = prose.locator(':scope > ul > li');
    assert.equal(await prose.locator(':scope > ul').count(), 1);
    assert.equal(await top.count(), 4, 'pull, branches, identity, and network are siblings');
    const identity = top.nth(2);
    const nested = identity.locator(':scope > ul > li');
    assert.equal(await identity.locator(':scope > ul').count(), 1);
    assert.deepEqual(await nested.allTextContents(), authors);
    assert.equal(await prose.locator('li').count(), 6, 'author rows are nested, not extra top-level bullets');
    assert.deepEqual(await identity.locator('code').allTextContents(), ['--author'],
                     'the wrapped continuation still renders inline code');
    assert.equal((await identity.textContent()).includes('`'), false);
    assert.match(await top.nth(3).textContent(), /git fetch.*git pull.*network access/s,
                 'dedentation returns the following bullet to the outer list');

    // Standard email autolinks retain readable addresses and safe mailto URLs.
    assert.deepEqual(await nested.locator('a').evaluateAll(links => links.map(link =>
      [link.textContent, link.getAttribute('href')])), [
      ['roland.goers@swisstph.ch', 'mailto:roland.goers@swisstph.ch'],
      ['myroslava.volosko@swisstph.ch', 'mailto:myroslava.volosko@swisstph.ch'],
    ]);
    assert.deepEqual(await nested.evaluateAll(items => items.map(item =>
      [...item.querySelectorAll('*')].map(node => node.tagName))), [['A'], ['A']]);

    for (const author of await nested.all()) {
      await author.scrollIntoViewIfNeeded();
      assert.equal(await author.isVisible(), true);
      const geometry = await author.evaluate(item => {
        const rect = item.getBoundingClientRect();
        const parent = item.parentElement.parentElement.getBoundingClientRect();
        const style = getComputedStyle(item);
        return {left: rect.left, right: rect.right, top: rect.top, bottom: rect.bottom,
                parentLeft: parent.left, display: style.display,
                marker: style.listStyleType, height: innerHeight};
      });
      assert.equal(geometry.display, 'list-item');
      assert.notEqual(geometry.marker, 'none');
      assert.ok(geometry.left >= geometry.parentLeft + 16,
                `nested bullet must be visibly indented: ${JSON.stringify(geometry)}`);
      assert.ok(geometry.left >= 0 && geometry.right <= width + 1,
                `author row fits horizontally: ${JSON.stringify(geometry)}`);
      assert.ok(geometry.top >= 0 && geometry.bottom <= geometry.height,
                'author row can be scrolled fully into view');
    }

    const overflow = await page.evaluate(() => [
      document.documentElement, document.getElementById('artifact-panel'),
      document.getElementById('artifact-body'), document.querySelector('.artifact-prose'),
    ].map(node => ({node: node.id || node.className || node.tagName,
                   scroll: node.scrollWidth, client: node.clientWidth})));
    for (const box of overflow) {
      assert.ok(box.client > 0 && box.scroll <= box.client + 1,
                `no horizontal overflow at ${width}px: ${JSON.stringify(box)}`);
    }
    assert.deepEqual(errors, [], 'the production renderer and artifact controller run without errors');
  });
}

test('project Markdown supports standard blocks, links, and literal highlighted fences', async t => {
  const browser = await chromium.launch({headless: true});
  t.after(() => browser.close());
  const page = await browser.newPage({viewport: {width: 1280, height: 800}});
  const literal = '<img src=x onerror="window.__markdownAttack = true">\n'
    + '```\n<script>window.__markdownAttack = true</script>\n';
  const source = [
    '# Markdown features', '', '## Heading **two**', '',
    '[Project docs](https://example.test/docs?x=1&y=2 "Guide") and [section](#topic).', '',
    'First line', 'Second line', 'Third line', '',
    '> Quoted **bold** and `inline`.', '',
    '3. Parent', '   - Child', '     2. Grandchild', '   - Sibling', '4. Next', '',
    '| Planet | Moons |', '|:---|---:|', '| Venus | 0 |', '| Earth | 1 |', '',
    '````html', literal.trimEnd(), '````', '',
    '```elisp', '(defun demo ()', '  "doc")', '```',
  ].join('\n');
  await openFile(page, source, 'markdown-features.md');
  const prose = page.locator('#artifact-body > .artifact-prose');
  assert.equal(await prose.locator(':scope > h1').textContent(), 'Markdown features');
  assert.equal(await prose.locator(':scope > h2 > strong').textContent(), 'two');
  assert.deepEqual(await prose.locator('a').evaluateAll(links => links.map(link =>
    [link.textContent, link.getAttribute('href'), link.getAttribute('title')])), [
    ['Project docs', 'https://example.test/docs?x=1&y=2', 'Guide'],
    ['section', '#topic', null],
  ]);
  const lines = prose.locator(':scope > p').filter({hasText: 'First line'});
  assert.equal(await lines.locator('br').count(), 2, 'single newlines remain visible line breaks');
  assert.equal(await prose.locator('blockquote strong').textContent(), 'bold');
  assert.equal(await prose.locator('blockquote code').textContent(), 'inline');
  const ordered = prose.locator(':scope > ol');
  assert.equal(await ordered.getAttribute('start'), '3');
  assert.equal(await ordered.locator(':scope > li').count(), 2);
  assert.equal(await ordered.locator(':scope > li:first-child > ul > li').count(), 2);
  assert.equal(await ordered.locator('ul > li > ol').getAttribute('start'), '2');
  assert.equal((await ordered.locator('ul > li > ol > li').textContent()).trim(), 'Grandchild');
  assert.equal(await prose.locator('.tablewrap > table.mdtable').count(), 1);
  assert.deepEqual(await prose.locator('thead th').allTextContents(), ['Planet', 'Moons']);
  assert.deepEqual(await prose.locator('tbody tr').evaluateAll(rows => rows.map(row =>
    [...row.cells].map(cell => cell.textContent))), [['Venus', '0'], ['Earth', '1']]);
  assert.match(await prose.locator('thead th').nth(1).evaluate(node => getComputedStyle(node).textAlign),
               /^(?:-webkit-)?right$/);
  const blocks = prose.locator('.codeblock > pre');
  assert.equal(await blocks.count(), 2, 'fenced blocks still use the viewer highlighter');
  assert.equal(await blocks.nth(0).textContent(), literal,
               'HTML and shorter inner fences stay literal inside a longer code fence');
  assert.equal(await blocks.nth(0).locator('img, script').count(), 0);
  assert.equal(await blocks.nth(1).textContent(), '(defun demo ()\n  "doc")\n');
  assert.equal(await blocks.nth(1).locator('.tok-kw').textContent(), 'defun');
  assert.equal(await blocks.nth(1).locator('.tok-str').textContent(), '"doc"');
  assert.equal(await page.evaluate(() => window.__markdownAttack), undefined);
});

test('project Markdown cannot inject scripts, handlers, URLs, SVG, frames, styles, or forms', async t => {
  const browser = await chromium.launch({headless: true});
  t.after(() => browser.close());
  const page = await browser.newPage({viewport: {width: 375, height: 800}});
  const errors = [];
  const dialogs = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('dialog', async dialog => { dialogs.push(dialog.message()); await dialog.dismiss(); });
  const attempts = [
    '<script>window.__markdownAttack = "script"</script>',
    '<img src=x onerror="window.__markdownAttack = \'image\'">',
    '<svg onload="window.__markdownAttack = \'svg\'"><a xlink:href="javascript:alert(1)">SVG</a></svg>',
    '<iframe srcdoc="<script>parent.__markdownAttack = true</script>"></iframe>',
    '<style>body { display: none !important; }</style>',
    '<form action="https://attacker.test/"><input autofocus onfocus="window.__markdownAttack = true"><button formaction="javascript:alert(1)">Submit</button></form>',
    '<a href="javascript:alert(1)" onclick="window.__markdownAttack = true">Raw unsafe link</a>',
    '<a href="jav&#x61;script:alert(1)">Entity unsafe link</a>',
    '<a href="data:text/html,<script>alert(1)</script>">Data unsafe link</a>',
  ];
  const source = '# Safe preview\n\n[Safe link](https://example.test/)\n\n'
    + '[Unsafe Markdown](javascript:alert%281%29)\n\n'
    + attempts.join('\n\n') + '\n\nStill **readable**.';
  const unexpectedRequests = [];
  page.on('request', request => {
    if (!request.url().startsWith('http://localhost:8765/')) unexpectedRequests.push(request.url());
  });
  await openFile(page, source, 'markdown-safety.md');
  const prose = page.locator('#artifact-body > .artifact-prose');
  assert.equal(await prose.locator('h1').textContent(), 'Safe preview');
  assert.equal(await prose.locator('strong').textContent(), 'readable');
  assert.equal(await prose.locator('script, img, svg, iframe, style, form, input, button, object, embed').count(), 0);
  const attributes = await prose.locator('*').evaluateAll(nodes => nodes.flatMap(node =>
    [...node.attributes].map(attr => ({name: attr.name, value: attr.value}))));
  assert.deepEqual(attributes.filter(attr => /^on/i.test(attr.name)
    || ['style', 'srcdoc', 'action', 'formaction'].includes(attr.name)), []);
  assert.deepEqual(await prose.locator('a[href]').evaluateAll(links =>
    links.map(link => link.getAttribute('href'))), ['https://example.test/']);
  assert.match(await prose.textContent(), /Unsafe Markdown/,
               'an unsafe link loses its URL without losing its label');
  for (const attempt of attempts) {
    assert.ok((await prose.textContent()).includes(attempt), 'raw HTML remains readable literal text');
  }
  assert.equal(page.frames().length, 1, 'the payload cannot create a nested browsing context');
  assert.equal(await page.evaluate(() => window.__markdownAttack), undefined);
  assert.equal(await prose.isVisible(), true, 'injected styles cannot hide the preview');
  assert.deepEqual(dialogs, []);
  assert.deepEqual(unexpectedRequests, []);
  assert.deepEqual(errors, []);
});

test('project Markdown refuses insertion when DOMPurify cannot sanitize', async t => {
  const browser = await chromium.launch({headless: true});
  t.after(() => browser.close());
  const page = await browser.newPage({viewport: {width: 375, height: 800}});
  await page.addInitScript(() => {
    Object.defineProperty(DOMImplementation.prototype, 'createHTMLDocument', {value: undefined});
  });
  const source = '# Must not render\n\n[Unsafe](javascript:alert%281%29)\n\n'
    + '<img src=x onerror="window.__markdownAttack = true">';
  await assert.rejects(openFile(page, source, 'unsupported-markdown.md'),
    /Markdown sanitization is not supported by this browser/);
  assert.equal(await page.evaluate(() => typeof document.implementation.createHTMLDocument), 'undefined',
               'the required DOM API was disabled before the actual bundle initialized');
  assert.equal(await page.locator('#artifact-panel').isVisible(), true,
               'the project-file path reached the production preview controller');
  assert.equal(await page.locator('#artifact-body').innerHTML(), '',
               'unsupported sanitization must not insert even partially rendered Markdown');
  assert.equal(await page.evaluate(() => window.__markdownAttack), undefined);
});
