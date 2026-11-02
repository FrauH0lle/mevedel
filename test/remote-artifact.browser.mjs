/* End-to-end acceptance for artifact links: a real relay built from
 * relay/ (so it embeds the current viewer), an isolated origin Emacs, the
 * remote-artifact package's own CLI and stdio MCP server, and Chromium,
 * all on one whiteboard.  The browser joins through the origin's session
 * room; the package through the artifact's edit link.  The package then
 * edits an HTML artifact's files through its own link, checked at the
 * origin; the origin's browser, with the page open, offers a reload that
 * shows the edit.
 *
 * The origin is shared-editing/test/room.el: it runs real tools and the
 * real sharing operation on request through files in its directory.
 *
 * Run: node --test test/remote-artifact.browser.mjs
 * Needs Go, Emacs with the Eask dependencies, and Playwright Chromium
 * from shared-editing/node_modules. */
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { createInterface } from 'node:readline';
import { mkdtemp, readdir, readFile, writeFile, rename, rm } from 'node:fs/promises';
import { createServer } from 'node:net';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';

const root = resolve(import.meta.dirname, '..');
const require = createRequire(resolve(root, 'shared-editing/package.json'));
const { chromium } = require('playwright');
const cli = join(root, 'remote-artifact/bin/mevedel-remote-artifact.mjs');
const delay = (ms) => new Promise((r) => setTimeout(r, ms));

async function until(fn, ms = 20000) {
  const deadline = Date.now() + ms;
  let error;
  while (Date.now() < deadline) {
    try {
      const result = await fn();
      if (result) return result;
    } catch (e) {
      error = e;
    }
    await delay(50);
  }
  throw error || new Error('Timed out');
}

async function freePort() {
  const server = createServer();
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const { port } = server.address();
  await new Promise((r) => server.close(r));
  return port;
}

/* A minimal MCP client over the package's stdio server. */
function mcpClient(env, logs) {
  const child = spawn(process.execPath, [cli, 'mcp'], { env });
  child.stderr.on('data', (d) => logs.push(String(d)));
  const waiting = new Map();
  let next = 1;
  createInterface({ input: child.stdout }).on('line', (line) => {
    const message = JSON.parse(line);
    waiting.get(message.id)?.(message);
    waiting.delete(message.id);
  });
  const request = (method, params) => new Promise((resolve, reject) => {
    const id = next++;
    const timer = setTimeout(() => reject(new Error(`MCP ${method} timed out`)), 20000);
    waiting.set(id, (message) => {
      clearTimeout(timer);
      if (message.error) reject(new Error(message.error.message));
      else resolve(message.result);
    });
    child.stdin.write(`${JSON.stringify({ jsonrpc: '2.0', id, method, params })}\n`);
  });
  return {
    child,
    async initialize() {
      await request('initialize', { protocolVersion: '2025-06-18', capabilities: {},
        clientInfo: { name: 'acceptance', version: '0' } });
      child.stdin.write(`${JSON.stringify({ jsonrpc: '2.0', method: 'notifications/initialized' })}\n`);
    },
    /* Call tool NAME; return { text, isError }. */
    async call(name, args) {
      const result = await request('tools/call', { name, arguments: args });
      return { text: result.content.filter((c) => c.type === 'text').map((c) => c.text).join('\n'),
        isError: result.isError === true };
    },
  };
}

test(
  'artifact link: MCP and browser edit one whiteboard live, and revoking cuts the package off',
  { timeout: 180000 },
  async () => {
    const directory = await mkdtemp(join(tmpdir(), 'mevedel-remote-artifact-')),
      port = await freePort();
    const logs = [],
      errors = [];
    let relay, host, browser, mcp;
    try {
      execFileSync('go', ['build', '-o', join(directory, 'relay'), '.'], {
        cwd: join(root, 'relay'),
        stdio: 'pipe',
      });
      relay = spawn(join(directory, 'relay'), [
        '-addr', `127.0.0.1:${port}`,
        '-vapid-key-file', join(directory, 'vapid.pem'),
      ]);
      relay.stderr.on('data', (d) => logs.push(String(d)));
      await until(async () => {
        try {
          return (await fetch(`http://127.0.0.1:${port}/healthz`)).ok;
        } catch {
          return false;
        }
      });
      // The relay serves the working tree's viewer, not an older build's.
      assert.equal(await (await fetch(`http://127.0.0.1:${port}/viewer.js`)).text(),
        await readFile(join(root, 'relay/viewer/viewer.js'), 'utf8'));
      const elpa = join(root, '.eask/31.1/elpa');
      const deps = (await readdir(elpa, { withFileTypes: true }))
        .filter((d) => d.isDirectory())
        .flatMap((d) => ['-L', join(elpa, d.name)]);
      const isolated = {
        ...process.env,
        HOME: directory,
        XDG_CONFIG_HOME: join(directory, 'config'),
        XDG_CACHE_HOME: join(directory, 'cache'),
        XDG_DATA_HOME: join(directory, 'data'),
        XDG_STATE_HOME: join(directory, 'state'),
      };
      host = spawn('emacs', [
        '-Q', '--batch', '-L', root, '-L', join(root, 'test'), ...deps,
        '-l', join(root, 'shared-editing/test/room.el'),
      ], { env: { ...isolated, MEVEDEL_EDITING_TEST_ROOT: directory,
        MEVEDEL_EDITING_RELAY: `ws://127.0.0.1:${port}` } });
      host.stdout.on('data', (d) => logs.push(String(d)));
      host.stderr.on('data', (d) => logs.push(String(d)));
      const links = await until(async () => {
        if (host.exitCode !== null) throw new Error(logs.join(''));
        return JSON.parse(await readFile(join(directory, 'links.json'), 'utf8'));
      });
      async function origin(tool, args = {}) {
        await writeFile(join(directory, 'request.tmp'), JSON.stringify({ tool, args }));
        await rename(join(directory, 'request.tmp'), join(directory, 'request.json'));
        const reply = await until(async () =>
          JSON.parse(await readFile(join(directory, 'reply.json'), 'utf8')));
        await rm(join(directory, 'reply.json'));
        assert.equal(reply.error, undefined, JSON.stringify(reply));
        return reply;
      }
      const created = await origin('SharedCreate', { kind: 'whiteboard', title: 'Acceptance board' });
      assert.equal(created.status, 'success', JSON.stringify(created));
      const boardId = JSON.parse((await origin('ReadShared')).result)
        .find((i) => i.kind === 'whiteboard').id;
      const elements = async () =>
        JSON.parse((await origin('ReadShared', { id: boardId })).result).content;
      const shared = await origin('ArtifactSharing', { id: boardId, operation: 'share' });
      assert.match(shared.edit, /^http:\/\/127\.0\.0\.1:\d+\/#/);

      // The package side: attach the edit link with the CLI, then serve MCP.
      const consumer = { ...isolated, XDG_CONFIG_HOME: join(directory, 'consumer') };
      const attached = execFileSync(process.execPath,
        [cli, 'attach', shared.edit, '--name', 'board', '--as', 'Acceptance'],
        { env: consumer, encoding: 'utf8' });
      assert.match(attached, /Attached board: Acceptance board \(whiteboard, edit link\)/);
      mcp = mcpClient(consumer, logs);
      await mcp.initialize();
      assert.match((await mcp.call('read_artifact', { name: 'board' })).text,
        /whiteboard "Acceptance board"/);

      // The browser side: the origin's room, with the board open.
      browser = await chromium.launch({ headless: true });
      const page = await browser.newPage();
      page.on('pageerror', (e) => errors.push(e.message));
      page.on('console', (m) => {
        if (m.type() === 'error') logs.push(m.text());
      });
      const url = new URL(links.full);
      url.searchParams.set('shared', '');
      await page.goto(url.href);
      await page.waitForFunction(() => !document.getElementById('editing-box').hidden);
      await page.locator('#editing-box').evaluate((e) => (e.open = true));
      await page.locator(`#editing-items [data-item-id="${boardId}"]`).click();
      const frame = page.frameLocator('#editing-body iframe');
      await frame.locator('#canvas').waitFor({ state: 'visible' });
      // A reload of the page or the editor would lose these markers.
      await page.evaluate(() => (window.acceptanceLoaded = true));
      await frame.locator('body').evaluate(() => (window.acceptanceLoaded = true));

      // An MCP edit appears in the browser without a reload.
      const box = { id: 'mcp_box', type: 'rectangle', x: 600, y: 80, width: 140, height: 70 };
      const edited = await mcp.call('edit_artifact',
        { name: 'board', action: 'patch', changes: [{ id: box.id, after: box }] });
      assert.equal(edited.isError, false, edited.text);
      await until(async () => (await frame.locator('[data-shape="mcp_box"]').count()) === 1);
      assert.equal(await page.evaluate(() => window.acceptanceLoaded), true);
      assert.equal(await frame.locator('body').evaluate(() => window.acceptanceLoaded), true);

      // A browser edit is visible in the next MCP read.
      await frame.locator('[data-tool="rectangle"]').click();
      const canvas = await frame.locator('#canvas').boundingBox();
      await page.mouse.move(canvas.x + 330, canvas.y + 120);
      await page.mouse.down();
      await page.mouse.move(canvas.x + 430, canvas.y + 180);
      await page.mouse.up();
      const drawn = await until(async () => {
        const content = await elements();
        return content.length === 2 && content.find((e) => e.id !== box.id);
      });
      const read = await mcp.call('read_artifact', { name: 'board' });
      assert.equal(read.isError, false, read.text);
      assert.ok(read.text.includes(`"id":"${drawn.id}"`), read.text);
      assert.ok(read.text.includes('"id":"mcp_box"'), read.text);

      // Revoking at the origin cuts the package off, saying why.
      await origin('ArtifactSharing', { id: boardId, operation: 'revoke' });
      const after = await mcp.call('read_artifact', { name: 'board' });
      assert.equal(after.isError, true, after.text);
      assert.match(after.text, /revoked this artifact link/);
      assert.equal((await elements()).length, 2);

      // An HTML artifact through its edit link: the primary file reads,
      // an edit lands at the origin, and a write after an origin-side
      // change is refused as stale with the current content.
      const html = '<!doctype html>\n<h1>Hello</h1>\n';
      await origin('HtmlFile', { id: 'landing', file: 'index.html', title: 'Landing', content: html });
      const site = await origin('ArtifactSharing', { id: 'landing', operation: 'share' });
      assert.match(site.edit, /^http:\/\/127\.0\.0\.1:\d+\/#/);
      assert.match(execFileSync(process.execPath, [cli, 'attach', site.edit, '--name', 'site'],
        { env: consumer, encoding: 'utf8' }), /Attached site: Landing \(html, edit link\)/);
      const listed = await mcp.call('list_artifacts', {});
      assert.match(listed.text, /^site: "Landing" html from 127\.0\.0\.1, edit link, origin reachable$/m, listed.text);
      const primary = await mcp.call('read_artifact', { name: 'site' });
      assert.equal(primary.isError, false, primary.text);
      assert.equal(primary.text, html);
      const files = await mcp.call('list_artifact_files', { name: 'site' });
      assert.equal(files.text, `index.html (${Buffer.byteLength(html)} bytes, primary)`);
      // The origin's browser has the page open in its panel, through the
      // link Project artifacts `o' opens.
      await page.goto(`${links.full}?artifact=landing`);
      const panelFrame = page.frameLocator('#artifact-body iframe');
      const heading = () => panelFrame.locator('h1').textContent();
      await until(async () => (await heading()) === 'Hello');
      await panelFrame.locator('body').evaluate(() => (window.beforeReload = true));
      const reloadButton = page.locator('#artifact-reload');
      assert.equal(await reloadButton.isHidden(), true);
      const fileEdit = await mcp.call('edit_artifact_file', { name: 'site', file: 'index.html',
        old_string: '<h1>Hello</h1>', new_string: '<h1>Hello, world</h1>' });
      assert.equal(fileEdit.isError, false, fileEdit.text);
      assert.match(fileEdit.text, /^Edited index\.html/);
      const htmlEdited = '<!doctype html>\n<h1>Hello, world</h1>\n';
      assert.equal((await origin('HtmlFile', { id: 'landing', file: 'index.html' })).text, htmlEdited);
      // The edit offers a reload without resetting the open page; the
      // reload shows it.
      await reloadButton.waitFor({ state: 'visible' });
      assert.equal(await heading(), 'Hello');
      assert.equal(await panelFrame.locator('body').evaluate(() => window.beforeReload), true);
      await reloadButton.click();
      await until(async () => (await heading()) === 'Hello, world');
      assert.equal(await reloadButton.isHidden(), true);
      const changed = '<!doctype html>\n<h1>Changed at the origin</h1>\n';
      await origin('HtmlFile', { id: 'landing', file: 'index.html', content: changed });
      const stale = await mcp.call('write_artifact_file', { name: 'site', file: 'index.html',
        content: '<!doctype html>\n<h1>Overwritten</h1>\n' });
      assert.equal(stale.isError, true, stale.text);
      assert.equal(stale.text, 'index.html changed since it was read; redo the change on its current '
        + `content.\nCurrent content of index.html:\n${changed}`);
      assert.equal((await origin('HtmlFile', { id: 'landing', file: 'index.html' })).text, changed);
      assert.deepEqual(errors, []);
    } catch (error) {
      throw new Error(`${error.stack}\n${errors.join('\n')}\n${logs.join('\n').slice(-10000)}`);
    } finally {
      mcp?.child.stdin.end();
      mcp?.child.kill();
      await browser?.close();
      if (host && host.exitCode === null) {
        await writeFile(join(directory, 'stop'), '');
        await until(() => host.exitCode !== null, 10000).catch(() => host.kill());
      }
      relay?.kill();
      await rm(directory, { recursive: true, force: true });
    }
  },
);
