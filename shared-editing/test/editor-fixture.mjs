import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import {readFileSync} from 'node:fs';
import {chromium,firefox} from 'playwright';
import {handle} from '../host.mjs';

// Packaged iframe and its real host API, connected over the native item port.
/* An in-memory stand-in for mevedel-shared-library: the personal, installed
   and built-in libraries. CATALOG lists public libraries; SOURCES maps a
   public library's source to its file text. */
export function libraryHost(catalog = [], sources = {}) {
  const lib = (items) => JSON.stringify({ type: 'excalidrawlib', version: 2, libraryItems: items });
  const builtin = readFileSync(new URL('../builtin.excalidrawlib', import.meta.url), 'utf8');
  let personal = [];
  const installed = new Map();
  const listing = () => ({ libraries: [
    ...(personal.length ? [{ name: 'My library', kind: 'personal', text: lib(personal) }] : []),
    ...[...installed].map(([name, text]) => ({ name, kind: 'installed', text })),
    { name: 'Built-in', kind: 'builtin', text: builtin },
  ] });
  const fetch = (source) => {
    if (!sources[source]) throw new Error('Library request failed: 404');
    return sources[source];
  };
  const handlers = {
    library: listing,
    'library-add': ({ text }) => {
      personal = [...JSON.parse(text).libraryItems.filter((item) => !personal.some((known) => known.id === item.id)), ...personal];
      return listing();
    },
    'library-remove': ({ ids }) => { personal = personal.filter((item) => !ids.includes(item.id)); return listing(); },
    'library-install': ({ source, name }) => { installed.set(name, fetch(source)); return listing(); },
    'library-uninstall': ({ name }) => { installed.delete(name); return listing(); },
    'library-catalog': () => ({ libraries: catalog }),
    'library-fetch': ({ source }) => ({ text: fetch(source) }),
  };
  const handle = (args) => { handle.requests.push(args); return handlers[args.action](args); };
  handle.requests = [];
  return handle;
}

export async function editorFixture(t, { library = libraryHost() } = {}) {
  const server = createServer(async (req, res) => {
    const name = new URL(req.url, 'http://localhost').pathname.slice(1);
    if (name === 'room') {
      res.end(await readFile(new URL('../../relay/viewer/index.html',import.meta.url),'utf8'));
    } else if (name === 'menu') {
      res.end((await readFile(new URL('../../relay/viewer/index.html', import.meta.url), 'utf8'))
        .replace(/<script[\s\S]*?<\/script>/g, ''));
    } else if (/^(?:viewer[\w-]*|transport|notifications|attachments)\.(?:css|js)$/.test(name) || ['shared-editor.html', 'shared-editor.css', 'shared-editor-layout.css', 'shared-editor.js', 'renderer.js'].includes(name)) {
      res.setHeader(
        'Content-Type',
        name.endsWith('.js') ? 'text/javascript' : name.endsWith('.css') ? 'text/css' : 'text/html',
      );
      res.end(await readFile(new URL(`../../relay/viewer/${name}`, import.meta.url)));
    } else
      res.end(
        '<style>body{margin:0}iframe{width:100vw;height:100dvh;border:0}</style><iframe sandbox="allow-scripts allow-forms" src="/shared-editor.html"></iframe>',
      );
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const engine = process.env.MEVEDEL_BROWSER === 'firefox' ? firefox : chromium;
  const browser = await engine.launch({ headless: true });
  /* SCENE, .excalidraw text, opens a whiteboard with its image files. */
  async function open({kind = 'whiteboard', viewport = {width:1000,height:700}, assistantDraft, appearance, content, scene, readOnly = false, actor = 'Guest: Alice'} = {}) {
    const page = await browser.newPage({ viewport });
    page.setDefaultTimeout(7000);
    const created = scene ? await handle({ action: 'import', format: 'excalidraw', id: 'test', opId: 'create', actor, data: scene })
      : await handle({
      action: 'create',
      id: 'test',
      opId: 'create',
      actor,
      kind,
      content: content ?? (kind === 'whiteboard'
          ? [{ id: 'ellipse', type: 'ellipse', x: 100, y: 100, width: 300, height: 160 }]
          : undefined),
    });
    let state = created.state;
    await page.exposeFunction('apply', async (args) => {
      // Emacs owns the element library; this stands in for mevedel-shared-library.
      if (args.action.startsWith('library')) return library(args);
      const reply = await handle({ ...args, action:args.action === 'ask' ? 'read' : args.action, sync:args.action === 'read', question:args.action === 'ask', state, actor: 'Guest: Alice' });
      state = reply.state || state;
      return { ...reply.result, transactions: state.transactions };
    });
    await page.goto(`http://127.0.0.1:${server.address().port}`);
    await page.evaluate(
      ({item, assistantDraft, appearance, readOnly}) => {
        const channel = new MessageChannel();
        window.port = channel.port1;
        window.messages = [];
        channel.port1.onmessage = async ({ data }) => {
          window.messages.push(data);
          if (data.type === 'request') {
            try {
              if (window.rejectQuestion && data.args.action === 'ask') throw new Error('Host refused this submission');
              const result = await window.apply(data.args);
              if (!data.args.action.startsWith('library')) channel.port1.postMessage({ type: 'changed', ...result });
              channel.port1.postMessage({ type: 'reply', reqId: data.reqId, result });
            } catch (error) {
              channel.port1.postMessage({ type: 'reply', reqId: data.reqId, error:error.message });
            }
          }
        };
        document
          .querySelector('iframe')
          .contentWindow.postMessage(
            { type: 'mevedel-editor', item, draft:{assistant:assistantDraft}, readOnly, name: 'Alice', appearance },
            '*',
            [channel.port2],
          );
      },
      {item:{ ...state, crdt: state.crdt }, assistantDraft, appearance, readOnly},
    );
    const frame = page.frameLocator('iframe');
    await frame.locator(kind === 'whiteboard' ? '#canvas' : '.tiptap').waitFor();
    return { page, frame };
  }
  t.after(async () => { await browser.close(); await new Promise(resolve => server.close(resolve)); });
  return {open, browser, url:`http://127.0.0.1:${server.address().port}`};
}
