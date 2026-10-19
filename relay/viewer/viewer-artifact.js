/* viewer-artifact.js -- artifact panel and transfer */
'use strict';

(() => {
  const MAX_BASE64 = 24 * 1024 * 1024;
  const CSP = '<meta http-equiv="Content-Security-Policy" '
    + 'content="default-src \'none\'; style-src \'unsafe-inline\'; '
    + 'img-src data: blob:; media-src data: blob:; font-src data:; '
    + 'script-src \'unsafe-inline\'">';
  // The frame is sandboxed without allow-same-origin, so the artifact cannot
  // read the viewer's root stamp. This prelude bakes the theme in force when
  // the frame is built and follows later toggles over postMessage; a page
  // themes itself off html[data-theme] alongside prefers-color-scheme.
  function themePrelude(theme) {
    return '<script>(() => {const root = document.documentElement;'
      + 'const set = t => {if (t === "light" || t === "dark") '
      + 'root.setAttribute("data-theme", t); '
      + 'else root.removeAttribute("data-theme");};'
      + `set(${JSON.stringify(theme)});`
      + 'addEventListener("message", e => {if (e.source === parent && e.data '
      + '&& e.data.t === "theme") set(e.data.theme);});'
      // srcdoc inherits the room's base URL. Resolve local fragment links
      // inside this opaque document instead of navigating to the room.
      + 'addEventListener("click", e => {'
      + 'const link = e.target.closest?.("a[href]");'
      + 'const href = link?.getAttribute("href");'
      + 'if (e.defaultPrevented || e.button !== 0 || e.ctrlKey || e.metaKey '
      + '|| e.shiftKey || e.altKey || !href?.startsWith("#")) return;'
      + 'e.preventDefault(); let id;'
      + 'try {id = decodeURIComponent(href.slice(1));} catch {id = href.slice(1);}'
      + 'const target = document.getElementById(id) || document.getElementsByName(id)[0];'
      + 'if (target) {target.scrollIntoView(); target.focus({preventScroll:true});}'
      + 'else if (!id) scrollTo(0,0);'
      + '});})()<\/script>';
  }

  function create({send, el, flash, summarize, reveal, canDiscuss, canDelete, busy, ask,
                   onPage = () => {}}) {
    const nav = document.getElementById('artifacts');
    const box = document.getElementById('artifacts-box');
    const boxSummary = document.getElementById('artifacts-summary');
    const panel = document.getElementById('artifact-panel');
    const title = document.getElementById('artifact-title');
    const metaEl = document.getElementById('artifact-meta');
    const tab = document.getElementById('artifact-tab');
    const download = document.getElementById('artifact-download');
    const remove = document.getElementById('artifact-delete');
    const closeButton = document.getElementById('artifact-close');
    const body = document.getElementById('artifact-body');
    const discussToggle = document.getElementById('artifact-discussion');
    const sidebarToggle = document.getElementById('artifact-discussions');
    const askButton = document.getElementById('artifact-ask');
    // KIND is 'artifact' for an artifact card or 'file' for a project file,
    // which has no discussions and is removed from the file tree instead.
    // STORE is the artifact store id discussions belong to, if any.
    const view = {id: null, name: null, filename: null, store: null, kind: 'artifact', record: null, reqId: 0,
                  staging: null, meta: null, bytes: null, urls: [], frames: []};
    let requestSequence = 0;
    let theme = null;
    const discussionKit = window.mevedelArtifactDiscussions;
    const side = document.getElementById('artifact-side');
    const discussions = discussionKit && side ? discussionKit.create({
      send, el, toggle: discussToggle, sidebarToggle, side,
      sideRoot: document.getElementById('artifact-discussion-sidebar'),
      count: document.getElementById('artifact-discussion-count'),
      close: document.getElementById('artifact-side-close'),
      peek: document.getElementById('artifact-peek'),
      // The panel covers the conversation, so showing a discussion's turn
      // closes the artifact first.
      reveal: typeof reveal === 'function' ? id => {
        close();
        reveal(id);
      } : null,
      canDiscuss: typeof canDiscuss === 'function' ? canDiscuss : () => false,
      busy: typeof busy === 'function' ? busy : () => false,
    }) : null;

    function note(text) {
      if (!body) return;
      body.replaceChildren(el('p', 'panel-note', text));
    }

    // Only the panel frame carries the discussion picker: its parent is this
    // viewer. A separate tab's frame answers to a shell with no room.
    function sandboxedFrame(doc, html, discussable) {
      const frame = doc.createElement('iframe');
      frame.setAttribute('sandbox', 'allow-scripts');
      frame.className = 'artifact-frame';
      frame.srcdoc = CSP + themePrelude(theme)
        + (discussable && discussions ? discussionKit.script() : '') + html;
      view.frames.push(frame);
      return frame;
    }

    // The viewer's explicit theme ('light' | 'dark') or null for system.
    function setTheme(next) {
      theme = next === 'light' || next === 'dark' ? next : null;
      view.frames = view.frames.filter(frame => frame.isConnected !== false);
      for (const frame of view.frames) {
        const target = frame.contentWindow;
        if (target && typeof target.postMessage === 'function') {
          target.postMessage({t: 'theme', theme}, '*');
        }
      }
    }

    function text() {
      return new TextDecoder().decode(view.bytes);
    }

    function close() {
      view.id = null;
      view.name = null;
      view.filename = null;
      view.store = null;
      view.record = null;
      view.staging = null;
      view.meta = null;
      view.bytes = null;
      view.urls.splice(0).forEach(url => URL.revokeObjectURL(url));
      view.frames = [];
      if (discussions) discussions.detach();
      if (panel) panel.hidden = true;
      if (body) body.replaceChildren();
      if (tab) tab.hidden = true;
      if (download) download.hidden = true;
      if (remove) remove.hidden = true;
      if (askButton) askButton.hidden = true;
      onPage();
    }

    // Deleting removes the artifact for everyone, with its versions and
    // discussions; the host resolves it from its own record of the card or
    // from its store id, and the card then reads as deleted.
    const deletions = new Map();
    let deleteSequence = 0;
    if (remove) {
      remove.addEventListener('click', async () => {
        if (!view.id || !window.confirm(`Delete ${view.name} for everyone, with its versions and discussions? This cannot be undone.`)) return;
        const reqId = ++deleteSequence;
        deletions.set(reqId, {identity: view.store || view.filename, name: view.name});
        if (!await send({t: 'artifact-delete', reqId, id: view.id})) {
          deletions.delete(reqId);
          flash('Connection lost; nothing was deleted.');
        }
      });
    }
    function handleDelete(frame) {
      const deleted = deletions.get(frame.reqId);
      if (!deleted) return;
      deletions.delete(frame.reqId);
      if (typeof frame.error === 'string') {
        flash(frame.error);
        return;
      }
      if ((view.store || view.filename) === deleted.identity) close();
      flash(`${deleted.name} deleted.`);
    }

    function fetch(kind, id, name, frame) {
      close();
      view.id = id;
      view.name = name;
      view.filename = name;
      view.kind = kind;
      view.reqId = ++requestSequence;
      view.staging = [];
      if (title) title.textContent = view.name;
      if (metaEl) metaEl.textContent = 'Loading…';
      note('Loading…');
      panel.hidden = false;
      const reqId = view.reqId;
      send({...frame, reqId}).then(ok => {
        if (!ok && view.reqId === reqId && metaEl) {
          metaEl.textContent = 'Connection lost';
        }
      });
    }

    function open(record) {
      if (!panel || !record || typeof record.id !== 'string') return;
      const version = Number.isSafeInteger(record.version) && record.version > 0;
      fetch(version ? 'version' : 'artifact', record.id,
            (record.title || record.artifact || 'artifact') + (version ? ` · Version ${record.version}` : ''),
            {t: 'artifact-get', id: record.id, ...(version ? {version: record.version} : {})});
      view.filename = record.artifact || 'artifact';
      view.store = typeof record.store === 'string' ? record.store : null;
      // What reopens this page after Back, Forward or a reload.
      view.record = {id: record.id};
      for (const key of ['artifact', 'title', 'store', 'version', 'size', 'item']) {
        if (record[key] !== undefined) view.record[key] = record[key];
      }
      onPage();
    }

    // A project file, named by its path in the project.
    function openFile(path) {
      if (!panel || typeof path !== 'string') return;
      fetch('file', path, path, {t: 'file-get', path});
    }

    // The open page, as {artifact: RECORD} or {file: PATH}, or null.
    function describe() {
      if (!panel || panel.hidden || !view.id) return null;
      if (view.kind === 'file') return {file: view.id};
      return view.record ? {artifact: view.record} : null;
    }

    function renderContent() {
      if (!body || !view.bytes) return;
      const {mime} = view.meta;
      const {formatBytes, renderMarkdown} = window.mevedelTranscriptRenderer;
      if (metaEl) metaEl.textContent = `${formatBytes(view.bytes.length)} · ${mime}`;
      body.replaceChildren();
      const artifact = view.kind === 'artifact';
      if (download) download.hidden = false;
      if (remove) remove.hidden = !(artifact && typeof canDelete === 'function' && canDelete());
      if (askButton) askButton.hidden = view.kind !== 'file' || !ask || !ask.available();
      // Discussions belong to a store artifact; the host keeps them on its
      // main file.
      const discussable = artifact && view.store !== null;
      if (mime === 'text/html') {
        if (tab) tab.hidden = false;
        const frame = sandboxedFrame(document, text(), discussable);
        body.append(frame);
        if (discussions && discussable) discussions.attach(frame, view.id, view.store);
      } else if (mime === 'text/markdown') {
        const prose = renderMarkdown(text());
        prose.className = 'prose artifact-prose';
        body.append(prose);
      } else if (mime.startsWith('image/')) {
        const url = URL.createObjectURL(new Blob([view.bytes], {type: mime}));
        view.urls.push(url);
        const image = el('img', 'artifact-image');
        image.src = url;
        image.alt = view.name;
        body.append(image);
      } else if (mime === 'text/plain' || mime === 'text/csv'
                 || mime === 'application/json') {
        body.append(el('pre', 'result artifact-text', text()));
      } else {
        note('This file type does not preview here — download it.');
      }
    }

    function handle(frame) {
      if (!view.id || frame.reqId !== view.reqId) return;
      if (typeof frame.error === 'string') {
        view.staging = null;
        if (metaEl) metaEl.textContent = '';
        note(frame.error);
        return;
      }
      if (!view.staging) return;
      if (!view.meta) {
        view.meta = {
          mime: typeof frame.mime === 'string' ? frame.mime
            : 'application/octet-stream',
          size: typeof frame.size === 'number' ? frame.size : 0,
        };
      }
      if (typeof frame.data === 'string') view.staging.push(frame.data);
      const collected = view.staging.reduce((sum, part) => sum + part.length, 0);
      if (collected > MAX_BASE64) {
        view.staging = null;
        note('Too large for this viewer; open it on the host.');
        return;
      }
      if (frame.final === true) {
        const encoded = view.staging.join('');
        view.staging = null;
        let binary;
        try { binary = atob(encoded); }
        catch (_error) {
          note('The transfer was corrupted; try again.');
          return;
        }
        const bytes = new Uint8Array(binary.length);
        for (let index = 0; index < binary.length; index++) {
          bytes[index] = binary.charCodeAt(index);
        }
        view.bytes = bytes;
        renderContent();
      } else if (metaEl && view.meta.size > 0) {
        const percent = Math.min(
          99, Math.round((collected * 0.75 * 100) / view.meta.size));
        metaEl.textContent = `Loading… ${percent}%`;
      }
    }

    function openTab() {
      if (!view.bytes || !view.meta) return;
      const opened = window.open('', '_blank');
      if (!opened) {
        flash('Popup blocked — the artifact stays in this panel.');
        return;
      }
      const doc = opened.document;
      doc.title = view.name;
      const frame = sandboxedFrame(doc, text());
      frame.setAttribute('style', 'border:0;width:100vw;height:100vh;display:block');
      doc.body.setAttribute('style', 'margin:0');
      doc.body.append(frame);
    }

    function downloadFile() {
      if (!view.bytes || !view.meta) return;
      const url = URL.createObjectURL(new Blob(
        [view.bytes], {type: view.meta.mime || 'application/octet-stream'}));
      view.urls.push(url);
      const link = document.createElement('a');
      link.href = url;
      link.download = view.filename.split('/').pop();
      if (typeof link.click === 'function') link.click();
    }

    // The room's cards, and the store artifacts attached to its session:
    // both list in the sidebar, once per artifact file.
    let published = [];
    let attached = [];
    function render(records) {
      published = records;
      if (discussions) discussions.records(records);
      paint();
    }

    // Whiteboards and documents list under Shared work, in their editor.
    function attachedRows(rows) {
      attached = Array.isArray(rows)
        ? rows.filter(row => row && row.attached === true && row.item !== true) : [];
      paint();
    }

    function paint() {
      if (!nav) return;
      const byName = new Map();
      attached.forEach(row => byName.set(row.artifact, {
        id: `artifact:${row.id}`, artifact: row.artifact, title: row.title, store: row.id,
        size: row.size, missing: row.missing === true,
      }));
      published.forEach(record => {
        if (record.artifact) byName.set(record.artifact, record);
      });
      nav.replaceChildren();
      const label = `${byName.size} artifact${byName.size === 1 ? '' : 's'}`;
      if (box) box.hidden = byName.size === 0;
      if (boxSummary) boxSummary.textContent = label;
      if (summarize) summarize('artifacts', byName.size ? label : '');
      byName.forEach(record => {
        const missing = record.missing === true;
        const chip = el('button', `dock-chip${missing ? ' stuck' : ''}`);
        chip.type = 'button';
        chip.disabled = missing;
        chip.title = missing
          ? `${record.artifact} was deleted on the host`
          : `Open ${record.title || record.artifact}`;
        chip.append(el('span', 'dock-chip-name', record.title || record.artifact));
        const size = missing ? 'deleted'
          : typeof record.size === 'number'
            ? window.mevedelTranscriptRenderer.formatBytes(record.size) : '';
        if (size) chip.append(el('span', 'dock-chip-meta', size));
        if (!missing) chip.addEventListener('click', () => open(record));
        nav.append(chip);
      });
    }

    if (closeButton) closeButton.addEventListener('click', close);
    if (tab) tab.addEventListener('click', openTab);
    if (download) download.addEventListener('click', downloadFile);
    // ASK is {available, run}: whether a file can start a session here,
    // and starting one about the file at a path.
    if (askButton && ask) {
      askButton.addEventListener('click', () => {
        const path = view.name;
        close();
        ask.run(path);
      });
    }
    function queue(entries) {
      if (discussions) discussions.queue(entries);
    }

    function handleDiscussion(frame) {
      if (discussions) discussions.handle(frame);
    }

    function storedDiscussions(frame) {
      if (discussions) discussions.stored(frame);
    }

    // The session started or finished a turn: markers show whether the
    // assistant is still working on their threads.
    function activity() {
      if (discussions) discussions.activity();
    }

    // A room message about store artifact STORE as a whole, with attachment
    // IMAGES, sent into the artifact's conversation.
    function discuss(store, text, images = []) {
      if (!discussions) return Promise.reject(new Error('Artifact messages are unavailable.'));
      return discussions.discuss(`artifact:${store}`, text, images);
    }

    return Object.freeze({open, openFile, render, attachedRows, handle, handleDelete, close,
                          describe,
                          page: () => (panel && !panel.hidden ? view.store : null),
                          setTheme, queue, handleDiscussion, storedDiscussions, discuss, activity});
  }

  window.mevedelArtifactView = Object.freeze({create});
})();
