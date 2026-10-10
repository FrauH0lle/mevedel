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

  function create({send, el, flash, summarize, reveal, canComment, canDelete, busy, ask}) {
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
    const commentToggle = document.getElementById('artifact-comment');
    const askButton = document.getElementById('artifact-ask');
    // KIND is 'artifact' for an artifact card or 'file' for a project file,
    // which has no comments and is removed from the file tree instead.
    // STORE is the artifact store id comments belong to, if any.
    const view = {id: null, name: null, store: null, kind: 'artifact', reqId: 0,
                  staging: null, meta: null, bytes: null, urls: [], frames: []};
    let requestSequence = 0;
    let theme = null;
    const commentKit = window.mevedelArtifactComments;
    const comments = commentKit && body ? commentKit.create({
      send, el, body, toggle: commentToggle, flash,
      renderMarkdown: text => window.mevedelTranscriptRenderer.renderMarkdown(text),
      // The panel covers the conversation, so showing a comment's turn
      // closes the artifact first.
      reveal: typeof reveal === 'function' ? id => {
        close();
        reveal(id);
      } : null,
      canComment: typeof canComment === 'function' ? canComment : () => false,
      busy: typeof busy === 'function' ? busy : () => false,
    }) : null;

    function note(text) {
      if (!body) return;
      body.replaceChildren(el('p', 'panel-note', text));
    }

    // Only the panel frame carries the comment picker: its parent is this
    // viewer. A separate tab's frame answers to a shell with no room.
    function sandboxedFrame(doc, html, commentable) {
      const frame = doc.createElement('iframe');
      frame.setAttribute('sandbox', 'allow-scripts');
      frame.className = 'artifact-frame';
      frame.srcdoc = CSP + themePrelude(theme)
        + (commentable && comments ? commentKit.script() : '') + html;
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
      view.store = null;
      view.staging = null;
      view.meta = null;
      view.bytes = null;
      view.urls.splice(0).forEach(url => URL.revokeObjectURL(url));
      view.frames = [];
      if (comments) comments.detach();
      if (panel) panel.hidden = true;
      if (body) body.replaceChildren();
      if (tab) tab.hidden = true;
      if (download) download.hidden = true;
      if (remove) remove.hidden = true;
      if (askButton) askButton.hidden = true;
    }

    // Deleting removes the artifact for everyone, with its versions and
    // comments; the host resolves it from its own record of the card or
    // from its store id, and the card then reads as deleted.
    const deletions = new Map();
    let deleteSequence = 0;
    if (remove) {
      remove.addEventListener('click', async () => {
        if (!view.id || !window.confirm(`Delete ${view.name} for everyone, with its versions and comments? This cannot be undone.`)) return;
        const reqId = ++deleteSequence;
        deletions.set(reqId, view.name);
        if (!await send({t: 'artifact-delete', reqId, id: view.id})) {
          deletions.delete(reqId);
          flash('Connection lost; nothing was deleted.');
        }
      });
    }
    function handleDelete(frame) {
      const name = deletions.get(frame.reqId);
      if (!name) return;
      deletions.delete(frame.reqId);
      if (typeof frame.error === 'string') {
        flash(frame.error);
        return;
      }
      if (view.name === name) close();
      flash(`${name} deleted.`);
    }

    function fetch(kind, id, name, frame) {
      close();
      view.id = id;
      view.name = name;
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
      fetch('artifact', record.id, record.artifact || 'artifact',
            {t: 'artifact-get', id: record.id});
      view.store = typeof record.store === 'string' ? record.store : null;
    }

    // A project file, named by its path in the project.
    function openFile(path) {
      if (!panel || typeof path !== 'string') return;
      fetch('file', path, path, {t: 'file-get', path});
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
      if (askButton) askButton.hidden = artifact || !ask || !ask.available();
      // Comments belong to a store artifact; the host keeps them on its
      // main file.
      const commentable = artifact && view.store !== null;
      if (mime === 'text/html') {
        if (tab) tab.hidden = false;
        const frame = sandboxedFrame(document, text(), commentable);
        body.append(frame);
        if (comments && commentable) comments.attach(frame, view.id, view.store);
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
      link.download = view.name.split('/').pop();
      if (typeof link.click === 'function') link.click();
    }

    // The room's cards, and the store artifacts attached to its session:
    // both list in the sidebar, once per artifact file.
    let published = [];
    let attached = [];
    function render(records) {
      published = records;
      if (comments) comments.records(records);
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
        id: `artifact:${row.id}`, artifact: row.artifact, store: row.id,
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
          : `Open ${record.artifact}`;
        chip.append(el('span', 'dock-chip-name', record.artifact));
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
      if (comments) comments.queue(entries);
    }

    function handleComment(frame) {
      if (comments) comments.handle(frame);
    }

    function storedComments(frame) {
      if (comments) comments.stored(frame);
    }

    // The session started or finished a turn: markers show whether the
    // assistant is still working on their threads.
    function activity() {
      if (comments) comments.activity();
    }

    // A room message about store artifact STORE as a whole, with attachment
    // IMAGES, sent into the artifact's conversation.
    function discuss(store, text, images = []) {
      if (!comments) return Promise.reject(new Error('Artifact messages are unavailable.'));
      return comments.discuss(`artifact:${store}`, text, images);
    }

    return Object.freeze({open, openFile, render, attachedRows, handle, handleDelete, close,
                          setTheme, queue, handleComment, storedComments, discuss, activity});
  }

  window.mevedelArtifactView = Object.freeze({create});
})();
