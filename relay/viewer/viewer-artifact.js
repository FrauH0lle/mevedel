/* viewer-artifact.js -- session artifact panel and transfer */
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

  function create({send, el, flash, summarize, reveal, canComment}) {
    const nav = document.getElementById('artifacts');
    const box = document.getElementById('artifacts-box');
    const boxSummary = document.getElementById('artifacts-summary');
    const panel = document.getElementById('artifact-panel');
    const title = document.getElementById('artifact-title');
    const metaEl = document.getElementById('artifact-meta');
    const tab = document.getElementById('artifact-tab');
    const download = document.getElementById('artifact-download');
    const closeButton = document.getElementById('artifact-close');
    const body = document.getElementById('artifact-body');
    const commentToggle = document.getElementById('artifact-comment');
    const view = {id: null, name: null, reqId: 0, staging: null,
                  meta: null, bytes: null, urls: [], frames: []};
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
    }

    function open(record) {
      if (!panel || !record || typeof record.id !== 'string') return;
      close();
      view.id = record.id;
      view.name = record.artifact || 'artifact';
      view.reqId = ++requestSequence;
      view.staging = [];
      if (title) title.textContent = view.name;
      if (metaEl) metaEl.textContent = 'Loading…';
      note('Loading…');
      panel.hidden = false;
      const reqId = view.reqId;
      send({t: 'artifact-get', reqId, id: record.id}).then(ok => {
        if (!ok && view.reqId === reqId && metaEl) {
          metaEl.textContent = 'Connection lost';
        }
      });
    }

    function renderContent() {
      if (!body || !view.bytes) return;
      const {mime} = view.meta;
      const {formatBytes, renderMarkdown} = window.mevedelTranscriptRenderer;
      if (metaEl) metaEl.textContent = `${formatBytes(view.bytes.length)} · ${mime}`;
      body.replaceChildren();
      if (download) download.hidden = false;
      if (mime === 'text/html') {
        if (tab) tab.hidden = false;
        const frame = sandboxedFrame(document, text(), true);
        body.append(frame);
        if (comments) comments.attach(frame, view.id, view.name);
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
        note('Artifact too large for this viewer.');
        return;
      }
      if (frame.final === true) {
        const encoded = view.staging.join('');
        view.staging = null;
        let binary;
        try { binary = atob(encoded); }
        catch (_error) {
          note('Artifact transfer was corrupted; try again.');
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

    let published = [];
    function render(records) {
      published = records;
      if (comments) comments.records(records);
      if (!nav) return;
      const byName = new Map();
      records.forEach(record => {
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
    function queue(entries) {
      if (comments) comments.queue(entries);
    }

    function handleComment(frame) {
      if (comments) comments.handle(frame);
    }

    function storedComments(frame) {
      if (comments) comments.stored(frame);
    }

    // A room message about artifact NAME as a whole, sent into the
    // artifact's own conversation through its latest published record.
    function discuss(name, text) {
      const record = published.filter(item => item && item.artifact === name && !item.missing)
        .at(-1);
      if (!comments || !record) {
        return Promise.reject(new Error(`${name} is not published in this room.`));
      }
      return comments.discuss(record.id, text);
    }

    return Object.freeze({open, render, handle, close, setTheme, queue, handleComment,
                          storedComments, discuss});
  }

  window.mevedelArtifactView = Object.freeze({create});
})();
