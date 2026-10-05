/* viewer-files.js -- project files: the lobby's file tree and uploads */
'use strict';

(() => {
  // Raw bytes per upload chunk: its base64 stays well under the host's
  // 1 MiB frame bound.
  const CHUNK_BYTES = 512 * 1024;
  const MAX_UPLOAD_BYTES = 16 * 1024 * 1024;
  const ACK_TIMEOUT_MS = 30000;

  function base64(bytes) {
    let binary = '';
    for (let start = 0; start < bytes.length; start += 0x8000) {
      binary += String.fromCharCode.apply(
        null, bytes.subarray(start, start + 0x8000));
    }
    return btoa(binary);
  }

  /* Uploads files into the project, one acknowledged chunk at a time, so
     a refusal stops the transfer and the socket never queues a whole
     file.  Shared by the lobby tree and a room's prompt attachments. */
  function uploader({send, setTimer = setTimeout, clearTimer = clearTimeout}) {
    const waiting = new Map();
    let sequence = 0;

    function acknowledged(reqId) {
      return new Promise((resolve, reject) => {
        const timer = setTimer(() => {
          waiting.delete(reqId);
          reject(new Error('The host did not answer the upload.'));
        }, ACK_TIMEOUT_MS);
        waiting.set(reqId, frame => {
          clearTimer(timer);
          if (typeof frame.error === 'string') reject(new Error(frame.error));
          else resolve(frame);
        });
      });
    }

    // The host holds one upload per guest, so uploads run one at a time.
    let queue = Promise.resolve();
    function upload(file, dir, options) {
      const run = queue.then(() => transfer(file, dir, options));
      queue = run.catch(() => {});
      return run;
    }

    // Upload FILE into folder DIR; RENAME lets the host number a taken
    // name. Resolves with the project path; PROGRESS gets 0..1.
    async function transfer(file, dir, {rename = false, progress = () => {}} = {}) {
      if (file.size > MAX_UPLOAD_BYTES) {
        throw new Error(`${file.name} is over the ${MAX_UPLOAD_BYTES / 1048576} MB upload limit.`);
      }
      const reqId = ++sequence;
      let offset = 0;
      for (;;) {
        const end = Math.min(file.size, offset + CHUNK_BYTES);
        const bytes = new Uint8Array(await file.slice(offset, end).arrayBuffer());
        const frame = {t: 'file-upload', reqId, data: base64(bytes)};
        if (offset === 0) Object.assign(frame, {dir, name: file.name, size: file.size, rename});
        if (end === file.size) frame.final = true;
        const answer = acknowledged(reqId);
        if (!await send(frame)) {
          waiting.get(reqId)?.({error: 'Connection lost; the upload stopped.'});
          waiting.delete(reqId);
        }
        const reply = await answer;
        progress(file.size ? end / file.size : 1);
        if (frame.final) return reply.path;
        offset = end;
      }
    }

    function handle(frame) {
      const settle = waiting.get(frame.reqId);
      if (!settle) return;
      waiting.delete(frame.reqId);
      settle(frame);
    }

    return Object.freeze({upload, handle});
  }

  function create({send, el, notice, uploads, openFile, confirm = text => window.confirm(text)}) {
    const section = document.getElementById('lobby-files');
    const crumbs = document.getElementById('files-path');
    const list = document.getElementById('files-list');
    const status = document.getElementById('files-status');
    const uploadButton = document.getElementById('files-upload');
    const picker = document.getElementById('files-input');
    const formatBytes = bytes => window.mevedelTranscriptRenderer.formatBytes(bytes);

    let dir = '';
    let latest = 0;
    let sequence = 0;
    let busy = false;
    const removals = new Map();

    const say = text => {
      status.textContent = text;
      status.hidden = !text;
    };
    const join = name => (dir ? `${dir}/${name}` : name);

    function load(next = dir) {
      latest = ++sequence;
      // Until the host answers, say so: a silent host would leave a blank.
      if (!busy) say('Loading…');
      send({t: 'files', reqId: latest, dir: next});
    }

    // The lobby header already names the project, so the root crumb is
    // only the way back up, and the top level shows no trail at all.
    function renderCrumbs() {
      const parts = dir ? dir.split('/') : [];
      crumbs.hidden = !parts.length;
      const link = (label, target) => {
        const button = el('button', 'files-crumb', label);
        button.type = 'button';
        button.addEventListener('click', () => load(target));
        return button;
      };
      const nodes = [link('All files', '')];
      parts.forEach((part, index) => {
        nodes.push(el('span', 'files-sep', '/'));
        nodes.push(link(part, parts.slice(0, index + 1).join('/')));
      });
      crumbs.replaceChildren(...nodes);
    }

    function renderEntry(entry) {
      const item = el('li', 'lobby-row files-row');
      const path = join(entry.name);
      const name = el('button', 'files-name');
      name.type = 'button';
      if (entry.kind === 'dir') {
        name.append(el('span', 'files-icon', '▸'), el('span', '', `${entry.name}/`));
        name.setAttribute('aria-label', `Open folder ${entry.name}`);
        name.addEventListener('click', () => load(path));
        item.append(name);
        return item;
      }
      name.append(el('span', 'files-icon', '·'), el('span', '', entry.name));
      name.setAttribute('aria-label', `View ${entry.name}`);
      name.addEventListener('click', () => openFile(path));
      item.append(name);
      if (typeof entry.size === 'number') item.append(el('span', 'lobby-meta', formatBytes(entry.size)));
      const remove = el('button', 'btn quiet danger', 'Remove');
      remove.type = 'button';
      remove.setAttribute('aria-label', `Remove ${entry.name}`);
      remove.addEventListener('click', () => {
        if (!confirm(`Move ${path} to the host's trash? It leaves the project for every session.`)) return;
        const reqId = ++sequence;
        removals.set(reqId, path);
        remove.disabled = true;
        send({t: 'file-remove', reqId, path});
      });
      item.append(remove);
      return item;
    }

    function listed(frame) {
      if (frame.reqId !== latest) return;
      if (typeof frame.error === 'string') {
        // A folder emptied or removed under us: fall back to the root.
        if (dir) {
          dir = '';
          load('');
        } else say(frame.error);
        return;
      }
      dir = typeof frame.dir === 'string' ? frame.dir : '';
      const entries = Array.isArray(frame.entries) ? frame.entries : [];
      renderCrumbs();
      list.replaceChildren(...entries.map(renderEntry));
      const more = typeof frame.omitted === 'number' ? frame.omitted : 0;
      if (!busy) {
        say(more ? `${more} more entries not shown.`
          : entries.length ? '' : 'No files here yet.');
      }
    }

    function removed(frame) {
      const path = removals.get(frame.reqId);
      if (!path) return;
      removals.delete(frame.reqId);
      notice(typeof frame.error === 'string' ? frame.error : `${path} moved to the trash.`);
      load();
    }

    // Another guest changed a folder; only the one on screen matters.
    function changed(frame) {
      if (!section.hidden && frame.dir === dir) load();
    }

    async function uploadAll(files) {
      if (busy || !files.length) return;
      busy = true;
      uploadButton.disabled = true;
      const target = dir;
      const done = [];
      try {
        for (const file of files) {
          try {
            const path = await uploads.upload(file, target, {
              progress: share => say(`Uploading ${file.name}… ${Math.round(share * 100)}%`),
            });
            done.push(path);
          } catch (error) {
            notice(error.message);
          }
        }
      } finally {
        busy = false;
        uploadButton.disabled = false;
        say('');
        if (done.length) notice(done.length === 1 ? `${done[0]} added.` : `${done.length} files added.`);
        load();
      }
    }

    uploadButton.addEventListener('click', () => picker.click());
    picker.addEventListener('change', () => {
      uploadAll([...picker.files]);
      picker.value = '';
    });
    // Files dropped on the tree land in the folder it shows.
    window.mevedelAttachments.bind({add: files => uploadAll(files)}, {target: section});

    // The tree is fetched when first shown and refreshed with the lobby.
    return Object.freeze({show: () => load(), refresh: () => load(), listed, removed, changed});
  }

  window.mevedelFilesView = Object.freeze({create, uploader, base64, CHUNK_BYTES});
})();
