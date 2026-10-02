'use strict';

/* Prompt attachments for every composer that asks the model.  Photos are
   downscaled client-side to fit the sealed prompt frame under the relay's
   read limit; other files are refused when they overrun it, because a log
   cannot be made smaller by resampling.  The host enforces the same
   allowlist and budget. */
(() => {
  const MAX_FILES = 3;
  // Decoded bytes, all attachments. Base64 costs a third and the prompt
  // text shares the frame, so this leaves the 2 MiB relay limit ~85 KiB
  // of headroom even with a maximum-length prompt beside it.
  const FILE_BUDGET = 1280 * 1024;
  // Mirrors the host's allowlist. Read decides text or media downstream.
  const MIME_BY_EXTENSION = {
    jpg: 'image/jpeg', jpeg: 'image/jpeg', png: 'image/png',
    webp: 'image/webp', pdf: 'application/pdf', txt: 'text/plain',
    log: 'text/plain', text: 'text/plain', md: 'text/markdown',
    csv: 'text/csv', json: 'application/json',
    patch: 'text/x-patch', diff: 'text/x-patch',
  };
  const ALLOWED_MIME = new Set(Object.values(MIME_BY_EXTENSION));

  // Browsers report "" or application/octet-stream for .log, .patch, and
  // friends, so the extension decides whenever the type is not one we take.
  function attachmentMime(file) {
    if (ALLOWED_MIME.has(file.type)) return file.type;
    const extension = (file.name || '').split('.').pop().toLowerCase();
    return MIME_BY_EXTENSION[extension] || null;
  }

  // Any other UTF-8 text -- source, markup, configuration -- goes as text
  // and keeps its own extension, which the host checks again.
  function textual(buffer) {
    if (buffer.includes(0)) return false;
    try { new TextDecoder('utf-8', {fatal: true}).decode(buffer); return true; }
    catch (_error) { return false; }
  }
  function ownExtension(file) {
    const name = (file.name || '').toLowerCase(), dot = name.lastIndexOf('.');
    const extension = dot > 0 ? name.slice(dot + 1) : '';
    return /^[a-z0-9]{1,12}$/.test(extension) ? extension : 'txt';
  }

  function base64OfBytes(buffer) {
    let binary = '';
    buffer.forEach(byte => { binary += String.fromCharCode(byte); });
    return btoa(binary);
  }

  async function downscaleImage(file, budget) {
    const bitmap = await createImageBitmap(file);
    try {
      const longest = Math.max(bitmap.width, bitmap.height);
      let scale = Math.min(1, 1568 / longest);
      for (let attempt = 0; attempt < 5; attempt++) {
        const canvas = document.createElement('canvas');
        canvas.width = Math.max(1, Math.round(bitmap.width * scale));
        canvas.height = Math.max(1, Math.round(bitmap.height * scale));
        canvas.getContext('2d').drawImage(bitmap, 0, 0,
                                          canvas.width, canvas.height);
        const quality = Math.max(0.4, 0.85 - attempt * 0.15);
        const blob = await new Promise(resolve =>
          canvas.toBlob(resolve, 'image/jpeg', quality));
        if (blob && blob.size <= budget) return blob;
        if (attempt >= 2) scale *= 0.7;
      }
      return null;
    } finally {
      bitmap.close();
    }
  }

  /* A tray of pending files rendered as chips into LIST.  NOTICE reports a
     refused file; ONCHANGE runs when the person adds or removes one.
     Thumbnails are data: URLs, which the editor frame's policy admits where
     blob: URLs are not. */
  function create({list, notice, onchange = () => {}}) {
    const pending = []; // {mime, label, data (base64), bytes}
    let generation = 0;
    let work = Promise.resolve();
    let inFlight = 0;

    function render() {
      if (!list) return;
      list.replaceChildren();
      pending.forEach((item, index) => {
        const chip = document.createElement('span');
        chip.className = 'attachment';
        if (item.mime.startsWith('image/')) {
          const thumb = document.createElement('img');
          thumb.className = 'attachment-thumb';
          thumb.src = `data:${item.mime};base64,${item.data}`;
          thumb.alt = `attachment ${index + 1}`;
          chip.append(thumb);
        } else {
          const name = document.createElement('span');
          name.className = 'attachment-name';
          name.textContent = item.label;
          chip.append(name);
        }
        const remove = document.createElement('button');
        remove.className = 'attachment-remove';
        remove.textContent = '✕';
        remove.type = 'button';
        remove.setAttribute('aria-label', `Remove attachment ${index + 1}`);
        remove.addEventListener('click', () => {
          pending.splice(index, 1);
          render();
          onchange();
        });
        chip.append(remove);
        list.append(chip);
      });
    }

    async function addNow(files, current) {
      for (const file of files) {
        if (current !== generation) return;
        let mime = attachmentMime(file), extension;
        if (!mime && file.size <= FILE_BUDGET) {
          const head = new Uint8Array(await file.arrayBuffer());
          if (current !== generation) return;
          if (textual(head)) [mime, extension] = ['text/plain', ownExtension(file)];
        }
        if (!mime) {
          notice(`${file.name || 'That file'} is neither text nor an accepted type.`);
          continue;
        }
        if (pending.length >= MAX_FILES) {
          notice(`At most ${MAX_FILES} attachments per prompt.`);
          break;
        }
        const budget = FILE_BUDGET - pending.reduce((sum, item) => sum + item.bytes, 0);
        let label = file.name || 'attachment';
        let blob = file;
        let type = mime;
        if (mime.startsWith('image/')) {
          blob = await downscaleImage(file, budget).catch(() => null);
          if (current !== generation) return;
          if (!blob) {
            notice('Image too large for the frame budget.');
            continue;
          }
          label = file.name || 'photo';
          type = 'image/jpeg';
        } else if (file.size > budget) {
          notice(`${file.name || 'That file'} is over the `
                 + `${Math.floor(budget / 1024)} KB left in this prompt.`);
          continue;
        }
        const buffer = new Uint8Array(await blob.arrayBuffer());
        if (current !== generation) return;
        pending.push({mime: type, label, data: base64OfBytes(buffer), bytes: buffer.length,
                      ...(extension ? {extension} : {})});
      }
      render();
      onchange();
    }

    return Object.freeze({
      // Queue FILES behind earlier additions; the promise settles when
      // they are in the tray.
      add(files) {
        const current = generation;
        inFlight += 1;
        const next = work.then(() => addNow(files, current))
          .finally(() => { inFlight -= 1; });
        work = next.catch(() => {});
        return next;
      },
      // Settles once every addition so far has been taken in.
      settled() { return work; },
      // Whether an addition is still being read or downscaled.
      busy() { return inFlight > 0; },
      items() { return pending.slice(); },
      // The frame representation of ITEMS.
      frame(items) {
        return items.map(item => ({mime: item.mime, data: item.data,
                                   ...(item.extension ? {extension: item.extension} : {})}));
      },
      remove(items) {
        items.forEach(item => {
          const index = pending.indexOf(item);
          if (index >= 0) pending.splice(index, 1);
        });
        render();
      },
      clear() {
        generation++;
        pending.splice(0);
        render();
      },
    });
  }

  /* Feed TRAY from the file picker behind BUTTON, from files pasted into
     INPUT, and from files dropped on TARGET, which is marked while a file
     drag is over it.  Other drags keep the browser default. */
  function bind(tray, {target, input, button, picker}) {
    if (button && picker) {
      button.addEventListener('click', () => picker.click());
      picker.addEventListener('change', () => {
        tray.add([...picker.files]);
        picker.value = '';
      });
    }
    if (input) {
      input.addEventListener('paste', event => {
        const files = [...(event.clipboardData?.items || [])]
          .filter(item => item.kind === 'file')
          .map(item => item.getAsFile())
          .filter(Boolean);
        if (files.length) tray.add(files);
      });
    }
    if (target) {
      const carriesFiles = event => [...(event.dataTransfer?.types || [])].includes('Files');
      let depth = 0;
      const settle = () => { depth = 0; delete target.dataset.dropping; };
      target.addEventListener('dragenter', event => {
        if (!carriesFiles(event)) return;
        depth += 1;
        target.dataset.dropping = '';
      });
      target.addEventListener('dragover', event => {
        if (carriesFiles(event)) event.preventDefault();
      });
      target.addEventListener('dragleave', event => {
        if (carriesFiles(event) && --depth <= 0) settle();
      });
      target.addEventListener('drop', event => {
        if (!carriesFiles(event)) return;
        event.preventDefault();
        // An editor canvas below takes dropped images itself.
        event.stopPropagation?.();
        settle();
        tray.add([...event.dataTransfer.files]);
      });
    }
  }

  window.mevedelAttachments = Object.freeze({create, bind, MAX_FILES, FILE_BUDGET});
})();
