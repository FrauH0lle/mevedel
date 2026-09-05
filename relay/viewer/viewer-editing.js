/* Trusted room controller. The opaque editor gets only an item-scoped port. */
'use strict';
window.mevedelEditingView = {
  create({ state, send, el, flash, summarize }) {
    const box = document.getElementById('editing-box'),
      list = document.getElementById('editing-items');
    const panel = document.getElementById('editing-panel'),
      holder = document.getElementById('editing-body');
    const pending = new Map(),
      transfers = new Map(),
      catalog = new Map();
    let port = null,
      current = null,
      connected = false,
      room = window.mevedelViewerTransport.parseFragment(window.location.hash)?.roomId || '',
      frame = null,
      sequence = 0,
      outbound = Promise.resolve();
    const b64 = (text) => {
      const bytes = new TextEncoder().encode(text);
      let raw = '';
      for (let i = 0; i < bytes.length; i += 8192)
        raw += String.fromCharCode(...bytes.subarray(i, i + 8192));
      return btoa(raw);
    };
    const decode = (text) =>
      new TextDecoder('utf-8', { fatal: true }).decode(
        Uint8Array.from(atob(text), (c) => c.charCodeAt(0)),
      );
    const draftKey = (id) => `mevedel-editing:${room}:${id}`;
    function readDraft(id) {
      try {
        const draft = JSON.parse(localStorage.getItem(draftKey(id)));
        return draft &&
          draft.id === id &&
          ['whiteboard', 'document'].includes(draft.kind) &&
          typeof draft.crdt === 'string' &&
          draft.crdt.length <= 24 * 1024 * 1024
          ? draft
          : null;
      } catch (_) {
        return null;
      }
    }
    function recoveryCatalog() {
      try {
        const prefix = draftKey('');
        for (let i = 0; i < localStorage.length; i++) {
          const key = localStorage.key(i);
          if (!key?.startsWith(prefix)) continue;
          const id = key.slice(prefix.length),
            draft = readDraft(id);
          if (draft && !catalog.has(id))
            catalog.set(id, { id, kind: draft.kind, title: draft.title, local: true });
        }
      } catch (_) {
        /* Existing open editors can still export without storage. */
      }
    }
    function request(args) {
      if (!connected) return Promise.reject(new Error('Disconnected; your edits remain pending'));
      const reqId = ++sequence,
        data = b64(JSON.stringify(args));
      if (data.length > 24 * 1024 * 1024)
        return Promise.reject(new Error('Shared content is too large'));
      if (
        pending.size >= 64 ||
        data.length + [...pending.values()].reduce((n, p) => n + p.bytes, 0) > 32 * 1024 * 1024
      )
        return Promise.reject(new Error('Wait for pending transfers'));
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          pending.delete(reqId);
          transfers.delete(reqId);
          reject(new Error('No save acknowledgement; reconnect to retry'));
        }, 45000);
        pending.set(reqId, { resolve, reject, timer, bytes: data.length });
        const work = outbound.then(async () => {
          for (let offset = 0; offset < data.length; offset += 65536)
            if (!pending.has(reqId)) throw new Error('Transfer expired');
            else if (
              !(await send({
                t: 'editing',
                reqId,
                offset,
                total: data.length,
                data: data.slice(offset, offset + 65536),
              }))
            )
              throw new Error('Disconnected; save is pending');
        });
        outbound = work.catch((error) => {
          clearTimeout(timer);
          pending.delete(reqId);
          reject(error);
        });
      });
    }
    function render() {
      box.hidden = false;
      list.replaceChildren();
      for (const item of catalog.values()) {
        const button = el(
          'button',
          'btn quiet',
          `${item.kind === 'whiteboard' ? '▧' : '▤'} ${item.title}${item.local ? ' (local recovery)' : ''}`,
        );
        button.dataset.itemId = item.id;
        button.type = 'button';
        button.onclick = () => open(item.id).catch((e) => flash(e.message));
        list.append(button);
      }
      document
        .querySelectorAll('[data-create-editor],#editing-import')
        .forEach((button) => (button.hidden = state.readOnly));
      summarize('editing', catalog.size ? `${catalog.size} shared` : 'Shared');
    }
    function saveDraft(id, draft) {
      try {
        const text = JSON.stringify(draft);
        if (text.length > 24 * 1024 * 1024) throw new Error('Draft too large');
        localStorage.setItem(draftKey(id), text);
        port?.postMessage({ type: 'storage-ok' });
        return true;
      } catch (_) {
        port?.postMessage({
          type: 'storage-error',
          message:
            'Browser recovery storage is unavailable. Download a recovery copy before closing.',
        });
        return false;
      }
    }
    function download(result, title) {
      const bytes = result.data
        ? Uint8Array.from(atob(result.data), (c) => c.charCodeAt(0))
        : result.text;
      const url = URL.createObjectURL(new Blob([bytes], { type: result.mime }));
      const a = el('a');
      a.href = url;
      a.download = `${(title || 'shared').replace(/[^\p{L}\p{N} _-]/gu, '_')}.${result.extension}`;
      a.click();
      setTimeout(() => URL.revokeObjectURL(url), 10000);
    }
    async function open(id) {
      if (current === id && frame) {
        panel.hidden = false;
        return;
      }
      const draft = readDraft(id);
      let result,
        recoveryOnly = false;
      try {
        result = await request({ action: 'read', id });
      } catch (error) {
        if (!draft) throw error;
        result = { ...draft, transactions: [] };
        recoveryOnly = true;
      }
      port?.close();
      current = id;
      panel.hidden = false;
      document.getElementById('editing-title').textContent = result.title;
      frame = el('iframe');
      frame.title = `Shared ${result.kind}: ${result.title}`;
      frame.setAttribute('sandbox', 'allow-scripts allow-forms');
      frame.src = '/shared-editor.html';
      holder.replaceChildren(frame);
      // Queue updates on the channel immediately, including while the iframe loads.
      const channel = new MessageChannel(),
        editorFrame = frame;
      port = channel.port1;
      port.onmessage = async ({ data }) => {
        if (!data || current !== id) return;
        if (data.type === 'draft') {
          saveDraft(id, data.draft);
          return;
        }
        if (data.type === 'recovery') {
          download(data.result, result.title);
          return;
        }
        if (data.type === 'presence') {
          if (connected && !state.readOnly)
            send({
              t: 'editing-presence',
              id,
              mode: data.mode,
              point: data.point,
              cursor: data.cursor,
              clientId: data.clientId,
              clock: data.clock,
            });
          return;
        }
        if (data.type !== 'request' || typeof data.reqId !== 'string') return;
        const args = data.args;
        if (
          !args ||
          !['read', 'update', 'rename', 'revert', 'export', 'ask'].includes(args.action) ||
          (state.readOnly && !['read', 'export'].includes(args.action))
        ) {
          channel.port1.postMessage({
            type: 'reply',
            reqId: data.reqId,
            error: 'This editor does not permit that operation',
          });
          return;
        }
        try {
          const value = await request({ ...args, id });
          if (current !== id) return;
          if (args.action === 'export') download(value, result.title);
          port.postMessage({ type: 'reply', reqId: data.reqId, result: value });
        } catch (error) {
          if (current === id)
            channel.port1.postMessage({ type: 'reply', reqId: data.reqId, error: error.message });
        }
      };
      frame.onload = () => {
        if (current !== id || frame !== editorFrame) return;
        frame.contentWindow.postMessage(
          {
            type: 'mevedel-editor',
            item: result,
            draft,
            readOnly: state.readOnly || recoveryOnly,
            online: connected && !recoveryOnly,
            name: state.guestName || 'Participant',
          },
          '*',
          [channel.port2],
        );
      };
    }
    async function welcome() {
      connected = true;
      // Room identity contains no bearer credentials. Never send the fragment to the editor.
      room = window.mevedelViewerTransport.parseFragment(`#${state.fragment}`)?.roomId || '';
      try {
        const items = await request({ action: 'list' });
        catalog.clear();
        items.forEach((item) => catalog.set(item.id, item));
        recoveryCatalog();
        render();
        if (current) {
          const result = await request({ action: 'read', id: current });
          port?.postMessage({ type: 'sync', item: result, readOnly: state.readOnly });
        }
      } catch (error) {
        flash(error.message);
      }
    }
    function connection(up) {
      if (up) return;
      // The main viewer restores stored credentials after stripping the URL hash.
      room = window.mevedelViewerTransport.parseFragment(`#${state.fragment}`)?.roomId || room;
      connected = false;
      transfers.clear();
      port?.postMessage({ type: 'offline' });
      for (const entry of pending.values()) {
        clearTimeout(entry.timer);
        entry.reject(new Error('Disconnected; changes remain pending'));
      }
      pending.clear();
      recoveryCatalog();
      if (catalog.size) render();
    }
    function receive(frame) {
      if (frame.t === 'editing-presence') {
        if (frame.id === current) port?.postMessage({ type: 'presence', ...frame });
        return;
      }
      if (frame.t !== 'editing') return;
      if (frame.reqId !== 'event' && !pending.has(frame.reqId)) return;
      if (
        !Number.isSafeInteger(frame.total) ||
        frame.total > 48 * 1024 * 1024 ||
        typeof frame.data !== 'string' ||
        frame.data.length > 65536
      )
        return;
      let t = transfers.get(frame.reqId);
      if (frame.offset === 0) {
        const reserved = [...transfers.entries()].reduce(
          (n, [id, value]) => n + (id === frame.reqId ? 0 : value.total),
          frame.total,
        );
        if (reserved > 64 * 1024 * 1024) return;
        t = { offset: 0, total: frame.total, parts: [] };
        transfers.set(frame.reqId, t);
      }
      if (
        !t ||
        t.offset !== frame.offset ||
        t.total !== frame.total ||
        t.offset + frame.data.length > t.total
      ) {
        transfers.delete(frame.reqId);
        return;
      }
      t.parts.push(frame.data);
      t.offset += frame.data.length;
      if (t.offset !== t.total) return;
      transfers.delete(frame.reqId);
      let value;
      try {
        value = JSON.parse(decode(t.parts.join('')));
      } catch (_) {
        return;
      }
      if (frame.reqId === 'event') {
        const { id, kind, title, revision } = value,
          previous = catalog.get(id);
        catalog.set(id, { id, kind, title, revision });
        if (!previous || previous.title !== title || previous.kind !== kind) render();
        if (id === current) {
          document.getElementById('editing-title').textContent = title;
          port?.postMessage({ type: 'changed', ...value });
        }
      } else {
        const entry = pending.get(frame.reqId);
        if (!entry) return;
        pending.delete(frame.reqId);
        clearTimeout(entry.timer);
        if (value.error) entry.reject(new Error(value.error));
        else entry.resolve(value.result);
      }
    }
    document.querySelectorAll('[data-create-editor]').forEach(
      (button) =>
        (button.onclick = async () => {
          try {
            const item = await request({
              action: 'create',
              kind: button.dataset.createEditor,
              id: crypto.randomUUID(),
              opId: crypto.randomUUID(),
              title: button.dataset.createEditor === 'whiteboard' ? 'Whiteboard' : 'Document',
            });
            await open(item.id);
          } catch (error) {
            flash(error.message);
          }
        }),
    );
    const input = document.getElementById('editing-file');
    document.getElementById('editing-import').onclick = () => input.click();
    input.onchange = async () => {
      const file = input.files[0];
      input.value = '';
      if (!file) return;
      try {
        if (file.size > 16 * 1024 * 1024) throw new Error('File is too large');
        let format = file.name.endsWith('.json')
            ? 'native'
            : file.name.endsWith('.md')
              ? 'markdown'
              : 'text',
          data;
        if (/^image\/(png|jpeg|webp)$/.test(file.type)) {
          const src = await new Promise((resolve, reject) => {
            const reader = new FileReader();
            reader.onload = () => resolve(reader.result);
            reader.onerror = reject;
            reader.readAsDataURL(file);
          });
          const bitmap = await createImageBitmap(file),
            scale = Math.min(1, 640 / bitmap.width);
          data = JSON.stringify({
            format: 'mevedel-editable-1',
            kind: 'whiteboard',
            title: file.name,
            content: [
              {
                id: crypto.randomUUID(),
                type: 'image',
                box: [0, 0, bitmap.width * scale, bitmap.height * scale],
                src,
              },
            ],
          });
          bitmap.close();
          format = 'native';
        } else {
          if (!/\.(md|txt|json)$/i.test(file.name))
            throw new Error('Use a native snapshot, Markdown, text, PNG, JPEG, or WebP file');
          data = await file.text();
        }
        const item = await request({
          action: 'import',
          format,
          data,
          title: file.name,
          id: crypto.randomUUID(),
          opId: crypto.randomUUID(),
        });
        await open(item.id);
      } catch (error) {
        flash(error.message);
      }
    };
    document.getElementById('editing-close').onclick = () => {
      panel.hidden = true;
      port?.postMessage({ type: 'closed' });
      if (connected && !state.readOnly)
        send({ t: 'editing-presence', id: current, mode: 'clear', point: null });
    };
    return { welcome, connection, receive, open };
  },
};
