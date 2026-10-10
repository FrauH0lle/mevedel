/* Trusted room controller. The opaque editor gets only an item-scoped port. */
'use strict';
window.mevedelEditingView = {
  create({ state, send, el, flash, summarize, onVisibility = () => {}, onCatalog = () => {} }) {
    const box = document.getElementById('editing-box'),
      list = document.getElementById('editing-items');
    const panel = document.getElementById('editing-panel'),
      holder = document.getElementById('editing-body');
    const editorTab = new URL(window.location.href).searchParams.has('shared');
    let requestedItem = new URL(window.location.href).searchParams.get('shared');
    const pending = new Map(),
      transfers = new Map(),
      catalog = new Map();
    let port = null,
      catalogKnown = false,
      catalogFailed = false,
      current = null,
      connected = false,
      room = window.mevedelViewerTransport.parseFragment(window.location.hash)?.roomId || '',
      frame = null,
      recovering = false,
      sequence = 0,
      openingGeneration = 0,
      outbound = Promise.resolve();
    let appearance = null, archived = [], conversationTruncated = false, conversationError = null;
    let available = false, checking = false, availabilityGeneration = 0;
    // The room lists only items attached to its session; the open item and
    // local recoveries stay listed so they remain reachable.
    let attached = new Set();
    let unavailableReason = 'Checking shared editing on the Emacs host…';
    function setAppearance(value) {
      appearance = value;
      port?.postMessage({ type: 'appearance', appearance });
    }
    function reserveTab() {
      if (editorTab) return null;
      const tab = window.open('about:blank', '_blank');
      if (!tab) throw new Error('Allow a new tab to open the editor');
      tab.opener = null;
      return tab;
    }
    // Open item ID as the Shared work list does, saying why when it cannot.
    async function openItem(id) {
      try {
        await launch(id);
      } catch (e) {
        flash(e.message);
      }
    }
    function launch(id, tab) {
      if (editorTab) return open(id);
      const url = new URL(window.location.href);
      url.searchParams.set('shared', id);
      url.hash = state.fragment || window.location.hash;
      (tab || reserveTab()).location.replace(url.href);
    }
    // Mobile keyboards resize the visual viewport, not necessarily the layout
    // viewport. Keep the complete editor above the keyboard, including in Safari.
    function viewport() {
      if (!window.visualViewport) return;
      panel.style.height = `${window.visualViewport.height}px`;
      panel.style.top = `${window.visualViewport.offsetTop}px`;
    }
    window.visualViewport?.addEventListener('resize', viewport);
    window.visualViewport?.addEventListener('scroll', viewport);
    viewport();
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
    function forgetDraft(id) {
      try { localStorage.removeItem(draftKey(id)); } catch (_) { /* storage unavailable */ }
    }
    /* List local drafts the catalog lacks as recoveries.  Only a catalog just
       listed by the host is AUTHORITATIVE about what it no longer has. */
    function recoveryCatalog(authoritative = false) {
      try {
        const prefix = draftKey('');
        const gone = [];
        for (let i = 0; i < localStorage.length; i++) {
          const key = localStorage.key(i);
          if (!key?.startsWith(prefix)) continue;
          const id = key.slice(prefix.length),
            draft = readDraft(id);
          // A draft of an item the host no longer has is worth keeping only
          // while it holds edits that never reached the host.
          if (authoritative && draft && !catalog.has(id) && !draft.pending?.length) gone.push(id);
          else if (draft && !catalog.has(id))
            catalog.set(id, {
              id,
              kind: draft.kind,
              title: draft.title,
              local: true,
            });
        }
        gone.forEach(forgetDraft);
      } catch (_) {
        /* Existing open editors can still export without storage. */
      }
    }
    function request(args) {
      if (!connected) return Promise.reject(new Error('Disconnected; your edits remain pending'));
      if (!available && !['list', 'status'].includes(args.action))
        return Promise.reject(new Error(unavailableReason));
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
    function setConversationHistory(result) {
      archived = result.conversation || [];
      conversationTruncated = Boolean(result.conversationTruncated);
      conversationError = result.conversationError || null;
      conversation();
    }
    async function refreshConversation() {
      if (!connected || !current) return;
      const id = current;
      try {
        const result = await request({ action: 'read', id });
        if (current === id) setConversationHistory(result);
      } catch (error) {
        if (current === id) {
          conversationError = error.message;
          conversation();
        }
      }
    }
    function conversation() {
      if (!port || !current) return;
      const live = [], questions = new Set();
      // A running request is the room's latest user turn.
      let shared, active = null;
      for (const record of state.records.values()) {
        if (record.kind === 'user') {
          shared = record.shared;
          active = shared?.itemId === current ? shared.questionId : null;
        }
        if (shared?.itemId === current) {
          live.push(record);
          questions.add(shared.questionId);
        }
      }
      // Compaction preserves a live tail. Prefer its entire question group over
      // the archived copy, including responses updated after the archive was saved.
      const records = [];
      shared = null;
      for (const record of archived) {
        if (record.kind === 'user') shared = record.shared;
        if (shared?.itemId === current && !questions.has(shared.questionId)) records.push(record);
      }
      records.push(...live);
      port.postMessage({type:'conversation', records, conversationTruncated, conversationError,
        own:(state.ownQueue || []).filter(entry => entry.shared?.itemId === current),
        busy:state.busy, active:state.busy ? active : null, paused:state.paused, connected,
        model:state.model});
    }
    function render() {
      box.hidden = false;
      document.getElementById('editing-status').textContent = available
        ? 'Shared editing is ready.' : unavailableReason;
      const recheck = document.getElementById('editing-recheck');
      recheck.disabled = !connected || checking;
      recheck.textContent = checking ? 'Checking…' : 'Recheck availability';
      list.replaceChildren();
      const shown = [...catalog.values()].filter(
        (item) => attached.has(item.id) || item.local || item.id === current);
      for (const item of shown) {
        const button = el('button', 'btn quiet');
        const symbol = el('span', `item-symbol${item.kind === 'whiteboard' ? ' board' : ''}`, item.kind === 'whiteboard' ? 'MAP' : 'DOC');
        symbol.setAttribute('aria-hidden', 'true');
        button.append(symbol, el('span', '', `${item.title}${item.local ? ' (local recovery)' : ''}`));
        button.dataset.itemId = item.id;
        button.type = 'button';
        button.disabled = !available && !(current === item.id && frame) && !readDraft(item.id);
        button.title = button.disabled ? unavailableReason : `Open ${item.title}`;
        button.setAttribute('aria-describedby', 'editing-status');
        button.onclick = () => openItem(item.id);
        list.append(button);
      }
      document
        .querySelectorAll('[data-create-editor],#editing-import')
        .forEach((button) => {
          button.hidden = state.readOnly;
          button.disabled = !available;
          button.title = available ? '' : unavailableReason;
          button.setAttribute('aria-describedby', 'editing-status');
        });
      summarize('editing', shown.length ? `${shown.length} shared` : 'Shared');
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
            'Browser recovery storage is unavailable. Download a recovery copy and copy your question draft before closing.',
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
    async function open(id, committed) {
      const generation = ++openingGeneration;
      if (editorTab) {
        const url = new URL(window.location.href);
        url.searchParams.set('shared', id);
        window.history.replaceState(null, '', url);
      }
      if (current === id && frame && !committed) {
        panel.hidden = false;
        onVisibility(true);
        document.title =
          state.editorTitle = `${document.getElementById('editing-title').textContent} · mevedel`;
        return;
      }
      const draft = readDraft(id);
      let result,
        recoveryOnly = false;
      try {
        result = committed || await request({ action: 'read', id });
      } catch (error) {
        if (!draft) throw error;
        result = { ...draft, transactions: [] };
        recoveryOnly = true;
      }
      if (generation !== openingGeneration) return;
      port?.close();
      recovering = recoveryOnly;
      current = id;
      setConversationHistory(result);
      panel.hidden = false;
      onVisibility(true);
      document.getElementById('editing-title').textContent = result.title;
      document.title = state.editorTitle = `${result.title} · mevedel`;
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
              trail: data.trail,
              preview: data.preview,
              cursor: data.cursor,
              clientId: data.clientId,
              clock: data.clock,
            });
          return;
        }
        if (data.type === 'delete') {
          if (!state.readOnly && catalog.has(id)) confirmDelete(catalog.get(id));
          return;
        }
        if (data.type !== 'request' || typeof data.reqId !== 'string') return;
        const args = data.args;
        if (
          !args ||
          !['read', 'update', 'rename', 'revert', 'export', 'ask', 'comment', 'reply-comment', 'resolve-comment',
            'library', 'library-add', 'library-remove', 'library-install', 'library-uninstall',
            'library-catalog', 'library-fetch'].includes(args.action) ||
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
            channel.port1.postMessage({
              type: 'reply',
              reqId: data.reqId,
              error: error.message,
            });
        }
      };
      conversation();
      frame.onload = () => {
        if (current !== id || frame !== editorFrame) return;
        frame.contentWindow.postMessage(
          {
            type: 'mevedel-editor',
            item: result,
            draft,
            readOnly: state.readOnly || recoveryOnly,
            online: connected && !recoveryOnly,
            appearance,
            name: state.guestName || 'Participant',
          },
          '*',
          [channel.port2],
        );
      };
    }
    function pendingView(message, retry = false) {
      if (!requestedItem || current) return;
      panel.hidden = false;
      onVisibility(true);
      document.getElementById('editing-title').textContent = catalog.get(requestedItem)?.title || 'Shared work';
      document.title = state.editorTitle = 'Shared work · mevedel';
      const note = el('p', 'panel-note', message);
      note.setAttribute('role', retry ? 'alert' : 'status');
      holder.replaceChildren(note);
      if (retry) {
        const button = el('button', 'btn quiet', 'Retry');
        button.type = 'button';
        button.onclick = recheck;
        holder.append(button);
      }
    }
    async function welcome() {
      connected = true;
      available = false;
      checking = true;
      const generation = ++availabilityGeneration;
      unavailableReason = 'Checking shared editing on the Emacs host…';
      pendingView(unavailableReason);
      render();
      // Room identity contains no bearer credentials. Never send the fragment to the editor.
      room = window.mevedelViewerTransport.parseFragment(`#${state.fragment}`)?.roomId || '';
      try {
        const items = await request({ action: 'list' });
        if (generation !== availabilityGeneration) return;
        catalog.clear();
        items.forEach((item) => catalog.set(item.id, item));
        catalogKnown = true;
        recoveryCatalog(true);
        render();
        onCatalog();
      } catch (error) {
        if (generation !== availabilityGeneration) return;
        flash(error.message);
        if (!catalogKnown && !catalogFailed) {
          catalogFailed = true;
          onCatalog();
        }
      }
      checking = false;
      await recheck();
    }
    async function recheck() {
      if (!connected || checking) return;
      const generation = ++availabilityGeneration;
      checking = true;
      available = false;
      unavailableReason = 'Checking shared editing on the Emacs host…';
      pendingView(unavailableReason);
      render();
      try {
        const result = await request({ action: 'status' });
        if (generation !== availabilityGeneration) return;
        if (result.available !== true) throw new Error('The host did not confirm shared editing availability.');
        available = true;
      } catch (error) {
        if (generation !== availabilityGeneration) return;
        unavailableReason = `Shared editing unavailable: ${error.message}`;
        port?.postMessage({ type: 'offline' });
        if (requestedItem) box.open = true;
      } finally {
        if (generation === availabilityGeneration) {
          checking = false;
          render();
        }
      }
      if (generation !== availabilityGeneration) return;
      try {
        if (requestedItem && !current) {
          if (!available && !readDraft(requestedItem)) {
            pendingView(unavailableReason, true);
            return;
          }
          const id = requestedItem;
          pendingView('Opening shared work…');
          await open(id);
          if (requestedItem === id) requestedItem = null;
          return;
        }
        if (current && available) {
          const result = await request({ action: 'read', id: current });
          setConversationHistory(result);
          if (recovering) await open(current, result);
          else port?.postMessage({
            type: 'sync',
            item: result,
            readOnly: state.readOnly,
          });
        }
      } catch (error) {
        pendingView(error.message, true);
        flash(error.message);
      }
    }
    function connection(up) {
      if (up) return;
      // The main viewer restores stored credentials after stripping the URL hash.
      room = window.mevedelViewerTransport.parseFragment(`#${state.fragment}`)?.roomId || room;
      connected = false;
      available = false;
      checking = false;
      availabilityGeneration++;
      unavailableReason = 'Shared editing unavailable while disconnected. Reconnect to check the Emacs host.';
      transfers.clear();
      port?.postMessage({ type: 'offline' });
      conversation();
      for (const entry of pending.values()) {
        clearTimeout(entry.timer);
        entry.reject(new Error('Disconnected; changes remain pending'));
      }
      pending.clear();
      recoveryCatalog();
      render();
      if (requestedItem && catalog.has(requestedItem) && !current) {
        const id = requestedItem;
        open(id).then(() => {
          if (requestedItem === id) requestedItem = null;
        }).catch((error) => {
          pendingView(error.message, true);
          flash(error.message);
        });
      }
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
      if (frame.reqId === 'event' && value.event === 'deleted') {
        removed(value);
      } else if (frame.reqId === 'event') {
        const { id, kind, title, revision } = value,
          previous = catalog.get(id);
        catalog.set(id, { id, kind, title, revision });
        if (!previous || previous.title !== title || previous.kind !== kind) render();
        if (id === current) {
          document.getElementById('editing-title').textContent = title;
          if (!panel.hidden) document.title = state.editorTitle = `${title} · mevedel`;
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
    // Deleting removes an item for everyone, with its comments and history.
    // There is no undo; the sheet offers a copy that can be imported again.
    const deleteSheet = document.getElementById('delete-shared');
    let deleting = null;
    function confirmDelete(item) {
      if (!deleteSheet || state.readOnly) return;
      deleting = item;
      const kind = item.kind === 'whiteboard' ? 'whiteboard' : 'document';
      document.getElementById('delete-shared-title').textContent = `Delete “${item.title}”?`;
      document.getElementById('delete-shared-note').textContent =
        `This removes the ${kind} for everyone in this room, with its comments and contribution history. `
        + 'It cannot be undone; a downloaded copy can be imported again.';
      deleteSheet.returnValue = '';
      deleteSheet.showModal();
    }
    document.getElementById('delete-shared-copy')?.addEventListener('click', async () => {
      if (!deleting) return;
      try {
        download(await request({ action: 'export', id: deleting.id, format: 'native' }), deleting.title);
      } catch (error) {
        flash(error.message);
      }
    });
    deleteSheet?.addEventListener('close', async () => {
      const item = deleting;
      deleting = null;
      if (!item || deleteSheet.returnValue !== 'delete') return;
      try {
        await request({ action: 'delete', id: item.id });
      } catch (error) {
        flash(error.message);
      }
    });
    // An item deleted anywhere leaves the list; an editor showing it closes.
    function removed({ id, actor }) {
      const item = catalog.get(id);
      catalog.delete(id);
      if (!readDraft(id)?.pending?.length) forgetDraft(id);
      recoveryCatalog();
      render();
      if (id === current && !panel.hidden) document.getElementById('editing-close').click();
      onCatalog();
      if (item) flash(`“${item.title}” was deleted${actor ? ` by ${actor}` : ''}.`);
    }
    document.querySelectorAll('[data-create-editor]').forEach(
      (button) =>
        (button.onclick = async () => {
          let tab;
          try {
            tab = reserveTab();
            const item = await request({
              action: 'create',
              kind: button.dataset.createEditor,
              id: crypto.randomUUID(),
              opId: crypto.randomUUID(),
              title: button.dataset.createEditor === 'whiteboard' ? 'Whiteboard' : 'Document',
            });
            await launch(item.id, tab);
          } catch (error) {
            tab?.close();
            flash(error.message);
          }
        }),
    );
    const input = document.getElementById('editing-file');
    document.getElementById('editing-import').onclick = () => input.click();
    document.getElementById('editing-recheck').onclick = recheck;
    input.onchange = async () => {
      const file = input.files[0];
      input.value = '';
      if (!file) return;
      let tab;
      try {
        tab = reserveTab();
        if (file.size > 16 * 1024 * 1024) throw new Error('File is too large');
        let format = /\.excalidraw$/i.test(file.name)
            ? 'excalidraw'
            : file.name.endsWith('.json')
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
            scale = Math.min(1, 640 / bitmap.width),
            fileId = crypto.randomUUID().replace(/-/g, '');
          // An image opens as an Excalidraw scene holding that one image.
          data = JSON.stringify({
            type: 'excalidraw',
            version: 2,
            elements: [
              {
                id: crypto.randomUUID(),
                type: 'image',
                x: 0,
                y: 0,
                width: bitmap.width * scale,
                height: bitmap.height * scale,
                fileId,
                status: 'saved',
              },
            ],
            files: { [fileId]: { id: fileId, mimeType: file.type, dataURL: src, created: Date.now() } },
          });
          bitmap.close();
          format = 'excalidraw';
        } else {
          if (!/\.(md|txt|json|excalidraw)$/i.test(file.name))
            throw new Error('Use an Excalidraw file, native snapshot, Markdown, text, PNG, JPEG, or WebP file');
          data = await file.text();
          // A .json file may be an Excalidraw scene saved under another name.
          if (format === 'native') {
            try {
              if (JSON.parse(data)?.type === 'excalidraw') format = 'excalidraw';
            } catch {
              /* The host reports malformed files. */
            }
          }
        }
        const item = await request({
          action: 'import',
          format,
          data,
          title: file.name,
          id: crypto.randomUUID(),
          opId: crypto.randomUUID(),
        });
        if (item.notes?.length) flash(item.notes.join(' '));
        await launch(item.id, tab);
      } catch (error) {
        tab?.close();
        flash(error.message);
      }
    };
    document.getElementById('editing-close').onclick = () => {
      openingGeneration++;
      panel.hidden = true;
      onVisibility(false);
      state.editorTitle = null;
      document.title = state.sessionName ? `${state.sessionName} · mevedel` : 'mevedel live session';
      requestedItem = null;
      const url = new URL(window.location.href);
      url.searchParams.delete('shared');
      window.history.replaceState(null, '', url);
      port?.postMessage({ type: 'closed' });
      if (connected && !state.readOnly)
        send({
          t: 'editing-presence',
          id: current,
          mode: 'clear',
          point: null,
        });
    };
    // A room message in an item's discussion asks about the whole item as
    // currently committed; the reply lands in that item's conversation.
    function ask(id, text, images = []) {
      const questionId = crypto.randomUUID();
      return request({ action: 'ask', id, opId: questionId, questionId, text, whole: true,
        ...(images.length ? { images } : {}) });
    }
    // Whether item ID still exists on the host: null until the host has
    // listed its items, and assumed when it could not list them.
    function present(id) {
      return catalogKnown ? catalog.has(id) : catalogFailed ? true : null;
    }
    function attachedItems(ids) {
      attached = new Set(ids);
      if (!box.hidden) render();
    }
    return { welcome, connection, receive, open, openItem, conversation, refreshConversation, setAppearance, ask, present,
             attachedItems };
  },
};
