/* viewer.js -- dependency-free sealed collaboration guest */
'use strict';

(() => {
  const transcript = document.getElementById('transcript');
  const connection = document.getElementById('connection');
  const notice = document.getElementById('notice');
  const liveButton = document.getElementById('live-button');
  const composer = document.getElementById('composer');
  const composerInput = document.getElementById('composer-input');
  const composerName = document.getElementById('composer-name');
  const sendButton = document.getElementById('send-button');
  const stopButton = document.getElementById('stop-button');
  const filterSelect = document.getElementById('filter');
  const requests = document.getElementById('requests');

  const PROTO = 2;
  const GIVE_UP_MS = 3 * 60 * 1000;
  const MAX_PROMPT_BYTES = 256 * 1024;

  const state = {
    roomId: null,
    key: null,
    writeToken: null,
    socket: null,
    ended: false,
    readOnly: true,
    reconnectTimer: null,
    backoffMs: 1000,
    downSince: null,
    records: new Map(),
    elements: new Map(),
    staging: null,
    filter: 'all',
    // Frames must apply in order; WebCrypto is async, so decryption is
    // serialized through this promise chain.
    inbound: Promise.resolve(),
  };

  function base64urlDecode(text) {
    if (typeof text !== 'string' || !/^[A-Za-z0-9_-]+$/.test(text)) return null;
    const standard = text.replace(/-/g, '+').replace(/_/g, '/');
    const padded = standard + '='.repeat((4 - standard.length % 4) % 4);
    try {
      const binary = atob(padded);
      const bytes = new Uint8Array(binary.length);
      for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
      return bytes;
    } catch (_error) {
      return null;
    }
  }

  function base64urlEncode(bytes) {
    let binary = '';
    bytes.forEach(byte => { binary += String.fromCharCode(byte); });
    return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
  }

  // A link is "<roomId>.<secret>": secret is the 32-byte room key (view
  // link) or key followed by the 16-byte write token (full link).
  function parseFragment(fragment) {
    const value = String(fragment || '').replace(/^#/, '');
    const separator = value.indexOf('.');
    if (separator <= 0 || separator === value.length - 1) return null;
    const roomId = value.slice(0, separator);
    if (!/^[A-Za-z0-9_-]{10,64}$/.test(roomId)) return null;
    const secret = base64urlDecode(value.slice(separator + 1));
    if (!secret) return null;
    if (secret.length === 32) return {roomId, keyBytes: secret, writeToken: null};
    if (secret.length === 48) {
      return {roomId, keyBytes: secret.slice(0, 32), writeToken: secret.slice(32)};
    }
    return null;
  }

  async function importKey(bytes) {
    return crypto.subtle.importKey('raw', bytes, 'AES-GCM', false,
                                   ['encrypt', 'decrypt']);
  }

  // Envelope: [4-byte peerId][12-byte nonce][ciphertext||16-byte tag].
  // The relay rewrites the guest's peer prefix, so zero is fine.
  async function sealFrame(frame) {
    const nonce = crypto.getRandomValues(new Uint8Array(12));
    const plaintext = new TextEncoder().encode(JSON.stringify(frame));
    const sealed = new Uint8Array(await crypto.subtle.encrypt(
      {name: 'AES-GCM', iv: nonce}, state.key, plaintext));
    const envelope = new Uint8Array(4 + nonce.length + sealed.length);
    envelope.set(nonce, 4);
    envelope.set(sealed, 4 + nonce.length);
    return envelope;
  }

  async function unsealEnvelope(bytes) {
    if (bytes.length < 4 + 12 + 16) return null;
    const nonce = bytes.slice(4, 16);
    const sealed = bytes.slice(16);
    try {
      const plaintext = await crypto.subtle.decrypt(
        {name: 'AES-GCM', iv: nonce}, state.key, sealed);
      return JSON.parse(new TextDecoder().decode(plaintext));
    } catch (_error) {
      return null;
    }
  }

  async function send(frame) {
    if (!state.socket || state.socket.readyState !== WebSocket.OPEN) return;
    state.socket.send(await sealFrame(frame));
  }

  function atLiveEdge() {
    return document.documentElement.scrollHeight - window.scrollY
      - window.innerHeight < 24;
  }

  function scrollToLive() {
    window.scrollTo({top: document.documentElement.scrollHeight, behavior: 'auto'});
  }

  function setConnection(text, className) {
    connection.textContent = text;
    connection.className = `connection ${className || ''}`;
  }

  function showNotice(text) {
    notice.textContent = text || '';
  }

  function setLiveButton(visible) {
    liveButton.hidden = !visible;
  }

  function updateLiveAffordance() {
    setLiveButton(!atLiveEdge());
  }

  function appendTextBody(parent, text, className) {
    const body = document.createElement('pre');
    body.className = className || 'card-body';
    body.textContent = typeof text === 'string' ? text : '';
    parent.append(body);
    return body;
  }

  function headerLabel(record) {
    if (record.kind === 'assistant') return 'Assistant';
    if (record.kind === 'tool') return 'Tool';
    return record.guest ? `Guest ${record.guest}` : 'You';
  }

  function createHeader(record, card) {
    const header = document.createElement('div');
    header.className = 'card-header';
    const label = document.createElement('span');
    label.textContent = headerLabel(record);
    if (record.guest) label.className = 'guest-badge';
    header.append(label);
    const status = document.createElement('span');
    status.className = `status ${record.status || ''}`;
    status.textContent = record.status || '';
    header.append(status);
    card.append(header);
    return {header, label, status};
  }

  function createRecordElement(record) {
    if (!['user', 'assistant', 'tool'].includes(record.kind)) return null;
    const card = document.createElement('article');
    card.className = `card ${record.kind}`;
    card.dataset.recordId = record.id;
    const parts = createHeader(record, card);
    if (record.kind === 'tool') {
      const details = document.createElement('details');
      const summary = document.createElement('summary');
      summary.textContent = `${record.summary || record.name || 'Tool'}${record.truncated ? ' (truncated)' : ''}`;
      details.append(summary);
      const result = document.createElement('pre');
      result.className = 'tool-result';
      result.textContent = record.result || '';
      details.append(result);
      card.append(details);
      return {card, label: parts.label, status: parts.status, body: result, summary};
    }
    return {card, label: parts.label, status: parts.status,
            body: appendTextBody(card, record.text)};
  }

  function updateRecordElement(record) {
    let element = state.elements.get(record.id);
    if (!element) {
      element = createRecordElement(record);
      if (!element) return;
      state.elements.set(record.id, element);
      transcript.append(element.card);
    } else if (record.kind === 'tool') {
      element.summary.textContent = `${record.summary || record.name || 'Tool'}${record.truncated ? ' (truncated)' : ''}`;
      element.body.textContent = record.result || '';
    } else {
      element.body.textContent = record.text || '';
    }
    element.label.textContent = headerLabel(record);
    element.label.className = record.guest ? 'guest-badge' : '';
    element.status.textContent = record.status || '';
    element.status.className = `status ${record.status || ''}`;
  }

  function replaceSnapshot(records) {
    const follow = atLiveEdge();
    state.records.clear();
    state.elements.clear();
    transcript.replaceChildren();
    records.forEach(record => {
      if (record && typeof record.id === 'string') state.records.set(record.id, record);
    });
    state.records.forEach(updateRecordElement);
    refreshFilter();
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function updateRecord(record) {
    if (!record || typeof record.id !== 'string') return;
    const follow = atLiveEdge();
    state.records.set(record.id, record);
    updateRecordElement(record);
    refreshFilter();
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function removeRecords(ids) {
    (Array.isArray(ids) ? ids : []).forEach(id => {
      state.records.delete(id);
      const element = state.elements.get(id);
      if (element) element.card.remove();
      state.elements.delete(id);
    });
    refreshFilter();
  }

  // ── Directive filter ──────────────────────────────────────────────────
  // Records inside a directive turn carry its id; the menu is derived
  // client-side, so filtering is per-guest and costs no round-trips.

  function recordVisible(record) {
    if (state.filter === 'all') return true;
    if (state.filter === 'main') return !record.directive;
    return record.directive === state.filter;
  }

  function directiveLabel(id) {
    for (const record of state.records.values()) {
      if (record.directive === id && record.kind === 'user' && record.text) {
        const line = record.text.split('\n', 1)[0];
        return line.length > 40 ? `${line.slice(0, 37)}…` : line;
      }
    }
    return id.slice(0, 8);
  }

  function refreshFilter() {
    if (filterSelect) {
      const ids = [];
      state.records.forEach(record => {
        if (record.directive && !ids.includes(record.directive)) {
          ids.push(record.directive);
        }
      });
      filterSelect.hidden = ids.length === 0;
      if (state.filter !== 'all' && state.filter !== 'main'
          && !ids.includes(state.filter)) {
        state.filter = 'all';
      }
      const add = (value, label) => {
        const option = document.createElement('option');
        option.value = value;
        option.textContent = label;
        filterSelect.append(option);
      };
      filterSelect.replaceChildren();
      add('all', 'All');
      if (ids.length) add('main', 'Main chat');
      ids.forEach(id => add(id, `◆ ${directiveLabel(id)}`));
      filterSelect.value = state.filter;
    }
    state.records.forEach(record => {
      const element = state.elements.get(record.id);
      if (element) element.card.hidden = !recordVisible(record);
    });
  }

  // ── Pending interactions ──────────────────────────────────────────────
  // The host presents permission/plan prompts to full-link guests; the
  // first answer (here or in Emacs) settles them everywhere.

  function removeRequest(reqId) {
    if (!requests) return;
    const card = requests.children.find
      ? requests.children.find(c => c.dataset.reqId === String(reqId))
      : [...requests.children].find(c => c.dataset.reqId === String(reqId));
    if (card) card.remove();
  }

  function renderRequest(frame) {
    if (!requests) return;
    removeRequest(frame.reqId);
    const card = document.createElement('section');
    card.className = 'request-card';
    card.dataset.reqId = String(frame.reqId);
    const body = document.createElement('pre');
    body.className = 'request-body';
    body.textContent = typeof frame.body === 'string' ? frame.body : '';
    card.append(body);
    const controls = document.createElement('div');
    controls.className = 'request-controls';
    (Array.isArray(frame.options) ? frame.options : []).forEach(option => {
      const button = document.createElement('button');
      button.type = 'button';
      button.className = 'composer-button';
      button.textContent = option.label;
      button.addEventListener('click', () => {
        send({t: 'ui-response', reqId: frame.reqId, option: option.id});
      });
      controls.append(button);
    });
    card.append(controls);
    if (frame.allowFeedback === true) {
      const feedbackRow = document.createElement('div');
      feedbackRow.className = 'request-controls';
      const feedback = document.createElement('input');
      feedback.type = 'text';
      feedback.className = 'composer-name request-feedback';
      feedback.placeholder = 'Feedback…';
      const sendFeedback = document.createElement('button');
      sendFeedback.type = 'button';
      sendFeedback.className = 'composer-button';
      sendFeedback.textContent = 'Send feedback';
      sendFeedback.addEventListener('click', () => {
        if (feedback.value.trim()) {
          send({t: 'ui-response', reqId: frame.reqId,
                feedback: feedback.value});
        }
      });
      feedbackRow.append(feedback, sendFeedback);
      card.append(feedbackRow);
    }
    requests.append(card);
  }

  function clearRequests() {
    if (requests) requests.replaceChildren();
  }

  function guestName() {
    return (composerName && composerName.value.trim())
      || localStorage.getItem('mevedel-guest-name')
      || 'browser';
  }

  function setComposerVisible(visible) {
    if (composer) composer.hidden = !visible;
  }

  function handleFrame(frame) {
    if (!frame || typeof frame.t !== 'string') return;
    if (frame.t === 'welcome') {
      state.readOnly = frame.readOnly !== false;
      state.staging = {records: [], live: []};
      setComposerVisible(!state.readOnly);
      // Active ui-requests are re-sent after the snapshot on every hello.
      clearRequests();
      setConnection('Loading snapshot…', 'connected');
    } else if (frame.t === 'snapshot-chunk') {
      if (!state.staging) return;
      if (Array.isArray(frame.records)) state.staging.records.push(...frame.records);
      if (frame.final === true) {
        const staged = state.staging;
        state.staging = null;
        replaceSnapshot(staged.records);
        staged.live.forEach(update => {
          if (update.t === 'record') updateRecord(update.record);
          else removeRecords(update.ids);
        });
        setConnection('Connected', 'connected');
        showNotice('');
      }
    } else if (frame.t === 'record') {
      if (state.staging) state.staging.live.push({t: 'record', record: frame.record});
      else updateRecord(frame.record);
    } else if (frame.t === 'remove') {
      if (state.staging) state.staging.live.push({t: 'remove', ids: frame.ids});
      else removeRecords(frame.ids);
    } else if (frame.t === 'queued') {
      showNotice('Follow-up queued for the session.');
    } else if (frame.t === 'ui-request') {
      renderRequest(frame);
    } else if (frame.t === 'ui-request-end') {
      removeRequest(frame.reqId);
    } else if (frame.t === 'bye') {
      state.ended = true;
      setConnection('Session ended', 'ended');
      showNotice('The shared session has ended.');
      setComposerVisible(false);
      clearRequests();
    } else if (frame.t === 'error') {
      state.ended = true;
      setConnection('Rejected', 'ended');
      showNotice(typeof frame.message === 'string' ? frame.message
                 : 'The host rejected this connection.');
    }
    // Unknown frame types from a newer host are tolerated silently.
  }

  function websocketUrl() {
    const url = new URL(`/r/${state.roomId}`, window.location.href);
    url.protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    url.search = '?role=guest';
    return url.toString();
  }

  // The relay garbage-collects the room the moment the host connection
  // drops, so "no such room" and "room closed" are retryable during a host
  // network blip.  Give up only after the retry window.
  function scheduleReconnect() {
    if (state.ended || state.reconnectTimer) return;
    if (!state.downSince) state.downSince = Date.now();
    if (Date.now() - state.downSince > GIVE_UP_MS) {
      state.ended = true;
      setConnection('Room closed', 'ended');
      showNotice('The room did not come back; the link is dead.');
      setComposerVisible(false);
      return;
    }
    setConnection('Reconnecting…');
    state.reconnectTimer = window.setTimeout(() => {
      state.reconnectTimer = null;
      connect();
    }, state.backoffMs);
    state.backoffMs = Math.min(15000, state.backoffMs * 2);
  }

  function connect() {
    if (state.ended || !state.roomId || !state.key) return;
    setConnection('Connecting…');
    const socket = new WebSocket(websocketUrl());
    socket.binaryType = 'arraybuffer';
    state.socket = socket;
    socket.addEventListener('open', async () => {
      state.downSince = null;
      state.backoffMs = 1000;
      const hello = {t: 'hello', proto: PROTO, name: guestName()};
      if (state.writeToken) hello.writeToken = base64urlEncode(state.writeToken);
      await send(hello);
    });
    socket.addEventListener('message', event => {
      if (typeof event.data === 'string') {
        // Unencrypted relay control; room-closed is retryable (host blip).
        try {
          const control = JSON.parse(event.data);
          if (control && control.t === 'room-closed') socket.close();
        } catch (_error) { /* ignore */ }
        return;
      }
      state.inbound = state.inbound.then(async () => {
        const frame = await unsealEnvelope(new Uint8Array(event.data));
        if (frame) handleFrame(frame);
        // Observability hook for the deterministic protocol test.
        window.mevedelViewerApplied = (window.mevedelViewerApplied || 0) + 1;
      });
    });
    socket.addEventListener('close', () => {
      if (state.socket === socket && !state.ended) scheduleReconnect();
    });
  }

  if (composer) {
    composer.addEventListener('submit', async event => {
      event.preventDefault();
      const text = composerInput.value;
      if (!text.trim()) return;
      if (new TextEncoder().encode(text).length > MAX_PROMPT_BYTES) {
        showNotice('Prompt too large.');
        return;
      }
      localStorage.setItem('mevedel-guest-name', guestName());
      await send({t: 'prompt', text, name: guestName()});
      composerInput.value = '';
    });
    // The Send button is type="submit", so the form's submit event already
    // covers it; a click handler here would double-send every prompt.
    stopButton.addEventListener('click', () => send({t: 'abort'}));
    if (composerName) {
      composerName.value = localStorage.getItem('mevedel-guest-name') || '';
    }
  }

  if (filterSelect) {
    filterSelect.addEventListener('change', () => {
      state.filter = filterSelect.value;
      refreshFilter();
    });
  }

  liveButton.addEventListener('click', () => {
    scrollToLive();
    setLiveButton(false);
  });
  window.addEventListener('scroll', updateLiveAffordance, {passive: true});

  const credentials = parseFragment(window.location.hash);
  if (!credentials) {
    setConnection('Invalid link', 'ended');
    showNotice('This collaboration link is missing or malformed.');
  } else if (!(crypto && crypto.subtle)) {
    setConnection('Insecure context', 'ended');
    showNotice('This page needs HTTPS (or localhost) to unseal the session.');
  } else {
    state.roomId = credentials.roomId;
    state.writeToken = credentials.writeToken;
    // The key remains only in this page's memory; remove it from the URL
    // and history before opening the socket.
    window.history.replaceState(null, '', `${window.location.pathname}${window.location.search}`);
    importKey(credentials.keyBytes).then(key => {
      state.key = key;
      connect();
    });
  }

  window.mevedelViewer = Object.freeze({
    parseFragment, atLiveEdge, base64urlDecode, base64urlEncode,
  });
})();
