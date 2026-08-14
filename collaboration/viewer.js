/* viewer.js -- dependency-free, read-only collaboration viewer */
'use strict';

(() => {
  const transcript = document.getElementById('transcript');
  const connection = document.getElementById('connection');
  const notice = document.getElementById('notice');
  const liveButton = document.getElementById('live-button');
  const state = {
    room: null,
    token: null,
    socket: null,
    ended: false,
    reconnectTimer: null,
    records: new Map(),
    elements: new Map(),
    order: [],
    staging: null,
  };

  function parseFragment(fragment) {
    const value = String(fragment || '').replace(/^#/, '');
    const separator = value.indexOf('.');
    if (separator <= 0 || separator === value.length - 1) return null;
    return {room: value.slice(0, separator), token: value.slice(separator + 1)};
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

  function createHeader(record, card) {
    const header = document.createElement('div');
    header.className = 'card-header';
    const label = document.createElement('span');
    label.textContent = record.kind === 'user' ? 'You'
      : record.kind === 'assistant' ? 'Assistant' : 'Tool';
    header.append(label);
    const status = document.createElement('span');
    status.className = `status ${record.status || ''}`;
    status.textContent = record.status || '';
    header.append(status);
    card.append(header);
    return {header, status};
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
      return {card, status: parts.status, body: result, summary};
    }
    return {card, status: parts.status, body: appendTextBody(card, record.text)};
  }

  function updateRecordElement(record) {
    let element = state.elements.get(record.id);
    if (!element) {
      element = createRecordElement(record);
      if (!element) return;
      state.elements.set(record.id, element);
      state.order.push(record.id);
      transcript.append(element.card);
    } else if (record.kind === 'tool') {
      element.summary.textContent = `${record.summary || record.name || 'Tool'}${record.truncated ? ' (truncated)' : ''}`;
      element.body.textContent = record.result || '';
    } else {
      element.body.textContent = record.text || '';
    }
    element.status.textContent = record.status || '';
    element.status.className = `status ${record.status || ''}`;
  }

  function replaceSnapshot(records) {
    const follow = atLiveEdge();
    state.records.clear();
    state.elements.clear();
    state.order = [];
    transcript.replaceChildren();
    records.forEach(record => {
      if (record && typeof record.id === 'string') state.records.set(record.id, record);
    });
    state.records.forEach(updateRecordElement);
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function updateRecord(record) {
    if (!record || typeof record.id !== 'string') return;
    const follow = atLiveEdge();
    state.records.set(record.id, record);
    updateRecordElement(record);
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function removeRecords(ids) {
    (Array.isArray(ids) ? ids : []).forEach(id => {
      state.records.delete(id);
      const element = state.elements.get(id);
      if (element) element.card.remove();
      state.elements.delete(id);
      state.order = state.order.filter(item => item !== id);
    });
  }

  function handleMessage(message) {
    if (!message || typeof message.type !== 'string') throw new Error('invalid protocol message');
    if (message.type === 'snapshot-begin') {
      state.staging = {id: message.snapshot, records: [], live: []};
      setConnection('Loading snapshot…', 'connected');
    } else if (message.type === 'snapshot-chunk') {
      if (state.staging && state.staging.id === message.snapshot && Array.isArray(message.records)) {
        state.staging.records.push(...message.records);
      }
    } else if (message.type === 'snapshot-end') {
      if (state.staging && state.staging.id === message.snapshot) {
        const staged = state.staging;
        replaceSnapshot(staged.records);
        state.staging = null;
        staged.live.forEach(update => {
          if (update.type === 'record') updateRecord(update.record);
          else removeRecords(update.ids);
        });
        setConnection('Connected', 'connected');
        showNotice('');
      }
    } else if (message.type === 'record') {
      if (state.staging) state.staging.live.push({type: 'record', record: message.record});
      else updateRecord(message.record);
    } else if (message.type === 'remove') {
      if (state.staging) state.staging.live.push({type: 'remove', ids: message.ids});
      else removeRecords(message.ids);
    } else if (message.type === 'status' && message.status === 'ended') {
      state.ended = true;
      setConnection('Session ended', 'ended');
      showNotice('The shared session has ended.');
    } else {
      throw new Error('unsupported protocol message');
    }
  }

  function websocketUrl() {
    const url = new URL('/ws', window.location.href);
    url.protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    return url.toString();
  }

  function scheduleReconnect() {
    if (state.ended || state.reconnectTimer) return;
    state.reconnectTimer = window.setTimeout(() => {
      state.reconnectTimer = null;
      connect();
    }, 1000);
  }

  function connect() {
    if (state.ended || !state.room || !state.token) return;
    setConnection('Connecting…');
    const socket = new WebSocket(websocketUrl());
    state.socket = socket;
    socket.addEventListener('open', () => {
      socket.send(JSON.stringify({type: 'auth', version: 1, room: state.room, token: state.token}));
    });
    socket.addEventListener('message', event => {
      try {
        const message = JSON.parse(event.data);
        handleMessage(message);
        if (Number.isInteger(message.seq) && typeof message['ack-token'] === 'string'
            && socket.readyState === WebSocket.OPEN) {
          socket.send(JSON.stringify({
            type: 'ack', seq: message.seq, 'ack-token': message['ack-token'],
          }));
        }
      } catch (_error) {
        socket.close();
        showNotice('The session sent an invalid update.');
      }
    });
    socket.addEventListener('error', () => showNotice('Connection lost; retrying…'));
    socket.addEventListener('close', event => {
      if ([1002, 1003, 1007, 1008, 1009].includes(event.code)) {
        state.ended = true;
        state.staging = null;
        setConnection('Link rejected', 'ended');
        showNotice('This collaboration link was rejected or expired.');
      } else if (!state.ended) {
        setConnection('Reconnecting…');
        scheduleReconnect();
      }
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
    showNotice('This collaboration link is missing its room credentials.');
  } else {
    state.room = credentials.room;
    state.token = credentials.token;
    // The token remains only in this page's memory; remove it from the URL
    // and history before opening the socket.
    window.history.replaceState(null, '', `${window.location.pathname}${window.location.search}`);
    connect();
  }

  window.mevedelViewer = Object.freeze({parseFragment, atLiveEdge});
})();
