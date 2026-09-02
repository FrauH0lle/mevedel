/* transport.js -- Sealed WebSocket transport for the mevedel viewer */
'use strict';

(() => {
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

  // A link is "<roomId>.<secret>", each tier a prefix of the next: the
  // 32-byte room key alone (view link), followed by the 16-byte write
  // token (full link), followed by the 16-byte owner token (owner link).
  function parseFragment(fragment) {
    const value = String(fragment || '').replace(/^#/, '');
    const separator = value.indexOf('.');
    if (separator <= 0 || separator === value.length - 1) return null;
    const roomId = value.slice(0, separator);
    if (!/^[A-Za-z0-9_-]{10,64}$/.test(roomId)) return null;
    const secret = base64urlDecode(value.slice(separator + 1));
    if (!secret) return null;
    if (secret.length === 32) {
      return {roomId, keyBytes: secret, writeToken: null, ownerToken: null};
    }
    if (secret.length === 48) {
      return {roomId, keyBytes: secret.slice(0, 32),
              writeToken: secret.slice(32), ownerToken: null};
    }
    if (secret.length === 64) {
      return {roomId, keyBytes: secret.slice(0, 32),
              writeToken: secret.slice(32, 48), ownerToken: secret.slice(48)};
    }
    return null;
  }

  function importKey(bytes) {
    return crypto.subtle.importKey('raw', bytes, 'AES-GCM', false,
                                   ['encrypt', 'decrypt']);
  }

  function create(options) {
    let socket = null;
    let ended = false;
    let reconnectTimer = null;
    let backoffMs = 1000;
    let downSince = null;
    let inbound = Promise.resolve();
    let outbound = Promise.resolve();

    async function sealFrame(text) {
      const nonce = crypto.getRandomValues(new Uint8Array(12));
      const plaintext = new TextEncoder().encode(text);
      const sealed = new Uint8Array(await crypto.subtle.encrypt(
        {name: 'AES-GCM', iv: nonce}, options.key, plaintext));
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
          {name: 'AES-GCM', iv: nonce}, options.key, sealed);
        return JSON.parse(new TextDecoder().decode(plaintext));
      } catch (_error) {
        return null;
      }
    }

    async function sendNow(text, target) {
      if (ended || !target || target.readyState !== WebSocket.OPEN) return false;
      const sealed = await sealFrame(text);
      if (ended || socket !== target || target.readyState !== WebSocket.OPEN) {
        return false;
      }
      target.send(sealed);
      return true;
    }

    function send(frame) {
      const target = socket;
      const text = JSON.stringify(frame);
      const work = outbound.then(
        () => sendNow(text, target),
        () => sendNow(text, target));
      outbound = work.catch(() => {});
      return work;
    }

    function websocketUrl() {
      const url = new URL(`/r/${options.roomId}`, window.location.href);
      url.protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
      url.search = '?role=guest';
      return url.toString();
    }

    // The relay garbage-collects the room the moment the host connection
    // drops, so "no such room" and "room closed" are retryable during a host
    // network blip. Give up only after the retry window.
    function scheduleReconnect() {
      if (ended || reconnectTimer) return;
      if (!downSince) downSince = Date.now();
      if (Date.now() - downSince > options.giveUpMs) {
        ended = true;
        options.onGiveUp();
        return;
      }
      options.onConnection('Reconnecting…');
      reconnectTimer = window.setTimeout(() => {
        reconnectTimer = null;
        connect();
      }, backoffMs);
      backoffMs = Math.min(15000, backoffMs * 2);
    }

    function connect() {
      if (ended) return;
      inbound = Promise.resolve();
      outbound = Promise.resolve();
      options.onConnection('Connecting…');
      const nextSocket = new WebSocket(websocketUrl());
      nextSocket.binaryType = 'arraybuffer';
      socket = nextSocket;
      nextSocket.addEventListener('open', async () => {
        if (ended || socket !== nextSocket
            || nextSocket.readyState !== WebSocket.OPEN) return;
        if (!await send(options.hello())) return;
        if (ended || socket !== nextSocket
            || nextSocket.readyState !== WebSocket.OPEN) return;
        await options.onOpen();
      });
      nextSocket.addEventListener('message', event => {
        if (ended || socket !== nextSocket) return;
        if (typeof event.data === 'string') {
          // Unencrypted relay control; room-closed is retryable (host blip).
          try {
            const control = JSON.parse(event.data);
            if (control && control.t === 'room-closed') nextSocket.close();
          } catch (_error) { /* ignore */ }
          return;
        }
        inbound = inbound.then(async () => {
          if (ended || socket !== nextSocket) return;
          const frame = await unsealEnvelope(new Uint8Array(event.data));
          if (ended || socket !== nextSocket) return;
          if (frame) {
            downSince = null;
            backoffMs = 1000;
            options.onFrame(frame);
          }
          // Observability hook for the deterministic protocol test.
          window.mevedelViewerApplied = (window.mevedelViewerApplied || 0) + 1;
        }).catch(() => {});
      });
      nextSocket.addEventListener('close', () => {
        if (socket === nextSocket && !ended) scheduleReconnect();
      });
    }

    function end() {
      ended = true;
      if (reconnectTimer) {
        clearTimeout(reconnectTimer);
        reconnectTimer = null;
      }
      if (socket) {
        const current = socket;
        socket = null;
        if (current.readyState !== WebSocket.CLOSED) current.close();
      }
    }

    return Object.freeze({connect, end, send});
  }

  window.mevedelViewerTransport = Object.freeze({
    base64urlDecode, base64urlEncode, create, importKey, parseFragment,
  });
})();
