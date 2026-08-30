/* viewer.js -- MevView: dependency-free sealed collaboration guest */
'use strict';

(() => {
  const transcript = document.getElementById('transcript');
  const connection = document.getElementById('connection');
  const notice = document.getElementById('notice');
  const liveButton = document.getElementById('live-button');
  const composer = document.getElementById('composer');
  const composerInput = document.getElementById('composer-input');
  const queueState = document.getElementById('queue-state');
  const composerName = document.getElementById('composer-name');
  const stopButton = document.getElementById('stop-button');
  const filterNav = document.getElementById('filter');
  const requests = document.getElementById('requests');
  const attachments = document.getElementById('attachments');
  const attachButton = document.getElementById('attach-button');
  const imageInput = document.getElementById('image-input');
  const sessionLabel = document.getElementById('session-label');
  const notifyButton = document.getElementById('notify-button');
  const composerScope = document.getElementById('composer-scope');
  const ownQueue = document.getElementById('own-queue');
  const skillChips = document.getElementById('skill-chips');

  const PROTO = 2;
  const GIVE_UP_MS = 3 * 60 * 1000;
  const MAX_PROMPT_BYTES = 256 * 1024;

  const state = {
    roomId: null,
    key: null,
    writeToken: null,
    // The raw "<roomId>.<secret>" this page was opened with, kept so an
    // opt-in can persist it for installed-app relaunches.
    fragment: null,
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
    connected: false,
    // Tab values ('main' or a directive id) with activity the guest has
    // not looked at since it arrived.
    unseen: new Set(),
    busy: null,
    // Host-curated invocation roster from the welcome, and the entry
    // the guest has armed for the next send.
    roster: [],
    armed: null,
    // Frames must apply in order; WebCrypto is async, so decryption is
    // serialized through this promise chain.
    inbound: Promise.resolve(),
  };

  /* ── Link grammar and sealing ─────────────────────────────────────── */

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

  /* ── Small DOM helpers ────────────────────────────────────────────── */

  function el(tag, className, text) {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (typeof text === 'string') node.textContent = text;
    return node;
  }

  function atLiveEdge() {
    return document.documentElement.scrollHeight - window.scrollY
      - window.innerHeight < 40;
  }

  function scrollToLive() {
    window.scrollTo({top: document.documentElement.scrollHeight, behavior: 'auto'});
  }

  function setConnection(text, className) {
    connection.textContent = text;
    connection.className = `conn ${className || ''}`;
  }

  function showNotice(text) {
    notice.textContent = text || '';
    notice.hidden = !text;
  }

  // Transient acknowledgements clear themselves; a later persistent
  // notice is never erased by a stale flash timer.
  let flashTimer = null;
  function flashNotice(text) {
    showNotice(text);
    if (flashTimer) clearTimeout(flashTimer);
    flashTimer = window.setTimeout(() => {
      flashTimer = null;
      if (notice.textContent === text) showNotice('');
    }, 4000);
  }

  function setLiveButton(visible) {
    liveButton.hidden = !visible;
  }

  function updateLiveAffordance() {
    setLiveButton(!atLiveEdge());
  }

  /* ── Notifications ────────────────────────────────────────────────── */
  // Opt-in through the bell; fired only while the tab is hidden, for the
  // two moments a pocketed phone cares about: the session needs an
  // answer, and the turn the guest was waiting on finished.

  function notificationsSupported() {
    return typeof Notification !== 'undefined';
  }

  function notificationsEnabled() {
    let optedIn = false;
    try { optedIn = localStorage.getItem('mevedel-notify') === 'on'; }
    catch (_error) { /* storage unavailable: stay off */ }
    return optedIn && notificationsSupported()
      && Notification.permission === 'granted';
  }

  function renderNotifyButton() {
    if (!notifyButton) return;
    notifyButton.hidden = !(state.connected && notificationsSupported());
    const on = notificationsEnabled();
    notifyButton.setAttribute('aria-pressed', on ? 'true' : 'false');
    notifyButton.className = `bell${on ? ' on' : ''}`;
    notifyButton.textContent = on ? '🔔' : '🔕';
  }

  function maybeNotify(title, body) {
    if (!document.hidden || !notificationsEnabled()) return;
    try {
      new Notification(title, body ? {body} : {});
    } catch (_error) { /* constructor may throw where unsupported */ }
  }

  // An installed home-screen app relaunches at start_url, which carries
  // no fragment; the credentials persist only after the notification
  // opt-in -- the gesture that install exists for -- and die with the
  // room.
  function persistShare() {
    if (!state.fragment) return;
    try { localStorage.setItem('mevedel-last-share', state.fragment); }
    catch (_error) { /* storage unavailable */ }
  }

  function forgetShare() {
    try {
      if (localStorage.getItem('mevedel-last-share') === state.fragment) {
        localStorage.removeItem('mevedel-last-share');
      }
    } catch (_error) { /* storage unavailable */ }
  }

  function storedShare() {
    try { return localStorage.getItem('mevedel-last-share'); }
    catch (_error) { return null; }
  }

  /* ── Markdown (DOM-built, textContent only, XSS-safe) ─────────────── */

  function renderInline(target, text) {
    // `code`, **bold**, *italic* -- one pass, longest marker first.
    const pattern = /(`[^`\n]+`)|(\*\*[^*\n]+\*\*)|(\*[^*\n]+\*)/g;
    let last = 0;
    let match;
    while ((match = pattern.exec(text)) !== null) {
      if (match.index > last) {
        target.append(text.slice(last, match.index));
      }
      const token = match[0];
      if (token.startsWith('`')) {
        target.append(el('code', '', token.slice(1, -1)));
      } else if (token.startsWith('**')) {
        const strong = el('strong');
        renderInline(strong, token.slice(2, -2));
        target.append(strong);
      } else {
        const em = el('em');
        renderInline(em, token.slice(1, -1));
        target.append(em);
      }
      last = match.index + token.length;
    }
    if (last < text.length) target.append(text.slice(last));
  }

  function renderMarkdown(text) {
    const root = el('div', 'prose');
    const lines = String(text || '').split('\n');
    let index = 0;
    let paragraph = [];
    const flush = () => {
      if (paragraph.length) {
        const p = el('p');
        renderInline(p, paragraph.join('\n'));
        root.append(p);
        paragraph = [];
      }
    };
    while (index < lines.length) {
      const line = lines[index];
      const fence = line.match(/^```(\S*)\s*$/);
      if (fence) {
        flush();
        const code = [];
        index++;
        while (index < lines.length && !/^```\s*$/.test(lines[index])) {
          code.push(lines[index]);
          index++;
        }
        index++; // closing fence
        root.append(renderCodeBlock(code.join('\n'), fence[1]));
        continue;
      }
      const heading = line.match(/^(#{1,4})\s+(.*)$/);
      if (heading) {
        flush();
        const h = el(`h${heading[1].length}`);
        renderInline(h, heading[2]);
        root.append(h);
        index++;
        continue;
      }
      const quote = line.match(/^>\s?(.*)$/);
      if (quote) {
        flush();
        const bq = el('blockquote');
        const inner = [quote[1]];
        index++;
        while (index < lines.length && /^>\s?/.test(lines[index])) {
          inner.push(lines[index].replace(/^>\s?/, ''));
          index++;
        }
        const p = el('p');
        renderInline(p, inner.join('\n'));
        bq.append(p);
        root.append(bq);
        continue;
      }
      const bullet = line.match(/^\s*([-*]|\d+\.)\s+/);
      if (bullet) {
        flush();
        const ordered = /^\s*\d+\./.test(line);
        const list = el(ordered ? 'ol' : 'ul');
        while (index < lines.length) {
          const item = lines[index].match(/^\s*(?:[-*]|\d+\.)\s+(.*)$/);
          if (!item) break;
          const li = el('li');
          renderInline(li, item[1]);
          list.append(li);
          index++;
          // Continuation lines indented under the item.
          while (index < lines.length && /^\s{2,}\S/.test(lines[index])
                 && !/^\s*(?:[-*]|\d+\.)\s+/.test(lines[index])) {
            li.append(' ' + lines[index].trim());
            index++;
          }
        }
        root.append(list);
        continue;
      }
      if (/^\s*$/.test(line)) {
        flush();
        index++;
        continue;
      }
      paragraph.push(line);
      index++;
    }
    flush();
    return root;
  }

  /* ── Fontification ────────────────────────────────────────────────── */

  const LANG_RULES = {
    lisp: [
      [/;.*$/m, 'tok-com'],
      [/"(?:[^"\\]|\\.)*"/, 'tok-str'],
      [/(?<=\()(?:defun|defmacro|defvar|defcustom|defconst|let\*?|lambda|if|when|unless|while|cond|pcase|setq|require|provide|interactive|dolist|dotimes|progn|or|and|not)\b/, 'tok-kw'],
      [/(?<=\(defun\s)[-a-zA-Z0-9_?!*/<>=]+/, 'tok-fn'],
    ],
    shell: [
      [/#.*$/m, 'tok-com'],
      [/"(?:[^"\\]|\\.)*"|'[^']*'/, 'tok-str'],
      [/\b(?:if|then|else|fi|for|do|done|while|case|esac|function|return|exit|export|local)\b/, 'tok-kw'],
    ],
    python: [
      [/#.*$/m, 'tok-com'],
      [/"""[\s\S]*?"""|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/, 'tok-str'],
      [/\b(?:def|class|return|if|elif|else|for|while|import|from|as|with|try|except|raise|lambda|None|True|False|and|or|not|in|is)\b/, 'tok-kw'],
    ],
    js: [
      [/\/\/.*$/m, 'tok-com'],
      [/`(?:[^`\\]|\\.)*`|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/, 'tok-str'],
      [/\b(?:const|let|var|function|return|if|else|for|while|class|new|await|async|import|export|null|true|false|typeof)\b/, 'tok-kw'],
    ],
    go: [
      [/\/\/.*$/m, 'tok-com'],
      [/`[^`]*`|"(?:[^"\\]|\\.)*"/, 'tok-str'],
      [/\b(?:func|return|if|else|for|range|type|struct|interface|package|import|var|const|go|defer|select|case|switch|nil|true|false)\b/, 'tok-kw'],
    ],
  };
  const LANG_ALIASES = {
    'emacs-lisp': 'lisp', elisp: 'lisp', lisp: 'lisp', scheme: 'lisp',
    sh: 'shell', bash: 'shell', shell: 'shell', zsh: 'shell',
    python: 'python', py: 'python',
    js: 'js', javascript: 'js', typescript: 'js', ts: 'js', json: 'js',
    go: 'go', golang: 'go',
  };

  function highlightInto(target, text, lang) {
    const rules = LANG_RULES[LANG_ALIASES[(lang || '').toLowerCase()] || ''];
    if (!rules) {
      target.textContent = text;
      return;
    }
    const combined = new RegExp(
      rules.map(rule => `(${rule[0].source})`).join('|'), 'gm');
    let last = 0;
    let match;
    while ((match = combined.exec(text)) !== null) {
      if (match.index > last) target.append(text.slice(last, match.index));
      let cls = '';
      for (let group = 1; group <= rules.length; group++) {
        if (match[group] !== undefined) {
          cls = rules[group - 1][1];
          break;
        }
      }
      target.append(el('span', cls, match[0]));
      last = match.index + match[0].length;
      if (match[0].length === 0) combined.lastIndex++;
    }
    if (last < text.length) target.append(text.slice(last));
  }

  // Tool results carry a path, not a fence, so the language comes from
  // the extension. Only families the highlighter actually knows are
  // mapped; anything else renders as plain text rather than guessing.
  const EXTENSION_LANG = {
    el: 'lisp', lisp: 'lisp', scm: 'lisp', clj: 'lisp',
    sh: 'shell', bash: 'shell', zsh: 'shell',
    py: 'python',
    js: 'js', mjs: 'js', cjs: 'js', ts: 'js', tsx: 'js', jsx: 'js', json: 'js',
    go: 'go',
  };

  function langForPath(path) {
    const match = /\.([A-Za-z0-9]+)$/.exec(String(path || '').trim());
    return match ? (EXTENSION_LANG[match[1].toLowerCase()] || '') : '';
  }

  // Read prints "<right-aligned line number>TAB<source>". Keeping the
  // gutter out of the highlighter stops line numbers being coloured as
  // literals and keeps the source column aligned.
  const NUMBERED_LINE = /^(\s*\d+\t)([\s\S]*)$/;

  function renderNumberedSource(text, lang) {
    const pre = el('pre', 'result');
    String(text).split('\n').forEach((line, index, all) => {
      const match = NUMBERED_LINE.exec(line);
      if (match) {
        pre.append(el('span', 'gutter', match[1]));
        highlightInto(pre, match[2], lang);
      } else {
        pre.append(line);
      }
      if (index < all.length - 1) pre.append('\n');
    });
    return pre;
  }

  // Grep prints "path:line:match", and a run can span several file
  // types, so each line picks its own language from its own path.
  const GREP_LINE = /^([^\s:][^:]*):(\d+):([\s\S]*)$/;

  function renderGrepResult(text) {
    const pre = el('pre', 'result');
    String(text).split('\n').forEach((line, index, all) => {
      const match = GREP_LINE.exec(line);
      if (match) {
        pre.append(el('span', 'gpath', match[1]));
        pre.append(el('span', 'gsep', ':'));
        pre.append(el('span', 'gline', match[2]));
        pre.append(el('span', 'gsep', ':'));
        highlightInto(pre, match[3], langForPath(match[1]));
      } else {
        pre.append(line);
      }
      if (index < all.length - 1) pre.append('\n');
    });
    return pre;
  }

  function renderToolResult(record, text) {
    const name = record.name || '';
    if (name === 'Grep') return renderGrepResult(text);
    const lang = langForPath(record.detail);
    if (lang && NUMBERED_LINE.test(text)) {
      return renderNumberedSource(text, lang);
    }
    if (lang) {
      const pre = el('pre', 'result');
      highlightInto(pre, text, lang);
      return pre;
    }
    return el('pre', 'result', text);
  }

  function renderCodeBlock(code, lang) {
    if ((lang || '').toLowerCase() === 'diff') {
      const block = el('div', 'codeblock');
      block.append(el('span', 'lang', 'diff'));
      block.append(renderDiff(code));
      return block;
    }
    const block = el('div', 'codeblock');
    if (lang) block.append(el('span', 'lang', lang));
    const pre = el('pre');
    highlightInto(pre, code, lang);
    block.append(pre);
    return block;
  }

  function renderDiff(text) {
    const container = el('div', 'diff');
    for (const line of String(text || '').split('\n')) {
      let cls = 'line';
      if (/^@@/.test(line)) cls += ' hunk';
      else if (/^(\*\*\*|\+\+\+|---|diff |Index:|=== )/.test(line)) cls += ' file';
      else if (/^\+/.test(line)) cls += ' add';
      else if (/^-/.test(line)) cls += ' del';
      container.append(el('span', cls, line));
    }
    return container;
  }

  function looksLikeDiff(text) {
    return /^(@@ |\+\+\+ |--- |\*\*\* (Begin|Update|Add|Delete))/m
      .test(String(text || ''));
  }

  /* ── Ledger rendering ─────────────────────────────────────────────── */

  function roleOf(record) {
    if (record.kind === 'user') return record.guest ? 'guest' : 'you';
    return 'ai';
  }

  function whoLine(record) {
    const who = el('div', 'who');
    if (record.kind === 'user') {
      who.append(el('span', 'name', record.guest || 'Host'));
      if (record.guest) who.append(el('span', 'badge', 'guest'));
    } else if (record.kind === 'assistant') {
      who.append(el('span', 'name', 'Assistant'));
    } else {
      who.append(el('span', 'name', 'Tool'));
    }
    if (record.directive) {
      who.append(el('span', 'dirchip', `◆ ${directiveLabel(record.directive)}`));
    }
    return who;
  }

  function renderContent(record) {
    if (record.kind === 'user') {
      const prose = renderMarkdown(record.text || '');
      prose.className = 'prose prompt';
      return prose;
    }
    if (record.kind === 'assistant') {
      return renderMarkdown(record.text || '');
    }
    // Tool row.
    const details = el('details', `tool ${record.status || ''}`);
    const summary = el('summary');
    summary.append(el('span', 'tname', record.name || 'Tool'));
    summary.append(el('span', 'targ',
                      record.detail
                      || (record.summary !== record.name ? record.summary : '')
                      || ''));
    summary.append(el('span', `chip ${record.status || ''}`,
                      record.status || ''));
    details.append(summary);
    // A patch travels as a dedicated diff field; the result text is only
    // the application summary.
    if (record.diff) {
      const body = renderDiff(record.diff);
      body.className = 'result diff';
      details.append(body);
    }
    const result = record.result || '';
    if (result) {
      if (!record.diff && looksLikeDiff(result)) {
        const body = renderDiff(result);
        body.className = 'result diff';
        details.append(body);
      } else {
        details.append(renderToolResult(record, result));
      }
      if (record.truncated) {
        details.append(el('pre', 'result', '[result truncated]'));
      }
    }
    return details;
  }

  function buildTurn(record) {
    const turn = el('article', `turn ${roleOf(record)}`);
    turn.dataset.recordId = record.id;
    turn.dataset.role = roleOf(record);
    const rail = el('div', 'rail');
    const glyphText = roleOf(record) === 'you' ? 'H'
      : roleOf(record) === 'guest' ? 'G' : '◆';
    rail.append(el('div', 'glyph', glyphText));
    turn.append(rail);
    const content = el('div', 'content');
    content.append(whoLine(record));
    const rendered = renderContent(record);
    content.append(rendered);
    turn.append(content);
    // Tool rows keep their disclosure state across updates; stashing the
    // details element avoids a querySelector the protocol test's fake DOM
    // does not implement.
    if (record.kind === 'tool') turn.toolDetails = rendered;
    return turn;
  }

  function markContinuations() {
    let previousRole = null;
    for (const turn of [...transcript.children]) {
      if (turn.hidden) continue;
      const role = turn.dataset.role || 'ai';
      const cont = role === 'ai' && previousRole === 'ai';
      turn.className = `turn ${role}${cont ? ' cont' : ''}`;
      previousRole = role;
    }
  }

  function updateRecordElement(record) {
    let turn = state.elements.get(record.id);
    if (!turn) {
      turn = buildTurn(record);
      state.elements.set(record.id, turn);
      transcript.append(turn);
    } else {
      const wasOpen = !!(turn.toolDetails && turn.toolDetails.open);
      const fresh = buildTurn(record);
      if (wasOpen && fresh.toolDetails) fresh.toolDetails.open = true;
      fresh.hidden = turn.hidden;
      turn.replaceWith(fresh);
      state.elements.set(record.id, fresh);
      turn = fresh;
    }
    return turn;
  }

  function replaceSnapshot(records) {
    const follow = atLiveEdge();
    state.records.clear();
    state.elements.clear();
    state.unseen.clear();
    transcript.replaceChildren();
    records.forEach(record => {
      if (record && typeof record.id === 'string') state.records.set(record.id, record);
    });
    state.records.forEach(record => updateRecordElement(record));
    refreshFilter();
    markContinuations();
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function updateRecord(record) {
    if (!record || typeof record.id !== 'string') return;
    const follow = atLiveEdge();
    state.records.set(record.id, record);
    // Activity outside the selected filter earns its tab an unseen dot.
    if (!recordVisible(record)) state.unseen.add(record.directive || 'main');
    updateRecordElement(record);
    refreshFilter();
    markContinuations();
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function removeRecords(ids) {
    (Array.isArray(ids) ? ids : []).forEach(id => {
      state.records.delete(id);
      const turn = state.elements.get(id);
      if (turn) turn.remove();
      state.elements.delete(id);
    });
    refreshFilter();
    markContinuations();
  }

  /* ── Directive filter ─────────────────────────────────────────────── */
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
        return line.length > 32 ? `${line.slice(0, 29)}…` : line;
      }
    }
    return id.slice(0, 8);
  }

  function refreshFilter() {
    if (filterNav) {
      const ids = [];
      const counts = {all: 0, main: 0};
      state.records.forEach(record => {
        counts.all++;
        if (record.directive) {
          if (!ids.includes(record.directive)) ids.push(record.directive);
          counts[record.directive] = (counts[record.directive] || 0) + 1;
        } else {
          counts.main++;
        }
      });
      // The strip is part of the surface once connected, even with no
      // directive yet: an always-present control needs no discovering.
      filterNav.hidden = !state.connected;
      if (state.filter !== 'all' && state.filter !== 'main'
          && !ids.includes(state.filter)) {
        state.filter = 'all';
      }
      filterNav.replaceChildren();
      const add = (value, label, dir) => {
        const unseen = state.unseen.has(value);
        const button = el('button',
                          `${dir ? 'dir' : ''}${unseen ? ' unseen' : ''}`,
                          label);
        button.type = 'button';
        button.setAttribute('aria-pressed',
                            state.filter === value ? 'true' : 'false');
        if (counts[value]) {
          button.append(el('span', 'cnt', String(counts[value])));
        }
        button.addEventListener('click', () => {
          state.filter = value;
          // Selecting a tab is looking at it; All shows everything.
          if (value === 'all') state.unseen.clear();
          else state.unseen.delete(value);
          refreshFilter();
        });
        filterNav.append(button);
      };
      add('all', 'All');
      if (ids.length) add('main', 'Main chat');
      ids.forEach(id => add(id, `◆ ${directiveLabel(id)}`, true));
    }
    state.records.forEach(record => {
      const turn = state.elements.get(record.id);
      if (turn) turn.hidden = !recordVisible(record);
    });
    // The composer follows the filter, so say where a prompt will land.
    if (composerInput && !state.armed) {
      composerInput.placeholder = placeholderForFilter();
    }
    renderComposerScope();
  }

  function placeholderForFilter() {
    return (state.filter !== 'all' && state.filter !== 'main')
      ? `Discuss ◆ ${directiveLabel(state.filter)}…`
      : 'Queue a follow-up for the session…';
  }

  // One line under the composer saying what the next send will do: run
  // an armed invocation, or land in a directive thread.
  function renderComposerScope() {
    if (!composerScope) return;
    composerScope.replaceChildren();
    const scoped = state.filter !== 'all' && state.filter !== 'main';
    if (state.armed) {
      composerScope.hidden = false;
      composerScope.className = 'composer-scope armed';
      composerScope.append(
        `Runs ${sigilFor(state.armed.kind)}${state.armed.name}`);
      const clear = el('button', 'scope-clear', '✕');
      clear.type = 'button';
      clear.setAttribute('aria-label', 'Cancel this command');
      clear.addEventListener('click', () => setArmedInvocation(null));
      composerScope.append(clear);
      return;
    }
    composerScope.className = 'composer-scope';
    composerScope.hidden = !scoped;
    if (scoped) {
      composerScope.append(`Sends to ◆ ${directiveLabel(state.filter)} · discuss`);
    }
  }

  /* ── Pending interactions ─────────────────────────────────────────── */
  // The host presents permission/patch/plan prompts to full-link guests;
  // the first answer (here or in Emacs) settles them everywhere.

  function removeRequest(reqId) {
    if (!requests) return;
    const cards = [...requests.children];
    const card = cards.find(c => c.dataset.reqId === String(reqId));
    if (card) card.remove();
  }

  // A questionnaire answers all questions atomically: option buttons or a
  // custom text per question, then one submit with the answers array.
  function renderQuestionnaire(card, frame) {
    const questions = frame.questions;
    const answers = questions.map(q => (typeof q.answer === 'string' ? q.answer : ''));
    const marks = [];
    questions.forEach((q, index) => {
      const block = el('div', 'question');
      block.append(el('p', 'question-text',
                      `${index + 1}. ${q.question || ''}`));
      const row = el('div', 'request-controls');
      const buttons = [];
      (Array.isArray(q.options) ? q.options : []).forEach(option => {
        const button = el('button', 'btn quiet option', option.label || '');
        button.type = 'button';
        if (option.description) button.setAttribute('title', option.description);
        button.addEventListener('click', () => {
          answers[index] = option.label || '';
          custom.value = '';
          marks[index]();
        });
        buttons.push(button);
        row.append(button);
      });
      const custom = el('input', 'request-feedback');
      custom.type = 'text';
      custom.placeholder = 'Custom answer…';
      custom.setAttribute('aria-label', `Custom answer ${index + 1}`);
      custom.addEventListener('input', () => {
        answers[index] = custom.value;
        marks[index]();
      });
      if (answers[index]
          && !buttons.some(b => b.textContent === answers[index])) {
        custom.value = answers[index];
      }
      marks[index] = () => {
        buttons.forEach(button => {
          button.setAttribute('aria-pressed',
                              button.textContent === answers[index]
                              && !custom.value
                              ? 'true' : 'false');
        });
      };
      marks[index]();
      row.append(custom);
      block.append(row);
      card.append(block);
    });
    const submitRow = el('div', 'request-controls');
    const submit = el('button', 'btn', 'Submit answers');
    submit.type = 'button';
    submit.addEventListener('click', () => {
      if (answers.every(answer => answer.trim())) {
        send({t: 'ui-response', reqId: frame.reqId, answers});
      } else {
        flashNotice('Answer every question before submitting.');
      }
    });
    submitRow.append(submit);
    // Dismiss settles only the questionnaire; the host's run continues.
    if (frame.allowCancel === true) {
      const dismiss = el('button', 'btn quiet', 'Dismiss');
      dismiss.type = 'button';
      dismiss.addEventListener('click', () => {
        send({t: 'ui-response', reqId: frame.reqId, cancel: true});
      });
      submitRow.append(dismiss);
    }
    card.append(submitRow);
  }

  function renderRequest(frame) {
    if (!requests) return;
    removeRequest(frame.reqId);
    const card = el('section', 'request-card');
    card.dataset.reqId = String(frame.reqId);
    card.append(el('span', 'rhead', 'Pending interaction'));
    if (frame.bodyKind === 'diff') {
      const body = renderDiff(frame.body || '');
      card.append(body);
    } else if (frame.body) {
      card.append(el('pre', 'request-body',
                     typeof frame.body === 'string' ? frame.body : ''));
    }
    if (Array.isArray(frame.questions) && frame.questions.length) {
      renderQuestionnaire(card, frame);
      requests.append(card);
      return;
    }
    const controls = el('div', 'request-controls');
    (Array.isArray(frame.options) ? frame.options : []).forEach(option => {
      const button = el('button', 'btn', option.label);
      button.type = 'button';
      button.addEventListener('click', () => {
        send({t: 'ui-response', reqId: frame.reqId, option: option.id});
      });
      controls.append(button);
    });
    card.append(controls);
    if (frame.allowFeedback === true) {
      const feedbackRow = el('div', 'request-controls');
      const feedback = el('input', 'request-feedback');
      feedback.type = 'text';
      feedback.placeholder = 'Feedback…';
      feedback.setAttribute('aria-label', 'Feedback');
      const sendFeedback = el('button', 'btn quiet', 'Send feedback');
      sendFeedback.type = 'button';
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

  /* ── Attachments ──────────────────────────────────────────────────── */
  // Photos are downscaled client-side to fit the sealed prompt frame under
  // the relay's read limit; other files are refused when they overrun it,
  // because a log cannot be made smaller by resampling. The host enforces
  // the same allowlist and budget.

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
  const pendingFiles = []; // {mime, label, data (base64), bytes, url}

  // Browsers report "" or application/octet-stream for .log, .patch, and
  // friends, so the extension decides whenever the type is not one we take.
  function attachmentMime(file) {
    if (ALLOWED_MIME.has(file.type)) return file.type;
    const extension = (file.name || '').split('.').pop().toLowerCase();
    return MIME_BY_EXTENSION[extension] || null;
  }

  function renderAttachments() {
    if (!attachments) return;
    attachments.replaceChildren();
    pendingFiles.forEach((item, index) => {
      const chip = el('span', 'attachment');
      if (item.url) {
        const thumb = el('img', 'attachment-thumb');
        thumb.src = item.url;
        thumb.alt = `attachment ${index + 1}`;
        chip.append(thumb);
      } else {
        chip.append(el('span', 'attachment-name', item.label));
      }
      const removeButton = el('button', 'attachment-remove', '✕');
      removeButton.type = 'button';
      removeButton.setAttribute('aria-label', `Remove attachment ${index + 1}`);
      removeButton.addEventListener('click', () => {
        pendingFiles.splice(index, 1);
        renderAttachments();
      });
      chip.append(removeButton);
      attachments.append(chip);
    });
  }

  function pendingFileBytes() {
    return pendingFiles.reduce((sum, item) => sum + item.bytes, 0);
  }

  function base64OfBytes(buffer) {
    let binary = '';
    buffer.forEach(byte => { binary += String.fromCharCode(byte); });
    return btoa(binary);
  }

  async function downscaleImage(file, budget) {
    const bitmap = await createImageBitmap(file);
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
  }

  async function addFiles(files) {
    for (const file of files) {
      const mime = attachmentMime(file);
      if (!mime) {
        flashNotice(`${file.name || 'That file'} is not an accepted type.`);
        continue;
      }
      if (pendingFiles.length >= MAX_FILES) {
        flashNotice(`At most ${MAX_FILES} attachments per prompt.`);
        break;
      }
      const budget = FILE_BUDGET - pendingFileBytes();
      if (mime.startsWith('image/')) {
        const blob = await downscaleImage(file, budget).catch(() => null);
        if (!blob) {
          flashNotice('Image too large for the frame budget.');
          continue;
        }
        const buffer = new Uint8Array(await blob.arrayBuffer());
        pendingFiles.push({
          mime: 'image/jpeg',
          label: file.name || 'photo',
          data: base64OfBytes(buffer),
          bytes: buffer.length,
          url: URL.createObjectURL(blob),
        });
      } else {
        if (file.size > budget) {
          flashNotice(`${file.name || 'That file'} is over the `
                      + `${Math.floor(budget / 1024)} KB left in this prompt.`);
          continue;
        }
        const buffer = new Uint8Array(await file.arrayBuffer());
        pendingFiles.push({
          mime,
          label: file.name || 'attachment',
          data: base64OfBytes(buffer),
          bytes: buffer.length,
          url: null,
        });
      }
    }
    renderAttachments();
  }

  /* ── Composer ─────────────────────────────────────────────────────── */

  function guestName() {
    return (composerName && composerName.value.trim())
      || localStorage.getItem('mevedel-guest-name')
      || 'browser';
  }

  // One random id per browser, minted on first use. It lets the host
  // match this guest's own queued entries across reconnects and page
  // reloads; peer numbers cannot, because the relay reassigns them.
  function guestId() {
    let id = null;
    try { id = localStorage.getItem('mevedel-guest-id'); }
    catch (_error) { /* storage unavailable */ }
    if (id && /^[A-Za-z0-9_-]{8,64}$/.test(id)) return id;
    const bytes = crypto.getRandomValues(new Uint8Array(12));
    id = base64urlEncode(bytes);
    try { localStorage.setItem('mevedel-guest-id', id); }
    catch (_error) { /* per-page id then */ }
    return id;
  }

  function setComposerVisible(visible) {
    if (composer) composer.hidden = !visible;
  }

  // The welcome's host-curated roster is the whole discovery surface.
  // Tapping a chip arms the invocation and focuses the composer rather
  // than sending: most commands and skills take arguments, and an
  // immediate send gives no chance to supply them. The armed name
  // travels as its own frame field, so composer text is never parsed
  // for a sigil.
  function sigilFor(kind) {
    return kind === 'skill' ? '$' : '/';
  }

  function setArmedInvocation(entry) {
    state.armed = entry || null;
    if (composerScope) renderComposerScope();
    if (composerInput) {
      composerInput.placeholder = entry
        ? (entry.hint
           ? `Arguments for ${sigilFor(entry.kind)}${entry.name} — ${entry.hint}`
           : `${sigilFor(entry.kind)}${entry.name} — no arguments needed`)
        : placeholderForFilter();
      if (typeof composerInput.focus === 'function') composerInput.focus();
    }
    renderSkillChips();
  }

  function renderSkillChips() {
    if (!skillChips) return;
    skillChips.replaceChildren();
    skillChips.hidden = state.roster.length === 0;
    state.roster.forEach(entry => {
      const armed = state.armed && state.armed.name === entry.name;
      const chip = el('button', `skill-chip${armed ? ' armed' : ''}`,
                      `${sigilFor(entry.kind)}${entry.name}`);
      chip.type = 'button';
      chip.setAttribute('aria-pressed', armed ? 'true' : 'false');
      if (entry.hint) chip.setAttribute('title', entry.hint);
      chip.addEventListener('click', () => {
        setArmedInvocation(armed ? null : entry);
      });
      skillChips.append(chip);
    });
  }

  function showSkillChips(entries) {
    state.roster = (Array.isArray(entries) ? entries : [])
      .filter(entry => entry && typeof entry.name === 'string' && entry.name)
      .map(entry => ({
        name: entry.name,
        kind: entry.kind === 'skill' ? 'skill' : 'command',
        hint: typeof entry.hint === 'string' ? entry.hint : null,
      }));
    state.armed = null;
    renderSkillChips();
  }

  // The guest's own pending prompts, echoed back per-peer by the host:
  // a persistent card with live position and a retract control, so a
  // queued prompt never reads as swallowed.
  function showOwnQueue(entries) {
    if (!ownQueue) return;
    ownQueue.replaceChildren();
    ownQueue.hidden = entries.length === 0;
    entries.forEach(entry => {
      if (!entry || typeof entry.id !== 'number') return;
      const card = el('section', 'own-entry');
      card.append(el('span', 'rhead',
                     typeof entry.position === 'number'
                     ? `Your queued prompt · #${entry.position} in line`
                     : 'Your queued prompt'));
      card.append(el('p', 'own-text',
                     typeof entry.text === 'string' ? entry.text : ''));
      const controls = el('div', 'request-controls');
      const retract = el('button', 'btn quiet', 'Retract');
      retract.type = 'button';
      retract.addEventListener('click', () => {
        send({t: 'retract', id: entry.id});
      });
      controls.append(retract);
      card.append(controls);
      ownQueue.append(card);
    });
  }

  // How many follow-ups are waiting, and whether the host has delivery
  // paused -- otherwise a queued prompt on a busy session looks dropped.
  function showQueueState(frame) {
    if (!queueState) return;
    const pending = typeof frame.pending === 'number' ? frame.pending : 0;
    queueState.hidden = pending === 0;
    queueState.className = `queue-state${frame.paused === true ? ' paused' : ''}`;
    if (pending === 0) return;
    queueState.textContent =
      `${pending} follow-up${pending === 1 ? '' : 's'} waiting`
      + (frame.paused === true ? ' — delivery paused in Emacs' : '');
  }

  /* ── Frame handling ───────────────────────────────────────────────── */

  // Request ids already notified about, so a re-sent card stays silent.
  const notifiedRequests = new Set();

  function handleFrame(frame) {
    if (!frame || typeof frame.t !== 'string') return;
    if (frame.t === 'welcome') {
      state.readOnly = frame.readOnly !== false;
      state.staging = {records: [], live: []};
      state.connected = true;
      renderNotifyButton();
      setComposerVisible(!state.readOnly);
      showSkillChips(state.readOnly ? [] : frame.commands);
      // Active ui-requests are re-sent after the snapshot on every hello.
      clearRequests();
      // The host sends `queue' only when it changes, so a reconnect
      // starts empty rather than showing the previous socket's count;
      // the own-entry card is rebuilt by the hello reply's echo.
      showQueueState({pending: 0});
      showOwnQueue([]);
      setConnection('Loading…', 'connected');
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
      flashNotice(typeof frame.position === 'number'
                  ? `Queued — #${frame.position} in line.`
                  : 'Follow-up queued for the session.');
    } else if (frame.t === 'queue') {
      showQueueState(frame);
      showOwnQueue(Array.isArray(frame.own) ? frame.own : []);
    } else if (frame.t === 'ui-request') {
      renderRequest(frame);
      // The host re-sends the same request id on every head redraw and
      // re-hello; one interaction earns one notification.
      if (!notifiedRequests.has(frame.reqId)) {
        notifiedRequests.add(frame.reqId);
        maybeNotify('Pending interaction',
                    typeof frame.body === 'string'
                    ? frame.body.slice(0, 120) : '');
      }
    } else if (frame.t === 'ui-request-end') {
      removeRequest(frame.reqId);
      notifiedRequests.delete(frame.reqId);
    } else if (frame.t === 'status') {
      if (state.busy === true && frame.busy !== true) {
        maybeNotify('Turn finished',
                    'The mevedel session is idle again.');
      }
      state.busy = frame.busy === true;
    } else if (frame.t === 'bye') {
      state.ended = true;
      forgetShare();
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

  /* ── Connection lifecycle ─────────────────────────────────────────── */

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
      forgetShare();
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
      const hello = {t: 'hello', proto: PROTO, name: guestName(),
                     guestId: guestId()};
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
      // An armed invocation may legitimately carry no arguments.
      if (!text.trim() && !pendingFiles.length && !state.armed) return;
      if (new TextEncoder().encode(text).length > MAX_PROMPT_BYTES) {
        flashNotice('Prompt too large.');
        return;
      }
      localStorage.setItem('mevedel-guest-name', guestName());
      const frame = {t: 'prompt', name: guestName()};
      if (state.armed) {
        // The name travels as its own field; the host resolves the
        // sigil and validates against its allowlist. Text is arguments.
        frame.invoke = state.armed.name;
        frame.text = text.trim();
      } else {
        frame.text = text.trim() || 'See the attached file.';
        // Filtering to a directive sends into that directive's discussion,
        // so the reply lands in the thread being read. The host drops an id
        // whose directive is gone and sends to main chat instead.
        if (state.filter !== 'all' && state.filter !== 'main') {
          frame.directive = state.filter;
        }
      }
      if (pendingFiles.length) {
        frame.images = pendingFiles.map(
          item => ({mime: item.mime, data: item.data}));
      }
      await send(frame);
      composerInput.value = '';
      pendingFiles.length = 0;
      renderAttachments();
      // One tap, one invocation: disarm so the next send is a prompt.
      if (state.armed) setArmedInvocation(null);
    });
    // The Send button is type="submit", so the form's submit event already
    // covers it; a click handler here would double-send every prompt.
    stopButton.addEventListener('click', () => send({t: 'abort'}));
    if (attachButton && imageInput) {
      attachButton.addEventListener('click', () => imageInput.click());
      imageInput.addEventListener('change', () => {
        addFiles([...imageInput.files]);
        imageInput.value = '';
      });
    }
    if (composerInput) {
      composerInput.addEventListener('paste', event => {
        const files = [...(event.clipboardData?.items || [])]
          .filter(item => item.kind === 'file')
          .map(item => item.getAsFile())
          .filter(Boolean);
        if (files.length) addFiles(files);
      });
    }
    if (composerName) {
      composerName.value = localStorage.getItem('mevedel-guest-name') || '';
    }
  }

  if (notifyButton) {
    notifyButton.addEventListener('click', async () => {
      if (notificationsEnabled()) {
        try { localStorage.setItem('mevedel-notify', 'off'); }
        catch (_error) { /* off is also the unset default */ }
      } else if (notificationsSupported()) {
        const permission = await Notification.requestPermission();
        try {
          localStorage.setItem('mevedel-notify',
                               permission === 'granted' ? 'on' : 'off');
        } catch (_error) { /* opt-in then lasts for this page only */ }
        if (permission === 'granted') {
          persistShare();
        } else {
          flashNotice('Notifications are blocked for this site.');
        }
      }
      renderNotifyButton();
    });
  }

  liveButton.addEventListener('click', () => {
    scrollToLive();
    setLiveButton(false);
  });
  window.addEventListener('scroll', updateLiveAffordance, {passive: true});

  // An installed app relaunches without the fragment; fall back to the
  // credentials the notification opt-in persisted.
  const rawFragment = String(window.location.hash || '').replace(/^#/, '')
    || storedShare() || '';
  const credentials = parseFragment(`#${rawFragment}`);
  if (!credentials) {
    setConnection('Invalid link', 'ended');
    showNotice('This collaboration link is missing or malformed. '
               + 'Open the share link from the host again.');
  } else if (!(crypto && crypto.subtle)) {
    setConnection('Insecure context', 'ended');
    showNotice('This page needs HTTPS (or localhost) to unseal the session.');
  } else {
    state.fragment = rawFragment;
    state.roomId = credentials.roomId;
    state.writeToken = credentials.writeToken;
    // Keep an existing opt-in's persisted share pointing at the room
    // most recently opened.
    if (notificationsEnabled()) persistShare();
    if (sessionLabel) sessionLabel.textContent = state.roomId.slice(0, 8);
    // The key remains only in this page's memory; remove it from the URL
    // and history before opening the socket.
    window.history.replaceState(null, '', `${window.location.pathname}${window.location.search}`);
    importKey(credentials.keyBytes).then(key => {
      state.key = key;
      connect();
    });
  }

  window.mevedelViewer = Object.freeze({
    parseFragment, atLiveEdge, base64urlDecode, base64urlEncode, addFiles,
  });
})();
