/* viewer-background.js -- Host-ordered background work, browser-local inspection */
'use strict';
(() => {
  const GRACE = 10000;
  function create({state, send, el, openAgent, openExecution, showConversation, href,
    now = Date.now, setTimer = setTimeout, clearTimer = clearTimeout}) {
    const panel = document.getElementById('background-work');
    const heading = document.getElementById('background-summary');
    const collapse = document.getElementById('background-collapse');
    const list = document.getElementById('background-list');
    const more = document.getElementById('background-more');
    const entries = new Map();
    let order = [], selected = null, expanded = true, all = false;
    let pointer = false, focus = false, frozen = null, timer = null, omitted = false;
    let rendering = false, dirty = false;
    const holding = () => pointer || focus;
    const flat = value => String(value || '').replace(/[\n\r\t]+/g, ' ');
    const terminalDeadline = row => now() + Math.max(0, GRACE - Math.max(0, row.terminalAgeSeconds || 0) * 1000);
    const settled = entry => entry.row.terminal || entry.stale;
    function held(value) {
      if (value && !frozen) frozen = [...list.children].map(node => node.dataset.key);
      if (!value) {
        for (const [key, entry] of entries) {
          if (settled(entry) && (frozen?.includes(key) || key === selected)) entry.until = now() + GRACE;
        }
        frozen = null;
      }
    }
    function toggle(key) {
      if (selected && entries.has(selected) && settled(entries.get(selected))) {
        entries.get(selected).until = now() + GRACE;
      }
      selected = selected === key ? null : key;
      render();
    }
    function action(label, callback) {
      const button = el('button', 'btn quiet', label);
      button.type = 'button';
      button.addEventListener('click', callback);
      return button;
    }
    function nodeFor(entry) {
      if (entry.node) return entry.node;
      const row = entry.row;
      const node = el('li', 'background-row');
      node.dataset.key = row.key;
      const button = action('', () => toggle(row.key));
      button.className = 'background-toggle';
      const mark = el('span', 'background-mark');
      const kind = el('span', 'background-kind');
      const label = el('span', 'background-name');
      button.append(mark, kind, label);
      const details = el('div', 'background-details');
      const meta = el('div', 'background-meta');
      const previewLabel = el('div', 'background-preview-label');
      const preview = el('pre', 'background-preview');
      const controls = el('div', 'background-controls');
      const open = row.kind === 'agent'
        ? window.mevedelNav.link(el('a', 'btn quiet'), href({agent: row}), () => {
          if (state.connected && !settled(entry)) openAgent(entry.row);
        })
        : action('', () => openExecution({owner: entry.row.owner, id: entry.row.id}));
      const conversation = action('Show in conversation', () => showConversation(entry.row));
      const stop = action('', () => {
        const current = entry.row;
        if (!state.connected || !state.writable || settled(entry)) return;
        send(current.kind === 'agent'
          ? {t: 'background-stop', key: current.key, path: current.path}
          : {t: 'background-stop', key: current.key, owner: current.owner, id: current.id});
      });
      stop.dataset.stop = '';
      controls.append(open, conversation, stop);
      details.append(meta, previewLabel, preview, controls);
      node.append(button, details);
      entry.node = node;
      entry.parts = {button, mark, kind, label, details, meta, previewLabel, preview, open, conversation, stop};
      return node;
    }
    function update(entry) {
      const row = entry.row, p = entry.parts;
      const agent = row.kind === 'agent';
      const finished = !!settled(entry);
      const status = entry.stale ? 'Unavailable' : flat(row.statusLabel || 'Unknown');
      p.mark.textContent = finished ? '✓' : row.status === 'blocked' ? '!' : '●';
      p.mark.className = `background-mark${['failure', 'errored', 'lost'].includes(row.outcome) || row.status === 'errored' ? ' failed' : ''}`;
      p.mark.title = row.status;
      p.kind.textContent = `${status} ${row.kind}`;
      p.label.textContent = flat(row.description || row.path || row.id);
      p.label.title = [p.label.textContent, row.owner, row.model, row.effort].filter(Boolean).join(' · ');
      p.button.setAttribute('aria-expanded', selected === row.key ? 'true' : 'false');
      p.button.style.paddingLeft = `${Math.min(6, Math.max(0, row.depth || 0)) * 8}px`;
      p.details.hidden = selected !== row.key;
      entry.node.classList.toggle('finished', finished);
      entry.node.classList.toggle('blocked', row.status === 'blocked');
      p.meta.textContent = [row.backend, row.model && `${row.model}${row.effort ? ':' + row.effort : ''}`,
        !agent && row.owner && row.owner !== '/root' ? row.owner : '',
        row.kind === 'monitor' && Number.isInteger(row.eventCount) ? `${row.eventCount} events` : '',
        row.kind === 'monitor' && typeof row.expiresAt === 'number'
          ? `until ${new Date(row.expiresAt * 1000).toLocaleTimeString()}` : '',
        entry.stale ? `Last known: ${row.status}` : finished ? row.status : ''].filter(Boolean).join(' · ') || '\u00a0';
      p.meta.title = p.meta.textContent;
      // Keep the preview/control geometry unchanged through completion.
      p.previewLabel.hidden = agent;
      p.preview.hidden = agent;
      p.previewLabel.textContent = entry.stale ? 'Last known output preview'
        : row.previewLabel || (row.previewAvailable ? 'Output preview' : 'Output unavailable');
      p.previewLabel.title = p.previewLabel.textContent;
      p.preview.textContent = Array.from({length: 3}, (_, n) => flat(row.preview?.[n])).join('\n');
      p.open.textContent = agent ? 'Open transcript' : 'Full output';
      p.open.disabled = !state.connected || (agent && finished);
      if (agent) {
        p.open.setAttribute('aria-disabled', String(p.open.disabled));
        p.open.tabIndex = p.open.disabled ? -1 : 0;
        if (p.open.disabled) p.open.removeAttribute('href');
        else p.open.href = href({agent: row});
      }
      p.conversation.disabled = !state.connected || !row.toolUseId || (agent && finished);
      p.stop.textContent = agent ? 'Interrupt' : 'Stop';
      p.stop.disabled = finished || !state.connected || !state.writable;
      p.stop.title = !state.writable ? 'Only full and owner links may stop work'
        : finished ? 'Work has finished' : agent ? 'Interrupt the current turn; keep the agent retained' : 'Stop this execution';
    }
    function render() {
      if (!panel) return;
      if (rendering) { dirty = true; return; }
      rendering = true;
      try {
      if (timer !== null) clearTimer(timer);
      timer = null;
      const current = now();
      const visible = order.filter(key => {
        const entry = entries.get(key);
        return entry && (!settled(entry) || key === selected
          || (holding() && frozen?.includes(key)) || entry.until > current);
      });
      const counts = {agent: 0, bash: 0, monitor: 0};
      let blocked = 0;
      visible.forEach(key => { const row = entries.get(key).row;
        counts[row.kind]++; if (row.status === 'blocked') blocked++; });
      const bits = [];
      if (blocked) bits.push(`${blocked} blocked`);
      if (counts.agent) bits.push(`${counts.agent} agent${counts.agent === 1 ? '' : 's'}`);
      if (counts.bash) bits.push(`${counts.bash} command${counts.bash === 1 ? '' : 's'}`);
      if (counts.monitor) bits.push(`${counts.monitor} monitor${counts.monitor === 1 ? '' : 's'}`);
      heading.textContent = `Background work${bits.length ? ' · ' + bits.join(' · ') : ''}${omitted ? ' · additional work omitted' : ''}`;
      heading.title = heading.textContent;
      panel.hidden = visible.length === 0 && !omitted;
      collapse.textContent = expanded ? '−' : '+';
      collapse.setAttribute('aria-expanded', expanded ? 'true' : 'false');
      collapse.setAttribute('aria-label', expanded ? 'Collapse background work' : 'Expand background work');
      list.hidden = !expanded;
      let keys = frozen ? frozen.filter(key => visible.includes(key))
        : all ? visible : visible.filter((key, index) => index < 5 || key === selected || settled(entries.get(key)));
      keys.forEach((key, index) => {
        const entry = entries.get(key), node = nodeFor(entry);
        update(entry);
        if (list.children[index] !== node) list.insertBefore(node, list.children[index] || null);
      });
      while (list.children.length > keys.length) list.lastElementChild.remove();
      more.hidden = !expanded || (keys.length >= visible.length && !all);
      more.textContent = all ? 'Show fewer' : `Show all ${visible.length}`;
      let deadline = Infinity;
      for (const [key, entry] of entries) {
        if (settled(entry) && key !== selected && !holding() && entry.until > current) deadline = Math.min(deadline, entry.until);
      }
      if (deadline < Infinity) timer = setTimer(render, Math.max(1, deadline - current));
      } finally {
        rendering = false;
        if (dirty) { dirty = false; render(); }
      }
    }
    function show(frame) {
      const rows = Array.isArray(frame.rows) ? frame.rows : [];
      const incoming = new Set();
      const next = [];
      rows.forEach(row => {
        if (!row || typeof row.key !== 'string' || !['agent', 'bash', 'monitor'].includes(row.kind)) return;
        incoming.add(row.key); next.push(row.key);
        const entry = entries.get(row.key);
        if (entry) {
          if (row.terminal && !entry.row.terminal) entry.until = terminalDeadline(row);
          if (!row.terminal) entry.until = Infinity;
          entry.stale = false;
          entry.row = {...row};
        } else entries.set(row.key, {row: {...row}, until: row.terminal ? terminalDeadline(row) : Infinity});
      });
      for (const [index, key] of order.entries()) if (!incoming.has(key)) {
        const entry = entries.get(key);
        if (key === selected && entry && !settled(entry)) {
          entry.stale = true;
          entry.until = now() + GRACE;
        }
        if (key === selected || (entry && settled(entry) && ((holding() && frozen?.includes(key)) || entry.until > now()))) {
          const anchor = order.slice(index + 1).find(other => next.includes(other));
          if (anchor) next.splice(next.indexOf(anchor), 0, key); else next.push(key);
        }
        else entries.delete(key);
      }
      order = next;
      omitted = frame.truncated === true;
      render();
    }
    collapse?.addEventListener('click', () => {
      expanded = !expanded;
      if (!expanded && selected) toggle(selected); else render();
    });
    more?.addEventListener('click', () => { all = !all; frozen = null; render();
      if (holding()) frozen = [...list.children].map(node => node.dataset.key); });
    panel?.addEventListener('pointerenter', event => {
      if (event.pointerType === 'touch') return;
      const before = holding(); pointer = true; if (!before) held(true);
    });
    panel?.addEventListener('pointerleave', event => {
      if (event.pointerType === 'touch') return;
      pointer = false; if (!holding()) { held(false); render(); }
    });
    panel?.addEventListener('focusin', () => {
      const before = holding(); focus = true; if (!before) held(true);
    });
    panel?.addEventListener('focusout', event => {
      if (panel.contains(event.relatedTarget)) return;
      focus = false; if (!holding()) { held(false); render(); }
    });
    function reset() {
      if (timer !== null) clearTimer(timer);
      timer = null; entries.clear(); order = []; selected = null;
      frozen = null; pointer = false; focus = false; omitted = false;
      render();
    }
    return Object.freeze({show, refresh: render, reset});
  }
  window.mevedelBackgroundView = Object.freeze({create});
})();
