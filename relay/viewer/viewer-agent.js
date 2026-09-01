/* viewer-agent.js -- retained-agent roster and transcript panel */
'use strict';

(() => {
  const POLL_MS = 2500;

  // Active agents stay visible; finished agents keep their retained
  // transcripts behind the disclosure.
  const ACTIVE = new Set(['running', 'blocked', 'waiting']);
  const FINISHED = new Set(['done', 'errored', 'interrupted']);

  function create({send, el, directiveLabel, openArtifact}) {
    const nav = document.getElementById('agents');
    const doneBox = document.getElementById('agents-done');
    const doneSummary = document.getElementById('agents-done-summary');
    const doneList = document.getElementById('agents-done-list');
    const panel = document.getElementById('agent-panel');
    const title = document.getElementById('agent-title');
    const metaEl = document.getElementById('agent-meta');
    const closeButton = document.getElementById('agent-close');
    const transcript = document.getElementById('agent-transcript');
    const view = {path: null, reqId: 0, digest: null, staging: null,
                  timer: null};
    let requestSequence = 0;

    function meta(row) {
      const bits = [];
      if (typeof row.role === 'string' && row.role) bits.push(row.role);
      bits.push(row.status);
      return bits.join(' · ');
    }

    function atLiveEdge() {
      if (!transcript) return true;
      return transcript.scrollHeight - (transcript.scrollTop || 0)
        - (transcript.clientHeight || 0) < 60;
    }

    function fetch() {
      if (!view.path) return;
      view.reqId = ++requestSequence;
      view.staging = null;
      const frame = {t: 'fetch-agent', reqId: view.reqId, path: view.path};
      if (view.digest) frame.known = view.digest;
      send(frame);
    }

    function renderRecords(records) {
      if (!transcript) return;
      const follow = atLiveEdge();
      transcript.replaceChildren();
      records.forEach(record => {
        if (!record || typeof record.id !== 'string') return;
        transcript.append(window.mevedelTranscriptRenderer.renderRecord(
          record, directiveLabel, openArtifact));
      });
      if (records.length === 0) {
        transcript.append(el('p', 'panel-note',
                             'Nothing visible in this transcript yet.'));
      }
      if (follow) transcript.scrollTop = transcript.scrollHeight;
    }

    function open(row) {
      if (!panel) return;
      view.path = row.path;
      view.digest = null;
      view.staging = null;
      if (title) title.textContent = row.path;
      if (metaEl) metaEl.textContent = meta(row);
      if (transcript) {
        transcript.replaceChildren(el('p', 'panel-note', 'Loading…'));
      }
      panel.hidden = false;
      fetch();
      if (view.timer) clearInterval(view.timer);
      view.timer = setInterval(fetch, POLL_MS);
    }

    function close() {
      if (view.timer) clearInterval(view.timer);
      view.timer = null;
      view.path = null;
      view.digest = null;
      view.staging = null;
      if (panel) panel.hidden = true;
      if (transcript) transcript.replaceChildren();
    }

    function show(rows) {
      if (!nav) return;
      const valid = rows.filter(row => row && typeof row.path === 'string'
        && (ACTIVE.has(row.status) || FINISHED.has(row.status)));
      const active = valid.filter(row => ACTIVE.has(row.status));
      const done = valid.filter(row => FINISHED.has(row.status));
      nav.replaceChildren();
      nav.hidden = active.length === 0;
      active.forEach(row => {
        const status = row.status;
        const stuck = status !== 'running';
        const chip = el('button', `dock-chip${stuck ? ' stuck' : ''}`);
        chip.type = 'button';
        chip.title = stuck
          ? `${row.path} is ${status} — it needs the host before it can continue`
          : `${row.path} is working — tap to watch`;
        chip.append(el('span', 'agent-dot'));
        chip.append(el('span', 'dock-chip-name', row.path));
        const label = meta(row);
        if (label) chip.append(el('span', 'dock-chip-meta', label));
        chip.addEventListener('click', () => open(row));
        nav.append(chip);
      });
      // Settled agents keep their retained transcripts reachable, one
      // quiet line of dock height until the reader opens the list.
      if (doneBox) {
        doneBox.hidden = done.length === 0;
        if (doneSummary) {
          doneSummary.textContent = `Finished agents · ${done.length}`;
        }
        if (doneList) {
          doneList.replaceChildren();
          done.forEach(row => {
            const item = el('li');
            const button = el('button', 'done-row');
            button.type = 'button';
            button.title = `Read ${row.path}'s transcript`;
            button.append(el('span', 'agent-dot'));
            button.append(el('span', 'done-path', row.path));
            button.append(el('span', 'done-meta', meta(row)));
            button.addEventListener('click', () => open(row));
            item.append(button);
            doneList.append(item);
          });
        }
      }
      if (view.path) {
        const watched = valid.find(row => row.path === view.path) || null;
        if (metaEl) metaEl.textContent = watched ? meta(watched) : 'settled';
        if (watched) fetch();
      }
    }

    function handle(frame) {
      if (!view.path || frame.reqId !== view.reqId) return;
      if (typeof frame.error === 'string') {
        if (transcript) {
          transcript.replaceChildren(el('p', 'panel-note', frame.error));
        }
        return;
      }
      if (frame.unchanged === true) return;
      (view.staging ||= []).push(
        ...(Array.isArray(frame.records) ? frame.records : []));
      if (frame.final === true) {
        const records = view.staging;
        view.staging = null;
        view.digest = typeof frame.digest === 'string' ? frame.digest : null;
        renderRecords(records);
      }
    }

    if (closeButton) closeButton.addEventListener('click', close);
    return Object.freeze({show, handle, close});
  }

  window.mevedelAgentView = Object.freeze({create});
})();
