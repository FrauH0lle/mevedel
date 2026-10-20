/* viewer-agent.js -- retained-agent metadata and transcript inspection */
'use strict';

(() => {
  const POLL_MS = 2500;

  function create({send, el, scopeChip, openArtifact, openExecution, flash, href}) {
    const panel = document.getElementById('agent-panel');
    const title = document.getElementById('agent-title');
    const metaEl = document.getElementById('agent-meta');
    const closeButton = document.getElementById('agent-close');
    const latestButton = document.getElementById('agent-latest');
    const transcript = document.getElementById('agent-transcript');
    const view = {path: null, row: null, reqId: 0, digest: null, staging: null,
                  timer: null, artifact: null};
    let requestSequence = 0;
    let roster = [];

    function meta(row) {
      const bits = [];
      if (typeof row.role === 'string' && row.role) bits.push(row.role);
      bits.push(row.status);
      if (row.backend) bits.push(row.backend);
      if (row.model) bits.push(row.model + (row.effort ? `:${row.effort}` : ''));
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
      if (Number.isSafeInteger(view.segment)) frame.segment = view.segment;
      if (view.digest) frame.known = view.digest;
      Promise.resolve(send(frame)).then(ok => {
        if (ok === false && view.artifact && view.reqId === frame.reqId) {
          artifactError('Connection lost; the agent artifact could not be resolved.');
        }
      });
    }

    function renderRecords(records) {
      if (!transcript) return;
      const follow = atLiveEdge();
      const renderer = window.mevedelTranscriptRenderer;
      const previous = new Map([...transcript.children].flatMap(turn => turn.groupDetails
        ? [...turn.groupDetails.children].filter(child => child.dataset.recordId) : [turn]).map(turn =>
        [turn.dataset.recordId, turn]));
      transcript.replaceChildren();
      records.forEach(record => {
        if (!record || typeof record.id !== 'string') return;
        if (record.artifact) record = {...record, agent: agentContext()};
        const turn = renderer.renderRecord(
          record, scopeChip, openArtifact, previous.get(record.id), openExecution,
          path => open({path}), href);
        transcript.append(turn);
      });
      renderer.groupActivity(transcript);
      renderer.markContinuations(transcript);
      if (records.length === 0) {
        transcript.append(el('p', 'panel-note',
                             'Nothing visible in this transcript yet.'));
      }
      if (follow) transcript.scrollTop = transcript.scrollHeight;
      if (view.toolId) {
        const toolId = view.toolId;
        view.toolId = null;
        if (!renderer.revealTool(transcript, toolId)) {
          flash('The located source has not reached the browser. Try Show in conversation again after the next update.');
        }
      }
    }

    function agentContext() {
      return {path: view.path, ...(view.segment === null ? {} : {segment: view.segment})};
    }

    function open(row, artifact = null) {
      if (!panel) return;
      row = {...roster.find(item => item.path === row.path), ...row};
      view.path = row.path;
      view.toolId = row.revealTool || null;
      view.segment = Number.isSafeInteger(row.segment) ? row.segment : null;
      view.row = {...row};
      view.digest = null;
      view.staging = null;
      view.artifact = artifact;
      if (title) title.textContent = row.path;
      if (metaEl) metaEl.textContent = (view.segment ? `Segment ${view.segment} · ` : '') + meta(row);
      if (latestButton) latestButton.hidden = !!artifact || view.segment === null;
      if (transcript) {
        transcript.replaceChildren(el('p', 'panel-note', 'Loading…'));
      }
      panel.hidden = false;
      fetch();
      if (view.timer) clearInterval(view.timer);
      // Cold live agents hydrate without replying to their first fetch. Keep
      // the existing polling path until resolution succeeds or fails.
      view.timer = view.segment === null ? setInterval(fetch, POLL_MS) : null;
    }

    // fetch-agent establishes the guest's host-side lookup authority. Never
    // substitute a store entry or filename for the exact transcript record.
    function resolveArtifact(record) {
      if (!record || typeof record.id !== 'string'
          || typeof record.agent?.path !== 'string' || !record.agent.path) {
        close();
        view.artifact = record || null;
        if (panel) panel.hidden = false;
        if (title) title.textContent = 'Agent artifact';
        if (metaEl) metaEl.textContent = '';
        if (latestButton) latestButton.hidden = true;
        artifactError('The artifact link has no valid agent context.');
        return;
      }
      open(record.agent, record);
    }

    function artifactError(message) {
      if (view.timer) clearInterval(view.timer);
      view.timer = null;
      view.reqId = 0;
      view.staging = null;
      if (transcript) transcript.replaceChildren(el('p', 'panel-note', message));
    }

    function cancelArtifactResolution() {
      if (view.artifact) close();
    }

    function close() {
      if (view.timer) clearInterval(view.timer);
      view.timer = null;
      view.path = null;
      view.row = null;
      view.digest = null;
      view.staging = null;
      view.artifact = null;
      if (panel) panel.hidden = true;
      if (transcript) transcript.replaceChildren();
    }

    function show(rows) {
      roster = rows.filter(row => row && typeof row.path === 'string');
      if (view.path) {
        const watched = roster.find(row => row.path === view.path) || null;
        if (metaEl) metaEl.textContent = (view.segment ? `Segment ${view.segment} · ` : '')
          + (watched ? meta(watched) : 'settled');
        if (watched) {
          view.row = {...watched};
          if (view.segment === null && !view.artifact) fetch();
        }
      }
    }

    function handle(frame) {
      if (!view.path || frame.reqId !== view.reqId) return;
      if (typeof frame.error === 'string') {
        if (view.artifact) { artifactError(frame.error); return; }
        view.toolId = null;
        if (transcript) {
          transcript.replaceChildren(el('p', 'panel-note', frame.error));
        }
        return;
      }
      if (frame.unchanged === true) {
        if (view.artifact) artifactError('The agent returned no records to resolve this artifact.');
        return;
      }
      (view.staging ||= []).push(
        ...(Array.isArray(frame.records) ? frame.records : []));
      if (frame.final === true) {
        const records = view.staging;
        view.staging = null;
        view.digest = typeof frame.digest === 'string' ? frame.digest : null;
        if (view.artifact) {
          const requested = view.artifact;
          const found = records.find(record => record?.id === requested.id && record.artifact && !record.missing);
          if (!found) {
            artifactError('This artifact is unavailable in the requested agent transcript.');
            return;
          }
          const resolved = {...found, agent: agentContext()};
          if (Number.isSafeInteger(requested.version) && requested.version > 0) resolved.version = requested.version;
          close();
          openArtifact(resolved);
          return;
        }
        renderRecords(records);
      }
    }

    if (closeButton) closeButton.addEventListener('click', close);
    if (latestButton) latestButton.addEventListener('click', () => {
      if (view.row) open({...view.row, segment: null, revealTool: null});
    });
    // The open transcript, as {agent: ROW}, or null.
    function describe() {
      if (panel && !panel.hidden && view.artifact) return {artifact: view.artifact};
      return panel && !panel.hidden && view.row
        ? {agent: {...view.row, ...(view.segment ? {segment: view.segment} : {})}} : null;
    }

    return Object.freeze({show, handle, close, open, describe, resolveArtifact, cancelArtifactResolution});
  }

  window.mevedelAgentView = Object.freeze({create});
})();
