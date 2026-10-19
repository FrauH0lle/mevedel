/* Archived segments are host-backed, read-only disclosures, separate from live state. */
'use strict';
window.mevedelHistoryView = Object.freeze({
  create({send, el, renderRecord, onArtifacts}) {
    const root = document.getElementById('history');
    const rows = new Map();
    let sequence = 0, online = false, staging = null, catalog = [];

    function finishReveal(row, found) {
      if (!row.sourceReveal) return;
      clearTimeout(row.sourceReveal.timer);
      row.sourceReveal.resolve(found);
      row.sourceReveal = null;
    }

    function failure(row, message) {
      finishReveal(row, false);
      row.reqId = null;
      row.body.replaceChildren(el('p', 'history-note', message));
      const retry = el('button', 'btn quiet', 'Retry');
      retry.type = 'button';
      retry.addEventListener('click', () => load(row));
      row.body.append(retry);
    }

    async function load(row) {
      if (row.loaded || row.reqId !== null) return;
      if (!online) return failure(row, 'Reconnect to load this earlier conversation.');
      const reqId = row.reqId = ++sequence;
      row.records = [];
      row.body.replaceChildren(el('p', 'history-note', 'Loading earlier conversation…'));
      const ok = await send({t:'history-get', reqId, segment:row.number});
      if (!ok && row.reqId === reqId) failure(row, 'Connection lost. Reconnect, then retry.');
    }

    function handle(frame) {
      if (frame.t === 'history-index') {
        if (!Number.isSafeInteger(frame.currentSegment) || frame.currentSegment < 1) return;
        if (!staging || staging.current !== frame.currentSegment) staging = {current:frame.currentSegment, records:[]};
        if (Array.isArray(frame.records)) staging.records.push(...frame.records);
        if (frame.final !== true) return;
        const chats = staging.records.filter(record => record.kind === 'chat');
        catalog = staging.records.filter(record => record.kind !== 'chat');
        staging = null;
        for (const [number, row] of rows) {
          if (number >= frame.currentSegment) { row.node.remove(); rows.delete(number); }
        }
        for (let number = 1; number < frame.currentSegment; number++) {
          if (rows.has(number)) continue;
          const node = el('details', 'history-segment');
          const summary = el('summary', '', `Earlier conversation · Segment ${number}`);
          node.append(summary);
          const collapse = el('button', 'history-collapse');
          collapse.type = 'button';
          collapse.title = `Collapse segment ${number}`;
          collapse.setAttribute('aria-label', collapse.title);
          const arrow = el('span', '', '‹');
          arrow.setAttribute('aria-hidden', 'true');
          collapse.append(arrow);
          collapse.addEventListener('click', () => {
            node.open = false;
            summary.scrollIntoView({block:'nearest'});
            summary.focus({preventScroll:true});
          });
          node.append(collapse);
          const body = el('div', 'history-body');
          node.append(body);
          const row = {number, node, body, loaded:false, reqId:null, records:[]};
          rows.set(number, row);
          node.addEventListener('toggle', () => { if (node.open) load(row); });
        }
        // The host's native chat descriptors distinguish /clear from a
        // compaction. Move existing disclosure nodes, preserving loaded bodies
        // and open state, rather than rebuilding them on every live update.
        const groups = chats.map(chat => {
          const group = el('section', 'history-chat');
          group.append(el('h2', 'history-chat-title', `Chat ${chat.number} · ${chat.title}`));
          for (let number = chat.firstSegment; number <= chat.lastSegment; number++) {
            if (rows.has(number)) group.append(rows.get(number).node);
          }
          if (chat.lastSegment === frame.currentSegment) {
            group.append(el('p', 'history-note', 'Live conversation · New messages go here.'));
          }
          return group;
        });
        root.replaceChildren(...groups);
        root.hidden = rows.size === 0 && chats.length < 2;
        onArtifacts();
      } else if (frame.t === 'history') {
        const row = [...rows.values()].find(value => value.reqId !== null && value.reqId === frame.reqId);
        if (!row) return;
        if (typeof frame.error === 'string') return failure(row, frame.error);
        if (frame.segment !== row.number) return;
        if (Array.isArray(frame.records)) row.records.push(...frame.records);
        if (frame.final !== true) return;
        row.body.replaceChildren(el('p', 'history-note', 'Archived conversation. New messages go to the live session.'));
        const ledger = el('div', 'ledger history-ledger');
        row.records.forEach(record => ledger.append(renderRecord(record)));
        window.mevedelTranscriptRenderer.groupActivity(ledger);
        window.mevedelTranscriptRenderer.markContinuations(ledger);
        row.body.append(ledger);
        row.records = [];
        row.loaded = true;
        row.reqId = null;
        if (row.sourceReveal) finishReveal(row,
          window.mevedelTranscriptRenderer.revealTool(row.body, row.sourceReveal.id));
      }
    }

    function connection(connected) {
      online = connected;
      if (!connected) {
        staging = null;
        rows.forEach(row => { if (row.reqId !== null) failure(row, 'Connection lost. Reconnect, then retry.'); });
      }
    }
    function reveal(segment, id) {
      const row = rows.get(segment);
      if (!row || !online) return Promise.resolve(false);
      let ancestor = row.node;
      while (ancestor && ancestor !== root.parentElement) {
        if (ancestor.tagName === 'DETAILS') ancestor.open = true;
        ancestor = ancestor.parentElement;
      }
      if (row.loaded) return Promise.resolve(window.mevedelTranscriptRenderer.revealTool(row.body, id));
      finishReveal(row, false);
      return new Promise(resolve => {
        row.sourceReveal = {id, resolve, timer: setTimeout(() => failure(row,
          'Source navigation timed out. Reconnect or retry this archive.'), 20000)};
        load(row);
      });
    }
    return Object.freeze({handle, connection, reveal, artifacts:() => catalog});
  },
});
