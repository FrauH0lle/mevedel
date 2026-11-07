/* viewer-pinned-prompt.js -- the room's pinned prompt row.
 *
 * Once the prompt of the exchange at the top of the transcript has
 * scrolled under the sticky header, a one-line preview of it overlays the
 * transcript just below the header; clicking it scrolls back to the prompt.
 * It is derived from the rendered prompt turns alone. The row overlays
 * rather than reserves space, so it costs a phone no height and never moves
 * the transcript. Like the Emacs view's pinned prompt, an archived ledger
 * uses only its own prompts, and the live transcript's content before its
 * first prompt continues the last prompt of the loaded segment it was
 * compacted from; a /clear starts a new chat and carries nothing over.
 *
 * Updates touch only the row's attributes and inline style. Changing its
 * text or hidden attribute would invalidate the viewer's body:has(...)
 * rules, which costs a long transcript far more than an attribute change.
 */
'use strict';
window.mevedelPinnedPrompt = Object.freeze({
  create({pin, transcript, history}) {
    const LIMIT = 512;
    let target = null, label = '', rowHeight = 0, refreshing = false;
    // The row's last written placement: rewriting an unchanged value still
    // invalidates layout, which the next read would then recompute.
    let placed = '';
    const shown = node => node.getClientRects().length > 0;
    const top = node => node.getBoundingClientRect().top;

    function edge() {
      let bottom = 0;
      for (const node of [document.querySelector('body > .topbar'), document.getElementById('filter')]) {
        if (node && shown(node)) bottom = Math.max(bottom, node.getBoundingClientRect().bottom);
      }
      return bottom;
    }

    // The prompt's own text: shared-content snapshots and edit notes stay out.
    function text(turn) {
      const prompt = turn.querySelector('.prompt');
      const parts = [...(prompt?.children || [])]
        .filter(node => !node.matches('details, small')).map(node => node.textContent);
      const flat = parts.join(' ').replace(/\s+/g, ' ').trim();
      return flat.length > LIMIT ? `${flat.slice(0, LIMIT - 1)}…` : flat;
    }

    // Prompt turns are direct ledger children, never grouped, and the filter
    // hides them with the hidden attribute: collecting them reads no layout
    // and walks no answer subtree.
    function prompts(ledger) {
      const list = [];
      for (const node of ledger.children) {
        const role = node.dataset.role;
        if ((role === 'you' || role === 'guest') && !node.hidden) list.push(node);
      }
      return list;
    }

    // The prompt governing the content at TOP, or null when its header is
    // still visible. A prompt starting within the row's height counts as
    // visible, so the row never covers a prompt header.
    function governing(edgeTop) {
      const ledgers = [...history.querySelectorAll('.history-ledger'), transcript];
      const ledger = ledgers.find(node => {
        if (!shown(node)) return false;
        const box = node.getBoundingClientRect();
        return box.top < edgeTop && box.bottom > edgeTop;
      });
      if (!ledger) return null;
      // The row's height comes from CSS: a hidden row measures zero.
      rowHeight ||= parseFloat(getComputedStyle(pin).getPropertyValue('--pinned-prompt-h')) || 0;
      const probe = edgeTop + rowHeight;
      const list = prompts(ledger);
      // Prompts run down the page, so a binary search reads O(log n) positions.
      let low = 0, high = list.length;
      while (low < high) {
        const middle = (low + high) >> 1;
        if (top(list[middle]) < probe) low = middle + 1;
        else high = middle;
      }
      const found = list[low - 1];
      if (found) return top(found) < edgeTop ? found : null;
      // Continuation after compaction: a live prompt on screen takes
      // precedence; otherwise the live chat's latest segment supplies it.
      if (ledger !== transcript || (list[0] && top(list[0]) < window.innerHeight)) return null;
      const segment = [...history.querySelectorAll('.history-chat[data-live] .history-segment')].pop();
      const archived = segment?.querySelector('.history-ledger');
      return archived ? prompts(archived).pop() || null : null;
    }

    // Scroll events and resize observations already arrive once per frame.
    // Every layout read precedes the writes, so an update never forces layout.
    function update() {
      const edgeTop = edge();
      const turn = governing(edgeTop);
      // A rebuilt record is a new element, so an unchanged target keeps its label.
      if (turn && turn !== target) label = text(turn);
      target = label ? turn : null;
      if (!target) { if (pin.hasAttribute('data-shown')) pin.removeAttribute('data-shown'); return; }
      const column = transcript.getBoundingClientRect();
      const place = `${edgeTop} ${column.left} ${column.width}`;
      if (place !== placed) {
        placed = place;
        pin.style.top = `${edgeTop}px`;
        pin.style.left = `${column.left}px`;
        pin.style.width = `${column.width}px`;
      }
      if (pin.dataset.label !== label) {
        pin.dataset.label = label;
        pin.title = label;
        pin.setAttribute('aria-label', `Show the prompt: ${label}`);
      }
      if (!pin.hasAttribute('data-shown')) pin.setAttribute('data-shown', '');
    }

    // Host updates can replace prompts without resizing anything. Refresh
    // once after the update that asked, before the browser renders it.
    function refresh() {
      if (refreshing) return;
      refreshing = true;
      Promise.resolve().then(() => { refreshing = false; update(); });
    }

    pin.addEventListener('click', () => {
      if (target && !target.isConnected) update();
      if (!target) return;
      const turn = target;
      for (let node = turn.parentElement; node; node = node.parentElement) {
        if (node.tagName === 'DETAILS') node.open = true;
      }
      window.scrollBy({top: turn.getBoundingClientRect().top - edge() - 8, behavior: 'smooth'});
      turn.classList.remove('turn-revealed');
      void turn.offsetWidth;
      turn.classList.add('turn-revealed');
      setTimeout(() => turn.classList.remove('turn-revealed'), 2400);
    });
    window.addEventListener('scroll', update, {passive: true});
    // Observing the body also covers viewport resizes and header changes.
    const layout = new ResizeObserver(update);
    [document.body, transcript, history].forEach(node => layout.observe(node));
    return Object.freeze({refresh});
  },
});
