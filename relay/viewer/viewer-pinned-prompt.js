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
 */
'use strict';
window.mevedelPinnedPrompt = Object.freeze({
  create({pin, transcript, history}) {
    const PROMPTS = '.turn[data-role="you"], .turn[data-role="guest"]';
    const LIMIT = 512;
    let target = null;
    const shown = node => node.getClientRects().length > 0;

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

    // The prompt governing the content at TOP, or null when its header is
    // still visible. A prompt starting within the row's height counts as
    // visible, so the row never covers a prompt header.
    function governing(top) {
      const ledgers = [...history.querySelectorAll('.history-ledger'), transcript];
      const ledger = ledgers.find(node => {
        if (!shown(node)) return false;
        const box = node.getBoundingClientRect();
        return box.top < top && box.bottom > top;
      });
      if (!ledger) return null;
      // The row's height comes from CSS: a hidden row measures zero.
      const probe = top + (parseFloat(getComputedStyle(pin).getPropertyValue('--pinned-prompt-h')) || 0);
      const prompts = [...ledger.querySelectorAll(PROMPTS)].filter(shown);
      let found = null;
      for (const turn of prompts) {
        if (turn.getBoundingClientRect().top >= probe) break;
        found = turn;
      }
      if (found) return found.getBoundingClientRect().top < top ? found : null;
      // Continuation after compaction: a live prompt on screen takes
      // precedence; otherwise the live chat's latest segment supplies it.
      if (ledger !== transcript
          || (prompts[0] && prompts[0].getBoundingClientRect().top < window.innerHeight)) return null;
      const segment = [...history.querySelectorAll('.history-chat[data-live] .history-segment')].pop();
      const archived = segment?.querySelector('.history-ledger')?.querySelectorAll(PROMPTS);
      return archived?.length ? archived[archived.length - 1] : null;
    }

    // Scroll events and resize observations already arrive once per frame.
    // Every layout read precedes the writes, so an update never forces layout.
    function update() {
      const top = edge();
      const turn = governing(top);
      const label = turn && text(turn);
      target = label ? turn : null;
      if (!target) { pin.hidden = true; return; }
      const column = transcript.getBoundingClientRect();
      pin.style.top = `${top}px`;
      pin.style.left = `${column.left}px`;
      pin.style.width = `${column.width}px`;
      if (pin.textContent !== label) pin.textContent = label;
      pin.title = label;
      pin.hidden = false;
    }

    pin.addEventListener('click', () => {
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
    return Object.freeze({update});
  },
});
