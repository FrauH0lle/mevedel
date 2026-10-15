/* viewer-header.js -- one page header for the room, its editor and panels */
'use strict';

/* Every page's head is a .topbar: the wordmark, a heading of a small line
   over the title, and .header-actions. The head on screen carries the
   room-wide controls, such as the Appearance menu. The header never wraps:
   actions that leave the heading too little room, or cut off its title while
   it holds less than half the header, fold into the head's ⋯ menu, lowest
   data-fold first and the rightmost among equals; .keep actions, such as
   presence, never fold. */
(() => {
  // The heading's least width before actions fold away.
  const MIN_HEADING = 120;

  function create({shared = []} = {}) {
    const heads = new Map();
    let scheduled = false;
    const observer = typeof MutationObserver === 'function'
      ? new MutationObserver(records => {
        // Folding moves nodes; only a changed action or title refits.
        if (records.some(record => record.type !== 'childList'
                         || record.target.closest?.('.room-heading'))) schedule();
      })
      : null;

    const order = node => Number(node.dataset.order);
    const fold = node => Number(node.dataset.fold || 5);
    const movable = node => !node.classList.contains('keep')
      && !node.classList.contains('head-more');

    function setup(head) {
      const actions = head.querySelector('.header-actions');
      if (!actions || heads.has(head)) return;
      const more = document.createElement('details');
      more.className = 'head-more keep';
      more.hidden = true;
      const summary = document.createElement('summary');
      summary.textContent = '⋯';
      summary.setAttribute('aria-label', 'More');
      summary.title = 'More';
      const body = document.createElement('div');
      body.className = 'head-more-body';
      more.append(summary, body);
      actions.append(more);
      // A chosen action is done with the menu; a nested menu is not.
      body.addEventListener('click', event => {
        if (event.target.closest('button, a')) more.open = false;
      });
      more.addEventListener('keydown', event => {
        if (event.key === 'Escape' && more.open) {
          more.open = false;
          summary.focus();
        }
      });
      [...actions.children].filter(movable)
        .forEach((node, index) => { node.dataset.order = String(index); });
      heads.set(head, {actions, more, body});
      observer?.observe(head, {subtree: true, childList: true, characterData: true,
                               attributes: true, attributeFilter: ['hidden']});
    }

    // The head on screen: an open panel's, else the room's.
    function active() {
      const panels = [...document.querySelectorAll('.viewer-panel')]
        .filter(panel => !panel.hidden);
      return panels.at(-1)?.querySelector('.topbar')
        || document.querySelector('body > .topbar');
    }

    function fit(head) {
      const entry = heads.get(head);
      if (!entry || !head.getClientRects().length) return;
      const {actions, more, body} = entry;
      const placed = [...actions.children, ...body.children].filter(movable)
        .sort((a, b) => order(a) - order(b));
      placed.forEach(node => actions.insertBefore(node, more));
      more.hidden = false;
      const heading = head.querySelector('.room-heading');
      const title = heading?.querySelector('h1, .heading-title');
      // A cut-off title matters more than an action, up to half the header.
      const cramped = () => {
        if (head.scrollWidth > head.clientWidth) return true;
        const width = heading ? heading.getBoundingClientRect().width : Infinity;
        return width < MIN_HEADING || (width < head.clientWidth / 2
          && title && title.scrollWidth > title.clientWidth + 1);
      };
      const queue = placed.slice()
        .sort((a, b) => fold(a) - fold(b) || order(b) - order(a));
      const folded = [];
      while (queue.length && cramped()) {
        const node = queue.shift();
        folded.push(node);
        body.append(node);
        // A hidden action takes no room; folding goes on past it.
        while (queue.length && queue[0].hidden) folded.push(queue.shift());
      }
      folded.sort((a, b) => order(a) - order(b)).forEach(node => body.append(node));
      more.hidden = !folded.some(node => !node.hidden);
      if (more.hidden) more.open = false;
      observer?.takeRecords();
    }

    // Room-wide controls follow the head on screen: kept ones lead its
    // actions, others come first among those that fold.
    function place() {
      const head = active();
      const entry = heads.get(head);
      if (!entry) return;
      const kept = shared.filter(node => node?.classList.contains('keep'));
      for (const node of shared) {
        if (!node || entry.actions.contains(node) || entry.body.contains(node)) continue;
        if (node.open) node.open = false;
        node.dataset.order = '-1';
        const anchor = kept.includes(node)
          ? [...entry.actions.children].find(child => !kept.includes(child))
          : [...entry.actions.children].find(movable) || entry.more;
        entry.actions.insertBefore(node, anchor);
      }
      for (const other of heads.keys()) fit(other);
    }

    function schedule() {
      if (scheduled) return;
      scheduled = true;
      const run = () => { scheduled = false; place(); };
      if (typeof requestAnimationFrame === 'function') requestAnimationFrame(run);
      else run();
    }

    document.querySelectorAll('.topbar').forEach(setup);
    // Opening or closing a panel changes which head is on screen.
    document.querySelectorAll('.viewer-panel').forEach(panel => observer?.observe(
      panel, {attributes: true, attributeFilter: ['hidden']}));
    window.addEventListener('resize', schedule);
    document.addEventListener('pointerdown', event => {
      for (const {more} of heads.values()) {
        if (more.open && !more.contains(event.target)) more.open = false;
      }
    });
    place();
    return Object.freeze({refit: schedule});
  }

  window.mevedelHeader = Object.freeze({create});
})();
