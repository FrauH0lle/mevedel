/* viewer-presence.js -- who else is on this page */
'use strict';

(() => {
  // PAGE returns the store artifact this tab shows, or null for its room
  // or lobby. Every [data-presence] element shows the same people: a tab
  // is on one page at a time.
  function create({send, el, page}) {
    let reported = null;

    // A hidden tab is away; a visible one counts as watching.
    function current() {
      return {page: page() || null, active: !document.hidden};
    }

    function report() {
      const next = current();
      if (reported && reported.page === next.page && reported.active === next.active) return;
      reported = next;
      send({t: 'viewing', ...next});
    }

    // The host forgets a tab's page with its connection, so every hello
    // carries it.
    function hello() {
      reported = current();
      return reported;
    }

    function initials(name) {
      return name.split(/\s+/).filter(Boolean).slice(0, 2)
        .map(word => [...word][0]).join('').toUpperCase() || '?';
    }

    function show(frame) {
      const people = Array.isArray(frame.people)
        ? frame.people.filter(person => person && typeof person.name === 'string') : [];
      const label = people.map(person => person.name + (person.active === true ? '' : ' (away)'));
      for (const holder of document.querySelectorAll('[data-presence]')) {
        holder.replaceChildren(...people.slice(0, 4).map((person, index) => {
          const chip = el('span', `presence-chip${person.active === true ? '' : ' away'}`,
                          initials(person.name));
          chip.title = label[index];
          return chip;
        }));
        if (people.length > 4) holder.append(el('span', 'presence-more', `+${people.length - 4}`));
        holder.hidden = people.length === 0;
        holder.title = label.join(', ');
        holder.setAttribute('aria-label', `Also here: ${label.join(', ')}`);
      }
    }

    document.addEventListener('visibilitychange', report);

    return Object.freeze({report, hello, show});
  }

  window.mevedelPresence = Object.freeze({create});
})();
