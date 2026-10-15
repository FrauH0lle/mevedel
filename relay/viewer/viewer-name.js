/* viewer-name.js -- the guest's display name */
'use strict';

/* A guest is called by the name they chose, remembered in this browser, or
   else by a random alliterative name. A random name lasts while any of this
   browser's tabs is open: a reload keeps the tab's own, and a new tab, the
   editor tab included, takes it from the open ones. Once every tab is
   closed, the next visit picks a new one. Names are labels, not identities. */
(() => {
  const FAMILIES = [
    [['Brave', 'Bright', 'Breezy'], ['Badger', 'Bear', 'Bison']],
    [['Calm', 'Clever', 'Curious'], ['Capybara', 'Crane', 'Cat']],
    [['Daring', 'Dapper', 'Dreamy'], ['Dolphin', 'Duck', 'Deer']],
    [['Eager', 'Earnest', 'Elegant'], ['Eagle', 'Egret', 'Elephant']],
    [['Fair', 'Friendly', 'Fearless'], ['Fox', 'Finch', 'Falcon']],
    [['Gentle', 'Graceful', 'Gallant'], ['Gecko', 'Gazelle', 'Gibbon']],
    [['Happy', 'Helpful', 'Hopeful'], ['Hedgehog', 'Heron', 'Hare']],
    [['Jolly', 'Jaunty', 'Joyful'], ['Jaguar', 'Jackal', 'Jay']],
    [['Kind', 'Keen', 'Kingly'], ['Koala', 'Kestrel', 'Kiwi']],
    [['Lively', 'Lucky', 'Loyal'], ['Lynx', 'Lemur', 'Lark']],
    [['Merry', 'Mellow', 'Mindful'], ['Mongoose', 'Meerkat', 'Marmot']],
    [['Patient', 'Playful', 'Plucky'], ['Panda', 'Puffin', 'Penguin']],
    [['Quiet', 'Quick', 'Quirky'], ['Quokka', 'Quail', 'Quetzal']],
    [['Ready', 'Radiant', 'Resourceful'], ['Robin', 'Raven', 'Raccoon']],
    [['Sunny', 'Swift', 'Spirited'], ['Sparrow', 'Seal', 'Squirrel']],
    [['Warm', 'Wise', 'Witty'], ['Wombat', 'Wolf', 'Wren']],
  ];
  const CHOSEN = 'mevedel-chosen-name';
  const RANDOM = 'mevedel-random-name';
  // How long a new tab waits for an open one to share its random name.
  const ASK_MS = 150;
  const MAX = 32;

  function generated() {
    const picks = crypto.getRandomValues(new Uint32Array(3));
    const [adjectives, animals] = FAMILIES[picks[0] % FAMILIES.length];
    return `${adjectives[picks[1] % adjectives.length]} ${animals[picks[2] % animals.length]}`;
  }

  const clean = value => (typeof value === 'string' ? value.trim().slice(0, MAX) : '');
  function read(storage, key) {
    try { return clean(storage.getItem(key)); }
    catch (_error) { return ''; }
  }
  function write(storage, key, value) {
    try {
      if (value) storage.setItem(key, value);
      else storage.removeItem(key);
    } catch (_error) { /* the name lasts for this page */ }
  }

  // ONCHANGE hears every later change of the name, from this tab or another.
  function create({onChange, initials}) {
    let random = read(sessionStorage, RANDOM);
    let chosen = read(localStorage, CHOSEN);
    let reported = null;
    const channel = typeof BroadcastChannel === 'function'
      ? new BroadcastChannel('mevedel-guest-name') : null;
    const current = () => chosen || random;
    const menu = document.getElementById('you-menu');
    const form = document.getElementById('you-form');
    const input = document.getElementById('you-name');
    const identity = document.getElementById('composer-identity');

    function render() {
      const name = current();
      const initial = document.getElementById('you-initials');
      const label = document.getElementById('you-label');
      if (initial) initial.textContent = initials(name);
      if (label) label.textContent = name;
      if (menu) menu.title = `You: ${name}`;
      if (identity) {
        identity.textContent = `as ${name}`;
        identity.title = 'Change the name others see';
      }
    }
    function changed() {
      const name = current();
      render();
      if (reported === null || name === reported) return;
      reported = name;
      onChange(name);
    }
    function adopt(name) {
      random = name;
      write(sessionStorage, RANDOM, name);
    }

    channel?.addEventListener('message', ({data}) => {
      const name = clean(data?.name);
      if (data?.t === 'ask' && random) channel.postMessage({t: 'name', name: random});
      else if (data?.t === 'name' && name && !random) adopt(name);
      else if (data?.t === 'random' && name) { adopt(name); changed(); }
    });
    window.addEventListener('storage', event => {
      if (event.key !== CHOSEN && event.key !== null) return;
      chosen = read(localStorage, CHOSEN);
      changed();
    });

    const ready = new Promise(resolve => {
      if (random || !channel) {
        if (!random) adopt(generated());
        resolve();
        return;
      }
      const done = () => {
        clearTimeout(timer);
        channel.removeEventListener('message', answered);
        if (!random) adopt(generated());
        resolve();
      };
      const answered = () => { if (random) done(); };
      const timer = setTimeout(done, ASK_MS);
      channel.addEventListener('message', answered);
      channel.postMessage({t: 'ask'});
    }).then(() => {
      reported = current();
      render();
      return reported;
    });

    // An empty name returns to the random one.
    function choose(value) {
      chosen = clean(value);
      write(localStorage, CHOSEN, chosen);
      changed();
    }
    function randomize() {
      chosen = '';
      write(localStorage, CHOSEN, '');
      let next = generated();
      while (next === random) next = generated();
      adopt(next);
      channel?.postMessage({t: 'random', name: next});
      changed();
    }

    menu?.addEventListener('toggle', () => {
      if (!menu.open || !input) return;
      input.value = chosen || random;
      input.focus();
      input.select();
    });
    form?.addEventListener('submit', event => {
      event.preventDefault();
      choose(input.value);
      if (menu) menu.open = false;
    });
    document.getElementById('you-random')?.addEventListener('click', () => {
      randomize();
      if (input) input.value = random;
    });
    menu?.addEventListener('keydown', event => {
      if (event.key === 'Escape' && menu.open) {
        menu.open = false;
        menu.querySelector('summary')?.focus();
      }
    });
    document.addEventListener('pointerdown', event => {
      if (menu?.open && !menu.contains(event.target)) menu.open = false;
    });
    identity?.addEventListener('click', () => {
      if (menu) menu.open = true;
    });

    return Object.freeze({ready, current, choose, randomize});
  }

  window.mevedelGuestName = Object.freeze({create});
})();
