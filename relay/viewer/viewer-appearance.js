/* Room preferences; shared editors receive them over their bound item port. */
'use strict';
window.mevedelAppearance = (() => {
  const choices = {palette: ['cool', 'warm'], theme: ['system', 'light', 'dark'], accents: ['selective', 'minimal']};
  const storageKey = 'mevedel-appearance';
  function normalize(value) {
    return Object.fromEntries(Object.entries(choices).map(([key, values]) =>
      [key, values.includes(value?.[key]) ? value[key] : values[0]]));
  }
  function read() {
    try { return normalize(JSON.parse(localStorage.getItem(storageKey))); }
    catch { return normalize(null); }
  }
  function stamp(value) {
    for (const [key, setting] of Object.entries(value)) {
      if (key === 'theme' && setting === 'system') document.documentElement.removeAttribute('data-theme');
      else document.documentElement.setAttribute(`data-${key}`, setting);
    }
  }
  stamp(read());
  function bind(changed) {
    const menu = document.getElementById('appearance-menu');
    const controls = {palette: document.getElementById('palette'), theme: document.getElementById('appearance'), accents: document.getElementById('accents')};
    function apply(value) {
      stamp(value);
      for (const key of Object.keys(controls)) controls[key].value = value[key];
      changed(value);
    }
    for (const control of Object.values(controls)) control.addEventListener('change', () => {
      const value = normalize(Object.fromEntries(Object.entries(controls).map(([key, input]) => [key, input.value])));
      try { localStorage.setItem(storageKey, JSON.stringify(value)); } catch { /* page-only preference */ }
      apply(value);
    });
    document.addEventListener('pointerdown', event => {
      if (!menu.contains(event.target)) menu.open = false;
    });
    menu.addEventListener('keydown', event => {
      if (event.key === 'Escape') { menu.open = false; menu.querySelector('summary').focus(); }
    });
    window.addEventListener('storage', event => {
      if (event.key === storageKey || event.key === null) apply(read());
    });
    apply(read());
  }
  function editorVisible(visible) {
    const menu = document.getElementById('appearance-menu');
    menu.open = false;
    if (visible) document.getElementById('editing-close').before(menu);
    else document.querySelector('.header-actions').prepend(menu);
  }
  return {bind, editorVisible};
})();
