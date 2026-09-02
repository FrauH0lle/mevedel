/* viewer-session.js -- permission mode and new-session controls */
'use strict';

(() => {
  const MODES = ['ask', 'edits', 'full-auto'];

  function create({state, send, el}) {
    const button = document.getElementById('new-session-button');
    const sheet = document.getElementById('new-session');
    const form = document.getElementById('new-session-form');
    const result = document.getElementById('new-session-result');
    const nameInput = document.getElementById('new-session-name');
    const promptInput = document.getElementById('new-session-prompt');
    const message = document.getElementById('new-session-message');
    const link = document.getElementById('new-session-link');
    const done = document.getElementById('new-session-done');
    const submit = document.getElementById('new-session-create');
    const lede = document.getElementById('new-session-lede');

    function modePicker() {
      const picker = el('select', 'ml mode-picker');
      picker.title = 'Permission mode';
      picker.setAttribute('aria-label', 'Permission mode');
      MODES.forEach(mode => {
        const option = el('option', null, mode);
        option.value = mode;
        picker.append(option);
      });
      picker.value = state.mode;
      picker.addEventListener('change', () => {
        // The host answers with a status frame; until it does the strip
        // must keep reporting the mode the session is actually in.
        const wanted = picker.value;
        picker.value = state.mode;
        send({t: 'set-mode', mode: wanted});
      });
      return picker;
    }

    function open() {
      form.hidden = false;
      result.hidden = true;
      submit.textContent = state.owner ? 'Create' : 'Ask host';
      lede.textContent = state.owner
        ? 'A separate room for separate work. This room, and everyone in '
          + 'it, stays exactly where it is.'
        : 'The host decides. This room, and everyone in it, stays exactly '
          + 'where it is either way.';
      sheet.showModal();
      nameInput.focus();
    }

    function submitRequest() {
      const name = nameInput.value.trim();
      if (!name) return;
      send({t: 'new-session', name, prompt: promptInput.value.trim()});
      showResult({waiting: true});
    }

    function showResult({waiting, ok, message: outcome, link: href, name}) {
      form.hidden = true;
      result.hidden = false;
      message.textContent = waiting
        ? (state.owner
           ? 'Creating…'
           : 'Sent. Waiting for the host — you can close this and keep '
             + 'chatting.')
        : (ok ? `${name} is open.` : (outcome || 'The request was refused.'));
      // A browser will not let a delayed approval open a tab on its own,
      // so a successful result remains a link the guest can tap.
      link.hidden = !(ok && href);
      if (ok && href) link.href = href;
      if (!sheet.open) sheet.showModal();
    }

    function setVisible(visible) {
      button.hidden = !visible;
    }

    button.addEventListener('click', open);
    sheet.addEventListener('close', () => {
      if (sheet.returnValue === 'create') submitRequest();
    });
    done.addEventListener('click', () => sheet.close(''));

    return Object.freeze({modePicker, setVisible, showResult});
  }

  window.mevedelSessionView = Object.freeze({create});
})();
