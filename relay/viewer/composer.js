/* composer.js -- shared browser message submission shortcut */
'use strict';

(() => {
  function bind(input, button) {
    const shortcut = /Mac|iPhone|iPad|iPod/.test(navigator.platform) ? '⌘+Enter' : 'Ctrl+Enter';
    button.title = `${button.title || 'Send message'} (${shortcut})`;
    button.setAttribute('aria-keyshortcuts', 'Control+Enter Meta+Enter');
    input.addEventListener('keydown', event => {
      if (event.defaultPrevented || event.isComposing || event.key !== 'Enter'
          || !(event.ctrlKey || event.metaKey) || event.altKey || event.shiftKey) return;
      event.preventDefault();
      if (event.repeat || input.disabled || input.readOnly || button.disabled) return;
      // Use the same validation, attachments and in-flight guards as a click.
      button.click();
    });
  }

  window.mevedelComposer = Object.freeze({bind});
})();
