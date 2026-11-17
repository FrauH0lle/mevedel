/* composer.js -- shared browser message inputs and submission shortcut */
'use strict';

(() => {
  function viewport() {
    const height = window.visualViewport?.height || window.innerHeight;
    const top = window.visualViewport?.offsetTop || 0;
    document.documentElement.style.setProperty('--input-viewport', `${height}px`);
    document.documentElement.style.setProperty('--input-keyboard-inset', `${Math.max(0, window.innerHeight - height - top)}px`);
    const header = document.querySelector('body > .topbar')?.getBoundingClientRect().bottom || top;
    document.querySelectorAll('.expandable-input.expanded').forEach(box => {
      const input = box.firstElementChild;
      const container = input.closest('.dock, .ds-view');
      if (!container) return;
      const bounds = container.getBoundingClientRect();
      const chrome = Math.max(bounds.height, container.scrollHeight) - input.getBoundingClientRect().height;
      const start = container.classList.contains('dock') ? Math.max(top, header + 8)
        : Math.max(top, header + 8, bounds.top);
      const available = Math.min(top + height, bounds.bottom) - start - chrome;
      box.style.setProperty('--input-expanded-max', `${Math.max(80, available)}px`);
    });
  }
  // Safari's keyboard resizes the visual viewport, not necessarily dvh.
  window.visualViewport?.addEventListener('resize', viewport);

  function collapse(input) {
    const box = input.parentElement;
    if (!box?.classList.contains('expandable-input')) return;
    box.classList.remove('expanded');
    input.closest('.dock')?.classList.remove('input-expanded');
    const toggle = box.lastElementChild;
    toggle.setAttribute('aria-label', 'Expand');
    toggle.setAttribute('aria-pressed', 'false');
    toggle.title = 'Expand';
  }

  function expandable(input) {
    if (input.parentElement?.classList.contains('expandable-input')) return;
    const box = document.createElement('div');
    box.className = 'expandable-input';
    input.replaceWith(box);
    const toggle = document.createElement('button');
    toggle.type = 'button';
    toggle.className = 'input-expand';
    toggle.setAttribute('aria-controls', input.id);
    toggle.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><path class="expand-icon" d="M14 4h6v6M20 4l-7 7M10 20H4v-6M4 20l7-7"/><path class="collapse-icon" d="M20 4l-7 7m0-6v6h6M4 20l7-7m-6 0h6v6"/></svg>';
    box.append(input, toggle);
    collapse(input);
    toggle.addEventListener('click', () => {
      const expanded = toggle.getAttribute('aria-pressed') !== 'true';
      box.classList.toggle('expanded', expanded);
      input.closest('.dock')?.classList.toggle('input-expanded', expanded);
      viewport();
      toggle.setAttribute('aria-label', expanded ? 'Collapse' : 'Expand');
      toggle.setAttribute('aria-pressed', String(expanded));
      toggle.title = expanded ? 'Collapse' : 'Expand';
      input.focus({preventScroll: true});
    });
  }

  function bind(input, button) {
    if (input.tagName === 'TEXTAREA') expandable(input);
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

  window.mevedelComposer = Object.freeze({bind, expandable, collapse});
})();
