/* notifications.js -- room-owned browser notifications for MevView */
'use strict';

(() => {
  function create({state, button, send, flash, decode}) {
    const baseTitle = document.title || 'mevedel live session';
    let registration = null;
    let pushWork = Promise.resolve();

    function supported() {
      return typeof Notification !== 'undefined';
    }

    function pushSupported() {
      return supported() && navigator.serviceWorker
        && typeof PushManager !== 'undefined';
    }

    function enabled() {
      let optedIn = false;
      try {
        optedIn = state.fragment
          && localStorage.getItem(`mevedel-notify:${state.fragment}`) === 'on';
      }
      catch (_error) { /* storage unavailable: stay off */ }
      return optedIn && supported() && Notification.permission === 'granted';
    }

    function away() {
      return document.hidden
        || (typeof document.hasFocus === 'function' && !document.hasFocus());
    }

    function markTitle(on) {
      document.title = on ? `\u25cf ${baseTitle}` : baseTitle;
    }

    function render() {
      if (!button) return;
      button.hidden = !(state.connected && supported());
      const on = enabled();
      const blocked = supported() && Notification.permission === 'denied';
      button.setAttribute('aria-pressed', on ? 'true' : 'false');
      const label = blocked
        ? 'Notifications blocked by the browser'
        : on
          ? state.pushSubscribed
            ? 'Notifications on — background delivery enabled'
            : 'Notifications on — you will be told while this tab is away'
          : 'Notifications off — click to be told when the session needs you';
      button.setAttribute('aria-label', label);
      button.setAttribute('title', label);
      button.className = `bell${on ? ' on' : ''}${blocked ? ' blocked' : ''}`;
      button.textContent = on ? '🔔' : '🔕';
    }

    function maybeNotify(title, body) {
      if (!away()) return;
      markTitle(true);
      if (!enabled() || state.pushSubscribed) return;
      try { new Notification(title, body ? {body} : {}); }
      catch (_error) { /* the title marker remains as the fallback */ }
    }

    function persistShare() {
      if (!state.fragment) return;
      try { localStorage.setItem('mevedel-last-share', state.fragment); }
      catch (_error) { /* storage unavailable */ }
    }

    function forgetShare() {
      try {
        if (localStorage.getItem('mevedel-last-share') === state.fragment) {
          localStorage.removeItem('mevedel-last-share');
        }
      } catch (_error) { /* storage unavailable */ }
    }

    function forgetPreference() {
      if (!state.fragment) return;
      try { localStorage.removeItem(`mevedel-notify:${state.fragment}`); }
      catch (_error) { /* storage unavailable */ }
    }

    function scope() {
      return `/pwa/${encodeURIComponent(state.fragment)}/`;
    }

    function sameBytes(left, right) {
      const a = new Uint8Array(left || 0);
      const b = new Uint8Array(right || 0);
      return a.length === b.length && a.every((byte, index) => byte === b[index]);
    }

    async function syncPushNow() {
      if (!pushSupported() || !state.fragment) {
        state.pushSubscribed = false;
        return false;
      }
      try {
        registration = await navigator.serviceWorker.register(
          '/service-worker.js', {scope: scope()});
        if (!enabled()) return false;
        const response = await fetch('/push-key', {cache: 'no-store'});
        if (!response.ok) throw new Error('push key unavailable');
        const payload = await response.json();
        const key = decode(payload && payload.key);
        if (!key || key.length !== 65) throw new Error('invalid push key');
        let subscription = await registration.pushManager.getSubscription();
        if (subscription
            && !sameBytes(subscription.options
                          && subscription.options.applicationServerKey, key)) {
          await subscription.unsubscribe();
          subscription = null;
        }
        if (!subscription) {
          subscription = await registration.pushManager.subscribe({
            userVisibleOnly: true,
            applicationServerKey: key,
          });
        }
        if (!enabled()) return false;
        if (!subscription || typeof subscription.endpoint !== 'string') {
          throw new Error('push subscription unavailable');
        }
        if (!await send({t: 'push-subscribe', endpoint: subscription.endpoint,
                         active: !away()})) {
          state.pushSubscribed = false;
          render();
          return false;
        }
        state.pushSubscribed = true;
        render();
        return true;
      } catch (_error) {
        state.pushSubscribed = false;
        render();
        return false;
      }
    }

    async function dropPushNow() {
      state.pushSubscribed = false;
      if (navigator.serviceWorker && state.fragment) {
        try {
          registration ||= await navigator.serviceWorker.getRegistration(scope());
          const subscription = registration && registration.pushManager
            && await registration.pushManager.getSubscription();
          if (subscription) await subscription.unsubscribe();
          if (registration) await registration.unregister();
        } catch (_error) { /* the relay also expires the room-bound endpoint */ }
        registration = null;
      }
      await send({t: 'push-unsubscribe'});
    }

    function queuePush(operation) {
      const work = pushWork.then(operation, operation);
      pushWork = work.catch(() => {});
      return work;
    }

    function syncPush() {
      return queuePush(syncPushNow);
    }

    function dropPush() {
      return queuePush(dropPushNow);
    }

    function forget() {
      forgetShare();
      forgetPreference();
      dropPush().catch(() => {});
    }

    function publishState() {
      if (state.pushSubscribed) {
        send({t: 'push-state', active: !away()}).catch(() => {});
      }
    }

    function bind() {
      if (button) {
        button.addEventListener('click', async () => {
          if (!state.connected) return;
          if (enabled()) {
            forgetPreference();
            await dropPush();
          } else if (supported()) {
            const permission = await Notification.requestPermission();
            if (!state.connected) return;
            try {
              localStorage.setItem(`mevedel-notify:${state.fragment}`,
                                   permission === 'granted' ? 'on' : 'off');
            } catch (_error) { /* opt-in then lasts for this page only */ }
            if (permission === 'granted') {
              persistShare();
              await syncPush();
            } else {
              flash('Notifications are blocked for this site.');
            }
          }
          render();
        });
      }
      const publishPresence = () => {
        if (!away()) markTitle(false);
        publishState();
      };
      for (const event of ['focus', 'blur']) {
        window.addEventListener(event, publishPresence);
      }
      document.addEventListener('visibilitychange', publishPresence);
    }

    return Object.freeze({
      bind, dropPush, enabled, forget, maybeNotify, persistShare, render,
      syncPush,
    });
  }

  function pastedShare(value) {
    const text = String(value || '').trim();
    if (!text) return '';
    const hash = text.indexOf('#');
    return (hash >= 0 ? text.slice(hash + 1) : text).trim();
  }

  function installed() {
    return navigator.standalone === true
      || (typeof window.matchMedia === 'function'
          && window.matchMedia('(display-mode: standalone)').matches);
  }

  // The URL wins, then the share this tab was already in, then the one
  // a notification opt-in persisted. The tab store is what lets a
  // plain reload survive: the fragment is wiped from the URL on connect,
  // and sessionStorage is per tab, dies with it, and never enters
  // history, so it keeps the reason for the wipe.
  function resolveShare(parse) {
    const fromUrl = String(window.location.hash || '').replace(/^#/, '');
    let tab = '';
    let stored = '';
    try { tab = sessionStorage.getItem('mevedel-tab-share') || ''; }
    catch (_error) { /* storage unavailable */ }
    try { stored = localStorage.getItem('mevedel-last-share') || ''; }
    catch (_error) { /* storage unavailable */ }
    for (const candidate of [fromUrl, tab, stored]) {
      if (parse(`#${candidate}`)) return candidate;
    }
    if (!installed() || typeof window.prompt !== 'function') return '';
    const pasted = pastedShare(window.prompt(
      'Paste the full MevView share link from the host. It stays on this device.'));
    return parse(`#${pasted}`) ? pasted : '';
  }

  window.mevedelViewerNotifications = Object.freeze({create, resolveShare});
})();
