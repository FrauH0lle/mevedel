/* collaboration-notifications-test.js -- browser notification assertions
 *
 * Run directly with: node test/collaboration-notifications-test.js
 * The full viewer runner also includes these checks.
 */
'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

class Button {
  constructor() {
    this.listeners = {};
    this.attributes = {};
    this.hidden = false;
  }
  addEventListener(type, callback) { this.listeners[type] = callback; }
  setAttribute(name, value) { this.attributes[name] = value; }
  dispatch(type) { this.listeners[type](); }
}

const tick = () => new Promise(resolve => setImmediate(resolve));

async function waitFor(predicate, what) {
  for (let i = 0; i < 500; i++) {
    if (predicate()) return;
    await tick();
  }
  throw new Error(`timed out waiting for ${what}`);
}

async function testServiceWorker() {
  const listeners = {};
  const notifications = [];
  let opened;
  const share = 'roomroomroomroom.secret';
  const client = {
    url: 'https://relay.example/', visibilityState: 'hidden', focused: false,
    focus: async () => { client.focused = true; },
  };
  const self = {
    addEventListener(type, callback) { listeners[type] = callback; },
    registration: {
      scope: `https://relay.example/pwa/${share}/`,
      showNotification: async (title, options) => notifications.push({title, options}),
    },
    location: {origin: 'https://relay.example'},
    clients: {
      matchAll: async () => [client],
      openWindow: async url => { opened = url; },
    },
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/service-worker.js', 'utf8'),
                     {self, URL});
  let pending;
  listeners.push({waitUntil(promise) { pending = promise; }});
  await pending;
  assert.equal(notifications.length, 1);
  assert.equal(notifications[0].title, 'New mevedel activity');
  assert.doesNotMatch(JSON.stringify(notifications[0]), /prompt|transcript|secret/i);

  let closed = false;
  listeners.notificationclick({
    notification: {close() { closed = true; }},
    waitUntil(promise) { pending = promise; },
  });
  await pending;
  assert.equal(closed, true);
  assert.equal(client.focused, false);
  assert.equal(opened, `https://relay.example/#${share}`);
}

async function testNotificationsModule() {
  const storage = new Map();
  const tabStorage = new Map();
  const registrations = new Map();
  const unsubscribed = [];
  const unregistered = [];
  const sent = [];
  const makeRegistration = scope => {
    let subscription = null;
    return {
      scope,
      unregister: async () => {
        unregistered.push(scope);
        registrations.delete(scope);
        return true;
      },
      pushManager: {
        getSubscription: async () => subscription,
        subscribe: async options => {
          subscription = {
            endpoint: `https://push.example/${registrations.size}`,
            options,
            unsubscribe: async () => {
              unsubscribed.push(scope);
              subscription = null;
            },
          };
          return subscription;
        },
      },
    };
  };
  const window = {
    location: {hash: ''},
    prompt: () => 'https://relay.example/#freshfreshfresh1.fresh-secret',
    matchMedia: () => ({matches: true}),
    addEventListener() {},
  };
  const context = {
    window,
    document: {title: 'mevedel', hidden: false, hasFocus: () => true,
               addEventListener() {}},
    navigator: {
      standalone: true,
      serviceWorker: {
        register: async (script, options) => {
          assert.equal(script, '/service-worker.js');
          assert.doesNotMatch(script, /fresh-secret|room-secret/);
          if (!registrations.has(options.scope)) {
            registrations.set(options.scope, makeRegistration(options.scope));
          }
          return registrations.get(options.scope);
        },
        getRegistration: async scope => registrations.get(scope),
      },
    },
    Notification: class Notification {
      static permission = 'granted';
      static async requestPermission() { return 'granted'; }
    },
    PushManager: class PushManager {},
    localStorage: {
      getItem: key => storage.get(key) || null,
      setItem: (key, value) => storage.set(key, value),
      removeItem: key => storage.delete(key),
    },
    sessionStorage: {
      getItem: key => tabStorage.get(key) || null,
      setItem: (key, value) => tabStorage.set(key, value),
      removeItem: key => tabStorage.delete(key),
    },
    fetch: async () => ({ok: true, json: async () => ({key: 'push-key'})}),
    Uint8Array, URL, encodeURIComponent, decodeURIComponent,
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/notifications.js', 'utf8'), context);
  const api = window.mevedelViewerNotifications;
  const parse = fragment => /^#[^.]{16}\..+/.test(fragment) ? {} : null;
  storage.set('mevedel-last-share', 'storedstoredsto1.stored-secret');
  tabStorage.set('mevedel-tab-share', 'tabtabtabtabtab1.tab-secret');
  assert.equal(api.resolveShare(parse), 'tabtabtabtabtab1.tab-secret');
  tabStorage.clear();
  assert.equal(api.resolveShare(parse), 'storedstoredsto1.stored-secret');
  storage.clear();
  assert.equal(api.resolveShare(parse), 'freshfreshfresh1.fresh-secret');

  const decode = () => Uint8Array.from({length: 65}, (_, index) => index);
  const make = (fragment, write = async frame => sent.push({fragment, frame})) => {
    const button = new Button();
    const state = {fragment, connected: true, pushSubscribed: false};
    const notifications = api.create({
      state,
      button, decode, flash() {},
      send: write,
    });
    notifications.bind();
    return {button, notifications, state};
  };
  const firstFragment = 'roomroomroomroom.room-secret-one';
  const secondFragment = 'otherroomroomroo.room-secret-two';
  const first = make(firstFragment);
  const second = make(secondFragment);

  first.button.dispatch('click');
  await waitFor(() => first.notifications.enabled(), 'first room opt-in');
  await waitFor(() => sent.some(item => item.fragment === firstFragment
                                       && item.frame.t === 'push-subscribe'),
                'first room push subscription');
  assert.equal(second.notifications.enabled(), false);

  second.button.dispatch('click');
  await waitFor(() => second.notifications.enabled(), 'second room opt-in');
  await waitFor(() => sent.filter(item => item.frame.t === 'push-subscribe').length === 2,
                'second room push subscription');
  assert.equal(registrations.size, 2);

  first.button.dispatch('click');
  await waitFor(() => !first.notifications.enabled(), 'first room opt-out');
  await waitFor(() => unsubscribed.length === 1, 'first room unsubscribe');
  assert.equal(second.notifications.enabled(), true);
  assert.equal(storage.has(`mevedel-notify:${firstFragment}`), false);
  assert.equal(storage.get(`mevedel-notify:${secondFragment}`), 'on');
  assert.deepEqual(unsubscribed, [`/pwa/${firstFragment}/`]);
  assert.deepEqual(unregistered, [`/pwa/${firstFragment}/`]);
  assert.equal(registrations.size, 1);
  assert.equal(sent.filter(item => item.frame.t === 'push-unsubscribe').length, 1);

  second.notifications.forget();
  await waitFor(() => unregistered.length === 2, 'second room unregister');
  assert.equal(storage.has(`mevedel-notify:${secondFragment}`), false);
  assert.equal(storage.has('mevedel-last-share'), false);
  assert.equal(registrations.size, 0);

  const failedFragment = 'failedroomroom12.room-secret-four';
  storage.set(`mevedel-notify:${failedFragment}`, 'on');
  const failed = make(failedFragment, async frame => {
    sent.push({fragment: failedFragment, frame});
    return false;
  });
  assert.equal(await failed.notifications.syncPush(), false);
  assert.equal(failed.state.pushSubscribed, false);
  await failed.notifications.dropPush();
  assert.equal(registrations.size, 0);

  const terminalFragment = 'terminalroomroom.room-secret-five';
  const subscribeAtTerminal = sent.filter(item =>
    item.frame.t === 'push-subscribe').length;
  let finishPermission;
  context.Notification.requestPermission = () => new Promise(resolve => {
    finishPermission = resolve;
  });
  const terminal = make(terminalFragment);
  terminal.button.dispatch('click');
  await waitFor(() => finishPermission, 'deferred notification permission');
  terminal.state.connected = false;
  terminal.notifications.forget();
  finishPermission('granted');
  await tick();
  await tick();
  assert.equal(storage.has(`mevedel-notify:${terminalFragment}`), false);
  assert.equal(storage.has('mevedel-last-share'), false);
  assert.equal(sent.filter(item => item.frame.t === 'push-subscribe').length,
               subscribeAtTerminal);
  context.Notification.requestPermission = async () => 'granted';

  // Opt-out owns the lifecycle even when service-worker registration is
  // still in flight: the stale setup must not subscribe after teardown.
  const delayedFragment = 'delayedroomroom1.room-secret-three';
  const subscribeBefore = sent.filter(item =>
    item.frame.t === 'push-subscribe').length;
  let releaseRegister;
  context.navigator.serviceWorker.register = async (_script, options) => {
    const delayed = makeRegistration(options.scope);
    registrations.set(options.scope, delayed);
    return new Promise(resolve => { releaseRegister = () => resolve(delayed); });
  };
  const delayed = make(delayedFragment);
  delayed.button.dispatch('click');
  await waitFor(() => releaseRegister, 'deferred service-worker registration');
  delayed.button.dispatch('click');
  releaseRegister();
  await waitFor(() => !registrations.size, 'deferred registration teardown');
  assert.equal(delayed.notifications.enabled(), false);
  assert.equal(sent.filter(item => item.frame.t === 'push-subscribe').length,
               subscribeBefore);
}

async function testLivePageFallback() {
  const fragment = 'roomroomroomroom.room-secret';
  const storage = new Map([[`mevedel-notify:${fragment}`, 'on']]);
  const shown = [];
  const document = {
    title: 'mevedel', hidden: false, hasFocus: () => !document.hidden,
    addEventListener() {},
  };
  const window = {addEventListener() {}};
  const context = {
    window, document, navigator: {},
    Notification: class Notification {
      static permission = 'granted';
      constructor(title, options) { shown.push({title, options}); }
    },
    localStorage: {
      getItem: key => storage.get(key) || null,
      setItem: (key, value) => storage.set(key, value),
      removeItem: key => storage.delete(key),
    },
    Uint8Array, encodeURIComponent,
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/notifications.js', 'utf8'), context);
  const notifications = window.mevedelViewerNotifications.create({
    state: {fragment, connected: true, pushSubscribed: false},
    button: null, decode() {}, flash() {}, send: async () => {},
  });

  notifications.maybeNotify('Focused', 'ignored');
  assert.equal(shown.length, 0);
  document.hidden = true;
  notifications.maybeNotify('Away', 'shown');
  assert.equal(shown.length, 1);
  assert.equal(shown[0].title, 'Away');
  assert.equal(shown[0].options.body, 'shown');
  storage.set(`mevedel-notify:${fragment}`, 'off');
  notifications.maybeNotify('Opted out', 'ignored');
  assert.equal(shown.length, 1);
}

async function runNotificationTests() {
  await testServiceWorker();
  await testNotificationsModule();
  await testLivePageFallback();
}

if (require.main === module) {
  runNotificationTests().then(() => console.log('viewer notifications passed'))
    .catch(error => {
      console.error(error);
      process.exit(1);
    });
}

module.exports = runNotificationTests;
