/* service-worker.js -- background notification delivery for MevView */
'use strict';

self.addEventListener('push', event => {
  event.waitUntil(
    self.registration.showNotification('New mevedel activity', {
      body: 'Open MevView to see the encrypted session update.',
      icon: '/icon.png',
      badge: '/icon.png',
      tag: 'mevedel-activity',
      renotify: true,
    }));
});

self.addEventListener('notificationclick', event => {
  event.notification.close();
  event.waitUntil((async () => {
    const path = new URL(self.registration.scope).pathname.split('/').filter(Boolean);
    const share = path[0] === 'pwa' && path.length === 2
      ? decodeURIComponent(path[1]) : '';
    if (!share) return;
    const target = `${self.location.origin}/#${share}`;
    const windows = await self.clients.matchAll({type: 'window', includeUncontrolled: true});
    const existing = windows.find(client => client.url === target);
    if (existing) await existing.focus();
    else await self.clients.openWindow(target);
  })());
});
