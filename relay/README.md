# mevedel collaboration relay

A content-blind rendezvous point for mevedel live browser collaboration.
The Emacs host and browser guests both dial it; it routes sealed binary
envelopes between them and serves the static viewer. It never holds a room
key: every session payload is AES-256-GCM sealed end to end, and the key
travels only in the share link's URL fragment.

## Wire contract

- `GET /` — the bundled viewer (`viewer/`, embedded via `go:embed`).
- `GET /r/<roomId>?role=host|guest` — WebSocket upgrade. The first host
  creates the room; a second host is closed with 4009, a guest joining a
  missing room with 4004.
- Binary frames are `[4-byte big-endian peerId][sealed payload]`. Host
  frames broadcast to every guest when the prefix is 0 and target guest N
  otherwise; guest frames get the prefix overwritten with the sender's
  peerId before forwarding to the host.
- TEXT control messages (unencrypted, no session data): relay→host
  `{"t":"peer-joined"|"peer-left","peer":N}`; relay→guest
  `{"t":"room-closed"}`; host→relay Web Push subscription routing and wake
  requests. Host disconnect closes every guest with 4001 and garbage-collects
  the room, including its subscriptions.
- `GET /push-key` — the relay's persistent VAPID public key. A notification
  opt-in registers an endpoint for that room and browser guest. Wake requests
  are empty Web Push POSTs: the push service receives no prompt, transcript,
  room key, room id, or notification text. The service worker shows a generic
  notification and the viewer reconnects for encrypted detail. The viewer
  owns each subscription through a room-specific registration scope derived
  locally from the bearer; the scope is not fetched, and notification clicks
  put the bearer back only in the URL fragment. A fresh installed app can
  bootstrap by locally parsing a pasted share link. The viewer reports its
  active state; wake requests skip actively focused subscribers. Away
  push-enabled viewers use Web Push even while their page is still live;
  browsers without an active Push subscription use the live-page Notification
  fallback.
- `GET /healthz` — liveness.

With `-host-token` set, a `role=host` upgrade must carry the token in the
`X-Mevedel-Host-Token` header or it is answered 404. Use this mode for a
public-facing relay to keep strangers from opening rooms, holding idle
connections, or driving outbound Web Push. Omit it for a tokenless localhost
or test relay.
Guests are never asked for it: their authority is the bearer link, and a
stranger's room carries only their own ciphertext. Set the matching
`mevedel-collaboration-relay-host-token` in Emacs when the relay uses one. A
header rather than a query parameter, because reverse proxies log query
strings.

The relay holds only live connections and their Web Push endpoints plus a
lazy max-room-age sweep (`-max-room-age`, default 24h) as a backstop against
a crashed host; the policy TTL lives in Emacs
(`mevedel-collaboration-share-ttl`). Push delivery accepts only bounded HTTPS
endpoints and refuses private or otherwise non-public destination addresses.
The Emacs host retains endpoint routing metadata while the room is live and
replays it after a relay transport reconnect.

## Releases

Every push to `master` that touches `relay/` builds and publishes static
Linux binaries for amd64 and arm64, tagged `relay-<short sha>`
(`.github/workflows/relay.yml`). The viewer is `go:embed`-ed, so a CSS or JS
change produces a new binary too. `go vet` and `go test` gate the release.

```bash
gh release download relay-<short sha> -p 'mevedel-relay-linux-amd64'
```

## Build and run

```bash
go build -o mevedel-relay .
./mevedel-relay -addr 127.0.0.1:7466 \
  -host-token "$(head -c 24 /dev/urandom | base64)" \
  -vapid-key-file ./mevedel-relay-vapid.pem
go test ./...
```

For local-network use, run it on the machine you like and point
`mevedel-collaboration-relay-url` at it (`ws://127.0.0.1:7466`).

## Deploying behind nginx

The relay binds loopback; TLS termination and the public hostname belong to
the reverse proxy. WebSocket upgrades need the `Upgrade` headers, and the
proxy read timeout must outlive the relay's 30s keepalive ping interval:

```nginx
location / {
    proxy_pass http://127.0.0.1:7466;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_read_timeout 120s;
}
```

Systemd unit:

```ini
[Unit]
Description=mevedel collaboration relay
After=network.target

[Service]
EnvironmentFile=/etc/mevedel-relay
ExecStart=/usr/local/bin/mevedel-relay -addr 127.0.0.1:7466 -host-token ${MEVEDEL_HOST_TOKEN} -vapid-key-file /var/lib/mevedel-relay/vapid.pem
DynamicUser=yes
StateDirectory=mevedel-relay
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

Create `/etc/mevedel-relay` as a root-owned mode-0600 file containing
`MEVEDEL_HOST_TOKEN=<random-token>`:

```bash
TOKEN=$(head -c 24 /dev/urandom | base64)
sudo install -o root -g root -m 600 /dev/null /etc/mevedel-relay
printf 'MEVEDEL_HOST_TOKEN=%s\n' "$TOKEN" | sudo tee /etc/mevedel-relay >/dev/null
```

Then set `mevedel-collaboration-relay-url` to `wss://collab.example.net` and
`mevedel-collaboration-relay-host-token` to the same token. To print only the
value expected by that option later, run
`sudo sed -n 's/^MEVEDEL_HOST_TOKEN=//p' /etc/mevedel-relay`.

### yunohost

Everything on a yunohost domain sits behind the SSOwat portal by default.
The relay paths must be reachable without the portal, or browser guests get
a login page and the Emacs host's WebSocket dial gets a redirect it cannot
follow. Hand-editing `/etc/ssowat/conf.json.persistent` is discouraged and
its `skipped_urls` handling has broken across releases, so go through the
app system instead: install the `redirect_ynh` app on a dedicated
(sub)domain in **reverse-proxy** mode targeting `http://127.0.0.1:7466`,
then make it public:

```bash
yunohost app install redirect  # domain: collab.example.net, path: /,
                               # mode: public proxy, target: http://127.0.0.1:7466
yunohost user permission update redirect.main --add visitors
```

Check that the generated nginx location carries the WebSocket `Upgrade`
headers and a `proxy_read_timeout` above the relay's 30s keepalive ping
(add them via a conf.d drop-in if not). Keep `/push-key` and
`/service-worker.js` on the same public origin as the viewer. The browser
needs HTTPS because WebCrypto, service workers, and Web Push require a secure
context.
