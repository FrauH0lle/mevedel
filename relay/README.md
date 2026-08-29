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
  `{"t":"room-closed"}`. Host disconnect closes every guest with 4001 and
  garbage-collects the room.
- `GET /healthz` — liveness.

With `-host-token` set, a `role=host` upgrade must carry the token in the
`X-Mevedel-Host-Token` header or it is answered 404, which keeps strangers
who find the endpoint from opening rooms and holding idle connections.
Guests are never asked for it: their authority is the bearer link, and a
stranger's room carries only their own ciphertext. Set the matching
`mevedel-collaboration-relay-host-token` in Emacs. A header rather than a
query parameter, because reverse proxies log query strings.

The relay holds no state beyond live connections plus a lazy max-room-age
sweep (`-max-room-age`, default 24h) as a backstop against a crashed host;
the policy TTL lives in Emacs (`mevedel-collaboration-share-ttl`).

## Build and run

```bash
go build -o mevedel-relay .
./mevedel-relay -addr 127.0.0.1:7466 -host-token "$(head -c 24 /dev/urandom | base64)"
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
ExecStart=/usr/local/bin/mevedel-relay -addr 127.0.0.1:7466
DynamicUser=yes
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

Then set `mevedel-collaboration-relay-url` to `wss://collab.example.net`.

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
(add them via a conf.d drop-in if not). The browser needs HTTPS anyway
because WebCrypto only unseals in a secure context.
