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

The relay holds no state beyond live connections plus a lazy max-room-age
sweep (`-max-room-age`, default 24h) as a backstop against a crashed host;
the policy TTL lives in Emacs (`mevedel-collaboration-share-ttl`).

## Build and run

```bash
go build -o mevedel-relay .
./mevedel-relay -addr 127.0.0.1:7466
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
The relay paths must be unprotected, or browser guests get a login page and
the Emacs host's WebSocket dial gets a redirect it cannot follow:

```bash
yunohost domain config set collab.example.net --args 'portal.enabled=false'
# or, per-path, mark the app/paths as unprotected in SSOwat's conf
```

Use a dedicated (sub)domain with the nginx snippet above; the browser needs
HTTPS anyway because WebCrypto only unseals in a secure context.
