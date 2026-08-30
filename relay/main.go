// Command relay is the content-blind rendezvous point for mevedel live
// browser collaboration.
//
// It serves the static viewer at "/", upgrades "/r/<roomId>?role=host|guest"
// to a WebSocket, and shovels sealed binary envelopes between one host and
// its guests. Envelopes are "[4-byte big-endian peerId][sealed payload]":
// host frames broadcast to every guest when the prefix is 0 and target guest
// N otherwise; guest frames get their prefix overwritten with the sender's
// peerId before forwarding to the host. The relay never parses a payload.
//
// Unencrypted TEXT control messages carry no session data:
//
//	relay -> host:  {"t":"peer-joined","peer":N} / {"t":"peer-left","peer":N}
//	relay -> guest: {"t":"room-closed"}
//	host -> relay: Web Push subscription routing and empty wake requests
//
// Close codes: 4001 room closed, 4004 no such room, 4009 second host.
//
// With -host-token set, a role=host upgrade must carry the token in the
// X-Mevedel-Host-Token header or it is answered 404.
package main

import (
	"context"
	"crypto/subtle"
	"embed"
	"encoding/binary"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"log"
	"net/http"
	"regexp"
	"strings"
	"sync"
	"time"

	"github.com/coder/websocket"
)

//go:embed viewer
var viewerFS embed.FS

const (
	closeRoomClosed websocket.StatusCode = 4001
	closeNoRoom     websocket.StatusCode = 4004
	closeSecondHost websocket.StatusCode = 4009
	envelopeHeader                       = 4
	// The host bounds one frame at 1 MiB on the wire, sealing and envelope
	// bytes included, and refuses to send a larger one; double it so a
	// compliant frame can never be undeliverable.
	maxFrameBytes = 2 << 20
	writeTimeout  = 5 * time.Second
	// Keepalive pings hold reverse proxies (nginx proxy_read_timeout) open
	// and detect dead peers.
	pingInterval = 30 * time.Second
	pingTimeout  = 10 * time.Second
)

var roomIDPattern = regexp.MustCompile(`^[A-Za-z0-9_-]{10,64}$`)

// hostTokenHeader carries -host-token. A header rather than a query
// parameter: reverse proxies log query strings.
const hostTokenHeader = "X-Mevedel-Host-Token"

const viewerContentSecurityPolicy = "default-src 'none'; " +
	"script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' blob:; " +
	"connect-src 'self'; worker-src 'self'; manifest-src 'self'; " +
	"object-src 'none'; frame-src 'none'; frame-ancestors 'none'; " +
	"base-uri 'none'; form-action 'none'"

type room struct {
	host          *websocket.Conn
	guests        map[uint32]*websocket.Conn
	subscriptions map[string]pushSubscription
	nextPeer      uint32
	created       time.Time
}

type relay struct {
	// ponytail: one global lock; per-room locks if guest counts ever matter.
	mu         sync.Mutex
	rooms      map[string]*room
	maxRoomAge time.Duration
	// hostToken, when non-empty, is required in the X-Mevedel-Host-Token
	// header of a role=host upgrade. Guests stay tokenless: their authority
	// is the bearer link, and their room carries only its host's ciphertext.
	hostToken string
	push      pushSender
}

func newRelay(maxRoomAge time.Duration, hostToken string, push pushSender) *relay {
	return &relay{
		rooms:      make(map[string]*room),
		maxRoomAge: maxRoomAge,
		hostToken:  hostToken,
		push:       push,
	}
}

// send writes one message with a bounded deadline so a stalled peer cannot
// wedge the sender's read loop.
func send(c *websocket.Conn, typ websocket.MessageType, data []byte) error {
	ctx, cancel := context.WithTimeout(context.Background(), writeTimeout)
	defer cancel()
	return c.Write(ctx, typ, data)
}

func controlToHost(event string, peer uint32) []byte {
	return fmt.Appendf(nil, `{"t":%q,"peer":%d}`, event, peer)
}

var roomClosedMsg = []byte(`{"t":"room-closed"}`)

func (rl *relay) mux() *http.ServeMux {
	viewer, err := fs.Sub(viewerFS, "viewer")
	if err != nil {
		panic(err)
	}
	mux := http.NewServeMux()
	files := http.FileServerFS(viewer)
	// The viewer is embedded in the binary; without this, browsers keep a
	// cached viewer.js across relay upgrades and run stale client code.
	mux.Handle("/", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", "no-cache")
		w.Header().Set("Content-Security-Policy", viewerContentSecurityPolicy)
		files.ServeHTTP(w, r)
	}))
	mux.HandleFunc("/healthz", func(w http.ResponseWriter, _ *http.Request) {
		io.WriteString(w, "ok\n")
	})
	mux.HandleFunc("/push-key", func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Cache-Control", "no-store")
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"key": rl.push.PublicKey()})
	})
	mux.HandleFunc("/r/", rl.handleRoom)
	return mux
}

func (rl *relay) handleRoom(w http.ResponseWriter, r *http.Request) {
	id := strings.TrimPrefix(r.URL.Path, "/r/")
	role := r.URL.Query().Get("role")
	if !roomIDPattern.MatchString(id) || (role != "host" && role != "guest") {
		http.NotFound(w, r)
		return
	}
	// 404 rather than 401: an unauthenticated prober learns nothing the
	// unknown-path response does not already tell them.
	if role == "host" && rl.hostToken != "" &&
		subtle.ConstantTimeCompare([]byte(r.Header.Get(hostTokenHeader)),
			[]byte(rl.hostToken)) != 1 {
		http.NotFound(w, r)
		return
	}
	c, err := websocket.Accept(w, r, nil)
	if err != nil {
		return
	}
	c.SetReadLimit(maxFrameBytes)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go keepalive(ctx, c)
	if role == "host" {
		rl.runHost(id, c)
	} else {
		rl.runGuest(id, c)
	}
}

func keepalive(ctx context.Context, c *websocket.Conn) {
	ticker := time.NewTicker(pingInterval)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			pctx, cancel := context.WithTimeout(ctx, pingTimeout)
			err := c.Ping(pctx)
			cancel()
			if err != nil {
				c.Close(websocket.StatusPolicyViolation, "ping timeout")
				return
			}
		}
	}
}

func (rl *relay) runHost(id string, c *websocket.Conn) {
	rl.mu.Lock()
	if _, exists := rl.rooms[id]; exists {
		rl.mu.Unlock()
		c.Close(closeSecondHost, "a host is already connected for this room")
		return
	}
	rm := &room{
		host: c, guests: make(map[uint32]*websocket.Conn),
		subscriptions: make(map[string]pushSubscription), nextPeer: 1, created: time.Now(),
	}
	rl.rooms[id] = rm
	rl.mu.Unlock()
	defer rl.closeRoom(id, rm)
	for {
		typ, data, err := c.Read(context.Background())
		if err != nil {
			return
		}
		if typ == websocket.MessageText {
			rl.handleHostControl(id, rm, data)
			continue
		}
		if typ != websocket.MessageBinary || len(data) < envelopeHeader {
			continue
		}
		peer := binary.BigEndian.Uint32(data)
		rl.mu.Lock()
		var targets []*websocket.Conn
		if peer == 0 {
			for _, g := range rm.guests {
				targets = append(targets, g)
			}
		} else if g, ok := rm.guests[peer]; ok {
			targets = append(targets, g)
		}
		rl.mu.Unlock()
		for _, g := range targets {
			if send(g, websocket.MessageBinary, data) != nil {
				// The guest's own read loop notices the dead socket and
				// cleans up; forwarding just stops here.
				g.Close(websocket.StatusPolicyViolation, "write failed")
			}
		}
	}
}

func (rl *relay) runGuest(id string, c *websocket.Conn) {
	rl.mu.Lock()
	rm, ok := rl.rooms[id]
	if !ok {
		rl.mu.Unlock()
		c.Close(closeNoRoom, "no such room")
		return
	}
	peer := rm.nextPeer
	rm.nextPeer++
	rm.guests[peer] = c
	host := rm.host
	rl.mu.Unlock()
	send(host, websocket.MessageText, controlToHost("peer-joined", peer))
	defer func() {
		rl.mu.Lock()
		_, live := rm.guests[peer]
		delete(rm.guests, peer)
		for guestID, subscription := range rm.subscriptions {
			if subscription.peer == peer {
				subscription.active = false
				subscription.peer = 0
				rm.subscriptions[guestID] = subscription
			}
		}
		current := rl.rooms[id] == rm
		rl.mu.Unlock()
		if live && current {
			send(host, websocket.MessageText, controlToHost("peer-left", peer))
		}
	}()
	for {
		typ, data, err := c.Read(context.Background())
		if err != nil {
			return
		}
		if typ != websocket.MessageBinary || len(data) < envelopeHeader {
			continue
		}
		binary.BigEndian.PutUint32(data, peer)
		// A failed host write means the host is gone; the host read loop
		// tears the room down and this guest gets closed there.
		send(host, websocket.MessageBinary, data)
	}
}

// closeRoom garbage-collects RM once its host is gone: every guest gets the
// room-closed control message and close 4001.
func (rl *relay) closeRoom(id string, rm *room) {
	rl.mu.Lock()
	if rl.rooms[id] != rm {
		rl.mu.Unlock()
		return
	}
	delete(rl.rooms, id)
	guests := make([]*websocket.Conn, 0, len(rm.guests))
	for _, g := range rm.guests {
		guests = append(guests, g)
	}
	rm.guests = make(map[uint32]*websocket.Conn)
	rl.mu.Unlock()
	for _, g := range guests {
		send(g, websocket.MessageText, roomClosedMsg)
		g.Close(closeRoomClosed, "room closed")
	}
}

// closeExpired closes the host socket of every room created before CUTOFF;
// the host read loop then runs the ordinary room teardown. This is the
// backstop against a crashed host whose TCP connection never died cleanly --
// the policy TTL lives host-side.
func (rl *relay) closeExpired(cutoff time.Time) {
	rl.mu.Lock()
	var hosts []*websocket.Conn
	for _, rm := range rl.rooms {
		if rm.created.Before(cutoff) {
			hosts = append(hosts, rm.host)
		}
	}
	rl.mu.Unlock()
	for _, h := range hosts {
		h.Close(websocket.StatusGoingAway, "room expired")
	}
}

func (rl *relay) sweepLoop() {
	for range time.Tick(time.Minute) {
		rl.closeExpired(time.Now().Add(-rl.maxRoomAge))
	}
}

func main() {
	addr := flag.String("addr", "127.0.0.1:7466", "listen address")
	maxRoomAge := flag.Duration("max-room-age", 24*time.Hour,
		"backstop: close rooms older than this")
	hostToken := flag.String("host-token", "",
		"optional "+hostTokenHeader+" token for room creation")
	vapidKeyFile := flag.String("vapid-key-file", "mevedel-relay-vapid.pem",
		"persistent Web Push signing key")
	flag.Parse()
	push, err := loadOrCreateWebPushSender(newPushHTTPClient(), *vapidKeyFile)
	if err != nil {
		log.Fatalf("load Web Push key: %v", err)
	}
	rl := newRelay(*maxRoomAge, *hostToken, push)
	go rl.sweepLoop()
	log.Printf("mevedel relay listening on %s", *addr)
	srv := &http.Server{
		Addr:              *addr,
		Handler:           rl.mux(),
		ReadHeaderTimeout: 10 * time.Second,
	}
	log.Fatal(srv.ListenAndServe())
}
