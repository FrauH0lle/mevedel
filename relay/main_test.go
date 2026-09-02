package main

import (
	"context"
	"encoding/binary"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/coder/websocket"
)

type fakePushSender struct {
	key  string
	sent chan string
}

func (s *fakePushSender) PublicKey() string { return s.key }

func (s *fakePushSender) Send(_ context.Context, endpoint string) (int, error) {
	s.sent <- endpoint
	return http.StatusCreated, nil
}

const (
	testHostToken = "test-host-token"
	testRoom      = "abcdefghij0123456789"
)

func startRelay(t *testing.T) (*relay, *httptest.Server) {
	t.Helper()
	push, err := newWebPushSender(newPushHTTPClient())
	if err != nil {
		t.Fatalf("new push sender: %v", err)
	}
	rl := newRelay(testHostToken, push)
	srv := httptest.NewServer(rl.mux())
	t.Cleanup(srv.Close)
	return rl, srv
}

func dial(t *testing.T, srv *httptest.Server, roomID, role string) *websocket.Conn {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	options := &websocket.DialOptions{HTTPHeader: http.Header{}}
	if role == "host" {
		options.HTTPHeader.Set(hostTokenHeader, testHostToken)
	}
	c, _, err := websocket.Dial(
		ctx, srv.URL+"/r/"+roomID+"?role="+role, options)
	if err != nil {
		t.Fatalf("dial %s as %s: %v", roomID, role, err)
	}
	t.Cleanup(func() { c.CloseNow() })
	return c
}

func TestHostTokenModes(t *testing.T) {
	push, err := newWebPushSender(newPushHTTPClient())
	if err != nil {
		t.Fatalf("new push sender: %v", err)
	}
	rl := newRelay("s3cret", push)
	srv := httptest.NewServer(rl.mux())
	t.Cleanup(srv.Close)

	hostDial := func(token string) (*websocket.Conn, *http.Response, error) {
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		opts := &websocket.DialOptions{HTTPHeader: http.Header{}}
		if token != "" {
			opts.HTTPHeader.Set(hostTokenHeader, token)
		}
		return websocket.Dial(ctx, srv.URL+"/r/"+testRoom+"?role=host", opts)
	}

	for _, token := range []string{"", "wrong"} {
		c, resp, err := hostDial(token)
		if err == nil {
			c.CloseNow()
			t.Fatalf("host token %q was accepted", token)
		}
		if resp == nil || resp.StatusCode != http.StatusNotFound {
			t.Fatalf("host token %q: want 404, got %v", token, resp)
		}
	}

	// A guest is never asked for the token, but it needs a room to join.
	host, _, err := hostDial("s3cret")
	if err != nil {
		t.Fatalf("dial with the right token: %v", err)
	}
	t.Cleanup(func() { host.CloseNow() })
	guest := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":1}`)
	guest.CloseNow()

	openRelay := newRelay("", push)
	openServer := httptest.NewServer(openRelay.mux())
	t.Cleanup(openServer.Close)
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	openHost, _, err := websocket.Dial(
		ctx, openServer.URL+"/r/"+testRoom+"?role=host", nil)
	if err != nil {
		t.Fatalf("dial tokenless relay: %v", err)
	}
	t.Cleanup(func() { openHost.CloseNow() })
}

func read(t *testing.T, c *websocket.Conn) (websocket.MessageType, []byte) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	typ, data, err := c.Read(ctx)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	return typ, data
}

func expectText(t *testing.T, c *websocket.Conn, want string) {
	t.Helper()
	typ, data := read(t, c)
	if typ != websocket.MessageText || string(data) != want {
		t.Fatalf("got %v %q, want TEXT %q", typ, data, want)
	}
}

func expectBinary(t *testing.T, c *websocket.Conn, want []byte) {
	t.Helper()
	typ, data := read(t, c)
	if typ != websocket.MessageBinary || string(data) != string(want) {
		t.Fatalf("got %v %v, want BINARY %v", typ, data, want)
	}
}

func expectClose(t *testing.T, c *websocket.Conn, want websocket.StatusCode) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	_, _, err := c.Read(ctx)
	if got := websocket.CloseStatus(err); got != want {
		t.Fatalf("close status %v (err %v), want %v", got, err, want)
	}
}

func envelope(peer uint32, payload string) []byte {
	frame := make([]byte, envelopeHeader+len(payload))
	binary.BigEndian.PutUint32(frame, peer)
	copy(frame[envelopeHeader:], payload)
	return frame
}

func writeBinary(t *testing.T, c *websocket.Conn, data []byte) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := c.Write(ctx, websocket.MessageBinary, data); err != nil {
		t.Fatalf("write: %v", err)
	}
}

func writeText(t *testing.T, c *websocket.Conn, text string) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := c.Write(ctx, websocket.MessageText, []byte(text)); err != nil {
		t.Fatalf("write text: %v", err)
	}
}

func expectNoPush(t *testing.T, sent <-chan string) {
	t.Helper()
	select {
	case endpoint := <-sent:
		t.Fatalf("unexpected push to %s", endpoint)
	case <-time.After(100 * time.Millisecond):
	}
}

func TestRouting(t *testing.T) {
	_, srv := startRelay(t)
	host := dial(t, srv, testRoom, "host")
	guest1 := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":1}`)
	guest2 := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":2}`)

	// Broadcast (peer 0) reaches every guest unchanged.
	writeBinary(t, host, envelope(0, "all"))
	expectBinary(t, guest1, envelope(0, "all"))
	expectBinary(t, guest2, envelope(0, "all"))

	// Targeted frame reaches only guest 2.
	writeBinary(t, host, envelope(2, "only-two"))
	expectBinary(t, guest2, envelope(2, "only-two"))

	// Guest frames arrive at the host with the prefix rewritten to the
	// sender's peerId, whatever the guest claimed.
	writeBinary(t, guest1, envelope(999, "from-one"))
	expectBinary(t, host, envelope(1, "from-one"))

	// guest1 saw nothing besides the broadcast: the next frame it receives
	// is a fresh broadcast, not the targeted one.
	writeBinary(t, host, envelope(0, "again"))
	expectBinary(t, guest1, envelope(0, "again"))
}

func TestPushSubscriptionLifecycle(t *testing.T) {
	rl, srv := startRelay(t)
	sender := &fakePushSender{key: "vapid-public", sent: make(chan string, 4)}
	rl.push = sender

	resp, err := http.Get(srv.URL + "/push-key")
	if err != nil {
		t.Fatalf("push key: %v", err)
	}
	var key struct {
		Key string `json:"key"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&key); err != nil {
		t.Fatalf("decode push key: %v", err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusOK || key.Key != sender.key {
		t.Fatalf("push key: status %d, key %q", resp.StatusCode, key.Key)
	}

	host := dial(t, srv, testRoom, "host")
	phone := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":1}`)
	tablet := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":2}`)
	writeText(t, host,
		`{"t":"push-subscribe","peer":1,"guestId":"phone-guest","endpoint":"https://push.example/subscription"}`)
	writeText(t, host,
		`{"t":"push-subscribe","peer":2,"guestId":"tablet-guest","endpoint":"https://push.example/tablet","active":true}`)
	writeText(t, host, `{"t":"push","guestIds":["phone-guest"]}`)
	select {
	case endpoint := <-sender.sent:
		if endpoint != "https://push.example/subscription" {
			t.Fatalf("push endpoint %q", endpoint)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("push was not sent")
	}
	expectNoPush(t, sender.sent)
	writeText(t, host, `{"t":"push"}`)
	select {
	case endpoint := <-sender.sent:
		if endpoint != "https://push.example/subscription" {
			t.Fatalf("active guest suppression sent to %q", endpoint)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("broadcast push was not sent")
	}
	expectNoPush(t, sender.sent)
	writeText(t, host,
		`{"t":"push-state","peer":2,"guestId":"tablet-guest","active":false}`)
	writeText(t, host, `{"t":"push"}`)
	woken := map[string]bool{}
	for range 2 {
		select {
		case endpoint := <-sender.sent:
			woken[endpoint] = true
		case <-time.After(5 * time.Second):
			t.Fatal("broadcast push was not sent")
		}
	}
	if !woken["https://push.example/subscription"] ||
		!woken["https://push.example/tablet"] {
		t.Fatalf("broadcast push endpoints: %v", woken)
	}

	writeText(t, host,
		`{"t":"push-unsubscribe","guestId":"phone-guest"}`)
	writeText(t, host, `{"t":"push","guestIds":["phone-guest"]}`)
	expectNoPush(t, sender.sent)
	writeText(t, host,
		`{"t":"push-unsubscribe","guestId":"tablet-guest"}`)

	writeText(t, host,
		`{"t":"push-subscribe","peer":1,"guestId":"phone-guest","endpoint":"http://internal.invalid/subscription"}`)
	writeText(t, host, `{"t":"push"}`)
	expectNoPush(t, sender.sent)
	// The Emacs host can replay a remembered endpoint after its relay
	// transport reconnects, before the suspended guest has a live peer.
	writeText(t, host,
		`{"t":"push-subscribe","peer":0,"guestId":"phone-guest","endpoint":"https://push.example/restored"}`)
	writeText(t, host, `{"t":"push","guestIds":["phone-guest"]}`)
	select {
	case endpoint := <-sender.sent:
		if endpoint != "https://push.example/restored" {
			t.Fatalf("restored push endpoint %q", endpoint)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("restored push was not sent")
	}
	phone.CloseNow()
	tablet.CloseNow()
}

func TestGuestLeaveNotifiesHost(t *testing.T) {
	_, srv := startRelay(t)
	host := dial(t, srv, testRoom, "host")
	guest := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":1}`)
	guest.Close(websocket.StatusNormalClosure, "bye")
	expectText(t, host, `{"t":"peer-left","peer":1}`)
}

func TestHostDisconnectClosesRoom(t *testing.T) {
	rl, srv := startRelay(t)
	host := dial(t, srv, testRoom, "host")
	guest := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":1}`)
	host.Close(websocket.StatusNormalClosure, "done")
	expectText(t, guest, `{"t":"room-closed"}`)
	expectClose(t, guest, closeRoomClosed)

	waitEmpty(t, rl)
	// The room id is reusable after teardown: retry semantics for guests.
	redial := dial(t, srv, testRoom, "host")
	defer redial.CloseNow()
}

func waitEmpty(t *testing.T, rl *relay) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for {
		rl.mu.Lock()
		n := len(rl.rooms)
		rl.mu.Unlock()
		if n == 0 {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("rooms not garbage-collected: %d left", n)
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func TestSecondHostClosed4009(t *testing.T) {
	_, srv := startRelay(t)
	first := dial(t, srv, testRoom, "host")
	defer first.CloseNow()
	second := dial(t, srv, testRoom, "host")
	expectClose(t, second, closeSecondHost)
}

func TestGuestWithoutRoomClosed4004(t *testing.T) {
	_, srv := startRelay(t)
	guest := dial(t, srv, testRoom, "guest")
	expectClose(t, guest, closeNoRoom)
}

func TestPathValidation(t *testing.T) {
	_, srv := startRelay(t)
	for _, url := range []string{
		srv.URL + "/r/short?role=host",                           // room id too short
		srv.URL + "/r/" + testRoom + "?role=admin",               // unknown role
		srv.URL + "/r/" + testRoom,                               // missing role
		srv.URL + "/r/bad!chars-aaaaaa?role=guest",               // invalid characters
		srv.URL + "/r/" + strings.Repeat("a", 65) + "?role=host", // too long
	} {
		resp, err := http.Get(url)
		if err != nil {
			t.Fatalf("GET %s: %v", url, err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusNotFound {
			t.Errorf("GET %s: status %d, want 404", url, resp.StatusCode)
		}
	}
}

func TestHealthzAndViewer(t *testing.T) {
	_, srv := startRelay(t)
	resp, err := http.Get(srv.URL + "/healthz")
	if err != nil {
		t.Fatalf("healthz: %v", err)
	}
	body, _ := io.ReadAll(resp.Body)
	resp.Body.Close()
	if resp.StatusCode != http.StatusOK || string(body) != "ok\n" {
		t.Fatalf("healthz: %d %q", resp.StatusCode, body)
	}
	resp, err = http.Get(srv.URL + "/")
	if err != nil {
		t.Fatalf("viewer: %v", err)
	}
	body, _ = io.ReadAll(resp.Body)
	resp.Body.Close()
	if resp.StatusCode != http.StatusOK || !strings.Contains(string(body), "mevedel") {
		t.Fatalf("viewer index: %d, body %q...", resp.StatusCode, body[:min(len(body), 80)])
	}
	// The installable-viewer assets ship in the embedded bundle.
	for _, path := range []string{
		"/manifest.json", "/icon.svg", "/icon.png", "/notifications.js",
		"/renderer.js", "/transport.js", "/viewer-agent.js",
		"/viewer-artifact.js", "/viewer-task.js", "/viewer-panel.css",
		"/viewer-agent.css", "/viewer-artifact.css", "/viewer-task.css",
		"/service-worker.js",
	} {
		resp, err = http.Get(srv.URL + path)
		if err != nil {
			t.Fatalf("%s: %v", path, err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			t.Fatalf("%s: %d", path, resp.StatusCode)
		}
	}
}

func TestViewerHeaders(t *testing.T) {
	_, srv := startRelay(t)
	resp, err := http.Get(srv.URL + "/viewer.js")
	if err != nil {
		t.Fatalf("viewer.js: %v", err)
	}
	resp.Body.Close()
	if got := resp.Header.Get("Cache-Control"); got != "no-cache" {
		t.Fatalf("Cache-Control = %q, want no-cache", got)
	}
	if got := resp.Header.Get("Content-Security-Policy"); got != viewerContentSecurityPolicy {
		t.Fatalf("Content-Security-Policy = %q, want %q",
			got, viewerContentSecurityPolicy)
	}
	if !strings.Contains(viewerContentSecurityPolicy, "img-src 'self' blob:;") {
		t.Fatalf("Content-Security-Policy blocks blob image previews: %q",
			viewerContentSecurityPolicy)
	}
}
