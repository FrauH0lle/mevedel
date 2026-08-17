package main

import (
	"context"
	"encoding/binary"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/coder/websocket"
)

const testRoom = "abcdefghij0123456789"

func startRelay(t *testing.T) (*relay, *httptest.Server) {
	t.Helper()
	rl := newRelay(24 * time.Hour)
	srv := httptest.NewServer(rl.mux())
	t.Cleanup(srv.Close)
	return rl, srv
}

func dial(t *testing.T, srv *httptest.Server, roomID, role string) *websocket.Conn {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	c, _, err := websocket.Dial(ctx, srv.URL+"/r/"+roomID+"?role="+role, nil)
	if err != nil {
		t.Fatalf("dial %s as %s: %v", roomID, role, err)
	}
	t.Cleanup(func() { c.CloseNow() })
	return c
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

func TestExpiredRoomSweep(t *testing.T) {
	rl, srv := startRelay(t)
	host := dial(t, srv, testRoom, "host")
	defer host.CloseNow()
	guest := dial(t, srv, testRoom, "guest")
	expectText(t, host, `{"t":"peer-joined","peer":1}`)
	// Everything is older than a cutoff in the future.
	rl.closeExpired(time.Now().Add(time.Hour))
	expectText(t, guest, `{"t":"room-closed"}`)
	expectClose(t, guest, closeRoomClosed)
	waitEmpty(t, rl)
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
}

func TestViewerNotCached(t *testing.T) {
	_, srv := startRelay(t)
	resp, err := http.Get(srv.URL + "/viewer.js")
	if err != nil {
		t.Fatalf("viewer.js: %v", err)
	}
	resp.Body.Close()
	if got := resp.Header.Get("Cache-Control"); got != "no-cache" {
		t.Fatalf("Cache-Control = %q, want no-cache", got)
	}
}
