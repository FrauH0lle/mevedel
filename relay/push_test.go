package main

import (
	"context"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"
)

func TestPublicPushIP(t *testing.T) {
	tests := []struct {
		address string
		public  bool
	}{
		{"1.1.1.1", true},
		{"100.128.0.1", true},
		{"198.20.0.1", true},
		{"::ffff:1.1.1.1", true},
		{"2001:200::1", true},
		{"2606:4700:4700::1111", true},
		{"2a00:1450:4001:81b::200e", true},
		{"0.0.0.0", false},
		{"10.0.0.1", false},
		{"100.64.0.1", false},
		{"100.127.255.254", false},
		{"127.0.0.1", false},
		{"169.254.1.1", false},
		{"172.16.0.1", false},
		{"192.0.0.9", false},
		{"192.0.2.1", false},
		{"192.31.196.1", false},
		{"192.52.193.1", false},
		{"192.88.99.1", false},
		{"192.168.1.1", false},
		{"192.175.48.1", false},
		{"198.18.0.1", false},
		{"198.51.100.1", false},
		{"203.0.113.1", false},
		{"224.0.0.1", false},
		{"240.0.0.1", false},
		{"255.255.255.255", false},
		{"::", false},
		{"::1", false},
		{"::ffff:127.0.0.1", false},
		{"64:ff9b::1", false},
		{"100::1", false},
		{"2001::1", false},
		{"2001:2::1", false},
		{"2001:db8::1", false},
		{"2002::1", false},
		{"2620:4f:8000::1", false},
		{"3ffe::1", false},
		{"3fff::1", false},
		{"5f00::1", false},
		{"fc00::1", false},
		{"fe80::1", false},
		{"ff00::1", false},
		{"4000::1", false},
	}
	for _, test := range tests {
		t.Run(test.address, func(t *testing.T) {
			if got := publicPushIP(net.ParseIP(test.address)); got != test.public {
				t.Fatalf("publicPushIP(%q) = %v, want %v",
					test.address, got, test.public)
			}
		})
	}
	if publicPushIP(nil) {
		t.Fatal("publicPushIP(nil) = true")
	}
}

func TestWebPushRequestIsContentBlind(t *testing.T) {
	requests := make(chan *http.Request, 1)
	server := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requests <- r.Clone(r.Context())
		w.WriteHeader(http.StatusCreated)
	}))
	defer server.Close()
	sender, err := newWebPushSender(server.Client())
	if err != nil {
		t.Fatalf("new push sender: %v", err)
	}
	status, err := sender.Send(context.Background(), server.URL+"/subscription")
	if err != nil || status != http.StatusCreated {
		t.Fatalf("send push: status %d, error %v", status, err)
	}
	request := <-requests
	if request.Method != http.MethodPost || request.Header.Get("TTL") == "" {
		t.Fatalf("push request: method %s, TTL %q", request.Method, request.Header.Get("TTL"))
	}
	if !strings.HasPrefix(request.Header.Get("Authorization"), "vapid t=") {
		t.Fatalf("Authorization = %q", request.Header.Get("Authorization"))
	}
	body, err := io.ReadAll(request.Body)
	if err != nil {
		t.Fatalf("read push body: %v", err)
	}
	if len(body) != 0 {
		t.Fatalf("push body leaked %d bytes", len(body))
	}
}

func TestPushHTTPClientRejectsRedirects(t *testing.T) {
	redirected := false
	target := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		redirected = true
	}))
	defer target.Close()
	source := httptest.NewServer(http.RedirectHandler(target.URL, http.StatusFound))
	defer source.Close()

	client := source.Client()
	client.CheckRedirect = newPushHTTPClient().CheckRedirect
	response, err := client.Get(source.URL)
	if err != nil {
		t.Fatalf("get redirect: %v", err)
	}
	response.Body.Close()
	if response.StatusCode != http.StatusFound {
		t.Fatalf("redirect status = %d, want %d", response.StatusCode, http.StatusFound)
	}
	if redirected {
		t.Fatal("redirect target was contacted")
	}
}

func TestVAPIDKeyPersists(t *testing.T) {
	path := filepath.Join(t.TempDir(), "vapid.pem")
	first, err := loadOrCreateWebPushSender(http.DefaultClient, path)
	if err != nil {
		t.Fatalf("create VAPID key: %v", err)
	}
	second, err := loadOrCreateWebPushSender(http.DefaultClient, path)
	if err != nil {
		t.Fatalf("reload VAPID key: %v", err)
	}
	if first.PublicKey() != second.PublicKey() {
		t.Fatalf("public key rotated: %q != %q",
			first.PublicKey(), second.PublicKey())
	}
}
