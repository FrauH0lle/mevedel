package main

import (
	"context"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/sha256"
	"crypto/x509"
	"encoding/base64"
	"encoding/json"
	"encoding/pem"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/netip"
	"net/url"
	"os"
	"regexp"
	"time"
)

const (
	pushTimeout          = 10 * time.Second
	maxPushSubscriptions = 64
	maxControlBytes      = 4096
)

var guestIDPattern = regexp.MustCompile(`^[A-Za-z0-9_-]{8,64}$`)

var (
	publicIPv6Prefix      = netip.MustParsePrefix("2000::/3")
	nonPublicPushPrefixes = []netip.Prefix{
		netip.MustParsePrefix("0.0.0.0/8"),
		netip.MustParsePrefix("10.0.0.0/8"),
		netip.MustParsePrefix("100.64.0.0/10"),
		netip.MustParsePrefix("127.0.0.0/8"),
		netip.MustParsePrefix("169.254.0.0/16"),
		netip.MustParsePrefix("172.16.0.0/12"),
		netip.MustParsePrefix("192.0.0.0/24"),
		netip.MustParsePrefix("192.0.2.0/24"),
		netip.MustParsePrefix("192.31.196.0/24"),
		netip.MustParsePrefix("192.52.193.0/24"),
		netip.MustParsePrefix("192.88.99.0/24"),
		netip.MustParsePrefix("192.168.0.0/16"),
		netip.MustParsePrefix("192.175.48.0/24"),
		netip.MustParsePrefix("198.18.0.0/15"),
		netip.MustParsePrefix("198.51.100.0/24"),
		netip.MustParsePrefix("203.0.113.0/24"),
		netip.MustParsePrefix("224.0.0.0/4"),
		netip.MustParsePrefix("240.0.0.0/4"),
		netip.MustParsePrefix("2001::/23"),
		netip.MustParsePrefix("2001:db8::/32"),
		netip.MustParsePrefix("2002::/16"),
		netip.MustParsePrefix("2620:4f:8000::/48"),
		netip.MustParsePrefix("3ffe::/16"),
		netip.MustParsePrefix("3fff::/20"),
	}
)

type pushSubscription struct {
	endpoint string
	peer     uint32
	active   bool
}

type hostControl struct {
	Type     string   `json:"t"`
	Peer     uint32   `json:"peer"`
	GuestID  string   `json:"guestId"`
	GuestIDs []string `json:"guestIds"`
	Endpoint string   `json:"endpoint"`
	Active   bool     `json:"active"`
}

type pushSender interface {
	PublicKey() string
	Send(context.Context, string) (int, error)
}

type webPushSender struct {
	client    *http.Client
	key       *ecdsa.PrivateKey
	publicKey string
}

func newWebPushSender(client *http.Client) (*webPushSender, error) {
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return nil, err
	}
	return webPushSenderFromKey(client, key), nil
}

func webPushSenderFromKey(client *http.Client, key *ecdsa.PrivateKey) *webPushSender {
	public := elliptic.Marshal(elliptic.P256(), key.PublicKey.X, key.PublicKey.Y)
	return &webPushSender{
		client: client, key: key,
		publicKey: base64.RawURLEncoding.EncodeToString(public),
	}
}

func loadOrCreateWebPushSender(client *http.Client, path string) (*webPushSender, error) {
	data, err := os.ReadFile(path)
	if err == nil {
		block, rest := pem.Decode(data)
		if block == nil || block.Type != "EC PRIVATE KEY" || len(rest) != 0 {
			return nil, fmt.Errorf("invalid VAPID private key file")
		}
		key, err := x509.ParseECPrivateKey(block.Bytes)
		if err != nil || key.Curve != elliptic.P256() {
			return nil, fmt.Errorf("invalid VAPID P-256 private key")
		}
		return webPushSenderFromKey(client, key), nil
	}
	if !os.IsNotExist(err) {
		return nil, err
	}
	sender, err := newWebPushSender(client)
	if err != nil {
		return nil, err
	}
	der, err := x509.MarshalECPrivateKey(sender.key)
	if err != nil {
		return nil, err
	}
	file, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0600)
	if err != nil {
		if os.IsExist(err) {
			return loadOrCreateWebPushSender(client, path)
		}
		return nil, err
	}
	if _, err = file.Write(pem.EncodeToMemory(&pem.Block{
		Type: "EC PRIVATE KEY", Bytes: der,
	})); err == nil {
		err = file.Close()
	} else {
		file.Close()
	}
	if err != nil {
		return nil, err
	}
	return sender, nil
}

func (s *webPushSender) PublicKey() string { return s.publicKey }

func jwtPart(value any) (string, error) {
	encoded, err := json.Marshal(value)
	if err != nil {
		return "", err
	}
	return base64.RawURLEncoding.EncodeToString(encoded), nil
}

func (s *webPushSender) authorization(endpoint string) (string, error) {
	u, err := url.Parse(endpoint)
	if err != nil {
		return "", err
	}
	header, err := jwtPart(map[string]string{"typ": "JWT", "alg": "ES256"})
	if err != nil {
		return "", err
	}
	claims, err := jwtPart(map[string]any{
		"aud": u.Scheme + "://" + u.Host,
		"exp": time.Now().Add(12 * time.Hour).Unix(),
	})
	if err != nil {
		return "", err
	}
	unsigned := header + "." + claims
	digest := sha256.Sum256([]byte(unsigned))
	r, ss, err := ecdsa.Sign(rand.Reader, s.key, digest[:])
	if err != nil {
		return "", err
	}
	signature := make([]byte, 64)
	r.FillBytes(signature[:32])
	ss.FillBytes(signature[32:])
	return "vapid t=" + unsigned + "." +
		base64.RawURLEncoding.EncodeToString(signature) +
		", k=" + s.publicKey, nil
}

func (s *webPushSender) Send(ctx context.Context, endpoint string) (int, error) {
	authorization, err := s.authorization(endpoint)
	if err != nil {
		return 0, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, endpoint, nil)
	if err != nil {
		return 0, err
	}
	req.Header.Set("Authorization", authorization)
	req.Header.Set("TTL", "60")
	resp, err := s.client.Do(req)
	if err != nil {
		return 0, err
	}
	defer resp.Body.Close()
	io.Copy(io.Discard, io.LimitReader(resp.Body, 4096))
	return resp.StatusCode, nil
}

func publicPushIP(ip net.IP) bool {
	address, ok := netip.AddrFromSlice(ip)
	if !ok {
		return false
	}
	address = address.Unmap()
	if address.Is6() && !publicIPv6Prefix.Contains(address) {
		return false
	}
	for _, prefix := range nonPublicPushPrefixes {
		if prefix.Contains(address) {
			return false
		}
	}
	return true
}

func newPushHTTPClient() *http.Client {
	dialer := &net.Dialer{Timeout: 5 * time.Second, KeepAlive: 30 * time.Second}
	transport := &http.Transport{
		DialContext: func(ctx context.Context, network, address string) (net.Conn, error) {
			host, port, err := net.SplitHostPort(address)
			if err != nil {
				return nil, err
			}
			ips, err := net.DefaultResolver.LookupIPAddr(ctx, host)
			if err != nil {
				return nil, err
			}
			for _, candidate := range ips {
				ip := candidate.IP
				if publicPushIP(ip) {
					return dialer.DialContext(ctx, network,
						net.JoinHostPort(ip.String(), port))
				}
			}
			return nil, fmt.Errorf("push endpoint did not resolve publicly")
		},
		TLSHandshakeTimeout:   5 * time.Second,
		ResponseHeaderTimeout: 5 * time.Second,
	}
	return &http.Client{
		Transport: transport,
		Timeout:   pushTimeout,
		CheckRedirect: func(_ *http.Request, _ []*http.Request) error {
			return http.ErrUseLastResponse
		},
	}
}

func validPushEndpoint(endpoint string) bool {
	if len(endpoint) == 0 || len(endpoint) > 2048 {
		return false
	}
	u, err := url.Parse(endpoint)
	return err == nil && u.Scheme == "https" && u.Hostname() != "" &&
		u.User == nil && u.Fragment == "" && (u.Port() == "" || u.Port() == "443")
}

func (rl *relay) handleHostControl(id string, rm *room, data []byte) {
	if len(data) > maxControlBytes {
		return
	}
	var control hostControl
	if json.Unmarshal(data, &control) != nil {
		return
	}
	rl.mu.Lock()
	if rl.rooms[id] != rm {
		rl.mu.Unlock()
		return
	}
	switch control.Type {
	case "push-subscribe":
		_, live := rm.guests[control.Peer]
		if (control.Peer == 0 || live) &&
			guestIDPattern.MatchString(control.GuestID) &&
			validPushEndpoint(control.Endpoint) &&
			(len(rm.subscriptions) < maxPushSubscriptions ||
				rm.subscriptions[control.GuestID].endpoint != "") {
			rm.subscriptions[control.GuestID] = pushSubscription{
				endpoint: control.Endpoint, peer: control.Peer, active: control.Active,
			}
		}
		rl.mu.Unlock()
	case "push-state":
		if guestIDPattern.MatchString(control.GuestID) {
			if subscription, ok := rm.subscriptions[control.GuestID]; ok {
				subscription.active = control.Active
				subscription.peer = control.Peer
				rm.subscriptions[control.GuestID] = subscription
			}
		}
		rl.mu.Unlock()
	case "push-unsubscribe":
		if guestIDPattern.MatchString(control.GuestID) {
			delete(rm.subscriptions, control.GuestID)
		}
		rl.mu.Unlock()
	case "push":
		subscriptions := make(map[string]string, len(rm.subscriptions))
		if len(control.GuestIDs) == 0 {
			for guestID, subscription := range rm.subscriptions {
				if !subscription.active {
					subscriptions[guestID] = subscription.endpoint
				}
			}
		} else {
			for _, guestID := range control.GuestIDs {
				if guestIDPattern.MatchString(guestID) {
					if subscription := rm.subscriptions[guestID]; subscription.endpoint != "" && !subscription.active {
						subscriptions[guestID] = subscription.endpoint
					}
				}
			}
		}
		rl.mu.Unlock()
		for guestID, endpoint := range subscriptions {
			go rl.sendPush(id, rm, guestID, endpoint)
		}
	default:
		rl.mu.Unlock()
	}
}

func (rl *relay) sendPush(id string, rm *room, guestID, endpoint string) {
	ctx, cancel := context.WithTimeout(context.Background(), pushTimeout)
	defer cancel()
	status, err := rl.push.Send(ctx, endpoint)
	if err != nil || (status != http.StatusNotFound && status != http.StatusGone) {
		return
	}
	rl.mu.Lock()
	defer rl.mu.Unlock()
	if subscription := rm.subscriptions[guestID]; rl.rooms[id] == rm && subscription.endpoint == endpoint {
		delete(rm.subscriptions, guestID)
	}
}
