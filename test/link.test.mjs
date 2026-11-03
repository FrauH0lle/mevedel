import test from 'node:test';
import assert from 'node:assert/strict';
import { parseLink } from '../src/link.mjs';

const b64 = (n, fill = 1) => Buffer.alloc(n, fill).toString('base64url');

test('a 32-byte secret is a view link and a 48-byte one an edit link', () => {
  const view = parseLink(`https://relay.example/#room-id-0001.${b64(32)}`);
  assert.equal(view.tier, 'view');
  assert.equal(view.roomId, 'room-id-0001');
  assert.equal(view.key.length, 32);
  assert.equal(view.writeToken, null);
  assert.equal(view.socketUrl, 'wss://relay.example/r/room-id-0001?role=guest');
  const edit = parseLink(`http://127.0.0.1:8080/#room-id-0001.${b64(48)}`);
  assert.equal(edit.tier, 'edit');
  assert.equal(edit.writeToken.length, 16);
  assert.equal(edit.socketUrl, 'ws://127.0.0.1:8080/r/room-id-0001?role=guest');
});

test('shell escapes a terminal paste adds are ignored', () => {
  // zsh's url-quote-magic pastes the link's # and ? as \# and \?.
  const edit = parseLink(`https://relay.example/\\#room-id-0001.${b64(48)}`);
  assert.equal(edit.tier, 'edit');
  assert.equal(edit.roomId, 'room-id-0001');
  assert.equal(edit.link, `https://relay.example/#room-id-0001.${b64(48)}`);
});

test('an owner-tier secret or a malformed link is not an artifact link', () => {
  assert.throws(() => parseLink(`https://relay.example/#room-id-0001.${b64(64)}`),
    /owner link of a session or lobby, not an artifact link/);
  for (const bad of ['', 'not a link', 'https://relay.example/', 'https://relay.example/#nodot',
    `ftp://relay.example/#room-id-0001.${b64(32)}`, `https://relay.example/#room-id-0001.${b64(20)}`]) {
    assert.throws(() => parseLink(bad), /not an artifact link/, bad);
  }
});
