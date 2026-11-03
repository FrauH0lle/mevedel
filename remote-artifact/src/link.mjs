// Artifact links: WEB-ORIGIN/#ROOM-ID.SECRET, where SECRET is the base64url
// room key (32 bytes, a view link) or the key and write token (48 bytes, an
// edit link).  The link names no artifact; the room's welcome does.

const NOT_ARTIFACT = 'This is not an artifact link. Share the artifact at its origin '
  + '(mevedel: M-x mevedel-collaboration-share-artifact) and attach the view or edit link it gives.';

/**
 * Parse artifact link TEXT.
 * Return { link, roomId, key, writeToken, tier, socketUrl }: KEY and
 * WRITE-TOKEN are Uint8Arrays (WRITE-TOKEN null for a view link), TIER is
 * "view" or "edit" and SOCKET-URL the relay endpoint a guest dials.
 * Throw when TEXT is not an artifact link; an owner link, which only
 * session rooms and lobbies have, is refused without connecting.
 * Backslashes are dropped: a link has none, and a terminal paste may
 * escape its # as \#.
 */
export function parseLink(text) {
  const link = typeof text === 'string' ? text.replaceAll('\\', '').trim() : '';
  const match = /^(https?):\/\/([^/#?\s]+)\/?#([A-Za-z0-9_-]{10,64})\.([A-Za-z0-9_-]+)$/.exec(link);
  if (!match) throw new Error(NOT_ARTIFACT);
  const [, scheme, host, roomId, encoded] = match;
  const secret = Buffer.from(encoded, 'base64url');
  if (secret.length === 64) {
    throw new Error('This is the owner link of a session or lobby, not an artifact link. '
      + 'Ask for the artifact\'s own link instead.');
  }
  if (secret.length !== 32 && secret.length !== 48) throw new Error(NOT_ARTIFACT);
  return {
    link,
    roomId,
    key: new Uint8Array(secret.subarray(0, 32)),
    writeToken: secret.length === 48 ? new Uint8Array(secret.subarray(32, 48)) : null,
    tier: secret.length === 48 ? 'edit' : 'view',
    socketUrl: `${scheme === 'https' ? 'wss' : 'ws'}://${host}/r/${roomId}?role=guest`,
  };
}
