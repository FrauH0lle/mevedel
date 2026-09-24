# ExcessNotificationGeneration from the permission notifier (2026-09-16)

Task: user reported `Notification error: (dbus-error
"org.freedesktop.Notifications.Error.ExcessNotificationGeneration"
"Created too many similar notifications in quick succession") [3 times]` in
*Messages*, suspected their `~/.emacs.d/site-lisp` config.

## Confirmed facts

- Emitter: the `mevedel-permission-notify-function` wrapper in
  `~/.dotfiles/hosts/yamato/home/##dot##emacs.d/site-lisp/config.el`
  (the only caller of `notifications-notify` in that config), which is
  invoked once per admitted card by `mevedel-permission--enqueue`
  (`mevedel-permission-queue.el:361`).
- Source of the error: KDE Plasma's notification server (plasmashell 6.7.5,
  `/usr/lib/libnotificationmanager.so.6.7.5`). Upstream
  `plasma-workspace/Plasma/6.7/libnotificationmanager/server_p.cpp` refuses a
  `Notify` call whose applicationName, summary, body, desktopEntry, eventId,
  actionNames, icon, and urls all equal those of the **last accepted**
  notification created under 1000 ms earlier, and replies with exactly this
  error. Refused calls do not update that "last accepted" record.
- `notifications-notify` catches the D-Bus error itself and reports it with
  `(message "Notification error: %S" err)`, returning nil. It therefore never
  signals, so mevedel's `with-demoted-errors` around the notifier call does
  not hide it. Verified with an unreachable session bus in batch Emacs.
- Corpus evidence: across all `*/permission-log.el` under `~/` (95 files,
  501 `permission-enqueued` events), 13 clusters of cards with an identical
  formatted body within the same second exist; one cluster of four identical
  `Glob` cards (2026-09-04T21:41:41) yields exactly the three refusals the
  user saw. Cluster bodies are always same-tool/same-argument parallel calls
  (`Glob` on one path, `Grep` on one path).

## Change made

User config only: the wrapper now keeps the last body sent plus its
`float-time` and skips an exact repeat inside 1.5 s (wider than the server's
1 s). Verified in batch Emacs by reading the real `(after! mevedel ...)` form
out of the edited file, evaluating it with a stubbed `notifications-notify`,
and checking call counts.

## Open lead

`docs/permissions.md` (~line 399) still shows wrappers without such a guard.
Any Plasma user pasting them hits the same silent refusal; mevedel's own doc
text ("No rate limiting is applied") is accurate for the harness but does not
warn about the desktop server's rule.
