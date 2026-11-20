Wake this conversation later with a note for yourself, or list or cancel
pending wake-ups.

### When to use `Schedule`

- The user asks you to check, do or say something later: "check CI in 20
  minutes and tell me only if it failed", "remind me tomorrow at 10:00".

### When NOT to use `Schedule`

- Waiting for agents or running commands: WaitAgent, Monitor and background
  Bash completions already wake you.

### How to use `Schedule`

- `create` needs `when` and `note`. `when` is a duration (`20m`, `1h30m`,
  units d, h, m, s) or a local time `YYYY-MM-DD HH:MM`; every result reports
  the current local time as `now`. Write `note` for your future self: what to
  check and when to tell the user. `expires` optionally drops a wake-up that
  could not be delivered in time.
- When due, the note arrives as a `WAKE_UP` message with `scheduled_at`,
  `fired_at` and, when late, `overdue_by`; judge whether it still matters.
- The woken turn finishes without notifying the user; call `Notify` in it when
  the user should hear the result.
- A wake-up waits for a running turn and for the user's draft or pending
  decisions, and runs with the session's current permissions.
- `list` shows pending and expired wake-ups; `cancel` removes one by `id`.
  At most 50 may be pending.
