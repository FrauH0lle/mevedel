Wake this conversation later with a note for yourself, or list or cancel
pending wake-ups.

### When to use `Schedule`

- The user asks you to check, do or say something later: "check CI in 20
  minutes and tell me only if it failed", "remind me tomorrow at 10:00",
  "babysit PR 412 every 20 minutes".

### When NOT to use `Schedule`

- Waiting for agents or running commands: WaitAgent, Monitor and background
  Bash completions already wake you.

### How to use `Schedule`

- `create` needs `when` and `note`. `when` is a duration (`20m`, `1h30m`,
  units d, h, m, s) or a local time `YYYY-MM-DD HH:MM` for one wake-up;
  `every 20m` (at least a minute) or 5-field cron in local time
  (`0 9 * * 1-5`; numbers, `*`, ranges, steps and lists) to repeat. Every
  result reports the current local time as `now`. Write `note` for your
  future self: what to check and when to tell the user. `expires` (a
  duration or local time) drops a wake-up that could not be delivered in
  time; repeating wake-ups expire after 7 days unless `expires` sets up to 30.
- Repeating wake-ups never catch up: runs missed while Emacs was closed or
  asleep arrive once, marked overdue.
- When due, the note arrives as a `WAKE_UP` message with `scheduled_at`,
  `fired_at` and, when late, `overdue_by`; judge whether it still matters.
- The woken turn finishes without notifying the user; call `Notify` in it when
  the user should hear the result.
- A wake-up waits for a running turn and for the user's draft or pending
  decisions, and runs with the session's current permissions.
- `list` shows pending and expired wake-ups; `cancel` removes one by `id`.
  At most 50 may be pending.
- A frequent check can run cheaper: pass `model` (a tier such as `fast`, or
  `BACKEND:MODEL`) and `effort` to `create`. Only the model and effort change;
  an unavailable model or unsupported effort is an error. Omit both for the
  session's own.
