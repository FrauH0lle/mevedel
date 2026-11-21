Read account-wide subscription quotas as plain data.

### When to use `SubscriptionUsage`

- Check how much of a 5-hour or weekly subscription window is used, or when it
  resets, before planning quota-bound work.

### When NOT to use `SubscriptionUsage`

- Estimating this conversation's context size or token cost; quotas are
  account-wide and include activity outside niwa.

### How to use `SubscriptionUsage`

- Takes no arguments and waits for each provider (up to about 30 seconds).
- Returns a JSON array with one object per configured Codex OAuth or Claude
  Code backend: `backend` (its name), `current` (true for this session's
  backend), and either `error` (an actionable message) or `readings`.
- Each reading has `quota` (the quota's name), `window` (such as "5 hours" or
  "7 days"), `used_percent` and `resets`. Codex resets are ISO 8601 times;
  Claude resets are the adapter's own text. Unknown values are null.
- Readings never include accounts, plans, credits or credentials.
