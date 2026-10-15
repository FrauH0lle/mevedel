---
name: "Separate feature enablement from its command setting"
description: "Use separate defcustoms for enabling a command-backed feature and configuring its command, with enablement off by default"
type: user
---

# Separate feature enablement from command configuration

User preference, 2026-10-15: an option enabling a feature and the option holding its command should be separate `defcustom`s. The on/off option should default to off.

Apply this when designing command-backed optional features. The motivating example was sleep inhibition: a boolean enablement option separate from the inhibitor command setting.
