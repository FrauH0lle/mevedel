---
name: "Keep user-facing customization at the main package boundary"
description: "Keybinding customization should work from the main package configuration without requiring submodule-specific setup."
type: user
---

# Keep user-facing customization at the main package boundary

**User statement, 2026-11-14** (`segment-0001.chat.org`, turn 4): user-facing keybinding customization should work from `use-package! mevedel :config`, rather than requiring the user to target an internal submodule.

The user also questioned an autoloaded `find-file-hook` because it removes user control over an important hook.

**How to apply:** when designing package loading, public keymaps, or feature activation, preserve a usable main-package customization boundary. Do not require users to discover internal module load order merely to customize advertised keybindings. Consider user control when proposing automatic global hook installation.

These statements do not select a particular activation mechanism, require eager loading of every optional module, or establish a blanket prohibition on autoloads. The proposed global mode, option-gated hook, and dir-locals alternatives were not settled in this evidence.
