Browser redesign verification — 2026-09-21, main agent

- User requested implementation of `.scratch/browser-redesign/`, then confirmed the relay had not been rebuilt.
- Current master already contains the redesign in `80b1eca`. Approved asset hashes match that commit except for a trailing blank line in `viewer-theme.css`; subsequent viewer changes add shared-editing availability controls.
- Rebuilt shared-editor bundles and `relay/mevedel-relay`. Git working tree remained clean.
- Current checks passed: viewer protocol, 21 shared-editing unit tests, 29 browser tests (including real relay/isolated Emacs), Go tests/vet, and diff whitespace check. Eask bytecode cleanup ran before tests.
- Running relay services were not restarted. Activation still requires deploying/restarting the rebuilt relay and reloading browser tabs; restart disconnects live rooms.
