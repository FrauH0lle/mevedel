---
name: "Verify the loaded runtime and dependency provenance"
description: "Distinguish loaded artifacts, cold-start behavior, relevant readiness, dependency provenance, and live event sequences"
type: feedback
---

# Verify the loaded runtime and dependency provenance

## Repository source is not necessarily running code

Captured environment observation, 2026-09-15: Emacs loaded separate compiled copies under `~/.emacs.d/straight/build/mevedel/`, while the build tree's `.el` files linked back to repository source. With `load-prefer-newer` nil, an older `.elc` still won after source edits.

When verifying a live change:
- Establish which artifact is actually loaded and test its behavior.
- Hot-loading edited source is provisional: it does not rebuild the installed artifact or survive a restart.
- Literal text searches and `symbol-file` alone do not establish behavior; macroexpansion can obscure source spellings.

## Live availability is not cold-start correctness

Observed 2026-09-16: a startup callback could be undefined before its owning module loaded, although an already-used inspecting session reported both `featurep` and `fboundp` true. Preloaded test fixtures hid the gap.

For loading, registration, and initialization changes, use a fresh process and exercise first use without preloading the owner. A later agent-spawn failure on 2026-10-14 provided another example: a declaration did not load the referenced handler, and a cold-load regression exposed the missing load boundary.

## Readiness probes must target the relevant initialization boundary

Observed 2026-10-20: an initial readiness probe timed out because it required `mevedel-session-codec-format-version` to be bound, although the daemon returned `:ready t`. Both restarted hosts reported `:codec-loaded nil :format nil`, yet returned `t` for `(and (featurep 'mevedel-collaboration-lobby) (fboundp 'mevedel-host-busy-p))`.

Reusable inference from those probes: when checking host initialization, do not require an unrelated, lazily loaded module as a readiness prerequisite. Choose checks tied to the capability being tested, without forcing incidental module loads. This does not excuse a missing required load or replace a first-use test of the codec itself; the successful probe established the checked initialization boundary, not all host behavior.

## Verify dependency provenance as well as controller provenance

Observed 2026-10-13: installed `gptel-request` and `gptel-openai-responses` sources differed from the upstream checkout used for review; `gptel-openai-oauth` matched. Explicit installed-controller checks with frozen dependencies whose source hashes matched the live installation passed 75/75 checks.

Reusable inference: in this locally patched environment, repository/Eask checks alone do not establish installed-runtime compatibility. Compare dependency source provenance and explicitly assert loaded controller and dependency paths when making that claim. A partner reported on 2026-10-14 that the installed source resolved to the user's fork at `/home/roland/gptel`; re-establish provenance for each investigation rather than assuming that path remains current.

## Native-function printing is not a cross-process identity check

Correction recorded 2026-10-14: printed native-compiled function objects contain process-specific identity. Hashing `(prin1-to-string (symbol-function ...))` is therefore not a valid cross-process identity comparison; a mismatch alone does not demonstrate different loaded behavior.

In that investigation, explicitly loading the live editor's native artifacts and checking `native-comp-function-p` and `subr-native-comp-unit` supported native-unit provenance. It did not establish equivalence with the user's live event sequence.

## Package-root and provenance checks need loaded-layout awareness

Earlier investigation, 2026-09-14–16, found that deriving a resource root from the compiled build directory could conflict with containment checks when resources were symlinked into the checkout. The reusable lesson is to distinguish wrong root selection from an invalid resource address rather than weakening a valid containment boundary.

Reloading source also need not reset an existing `defvar`. Separately, `locate-library` identifies what would load now, not necessarily the artifact already loaded; this was an inspected-code inference, not a runtime-tested provenance guarantee.
