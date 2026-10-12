;; Prebuilt native modules attached to GitHub releases.
;; Written by .github/workflows/native.yml: one entry per source hash (the
;; first 16 hex digits of the SHA-256 of native/*.c in byte order), mapping
;; each architecture to its module's SHA-256.
;; `mevedel-native-install' loads a download only when it matches.
(("927c5b47dac71ceb" ("aarch64" . "2dd6c7211e5a3ca0dd3a792366e2e653eaa1f9aa8dd781029e03d39279ca2e24") ("x86_64" . "fcb5c74b092b5302914dd581494cb0e3a423fc1801aff344c1801e9be36f8741")))
