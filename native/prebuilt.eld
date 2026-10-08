;; Prebuilt native animation modules attached to GitHub releases.
;; Written by .github/workflows/native.yml: one entry per source hash (the
;; first 16 hex digits of native/mevedel-view-native.c's SHA-256), mapping
;; each architecture to its module's SHA-256.  `mevedel-view-native-install'
;; loads a download only when it matches.
()
