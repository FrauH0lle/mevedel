;; Prebuilt native animation modules attached to GitHub releases.
;; Written by .github/workflows/native.yml: one entry per source hash (the
;; first 16 hex digits of native/mevedel-view-native.c's SHA-256), mapping
;; each architecture to its module's SHA-256.  `mevedel-view-native-install'
;; loads a download only when it matches.
(("e535157415e849e2" ("aarch64" . "9df15e6e7fc66f5aa5d775b1583fa0840b11e383db524537fd589bed13d7b6ce") ("x86_64" . "8f2c25eb59b57a9bfdffea906d106fd34cc4a1a3a36c3e647f7a7bee110b7aac")))
