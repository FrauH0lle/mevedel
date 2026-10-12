;; Prebuilt native modules attached to GitHub releases.
;; Written by .github/workflows/native.yml: one entry per source hash (the
;; first 16 hex digits of the SHA-256 of native/*.c in byte order), mapping
;; each architecture to its module's SHA-256.
;; `mevedel-native-install' loads a download only when it matches.
(("92d5ae8d323bc7c2" ("aarch64" . "70681cb1cdb0df12878f2b657148b3bf7f18c8d9391f6b2fd0a6e4a439e9459f") ("x86_64" . "fc428aa241b1faf7f061d7bf2f71c61c09d7e22a474a74789ecf2e8d7ac764ff")))
