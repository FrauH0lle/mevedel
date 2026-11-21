;;; test-niwa-tool-media.el --- Tests for tool-result media -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-tool-media)
(require 'niwa-execution-target)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-tool-media-normalize-items ()
  ,test
  (test)
  :doc "accepts supported captured media and removes unknown keys"
  (should
   (equal '((:mime "image/png" :kind image :data "QUJD"
             :path "/tmp/a.png" :page 2))
          (niwa-tool-media-normalize-items
           '((:mime "image/png" :kind image :data "QUJD"
              :path "/tmp/a.png" :page 2 :unknown secret)))))
  :doc "rejects unsupported or empty media collections"
  (should-not (niwa-tool-media-normalize-items nil))
  (should-not
   (niwa-tool-media-normalize-items
    '((:mime "text/plain" :kind document :data "QUJD"))))
  (should-not
   (niwa-tool-media-normalize-items
   '((:mime "image/png" :kind image :data ""))))
  :doc "retains only correctly typed metadata without mutating input"
  (let* ((item '(:mime "image/png" :kind image :data "QUJD"
                :path 42 :source "capture" :page 0 :unknown secret))
         (original (copy-tree item)))
    (should (equal '((:mime "image/png" :kind image :data "QUJD"
                     :source "capture" :page 0))
                   (niwa-tool-media-normalize-items (list item))))
    (should (equal original item)))
  :doc "rejects the whole collection when one item is invalid"
  (should-not
   (niwa-tool-media-normalize-items
    '((:mime "image/png" :kind image :data "QUJD")
      (:mime "image/png" :kind image :data "")))))

(niwa-deftest niwa-tool-media-reference-items ()
  ,test
  (test)
  :doc "preserves media metadata without retaining payload bytes"
  (let* ((items '((:mime "image/png" :kind image :data "QUJD"
                  :path "/tmp/a.png" :page 2)))
         (references (niwa-tool-media-reference-items items)))
    (should (equal '((:mime "image/png" :kind image
                      :path "/tmp/a.png" :page 2))
                   references))
    (should (equal "QUJD" (plist-get (car items) :data)))))

(niwa-deftest niwa-tool-media-attach-result
    (:vars ((niwa-tool-media--store nil)))
  ,test
  (test)
  :doc "stores captured bytes behind an opaque transcript reference"
  (let* ((dir (make-temp-file "niwa-tool-media-" t))
         (media '((:mime "image/png" :kind image :data "QUJD"
                   :path "/tmp/a.png")))
         (result (niwa-tool-media-attach-result
                  "visible" media dir "toolu_1")))
    (unwind-protect
        (progn
          (should-not (string-search "QUJD" result))
          (should (string-search niwa-tool-media--data-open result))
          (let* ((media-dir (file-name-concat dir "media"))
                 (file (car (directory-files media-dir t
                                             (rx "media-" (+ hex) ".el"))))
                 (read-eval nil)
                 (record (with-temp-buffer
                           (insert-file-contents-literally file)
                           (read (current-buffer)))))
            (should (equal 1 (plist-get record :version)))))
      (delete-directory dir t)))
  :doc "summarizes envelopes even when a malformed media record is rejected"
  (let* ((result (concat "<media-file>\n"
                         "encoding: base64\n"
                         "data:\nSECRET\n</media-file>"))
         (attached (niwa-tool-media-attach-result
                    result '((:kind image)) nil "toolu_1")))
    (should (string-search "native media block attached" attached))
    (should-not (string-search "SECRET" attached))
    (should-not (string-search niwa-tool-media--data-open attached)))
  :doc "publishes remote media records through session durability"
  (let* ((target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :name "remote-media"
                   :execution-target target
                   :working-directory "/ssh:user@host:/srv/project/"))
         (dir "/ssh:user@host:/srv/project/.niwa/sessions/s/tool-results")
         published)
    (cl-letf (((symbol-function
                'niwa-session-artifacts-publish-text)
               (lambda (actual-session path content &optional coding)
                 (setq published
                       (list actual-session path content coding)))))
      (niwa-tool-media-attach-result
       "visible"
       '((:mime "image/png" :kind image :data "QUJD"))
       dir "toolu_remote" session))
    (should (eq session (car published)))
    (should (string-prefix-p (concat dir "/media/media-")
                             (cadr published)))
    (should (string-search ":tool-use-id \"toolu_remote\""
                           (nth 2 published)))
    (should (eq 'utf-8-unix (nth 3 published))))
  :doc "bounds retained payload bytes and replays evicted durable records"
  (let* ((dir (make-temp-file "niwa-tool-media-bound-" t))
         (niwa-tool-media-cache-max-bytes 16)
         (first-media '((:mime "image/png" :kind image :data "QUJDQUJD")))
         (second-media '((:mime "image/png" :kind image :data "WFlaWFla")))
         (third-media '((:mime "image/png" :kind image :data "MTIzNDU2")
                        (:mime "image/png" :kind image :data "Nzg5MDEy")))
         (first (substring-no-properties
                 (niwa-tool-media-attach-result
                  "one" first-media dir "toolu_1"))))
    (unwind-protect
        (progn
          (niwa-tool-media-attach-result
           "two" second-media dir "toolu_2")
          (should (= 2 (length niwa-tool-media--store)))
          (niwa-tool-media-attach-result
           "three" third-media dir "toolu_3")
          (should (= 1 (length niwa-tool-media--store)))
          (should (equal first-media
                         (cdr (niwa-tool-media-extract
                               first dir "toolu_1")))))
      (delete-directory dir t)))
  :doc "keeps a record whose payload alone exceeds the bound"
  (let* ((niwa-tool-media-cache-max-bytes 4)
         (media '((:mime "image/png" :kind image :data "QUJDQUJD")))
         (stored (substring-no-properties
                  (niwa-tool-media-attach-result
                   "one" media nil "toolu_1"))))
    (should (= 1 (length niwa-tool-media--store)))
    (should (equal media
                   (cdr (niwa-tool-media-extract stored nil "toolu_1")))))
  :doc "drops evicted ephemeral media that has no durable record"
  (let* ((niwa-tool-media-cache-max-bytes 8)
         (first (substring-no-properties
                 (niwa-tool-media-attach-result
                  "one" '((:mime "image/png" :kind image :data "QUJDQUJD"))
                  nil "toolu_1"))))
    (niwa-tool-media-attach-result
     "two" '((:mime "image/png" :kind image :data "WFlaWFla")) nil "toolu_2")
    (should (= 1 (length niwa-tool-media--store)))
    (should-not (cdr (niwa-tool-media-extract first nil "toolu_1")))))

(niwa-deftest niwa-tool-media-strip-blocks
    (:vars ((niwa-tool-media--store nil)))
  ,test
  (test)
  :doc "returns the argument itself when nothing is stripped"
  ;; This runs per transcript segment during live rendering; the no-op
  ;; path must not copy the string.
  (let ((plain "just some tool output\nwith lines"))
    (should (eq plain (niwa-tool-media-strip-blocks plain)))
    (should (eq plain (niwa-tool-media-strip-blocks plain "toolu_1"))))
  :doc "still strips an owned propertized block"
  (let ((stored (niwa-tool-media-attach-result
                 "visible" '((:mime "image/png" :kind image :data "QUJD"))
                 nil "toolu_strip")))
    (should (equal "visible"
                   (niwa-tool-media-strip-blocks stored)))))

(niwa-deftest niwa-tool-media-extract
    (:vars ((niwa-tool-media--store nil)))
  ,test
  (test)
  :doc "restores persisted media after transcript properties are lost"
  (let* ((dir (make-temp-file "niwa-tool-media-replay-" t))
         (media '((:mime "image/png" :kind image :data "QUJD")))
         (stored (substring-no-properties
                  (niwa-tool-media-attach-result
                   "visible" media dir "toolu_1"))))
    (unwind-protect
        (let ((result (niwa-tool-media-extract
                       stored dir "toolu_1")))
          (should (equal "visible" (car result)))
          (should (equal media (cdr result))))
      (delete-directory dir t)))
  :doc "does not trust a copied reference owned by another tool call"
  (let* ((media '((:mime "image/png" :kind image :data "QUJD")))
         (stored (niwa-tool-media-attach-result
                  "visible" media nil "toolu_original"))
         (result (niwa-tool-media-extract
                  stored nil "toolu_other")))
    (should (equal stored (car result)))
    (should-not (cdr result)))
  :doc "cold remote replay reads the committed record instead of its fixed cache"
  (let* ((host "media-replay")
         (local-store (make-temp-file "niwa-media-source-" t))
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-media-remote-" t)))
         (remote-root (format "/niwamock:%s:%s" host local-root))
         (save-path (concat remote-root "session/"))
         (tool-results (file-name-concat save-path "tool-results"))
         (media '((:mime "image/png" :kind image :data "QUJD")))
         (stored (substring-no-properties
                  (niwa-tool-media-attach-result
                   "visible" media local-store "toolu_remote_replay")))
         (source (car (directory-files
                       (file-name-concat local-store "media") t
                       (rx "media-" (+ hex) ".el"))))
         (name (file-name-nondirectory source))
         (id (substring name 6 -3))
         (logical (file-name-concat "tool-results" "media" name))
         (fixed (file-name-concat save-path logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally source)
                  (buffer-string)))
         (target (niwa-execution-target-create remote-root))
         (session (niwa-session--create
                   :authority-mode 'portable
                   :name "remote-media" :execution-target target
                   :save-path save-path)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "(:version 1 :items nil)" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil published nil 'silent))
          (setf (niwa-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil
                      :artifacts
                      (list (list logical :published published
                                  :sha256 (secure-hash 'sha256 bytes)))))
          (setq niwa-tool-media--store nil)
          (should (equal logical (file-relative-name fixed save-path)))
          (should
           (equal bytes
                  (niwa-session-artifacts-read-artifact
                   session logical)))
          (let ((record (with-temp-buffer
                          (insert bytes)
                          (goto-char (point-min))
                          (read (current-buffer)))))
            (should (equal id (plist-get record :id)))
            (should (equal "toolu_remote_replay"
                           (plist-get record :tool-use-id)))
            (should (equal media (plist-get record :items))))
          (should
           (equal media
                  (niwa-tool-media--read-media-store-record
                   id tool-results "toolu_remote_replay" session)))
          (let ((result
                 (niwa-tool-media-extract
                  stored tool-results "toolu_remote_replay" nil session)))
            (should (equal "visible" (car result)))
            (should (equal media (cdr result)))))
      (when (file-directory-p local-store)
        (delete-directory local-store t))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

(niwa-deftest niwa-tool-media-prepare-tool-result
    (:vars ((niwa-tool-media--store nil) (raw nil))
     :before-each
     (setq raw (niwa-tool-media-attach-result
                (concat "<media-file>\n"
                        "mime_type: image/png\n"
                        "encoding: base64\n"
                        "data:\nQUJD\n</media-file>")
                '((:mime "image/png" :kind image :data "QUJD"))
                nil "toolu_original")))
  ,test
  (test)
  :doc "keeps provider-independent model text free of captured base64"
  (let* ((prepared
          (niwa-tool-media-prepare-tool-result
           'unknown-backend
           (list :id "toolu_original" :name "Read" :result raw)
           nil)))
    (should (string-search "media omitted" (car prepared)))
    (should-not (string-search "QUJD" (car prepared)))
    (should-not (cdr prepared)))

  :doc "shared content media uses the same provider boundary and reports missing vision"
  (let* ((media '((:mime "image/png" :kind image :data "QUJD")))
         (raw (niwa-tool-media-attach-result "{\"kind\":\"whiteboard\"}"
                                                media nil "shared-edit"))
         (prepared (niwa-tool-media-prepare-tool-result
                    'unknown-backend
                    (list :id "shared-edit" :name "SharedEdit" :result raw) nil)))
    (should (string-search "media omitted" (car prepared)))
    (should-not (string-search "QUJD" (car prepared)))
    (should-not (cdr prepared)))

  :doc "an owned unresolvable reference is stripped with an honest note"
  ;; The store is empty and no durable record exists, so the reference
  ;; cannot resolve.  The model used to keep both the internal block and
  ;; the now-false "native media block attached" note.
  (let* ((niwa-tool-media--store nil)
         (prepared
          (niwa-tool-media-prepare-tool-result
           'unknown-backend
           (list :id "toolu_original" :name "Read" :result raw)
           nil)))
    (should-not (string-search "niwa-media-data" (car prepared)))
    (should (string-search "<media no longer available>" (car prepared)))
    (should-not (string-search "native media block attached" (car prepared)))
    (should-not (cdr prepared)))

  :doc "a foreign-id reference block stays literal even when unresolvable"
  (let* ((niwa-tool-media--store nil)
         (prepared
          (niwa-tool-media-prepare-tool-result
           'unknown-backend
           (list :id "toolu_other" :name "Read" :result raw)
           nil)))
    (should (string-search "niwa-media-data" (car prepared)))
    (should-not (cdr prepared))))

(niwa-deftest niwa-tool-media--provider-blocks ()
  ,test
  (test)
  :doc "provider blocks use captured data without rereading the source path"
  (let ((item '(:path "/definitely/missing.png"
                :mime "image/png" :kind image :data "captured")))
    (should
     (equal "captured"
            (plist-get
             (plist-get (niwa-tool-media--anthropic-media-block item)
                        :source)
             :data)))
    (should
     (equal "captured"
            (plist-get
             (plist-get
              (plist-get (niwa-tool-media--bedrock-media-block item)
                         :image)
              :source)
             :bytes)))
    (should
     (equal "data:image/png;base64,captured"
            (plist-get
             (niwa-tool-media--openai-responses-media-block item)
             :image_url)))
    (should
     (equal "data:image/png;base64,captured"
            (plist-get
             (plist-get (niwa-tool-media--openai-media-block item)
                        :image_url)
             :url)))))

(provide 'test-niwa-tool-media)
;;; test-niwa-tool-media.el ends here
