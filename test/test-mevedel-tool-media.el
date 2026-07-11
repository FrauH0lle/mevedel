;;; test-mevedel-tool-media.el --- Tests for tool-result media -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-media)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-media-normalize-items ()
  ,test
  (test)
  :doc "accepts supported captured media and removes unknown keys"
  (should
   (equal '((:mime "image/png" :kind image :data "QUJD"
             :path "/tmp/a.png" :page 2))
          (mevedel-tool-media-normalize-items
           '((:mime "image/png" :kind image :data "QUJD"
              :path "/tmp/a.png" :page 2 :unknown secret)))))
  :doc "rejects unsupported or empty media collections"
  (should-not (mevedel-tool-media-normalize-items nil))
  (should-not
   (mevedel-tool-media-normalize-items
    '((:mime "text/plain" :kind document :data "QUJD"))))
  (should-not
   (mevedel-tool-media-normalize-items
    '((:mime "image/png" :kind image :data "")))))

(mevedel-deftest mevedel-tool-media-attach-result ()
  ,test
  (test)
  :doc "stores captured bytes behind an opaque transcript reference"
  (let* ((dir (make-temp-file "mevedel-tool-media-" t))
         (media '((:mime "image/png" :kind image :data "QUJD"
                   :path "/tmp/a.png")))
         (result (mevedel-tool-media-attach-result
                  "visible" media dir "toolu_1")))
    (unwind-protect
        (progn
          (should-not (string-search "QUJD" result))
          (should (string-search mevedel-tool-media--data-open result))
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
         (attached (mevedel-tool-media-attach-result
                    result '((:kind image)) nil "toolu_1")))
    (should (string-search "native media block attached" attached))
    (should-not (string-search "SECRET" attached))
    (should-not (string-search mevedel-tool-media--data-open attached))))

(mevedel-deftest mevedel-tool-media-extract ()
  ,test
  (test)
  :doc "restores persisted media after transcript properties are lost"
  (let* ((dir (make-temp-file "mevedel-tool-media-replay-" t))
         (media '((:mime "image/png" :kind image :data "QUJD")))
         (stored (substring-no-properties
                  (mevedel-tool-media-attach-result
                   "visible" media dir "toolu_1"))))
    (unwind-protect
        (let ((result (mevedel-tool-media-extract
                       stored dir "toolu_1")))
          (should (equal "visible" (car result)))
          (should (equal media (cdr result))))
      (delete-directory dir t)))
  :doc "does not trust a copied reference owned by another tool call"
  (let* ((media '((:mime "image/png" :kind image :data "QUJD")))
         (stored (mevedel-tool-media-attach-result
                  "visible" media nil "toolu_original"))
         (result (mevedel-tool-media-extract
                  stored nil "toolu_other")))
    (should (equal stored (car result)))
    (should-not (cdr result))))

(mevedel-deftest mevedel-tool-media-prepare-tool-result ()
  ,test
  (test)
  :doc "keeps provider-independent model text free of captured base64"
  (let* ((media '((:mime "image/png" :kind image :data "QUJD")))
         (raw (mevedel-tool-media-attach-result
               (concat "<media-file>\n"
                       "mime_type: image/png\n"
                       "encoding: base64\n"
                       "data:\nQUJD\n</media-file>")
               media nil "toolu_1"))
         (prepared
          (mevedel-tool-media-prepare-tool-result
           'unknown-backend
           (list :id "toolu_1" :name "Read" :result raw)
           nil)))
    (should (string-search "media omitted" (car prepared)))
    (should-not (string-search "QUJD" (car prepared)))
    (should-not (cdr prepared))))

(mevedel-deftest mevedel-tool-media--provider-blocks ()
  ,test
  (test)
  :doc "provider blocks use captured data without rereading the source path"
  (let ((item '(:path "/definitely/missing.png"
                :mime "image/png" :kind image :data "captured")))
    (should
     (equal "captured"
            (plist-get
             (plist-get (mevedel-tool-media--anthropic-media-block item)
                        :source)
             :data)))
    (should
     (equal "captured"
            (plist-get
             (plist-get
              (plist-get (mevedel-tool-media--bedrock-media-block item)
                         :image)
              :source)
             :bytes)))
    (should
     (equal "data:image/png;base64,captured"
            (plist-get
             (mevedel-tool-media--openai-responses-media-block item)
             :image_url)))
    (should
     (equal "data:image/png;base64,captured"
            (plist-get
             (plist-get (mevedel-tool-media--openai-media-block item)
                        :image_url)
             :url)))))

(provide 'test-mevedel-tool-media)
;;; test-mevedel-tool-media.el ends here
