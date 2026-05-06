;;; test-mevedel-tool-web.el --- Tests for mevedel-tool-web.el -*- lexical-binding: t -*-

;;; Commentary:

;; Registration-only tests: the web tools now wrap gptel-agent's
;; upstream structs via :wrap, so the assertions focus on the wrap
;; metadata rather than on any local handler function.

;;; Code:

(require 'mevedel-tool-registry)
(require 'gptel-request)
(require 'gptel-agent-tools)
(require 'mevedel-view)
(require 'mevedel-tool-web)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Registration

(mevedel-deftest mevedel-tool-web--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers WebSearch as wrapped read-only web tool"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebSearch" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (memq 'web (mevedel-tool-groups tool)))
      (let ((arg-names (mapcar #'car (mevedel-tool-args tool))))
        (should (memq 'query arg-names)))))

  :doc "registers WebFetch with max-result-size"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebFetch" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (= 50000 (mevedel-tool-max-result-size tool)))))

  :doc "WebFetch :get-domain extracts host from :url"
  (progn
    (mevedel-tool-web--register)
    (let* ((tool (mevedel-tool-get "WebFetch" "mevedel-gptel-agent"))
           (fn (mevedel-tool-get-domain tool)))
      (should fn)
      (should (equal "example.com"
                     (funcall fn '(:url "https://example.com/path"))))
      (should-not (funcall fn '(:url "not-a-url")))))

  :doc "registers YouTube tool"
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "YouTube" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (= 50000 (mevedel-tool-max-result-size tool)))))

  :doc "YouTube :get-domain extracts host from :url"
  (progn
    (mevedel-tool-web--register)
    (let* ((tool (mevedel-tool-get "YouTube" "mevedel-gptel-agent"))
           (fn (mevedel-tool-get-domain tool)))
      (should fn)
      (should (equal "www.youtube.com"
                     (funcall fn '(:url "https://www.youtube.com/watch?v=xyz"))))))

  :doc "all three tools share the web group"
  (progn
    (mevedel-tool-web--register)
    (let ((web-tools (mevedel-tool-for-groups '(web))))
      (should (<= 3 (length web-tools)))
      (should (cl-every (lambda (tool) (mevedel-tool-read-only-p tool))
                        web-tools))))

  :doc "wrap leaves the upstream gptel-agent entries in place"
  (progn
    (mevedel-tool-web--register)
    (should (gptel-get-tool '("gptel-agent" "WebSearch")))
    (should (gptel-get-tool '("gptel-agent" "WebFetch")))
    (should (gptel-get-tool '("gptel-agent" "YouTube")))))


;;
;;; Renderers

(mevedel-deftest mevedel-tool-web--render-fetch ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-web--render-fetch
                 "WebFetch" '(:url "https://example.com/p") nil nil)))

  :doc "header extracts host from url; body-mode tracks data buffer"
  (let* ((body "Some fetched content\n")
         (plist (mevedel-tool-web--render-fetch
                 "WebFetch" '(:url "https://example.com/page") body nil)))
    (should (string-match-p "\\`WebFetch: example\\.com " (plist-get plist :header)))
    ;; No data buffer in this test → body-mode is nil (verbatim).
    (should (null (plist-get plist :body-mode))))

  :doc "body-mode tracks the data buffer's major mode when one is attached"
  (let ((data-buf (generate-new-buffer " *mev-test-fetch-data*"))
        (view-buf (generate-new-buffer " *mev-test-fetch-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf (org-mode))
          (with-current-buffer view-buf
            (setq-local mevedel--data-buffer data-buf)
            (let ((plist (mevedel-tool-web--render-fetch
                          "WebFetch" '(:url "https://example.com/")
                          "body\n" nil)))
              (should (eq 'org-mode (plist-get plist :body-mode))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "falls back to the url when host cannot be parsed"
  (let* ((body "content\n")
         (plist (mevedel-tool-web--render-fetch
                 "WebFetch" '(:url "not-a-url") body nil)))
    (should (string-match-p "WebFetch: " (plist-get plist :header)))))

(mevedel-deftest mevedel-tool-web--render-search ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-web--render-search
                 "WebSearch" '(:query "q") nil nil)))

  :doc "header includes the query and line count"
  (let* ((body "- r1\n- r2\n- r3\n")
         (plist (mevedel-tool-web--render-search
                 "WebSearch" '(:query "mevedel") body nil)))
    (should (string-match-p "\\`WebSearch: mevedel " (plist-get plist :header)))
    ;; No data buffer in this test → body-mode is nil.
    (should (null (plist-get plist :body-mode))))

  :doc "body-mode tracks the data buffer's major mode when one is attached"
  (let ((data-buf (generate-new-buffer " *mev-test-search-data*"))
        (view-buf (generate-new-buffer " *mev-test-search-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf (org-mode))
          (with-current-buffer view-buf
            (setq-local mevedel--data-buffer data-buf)
            (let ((plist (mevedel-tool-web--render-search
                          "WebSearch" '(:query "x") "- a\n- b\n" nil)))
              (should (eq 'org-mode (plist-get plist :body-mode))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(provide 'test-mevedel-tool-web)
;;; test-mevedel-tool-web.el ends here
