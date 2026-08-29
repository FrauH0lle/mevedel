;;; test-mevedel-tool-web.el --- Tests for mevedel-tool-web.el -*- lexical-binding: t -*-

;;; Commentary:

;; WebSearch and WebFetch are both registered natively: WebSearch to own a
;; schema whose upstream `count' argument the upstream callback ignores,
;; WebFetch to own the response buffers of the upstream call.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-pipeline)
(require 'mevedel-tools)
(require 'gptel-request)
(require 'gptel-agent-tools)
(require 'mevedel-view)
(require 'mevedel-tool-web)
(require 'mevedel-view-render)
(require 'mevedel-view-segments)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-mevedel-tool-web--response-buffer (name &optional continuation)
  "Return a fake response buffer NAME retrieved for CONTINUATION.
Records CONTINUATION the way `url-retrieve' records its callback
arguments in a response buffer."
  (let ((buffer (generate-new-buffer name)))
    (when continuation
      (with-current-buffer buffer
        (setq-local url-callback-arguments (list continuation))))
    buffer))


;;
;;; Registration

(mevedel-deftest mevedel-tool-web--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers WebSearch natively without the inert count argument"
  ;; Upstream advertises `count' and its callback hardcodes five results,
  ;; so offering the argument teaches the model a lie.
  (progn
    (mevedel-tool-web--register)
    (let ((tool (mevedel-tool-get "WebSearch" "mevedel-gptel-agent")))
      (should tool)
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (memq 'web (mevedel-tool-groups tool)))
      (should (mevedel-tool-async-p tool))
      (let ((arg-names (mapcar #'car (mevedel-tool-args tool))))
        (should (memq 'query arg-names))
        (should-not (memq 'count arg-names)))))

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

  :doc "WebFetch :get-domain reads a YouTube host from :url"
  (progn
    (mevedel-tool-web--register)
    (let* ((tool (mevedel-tool-get "WebFetch" "mevedel-gptel-agent"))
           (fn (mevedel-tool-get-domain tool)))
      (should fn)
      (should (equal "www.youtube.com"
                     (funcall fn '(:url "https://www.youtube.com/watch?v=xyz"))))))

  :doc "both tools share the web group"
  (progn
    (mevedel-tool-web--register)
    (let ((web-tools (mevedel-tool-for-groups '(web))))
      (should (<= 2 (length web-tools)))
      (should (cl-every (lambda (tool) (mevedel-tool-read-only-p tool))
                        web-tools))))

  :doc "registration leaves the upstream gptel-agent entries in place"
  (progn
    (mevedel-tool-web--register)
    (should (gptel-get-tool '("gptel-agent" "WebSearch")))
    (should (gptel-get-tool '("gptel-agent" "WebFetch"))))

  :doc "re-registering web tools replaces existing wrappers"
  (progn
    (mevedel-tool-web--register)
    (let ((initial (mevedel-tool-get "WebSearch" "mevedel-gptel-agent")))
      (mevedel-tool-web--register)
      (let ((refreshed (mevedel-tool-get "WebSearch" "mevedel-gptel-agent")))
        (should refreshed)
        (should-not (eq initial refreshed))
        (should (mevedel-tool-get "WebFetch" "mevedel-gptel-agent"))))))


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


;;
;;; Response buffer ownership

(mevedel-deftest mevedel-tool-web--fetch ()
  ,test
  (test)
  :doc "releases only the response buffers its own retrievals created"
  (let* ((unrelated (test-mevedel-tool-web--response-buffer
                     " *test-fetch-unrelated*"))
         (foreign (test-mevedel-tool-web--response-buffer
                   " *test-fetch-foreign*" #'ignore))
         (owned nil)
         (result nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
                     (lambda ()
                       (lambda (continuation _url)
                         (setq owned
                               (list (test-mevedel-tool-web--response-buffer
                                      " *test-fetch-watch*" continuation)
                                     (test-mevedel-tool-web--response-buffer
                                      " *test-fetch-redirect*" continuation)
                                     (test-mevedel-tool-web--response-buffer
                                      " *test-fetch-caption*" continuation)))
                         (funcall continuation "transcript")))))
            (mevedel-tool-web--fetch
             (lambda (value) (setq result value))
             '(:url "https://www.youtube.com/watch?v=abc")))
          (should (equal '(:result "transcript") result))
          (should-not (cl-find-if #'buffer-live-p owned))
          (should (buffer-live-p foreign))
          (should (buffer-live-p unrelated)))
      (dolist (buffer (cons unrelated (cons foreign owned)))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))
  :doc "releases buffers when the call reports an error"
  (let ((owned nil)
        (result nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
                     (lambda ()
                       (lambda (continuation _url)
                         (setq owned
                               (test-mevedel-tool-web--response-buffer
                                " *test-fetch-failed*" continuation))
                         (funcall continuation
                                  "Error fetching page: failed")))))
            (mevedel-tool-web--fetch
             (lambda (value) (setq result value))
             '(:url "https://www.youtube.com/watch?v=abc")))
          (should (string-prefix-p "Error fetching page"
                                   (plist-get result :result)))
          (should-not (buffer-live-p owned)))
      (when (buffer-live-p owned) (kill-buffer owned))))
  :doc "settles once when the handler calls back and then signals"
  (let ((results nil))
    (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
               (lambda ()
                 (lambda (continuation _url)
                   (funcall continuation "first")
                   (error "Upstream failed after answering")))))
      (mevedel-tool-web--fetch
       (lambda (value) (push value results))
       '(:url "https://www.youtube.com/watch?v=abc")))
    (should (equal '((:result "first")) results)))
  :doc "reports a synchronous upstream failure as an error result"
  (let ((result nil))
    (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
               (lambda () (lambda (_only-one-argument) nil))))
      (mevedel-tool-web--fetch
       (lambda (value) (setq result value))
       '(:url "https://www.youtube.com/watch?v=abc")))
    (should (string-prefix-p "Error: " (plist-get result :result))))
  :doc "reports an unavailable upstream tool instead of calling nil"
  (let ((result nil))
    (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
               (lambda () nil)))
      (mevedel-tool-web--fetch
       (lambda (value) (setq result value))
       '(:url "https://www.youtube.com/watch?v=abc")))
    (should (string-match-p "unavailable" (plist-get result :result)))))

(mevedel-deftest mevedel-tool-web--fetch-function ()
  ,test
  (test)
  :doc "resolves the upstream handler only while it stays asynchronous"
  (let* ((tool (gptel-get-tool '("gptel-agent" "WebFetch")))
         (sync (and tool (gptel--copy-tool tool))))
    (should (functionp (mevedel-tool-web--fetch-function)))
    (setf (gptel-tool-async sync) nil)
    (cl-letf (((symbol-function 'gptel-get-tool) (lambda (_path) sync)))
      (should-not (mevedel-tool-web--fetch-function))))
  :doc "url-http stamps retrieval arguments into the response buffer"
  (let* ((server nil)
         (port nil)
         (continuation (lambda (&rest _) nil))
         (buffer nil))
    (unwind-protect
        (progn
          (setq server
                (make-network-process
                 :name "mevedel-web-test-server" :server t :host 'local
                 :service t :family 'ipv4 :noquery t
                 :filter
                 (lambda (proc _string)
                   (process-send-string
                    proc "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi")
                   (process-send-eof proc))))
          (setq port (plist-get (process-contact server t) :service))
          ;; Upstream passes its callback among the retrieval arguments,
          ;; which is what makes it identify its own response buffers.
          (setq buffer
                (url-retrieve (format "http://127.0.0.1:%s/" port)
                              #'ignore (list continuation) t))
          (should (buffer-live-p buffer))
          (should (with-current-buffer buffer
                    (memq continuation
                          (bound-and-true-p url-callback-arguments)))))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))
      (when (process-live-p server) (delete-process server)))))


(mevedel-deftest mevedel-tool-web--fetch/pipeline
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "the registered tool returns its result through the handler step"
  (let ((context nil))
    (mevedel-tool-web--register)
    (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
               (lambda () (lambda (callback _url) (funcall callback "shown")))))
      (mevedel-pipeline--step-handler
       (list :tool (mevedel-tool-get "WebFetch" "mevedel-gptel-agent")
             :args '(:url "https://www.youtube.com/watch?v=abc")
             :name "WebFetch")
       (lambda (value) (setq context value))
       #'ignore))
    (should (equal "shown" (plist-get context :result)))
    (should-not (eq 'error (plist-get context :status))))
  :doc "an unresolvable upstream handler settles as an error result"
  (let ((context nil))
    (mevedel-tool-web--register)
    (cl-letf (((symbol-function 'mevedel-tool-web--fetch-function)
               (lambda () nil)))
      (mevedel-pipeline--step-handler
       (list :tool (mevedel-tool-get "WebFetch" "mevedel-gptel-agent")
             :args '(:url "https://www.youtube.com/watch?v=abc")
             :name "WebFetch")
       (lambda (value) (setq context value))
       #'ignore))
    (should (string-match-p "unavailable" (plist-get context :result)))))


(provide 'test-mevedel-tool-web)
;;; test-mevedel-tool-web.el ends here
