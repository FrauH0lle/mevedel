;;; test-mevedel-tool-web.el --- Tests for mevedel-tool-web.el -*- lexical-binding: t -*-

;;; Commentary:

;; Native web tools, real HTTP retrieval, redirects, timeouts and cleanup.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-pipeline)
(require 'mevedel-tools)
(require 'gptel-request)
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
;;; Registration

(mevedel-deftest mevedel-tool-web--register
  (:before-each (progn (mevedel-tool-clear-registry)
                       (mevedel-tool-web--register))
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers WebSearch natively without the inert count argument"
  ;; Upstream advertises `count' and its callback hardcodes five results,
  ;; so offering the argument teaches the model a lie.
  (let ((tool (mevedel-tool-get "WebSearch" "mevedel-web")))
    (should tool)
    (should (eq t (mevedel-tool-read-only-p tool)))
    (should (memq 'web (mevedel-tool-groups tool)))
    (should (mevedel-tool-async-p tool))
    (let ((arg-names (mapcar #'car (mevedel-tool-args tool))))
      (should (memq 'query arg-names))
      (should-not (memq 'count arg-names))))

  :doc "registers WebFetch with max-result-size"
  (let ((tool (mevedel-tool-get "WebFetch" "mevedel-web")))
    (should tool)
    (should (eq t (mevedel-tool-read-only-p tool)))
    (should (= 50000 (mevedel-tool-max-result-size tool))))

  :doc "WebFetch :get-domain extracts ordinary and YouTube hosts and rejects invalid URLs"
  (let ((fn (mevedel-tool-get-domain
             (mevedel-tool-get "WebFetch" "mevedel-web"))))
    (should fn)
    (should (equal "example.com"
                   (funcall fn '(:url "https://example.com/path"))))
    (should (equal "www.youtube.com"
                   (funcall fn '(:url "https://www.youtube.com/watch?v=xyz"))))
    (should-not (funcall fn '(:url "not-a-url"))))

  :doc "both tools share the web group"
  (let ((web-tools (mevedel-tool-for-groups '(web))))
    (should (<= 2 (length web-tools)))
    (should (cl-every (lambda (tool) (mevedel-tool-read-only-p tool))
                      web-tools)))

  :doc "re-registering web tools replaces existing wrappers"
  (let ((initial (mevedel-tool-get "WebSearch" "mevedel-web")))
    (mevedel-tool-web--register)
    (let ((refreshed (mevedel-tool-get "WebSearch" "mevedel-web")))
      (should refreshed)
      (should-not (eq initial refreshed))
      (should (mevedel-tool-get "WebFetch" "mevedel-web")))))


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
  (with-temp-buffer
    (org-mode)
    (let ((data-buf (current-buffer)))
      (with-temp-buffer
        (setq-local mevedel--data-buffer data-buf)
        (let ((plist (mevedel-tool-web--render-fetch
                      "WebFetch" '(:url "https://example.com/") "body\n" nil)))
          (should (eq 'org-mode (plist-get plist :body-mode)))))))

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
  (with-temp-buffer
    (org-mode)
    (let ((data-buf (current-buffer)))
      (with-temp-buffer
        (setq-local mevedel--data-buffer data-buf)
        (let ((plist (mevedel-tool-web--render-search
                      "WebSearch" '(:query "x") "- a\n- b\n" nil)))
          (should (eq 'org-mode (plist-get plist :body-mode))))))))


;;
;;; HTTP behavior through the tool handler boundary

(defun test-mevedel-tool-web--call (name args)
  "Run registered web tool NAME with ARGS and await its handler result."
  (let (results)
    (mevedel-pipeline--step-handler
     (list :tool (mevedel-tool-get name "mevedel-web") :args args :name name)
     (lambda (result) (push result results))
     (lambda (error) (ert-fail error)))
    (let ((deadline (+ (float-time) 4)))
      (while (and (not results) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should (= 1 (length results)))
    (car results)))

(mevedel-deftest mevedel-tool-web--fetch
  (:before-each (mevedel-tool-web--register))
  ,test
  (test)
  :doc "retrieves HTML through a real redirect and releases only owned buffers"
  (let ((before (buffer-list))
        (foreign (generate-new-buffer " *foreign-web-response*")))
    (unwind-protect
        (mevedel-test-http
         (lambda (request)
           (if (string-match-p " /redirect " request)
               '("302 Found" "Location: /page\r\n" "")
             '("200 OK" "Content-Type: text/html; charset=utf-8\r\n"
               "<html><body><p>Readable page text.</p></body></html>")))
         (lambda (base)
           (let ((result (test-mevedel-tool-web--call
                          "WebFetch" (list :url (concat base "/redirect")))))
             (should (string-match-p "Readable page text" (plist-get result :result)))
             (should (eq 'success (plist-get result :handler-status))))
           (should (buffer-live-p foreign))
           (dolist (buffer (buffer-list))
             (unless (memq buffer before)
               (should-not (with-current-buffer buffer
                             (bound-and-true-p url-callback-arguments)))))))
      (kill-buffer foreign)))

  :doc "HTTP failure becomes a canonical tool error"
  (mevedel-test-http
   (lambda (_) '("500 Internal Server Error" "" "failed"))
   (lambda (base)
     (let ((result (test-mevedel-tool-web--call "WebFetch" (list :url base))))
       (should (eq 'error (plist-get result :handler-status)))
       (should (string-prefix-p "Error:" (plist-get result :result))))))

  :doc "a redirected request that never answers times out and releases buffers"
  (let ((mevedel-tool-web--timeout 0.1) (before (buffer-list)))
    (mevedel-test-http
     (lambda (request)
       (when (string-match-p " /redirect " request)
         '("302 Found" "Location: /hang\r\n" "")))
     (lambda (base)
       (let ((result (test-mevedel-tool-web--call
                      "WebFetch" (list :url (concat base "/redirect")))))
         (should (eq 'error (plist-get result :handler-status)))
         (should (string-match-p "timed out" (plist-get result :result))))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (should-not (with-current-buffer buffer
                         (bound-and-true-p url-callback-arguments)))))))))

(mevedel-deftest mevedel-tool-web--retrieve ()
  ,test
  (test)
  :doc "parse errors settle once and a late callback cannot reenter parsing"
  (let (saved-callback saved-args results timer)
    (mevedel-test-http
     (lambda (_) '("200 OK" "" "body"))
     (lambda (base)
       (let ((retrieve (symbol-function 'url-retrieve))
             (schedule (symbol-function 'run-at-time)))
         (cl-letf (((symbol-function 'url-retrieve)
                    (lambda (url callback args &rest options)
                      (setq saved-callback callback saved-args args)
                      (apply retrieve url callback args options)))
                   ((symbol-function 'run-at-time)
                    (lambda (&rest args)
                      (setq timer (apply schedule args)))))
           (mevedel-tool-web--retrieve
            base (lambda () (error "Malformed body"))
            (lambda (value error) (push (list value error) results))))
         (let ((deadline (+ (float-time) 4)))
           (while (and (not results) (< (float-time) deadline))
             (accept-process-output nil 0.01)))
         (should (equal '((nil "Malformed body")) results))
         (should-not (memq timer timer-list))
         (with-temp-buffer (apply saved-callback nil saved-args))
         (should (= 1 (length results)))))))

  :doc "synchronous transport failure settles once and cancels its timer"
  (let (results timer)
    (let ((schedule (symbol-function 'run-at-time)))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (error "Transport failure")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest args) (setq timer (apply schedule args)))))
        (mevedel-tool-web--retrieve
         "https://example.invalid/" #'ignore
         (lambda (value error) (push (list value error) results)))))
    (should (equal '((nil "Transport failure")) results))
    (should-not (memq timer timer-list))))

(mevedel-deftest mevedel-tool-web--websearch
  (:before-each (mevedel-tool-web--register))
  ,test
  (test)
  :doc "configured EWW search returns five links and excerpts through the pipeline"
  (mevedel-test-http
   (lambda (request)
     (should (string-search "/search?q=two%20words" request))
     (list "200 OK" "Content-Type: text/html\r\n"
           (concat "<html><body>"
                   (mapconcat (lambda (n)
                                (format "<p><a href=\"https://example.com/%d\">Title %d</a> Excerpt %d</p>" n n n))
                              '(1 2 3 4 5 6) "")
                   "</body></html>")))
   (lambda (base)
     (let* ((eww-search-prefix (concat base "/search?q="))
            (result (test-mevedel-tool-web--call "WebSearch" '(:query "two words"))))
       (should (string-match-p "https://example.com/1" (plist-get result :result)))
       (should (string-match-p "Excerpt 5" (plist-get result :result)))
       (should-not (string-match-p "example.com/6" (plist-get result :result)))
       (should (zerop mevedel-tool-web--search-active))
       (should-not mevedel-tool-web--search-queue)))))

(mevedel-deftest mevedel-tool-web--start-searches ()
  ,test
  (test)
  :doc "queued searches drain after timeouts with at most two active"
  (let ((mevedel-tool-web--search-active 0)
        (mevedel-tool-web--search-queue nil)
        (mevedel-tool-web--timeout 0.1)
        results requests)
    (mevedel-test-http
     (lambda (request) (push request requests) nil)
     (lambda (base)
       (let ((eww-search-prefix (concat base "/?q=")))
         (dotimes (_ 3)
           (mevedel-tool-web--websearch
            (lambda (result) (push result results)) '(:query "hang")))
         (setq eww-search-prefix (concat base "/wrong?q="))
         (should (= 2 mevedel-tool-web--search-active))
         (should (= 1 (length mevedel-tool-web--search-queue)))
         (let ((deadline (+ (float-time) 4)))
           (while (and (< (length results) 3) (< (float-time) deadline))
             (accept-process-output nil 0.01)))
         (should (= 3 (length results)))
         (should (= 3 (length requests)))
         (should (cl-every (lambda (request)
                             (string-prefix-p "GET /?q=hang " request)) requests))
         (should (cl-every (lambda (result) (eq 'error (plist-get result :status))) results))
         (should (zerop mevedel-tool-web--search-active))
         (should-not mevedel-tool-web--search-queue))))))

(mevedel-deftest mevedel-tool-web--yt-fetch
  (:before-each (mevedel-tool-web--register))
  ,test
  (test)
  :doc "all YouTube stages use HTTP ownership and render description with timestamps"
  (let ((before (buffer-list)) requests)
    (mevedel-test-http
     (lambda (request)
       (push request requests)
       (cond
        ((string-prefix-p "GET /watch?" request)
         '("302 Found" "Location: /watch-page\r\n" ""))
        ((string-match-p "GET /watch-page" request)
         '("200 OK" "" "{\"INNERTUBE_API_KEY\":\"key\"}"))
        ((string-match-p "POST /youtubei/" request)
         '("200 OK" "Content-Type: application/json\r\n"
           "{\"videoDetails\":{\"shortDescription\":\"Video description\"},\"captions\":{\"playerCaptionsTracklistRenderer\":{\"captionTracks\":[{\"languageCode\":\"en\",\"baseUrl\":\"https://youtube.com/captions\"}]}}}"))
        ((string-match-p "GET /captions" request)
         '("200 OK" "" "<transcript><text start=\"0\">First line</text><text start=\"32\">Second line</text></transcript>"))
        (t '("404 Not Found" "" "missing"))))
     (lambda (base)
       (let ((retrieve (symbol-function 'url-retrieve)))
         (cl-letf (((symbol-function 'url-retrieve)
                    (lambda (url &rest args)
                      (apply retrieve
                             (if (string-prefix-p base url) url
                               (concat base (url-filename (url-generic-parse-url url)))) args))))
           (let ((result (test-mevedel-tool-web--call
                          "WebFetch" '(:url "https://youtube.com/watch?v=abc"))))
             (should (eq 'success (plist-get result :handler-status)))
             (should (string-match-p "Video description" (plist-get result :result)))
             (should (string-match-p "\\[0:32\\]" (plist-get result :result)))
             (should (string-match-p "Second line" (plist-get result :result))))))
       (should (= 4 (length requests)))
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (should-not (with-current-buffer buffer
                         (bound-and-true-p url-callback-arguments)))))))))

(mevedel-deftest mevedel-tool-web--yt-video-id ()
  ,test
  (test)
  :doc "recognizes short and watch URLs and ignores ordinary pages"
  (should (equal "abc" (mevedel-tool-web--yt-video-id "https://youtu.be/abc?t=1")))
  (should (equal "abc" (mevedel-tool-web--yt-video-id "https://www.youtube.com/watch?v=abc&x=1")))
  (should-not (mevedel-tool-web--yt-video-id "https://example.com/abc")))

(mevedel-deftest mevedel-tool-web--yt-captions ()
  ,test
  (test)
  :doc "missing captions retain the video description"
  (let (result)
    (mevedel-tool-web--yt-captions
     (lambda (value error) (should-not error) (setq result value))
     '(:videoDetails (:shortDescription "description")))
    (should (string-match-p "description" result))
    (should (string-match-p "No transcript available" result))))



(mevedel-deftest mevedel-tool-web--yt-fetch/failures
  (:before-each (mevedel-tool-web--register))
  ,test
  (test)
  :doc "each YouTube stage settles errors, malformed replies and timeouts without leaks"
  (dolist (stage '(watch metadata captions))
    (dolist (failure '(http malformed timeout))
      (let ((mevedel-tool-web--timeout 0.1)
            (before (buffer-list))
            (timers-before (copy-sequence timer-list))
            (requests 0))
        (ert-info ((format "%s/%s" stage failure))
          (mevedel-test-http
           (lambda (request)
             (cl-incf requests)
             (let ((current (cond ((string-prefix-p "GET /watch?" request) 'watch)
                                  ((string-prefix-p "POST /youtubei/" request) 'metadata)
                                  (t 'captions))))
               (if (eq current stage)
                   (pcase failure
                     ('http '("500 Internal Server Error" "" "failed"))
                     ('malformed '("200 OK" "" "not valid content"))
                     ('timeout nil))
                 (pcase current
                   ('watch '("200 OK" "" "{\"INNERTUBE_API_KEY\":\"key\"}"))
                   ('metadata
                    '("200 OK" ""
                      "{\"videoDetails\":{\"shortDescription\":\"Saved description\"},\"captions\":{\"playerCaptionsTracklistRenderer\":{\"captionTracks\":[{\"languageCode\":\"en\",\"baseUrl\":\"https://youtube.com/captions\"}]}}}"))
                   (_ '("200 OK" "" "<transcript/>"))))))
           (lambda (base)
             (let ((retrieve (symbol-function 'url-retrieve)))
               (cl-letf (((symbol-function 'url-retrieve)
                          (lambda (url &rest args)
                            (apply retrieve (concat base (url-filename (url-generic-parse-url url))) args))))
                 (let ((result (test-mevedel-tool-web--call
                                "WebFetch" '(:url "https://youtube.com/watch?v=abc"))))
                   (should (eq 'error (plist-get result :handler-status)))
                   (should (string-match-p "Error" (plist-get result :result)))
                   (when (eq stage 'captions)
                     (should (string-match-p "Saved description" (plist-get result :result)))))))
             (should (= requests (pcase stage ('watch 1) ('metadata 2) (_ 3))))
             (dolist (buffer (buffer-list))
               (unless (memq buffer before)
                 (should-not (with-current-buffer buffer
                               (bound-and-true-p url-callback-arguments)))))
             (dolist (timer timer-list)
               (should (memq timer timers-before))))))))))

(mevedel-deftest mevedel-tool-web--yt-parse-captions ()
  ,test
  (test)
  :doc "decodes caption entities before formatting timestamps"
  (let* ((dom (mevedel-tool-web--yt-parse-captions
               "<transcript><text start=\"5\">A &amp;amp; B</text></transcript>"))
         (text (mevedel-tool-web--yt-format-captions dom)))
    (should (string-match-p "A & B" text))
    (should (string-prefix-p "[0:00]" text))))

(mevedel-deftest mevedel-tool-web--yt-format-captions ()
  ,test
  (test)
  :doc "groups captions into timestamped paragraphs and rejects other XML roots"
  (should (equal "[0:00]\nFirst next\n\n[0:32]\nLast\n\n"
                 (mevedel-tool-web--yt-format-captions
                  '(transcript nil (text ((start . "0")) "First")
                               (text ((start . "2")) "next")
                               (text ((start . "32")) "Last")))))
  (should-not (mevedel-tool-web--yt-format-captions '(html nil "bad"))))

(provide 'test-mevedel-tool-web)
;;; test-mevedel-tool-web.el ends here
