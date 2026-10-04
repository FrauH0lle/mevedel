;;; test-mevedel-tool-web.el --- Tests for mevedel-tool-web.el -*- lexical-binding: t -*-

;;; Commentary:

;; Native web tools, real HTTP retrieval, redirects, timeouts and cleanup.

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-pipeline)
(require 'mevedel-tools)
(require 'gptel-request)
(require 'mevedel-view)
(require 'mevedel-tool-permission)
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

  :doc "header includes the query and result count"
  (let* ((body "1. A\n   https://a/\n\n2. B\n   https://b/")
         (plist (mevedel-tool-web--render-search
                 "WebSearch" '(:query "mevedel") body nil)))
    (should (equal "WebSearch: mevedel (2 results)" (plist-get plist :header)))
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

  :doc "hands a cross-host redirect back when no session can approve it"
  (mevedel-test-http
   (lambda (_) '("302 Found" "Location: https://other.invalid/page\r\n" ""))
   (lambda (base)
     (with-temp-buffer
       (let ((result (test-mevedel-tool-web--call "WebFetch" (list :url base))))
         (should (eq 'success (plist-get result :handler-status)))
         (should (string-prefix-p "REDIRECT: 127.0.0.1 redirects to https://other.invalid/page"
                                  (plist-get result :result)))))))

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
    (should-not (memq timer timer-list)))

  :doc "refuses non-http schemes before connecting"
  (dolist (url '("file:///etc/passwd" "ftp://example.com/x" "example.com"))
    (should (string-prefix-p
             "Unsupported URL"
             (cadr (test-mevedel-tool-web--retrieve url #'ignore)))))

  :doc "refuses a redirect into a non-http scheme"
  (mevedel-test-http
   (lambda (_) '("302 Found" "Location: file:///etc/passwd\r\n" ""))
   (lambda (base)
     (should (equal '(nil "Unsupported URL (only http and https are retrieved): file:///etc/passwd")
                    (test-mevedel-tool-web--retrieve base #'ignore)))))

  :doc "follows same-host redirects up to the limit and releases every hop"
  (let ((mevedel-tool-web--max-redirects 2) (before (buffer-list)) requests)
    (mevedel-test-http
     (lambda (request) (push request requests) '("302 Found" "Location: /loop\r\n" ""))
     (lambda (base)
       (should (equal '(nil "Too many redirects (more than 2)")
                      (test-mevedel-tool-web--retrieve base #'ignore)))))
    (should (= 3 (length requests)))
    (dolist (buffer (buffer-list))
      (unless (memq buffer before)
        (should-not (with-current-buffer buffer
                      (bound-and-true-p url-callback-arguments))))))

  :doc "asks REDIRECT about a cross-host hop that follows a same-host hop"
  (let (calls)
    (mevedel-test-http
     (lambda (request)
       (if (string-match-p " /first " request)
           '("302 Found" "Location: /second\r\n" "")
         '("301 Moved Permanently" "Location: http://other.invalid/page\r\n" "")))
     (lambda (base)
       (should (equal '("handed back" nil)
                      (test-mevedel-tool-web--retrieve
                       (concat base "/first") #'ignore
                       :redirect (lambda (from target)
                                   (push (list from target) calls)
                                   '(result . "handed back")))))
       (should (equal (list (list (concat base "/second") "http://other.invalid/page"))
                      calls)))))

  :doc "a REDIRECT error settles as an error"
  (mevedel-test-http
   (lambda (_) '("302 Found" "Location: http://other.invalid/\r\n" ""))
   (lambda (base)
     (should (equal '(nil "blocked")
                    (test-mevedel-tool-web--retrieve
                     base #'ignore :redirect (lambda (_ _) '(error . "blocked")))))))

  :doc "keeps the method through a 307 and switches a 302 to GET"
  (let (requests)
    (mevedel-test-http
     (lambda (request)
       (push request requests)
       (cond ((string-match-p " /temporary " request)
              '("307 Temporary Redirect" "Location: /target\r\n" ""))
             ((string-match-p " /found " request)
              '("302 Found" "Location: /target\r\n" ""))
             (t '("200 OK" "Content-Type: text/plain\r\n" "done"))))
     (lambda (base)
       (let ((url-request-method "POST") (url-request-data "x"))
         (test-mevedel-tool-web--retrieve (concat base "/temporary") #'ignore)
         (test-mevedel-tool-web--retrieve (concat base "/found") #'ignore))))
    (should (equal '("POST /temporary" "POST /target" "POST /found" "GET /target")
                   (mapcar (lambda (request) (substring request 0 (string-search " HTTP" request)))
                           (reverse requests))))))

(defun test-mevedel-tool-web--retrieve (url parse &rest keys)
  "Retrieve URL with PARSE and KEYS; return the settled (VALUE ERROR)."
  (let (results)
    (apply #'mevedel-tool-web--retrieve url parse
           (lambda (value error) (push (list value error) results))
           keys)
    (let ((deadline (+ (float-time) 4)))
      (while (and (not results) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should (= 1 (length results)))
    (car results)))

(mevedel-deftest mevedel-tool-web--unsupported-url ()
  ,test
  (test)
  :doc "accepts http and https URLs with a host"
  (should-not (mevedel-tool-web--unsupported-url "https://example.com/a"))
  (should-not (mevedel-tool-web--unsupported-url "HTTP://example.com"))
  :doc "rejects other schemes, missing hosts and non-strings"
  (should (mevedel-tool-web--unsupported-url "file:///etc/passwd"))
  (should (mevedel-tool-web--unsupported-url "http://"))
  (should (mevedel-tool-web--unsupported-url nil)))

(mevedel-deftest mevedel-tool-web--local-host-p ()
  ,test
  (test)
  :doc "recognizes loopback, private and link-local hosts"
  (dolist (host '("localhost" "app.localhost" "127.0.0.1" "10.1.2.3" "192.168.0.1"
                  "172.16.0.1" "172.31.255.1" "169.254.1.1" "[::1]" "fd12:3456::1"
                  "fe80::1" "0.0.0.0"))
    (should (mevedel-tool-web--local-host-p host)))
  :doc "treats public hosts as remote"
  (dolist (host '("example.com" "172.32.0.1" "11.0.0.1" "localhost.example.com" nil))
    (should-not (mevedel-tool-web--local-host-p host))))

(mevedel-deftest mevedel-tool-web--redirect-decision
  (:before-each (mevedel-tool-web--register))
  ,test
  (test)
  :doc "follows, blocks or hands back according to the target's permission"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mevedel-hook-rules nil)
        (mevedel-permission-log-enabled nil))
    (with-temp-buffer
      (setq-local mevedel--session
                  (mevedel-session--create
                   :name "redirect" :permission-mode 'ask
                   :permission-rules '(("WebFetch" :domain "denied.org" :action deny)
                                       ("WebFetch" :domain "asked.org" :action ask))))
      (let ((buffer (current-buffer)))
        (with-temp-buffer
          (should (eq 'follow (mevedel-tool-web--redirect-decision
                               buffer "https://a.org/" "https://b.org/")))
          (should (equal '(error . "Redirect from a.org to https://denied.org/x blocked: the target host is denied by permission rules")
                         (mevedel-tool-web--redirect-decision
                          buffer "https://a.org/" "https://denied.org/x")))
          (should (equal '(result . "REDIRECT: a.org redirects to https://asked.org/x.  That host needs approval; call WebFetch with url=\"https://asked.org/x\" to continue.")
                         (mevedel-tool-web--redirect-decision
                          buffer "https://a.org/" "https://asked.org/x")))))))

  :doc "hands back a public-to-local redirect even when the target is allowed"
  (let ((mevedel-permission-rules nil) (mevedel-hook-rules nil))
    (with-temp-buffer
      (setq-local mevedel--session
                  (mevedel-session--create :name "local" :permission-mode 'ask))
      (should (eq 'result (car (mevedel-tool-web--redirect-decision
                                (current-buffer) "https://a.org/" "http://127.0.0.1:8080/"))))
      (should (eq 'follow (mevedel-tool-web--redirect-decision
                           (current-buffer) "http://localhost:3000/" "http://127.0.0.1:3000/")))))

  :doc "hands back without a session"
  (with-temp-buffer
    (should (eq 'result (car (mevedel-tool-web--redirect-decision
                              (current-buffer) "https://a.org/" "https://b.org/"))))))

(defun test-mevedel-tool-web--ddg-result (url title snippet &optional class)
  "Return a DuckDuckGo HTML result block for URL with TITLE and SNIPPET.
CLASS adds result classes.  Like the live page, the title, icon,
display URL and snippet all link to DuckDuckGo's redirect for URL."
  (let ((href (format "//duckduckgo.com/l/?uddg=%s&amp;rut=4f2c9e"
                      (url-hexify-string url))))
    (format "<div class=\"result results_links results_links_deep web-result %s\">
<div class=\"links_main links_deep result__body\">
<h2 class=\"result__title\"><a rel=\"nofollow\" class=\"result__a\" href=\"%s\">%s</a></h2>
<div class=\"result__extras\"><div class=\"result__extras__url\">
<span class=\"result__icon\"><a rel=\"nofollow\" href=\"%s\"><img class=\"result__icon__img\" src=\"//external-content.duckduckgo.com/ip3/x.ico\"></a></span>
<a class=\"result__url\" href=\"%s\">%s</a></div></div>
<a class=\"result__snippet\" href=\"%s\">%s</a>
<div class=\"clear\"></div></div></div>"
            (or class "") href title href href url href snippet)))

(defun test-mevedel-tool-web--ddg-page (&rest results)
  "Return a DuckDuckGo HTML results page holding RESULTS."
  (concat "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"></head>"
          "<body class=\"body--html\"><div class=\"serp__results\">"
          "<div id=\"links\" class=\"results\">"
          (apply #'concat results)
          "</div></div></body></html>"))

(defconst test-mevedel-tool-web--ddg-challenge
  (concat "<html><body><center id=\"lite_wrapper\">"
          "<form id=\"challenge-form\" action=\"//duckduckgo.com/anomaly.js\" method=\"POST\">"
          "<div class=\"anomaly-modal__mask\">"
          "<div class=\"anomaly-modal__modal  is-ie\" data-testid=\"anomaly-modal\">"
          "<div class=\"anomaly-modal__title\">Unfortunately, bots use DuckDuckGo too.</div>"
          "</div></div></form></center></body></html>")
  "Trimmed DuckDuckGo bot challenge page, served with HTTP 202.")

(defun test-mevedel-tool-web--parse-search (html &optional allowed blocked)
  "Parse search response HTML with ALLOWED and BLOCKED domains."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string html 'utf-8))
    (goto-char (point-min))
    (let ((url-http-content-type "text/html; charset=UTF-8"))
      (mevedel-tool-web--search-results allowed blocked))))

(mevedel-deftest mevedel-tool-web--search-results ()
  ,test
  (test)
  :doc "returns one numbered entry per result with its unwrapped URL"
  (let ((text (test-mevedel-tool-web--parse-search
               (test-mevedel-tool-web--ddg-page
                (test-mevedel-tool-web--ddg-result
                 "https://www.gnu.org/software/emacs/manual/html_mono/eww.html"
                 "Emacs Web Wowser - GNU"
                 "<b>EWW</b>, the <b>Emacs</b> Web Wowser,\n  is a web browser")
                (test-mevedel-tool-web--ddg-result
                 "https://en.wikipedia.org/wiki/Eww_(web_browser)"
                 "Eww (web browser)" "A browser")))))
    (should (equal text
                   (concat "1. Emacs Web Wowser - GNU\n"
                           "   https://www.gnu.org/software/emacs/manual/html_mono/eww.html\n"
                           "   EWW, the Emacs Web Wowser, is a web browser\n\n"
                           "2. Eww (web browser)\n"
                           "   https://en.wikipedia.org/wiki/Eww_(web_browser)\n"
                           "   A browser"))))

  :doc "skips ads and duplicate destinations and caps the count"
  (let* ((mevedel-tool-web--search-limit 2)
         (text (test-mevedel-tool-web--parse-search
                (test-mevedel-tool-web--ddg-page
                 (test-mevedel-tool-web--ddg-result
                  "https://ads.example/" "Ad" "Buy" "result--ad")
                 (test-mevedel-tool-web--ddg-result "https://a.example/" "A" "a")
                 (test-mevedel-tool-web--ddg-result "https://a.example/" "A again" "a")
                 (test-mevedel-tool-web--ddg-result "https://b.example/" "B" "b")
                 (test-mevedel-tool-web--ddg-result "https://c.example/" "C" "c")))))
    (should (= 2 (mevedel-tool-web--result-count text)))
    (should-not (string-search "ads.example" text))
    (should-not (string-search "A again" text))
    (should (string-search "https://b.example/" text)))

  :doc "re-encodes a non-ASCII destination as a valid URL"
  (let ((text (test-mevedel-tool-web--parse-search
               (test-mevedel-tool-web--ddg-page
                (test-mevedel-tool-web--ddg-result
                 "https://de.example/Grüße" "Grüße" "Hallo")))))
    (should (string-search "https://de.example/Gr%C3%BC%C3%9Fe" text))
    (should (string-search "1. Grüße" text)))

  :doc "allowed and blocked domains match hosts at label boundaries"
  (let ((page (test-mevedel-tool-web--ddg-page
               (test-mevedel-tool-web--ddg-result "https://docs.gnu.org/x" "Docs" "d")
               (test-mevedel-tool-web--ddg-result "https://notgnu.org/y" "Not" "n")
               (test-mevedel-tool-web--ddg-result "https://gnu.org/z" "Root" "r"))))
    (let ((text (test-mevedel-tool-web--parse-search page '("gnu.org"))))
      (should (= 2 (mevedel-tool-web--result-count text)))
      (should-not (string-search "notgnu.org" text)))
    (let ((text (test-mevedel-tool-web--parse-search page nil '("gnu.org"))))
      (should (= 1 (mevedel-tool-web--result-count text)))
      (should (string-search "notgnu.org" text)))
    (should (equal "No results on the requested domains."
                   (test-mevedel-tool-web--parse-search page '("example.com")))))

  :doc "reports an empty page as no results"
  (should (equal "No results."
                 (test-mevedel-tool-web--parse-search
                  (test-mevedel-tool-web--ddg-page))))

  :doc "signals the bot challenge instead of returning nothing"
  (should (equal "DuckDuckGo refused the search (bot challenge); retry later"
                 (cadr (should-error (test-mevedel-tool-web--parse-search
                                      test-mevedel-tool-web--ddg-challenge))))))

(mevedel-deftest mevedel-tool-web--result-url ()
  ,test
  (test)
  :doc "unwraps the uddg parameter without DuckDuckGo's tracking suffix"
  (should (equal "https://example.com/a?b=c"
                 (mevedel-tool-web--result-url
                  "//duckduckgo.com/l/?uddg=https%3A%2F%2Fexample.com%2Fa%3Fb%3Dc&rut=9f")))
  :doc "keeps an absolute destination and rejects other links"
  (should (equal "https://example.com/"
                 (mevedel-tool-web--result-url "https://example.com/")))
  (should-not (mevedel-tool-web--result-url "//duckduckgo.com/y.js?ad_domain=x"))
  (should-not (mevedel-tool-web--result-url nil)))

(mevedel-deftest mevedel-tool-web--domains ()
  ,test
  (test)
  :doc "normalizes vectors of domain patterns to lowercase host suffixes"
  (should (equal '("example.com" "docs.gnu.org" "a.org")
                 (mevedel-tool-web--domains [" Example.COM" "*.docs.gnu.org" ".a.org"])))
  (should-not (mevedel-tool-web--domains nil)))

(mevedel-deftest mevedel-tool-web--domain-match-p ()
  ,test
  (test)
  :doc "matches the domain itself and its subdomains only"
  (should (mevedel-tool-web--domain-match-p "https://gnu.org/" '("gnu.org")))
  (should (mevedel-tool-web--domain-match-p "https://WWW.GNU.org/" '("gnu.org")))
  (should-not (mevedel-tool-web--domain-match-p "https://notgnu.org/" '("gnu.org")))
  (should-not (mevedel-tool-web--domain-match-p "not-a-url" '("gnu.org"))))

(mevedel-deftest mevedel-tool-web--result-count ()
  ,test
  (test)
  :doc "counts numbered entries, not lines"
  (should (= 2 (mevedel-tool-web--result-count
                "1. A\n   https://a/\n   x\n\n2. B\n   https://b/")))
  (should (= 0 (mevedel-tool-web--result-count "No results."))))

(mevedel-deftest mevedel-tool-web--websearch
  (:before-each (mevedel-tool-web--register))
  ,test
  (test)
  :doc "searches DuckDuckGo through the pipeline and returns parsed results"
  (mevedel-test-http
   (lambda (request)
     (should (string-search "/html/?q=two%20words " request))
     (list "200 OK" "Content-Type: text/html; charset=UTF-8\r\n"
           (test-mevedel-tool-web--ddg-page
            (test-mevedel-tool-web--ddg-result "https://example.com/1" "Title 1" "Excerpt 1"))))
   (lambda (base)
     (let* ((mevedel-tool-web--search-url (concat base "/html/?q="))
            (result (test-mevedel-tool-web--call "WebSearch" '(:query "two words"))))
       (should (eq 'success (plist-get result :handler-status)))
       (should (equal "1. Title 1\n   https://example.com/1\n   Excerpt 1"
                      (plist-get result :result)))
       (should (zerop mevedel-tool-web--search-active))
       (should-not mevedel-tool-web--search-queue))))

  :doc "adds domain filters to the query"
  (let (requests)
    (mevedel-test-http
     (lambda (request)
       (push request requests)
       (list "200 OK" "Content-Type: text/html\r\n" (test-mevedel-tool-web--ddg-page)))
     (lambda (base)
       (let ((mevedel-tool-web--search-url (concat base "/html/?q=")))
         (test-mevedel-tool-web--call
          "WebSearch" '(:query "eww" :allowed_domains ["gnu.org" "github.com"]))
         (test-mevedel-tool-web--call
          "WebSearch" '(:query "eww" :blocked_domains ["reddit.com"])))))
    (should (string-search "?q=eww%20site%3Agnu.org%20OR%20site%3Agithub.com " (cadr requests)))
    (should (string-search "?q=eww%20-site%3Areddit.com " (car requests))))

  :doc "rejects allowed and blocked domains together without searching"
  (let ((result (test-mevedel-tool-web--call
                 "WebSearch" '(:query "q" :allowed_domains ["a.org"]
                                         :blocked_domains ["b.org"]))))
    (should (eq 'error (plist-get result :handler-status)))
    (should (string-search "not both" (plist-get result :result)))
    (should-not mevedel-tool-web--search-queue))

  :doc "a bot challenge becomes a tool error"
  (mevedel-test-http
   (lambda (_) (list "202 Accepted" "Content-Type: text/html\r\n"
                     test-mevedel-tool-web--ddg-challenge))
   (lambda (base)
     (let* ((mevedel-tool-web--search-url (concat base "/html/?q="))
            (result (test-mevedel-tool-web--call "WebSearch" '(:query "q"))))
       (should (eq 'error (plist-get result :handler-status)))
       (should (string-search "bot challenge" (plist-get result :result)))))))

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
       (let ((mevedel-tool-web--search-url (concat base "/?q=")))
         (dotimes (_ 3)
           (mevedel-tool-web--websearch
            (lambda (result) (push result results)) '(:query "hang")))
         (setq mevedel-tool-web--search-url (concat base "/wrong?q="))
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
