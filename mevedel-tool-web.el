;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Web tool registration: WebSearch and WebFetch.  Every call flows through
;; the mevedel pipeline (permissions, result persistence, display) while the
;; implementation stays in gptel-agent.  Both are adapted natively rather
;; than wrapped: the upstream schemas and handlers do not survive wrapping
;; unchanged (see each registration for why).

;;; Code:

(require 'gptel-agent-tools)

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-tool-async "ext:gptel-request" (tool))
(declare-function gptel-tool-function "ext:gptel-request" (tool))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--positional-to-plist
                  "mevedel-pipeline" (raw-args specs))
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())


;;
;;; Helpers

(defun mevedel-tool-web--url-host (url)
  "Return the host component of URL, or nil if it cannot be parsed."
  (when (stringp url)
    (ignore-errors
      (let ((host (url-host (url-generic-parse-url url))))
        (and host (not (string-empty-p host)) host)))))


;;
;;; Upstream handlers and response ownership

(defun mevedel-tool-web--websearch-function ()
  "Return the upstream asynchronous WebSearch handler, or nil.
Returns nil when the tool is absent or has stopped being asynchronous,
because this adapter calls it with a continuation."
  (when-let* ((tool (gptel-get-tool '("gptel-agent" "WebSearch")))
              ((gptel-tool-async tool)))
    (gptel-tool-function tool)))

(defun mevedel-tool-web--websearch (callback query)
  "Search the web for QUERY and deliver the result to CALLBACK."
  (let ((search (or (mevedel-tool-web--websearch-function)
                    (error "The upstream WebSearch tool is unavailable"))))
    (funcall search callback query)))

(defun mevedel-tool-web--fetch-function ()
  "Return the upstream asynchronous WebFetch handler, or nil.
Returns nil when the tool is absent or has stopped being asynchronous,
because this adapter calls it with a continuation."
  (when-let* ((tool (gptel-get-tool '("gptel-agent" "WebFetch")))
              ((gptel-tool-async tool)))
    (gptel-tool-function tool)))

(defun mevedel-tool-web--release-fetch-responses (continuation)
  "Kill the response buffers retrieved for CONTINUATION.
`url-http' records the retrieval arguments in every response buffer it
creates and carries them across redirects, and the upstream handler passes
its callback among those arguments at every stage, so CONTINUATION
identifies exactly the buffers this call produced and nothing else."
  (let ((kill-buffer-query-functions nil))
    (dolist (buffer (buffer-list))
      (when (with-current-buffer buffer
              (memq continuation (bound-and-true-p url-callback-arguments)))
        (kill-buffer buffer)))))

(defun mevedel-tool-web--fetch (callback args)
  "Fetch the URL in ARGS and call CALLBACK with the result.

For a YouTube URL the upstream handler retrieves the watch page, the
metadata API, and the caption track, and kills none of those response
buffers, so this adapter owns them: the buffers that carry its own
continuation are killed once the call settles.  A call that never settles
keeps its buffers."
  (if-let* ((fn (mevedel-tool-web--fetch-function)))
      (letrec ((settled nil)
               (continuation
                (lambda (result)
                  (unless settled
                    (setq settled t)
                    (mevedel-tool-web--release-fetch-responses continuation)
                    (funcall callback (list :result result))))))
        (condition-case error
            (funcall fn continuation (plist-get args :url))
          (error
           (funcall continuation
                    (format "Error: %s" (error-message-string error))))))
    (funcall callback
             (list :result
                   "Error: gptel-agent's WebFetch tool is unavailable"))))


;;
;;; Renderers

(defun mevedel-tool-web--render-transform (name args result)
  "Return bounded render metadata for web tool NAME with ARGS and RESULT."
  (let ((url (plist-get args :url))
        (query (plist-get args :query)))
    (list :kind 'web
          :tool name
          :host (and url (mevedel-tool-web--url-host url))
          :query query
          :lines (length (split-string result "\n" t))
          :chars (length result))))

(defun mevedel-tool-web--render-fetch (name args result render-data)
  "Return rendering plist for NAME using ARGS, RESULT, and RENDER-DATA.
Header shows the URL's host and the fetched size; body fontifies in
the data buffer's major mode.  The view parser passes renderers
unescaped tool results, so `org-mode' storage escapes are not shown in
the expanded body."
  (when (stringp result)
    (let* ((url (plist-get args :url))
           (host (or (mevedel-tool-web--url-host url) url "?"))
           (chars (or (plist-get render-data :chars)
                      (length result))))
      (list :header (format "%s: %s (%d chars)"
                            (or name "WebFetch") host chars)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))

(defun mevedel-tool-web--render-search (name args result render-data)
  "Return rendering plist for NAME using ARGS, RESULT, and RENDER-DATA.
Header shows the query and output line count; body fontifies in the
data buffer's major mode (see `mevedel-tool-web--render-fetch' for
why)."
  (when (stringp result)
    (let* ((query (or (plist-get args :query) ""))
           (lines (or (plist-get render-data :lines)
                      (length (split-string result "\n" t)))))
      (list :header (format "%s: %s (%d lines)"
                            (or name "WebSearch") query lines)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-tool-web--register ()
  "Register mevedel's web tools over gptel-agent's implementations."

  ;; Registered natively rather than wrapped, because the upstream
  ;; schema advertises a `count' argument its own callback hardcodes
  ;; away; wrapping freezes that schema, so owning it here is the only
  ;; way to stop promising the model an argument that does nothing.
  (mevedel-define-tool
    :name "WebSearch"
    :description "Search the web for the top results to a query."
    :summary "Search the web for the top results to a query."
    :prompt-file "prompts/tools/websearch.md"
    :handler #'mevedel-tool-web--websearch
    :args ((query string :required
                  "The natural language search query, can be multiple words."))
    :async-p t
    :category "mevedel-gptel-agent"
    :groups (web)
    :read-only-p t
    :render-transform #'mevedel-tool-web--render-transform
    :renderer #'mevedel-tool-web--render-search)

  ;; Registered natively rather than wrapped, because the upstream handler
  ;; leaks the response buffers a YouTube URL retrieves and only an owner
  ;; of the call can release them.
  (mevedel-define-tool
    :name "WebFetch"
    :description "Fetch and read the contents of a URL."
    :summary "Fetch and read the contents of a URL."
    :prompt-file "prompts/tools/webfetch.md"
    :handler #'mevedel-tool-web--fetch
    :args ((url string :required "The URL to fetch."))
    :async-p t
    :category "mevedel-gptel-agent"
    :groups (web)
    :read-only-p t
    :max-result-size 50000
    :get-domain (lambda (args)
                  (mevedel-tool-web--url-host (plist-get args :url)))
    :render-transform #'mevedel-tool-web--render-transform
    :renderer #'mevedel-tool-web--render-fetch))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
