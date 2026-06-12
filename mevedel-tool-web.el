;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Web-related tool wrappings: WebSearch, WebFetch, YouTube.
;; These wrap the upstream gptel-agent tools so every call flows
;; through the mevedel pipeline (permissions, result persistence,
;; display) while the underlying implementation stays in gptel-agent.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'gptel-agent-tools)

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--register-wrap
                  "mevedel-tool-registry" (&rest keys))

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

(defun mevedel-tool-web--render-transform (name args result)
  "Return bounded render metadata for wrapped web tool RESULT."
  (let ((url (plist-get args :url))
        (query (plist-get args :query)))
    (list :kind 'web
          :tool name
          :host (and url (mevedel-tool-web--url-host url))
          :query query
          :lines (length (split-string result "\n" t))
          :chars (length result))))


;;
;;; Renderers

(defun mevedel-tool-web--render-fetch (name args result render-data)
  "Rendering plist for the WebFetch / YouTube tools.
Header shows the URL's host and the fetched size; body fontifies in
the data buffer's major mode.  The view parser passes renderers
unescaped tool results, so org-mode storage escapes are not shown in
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
  "Rendering plist for the WebSearch tool.
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
  "Wrap gptel-agent's web tools for mevedel."

  (mevedel-define-tool
    :wrap (gptel-get-tool '("gptel-agent" "WebSearch"))
    :summary "Search the web for the top results to a query."
    :prompt-file "tools/websearch.md"
    :groups (web)
    :read-only-p t
    :render-transform #'mevedel-tool-web--render-transform
    :renderer #'mevedel-tool-web--render-search)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("gptel-agent" "WebFetch"))
    :summary "Fetch and read the contents of a URL."
    :prompt-file "tools/webfetch.md"
    :groups (web)
    :read-only-p t
    :max-result-size 50000
    :get-domain (lambda (args)
                  (mevedel-tool-web--url-host (plist-get args :url)))
    :render-transform #'mevedel-tool-web--render-transform
    :renderer #'mevedel-tool-web--render-fetch)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("gptel-agent" "YouTube"))
    :summary "Find the description and transcript for a YouTube video."
    :prompt-file "tools/youtube.md"
    :groups (web)
    :read-only-p t
    :max-result-size 50000
    :get-domain (lambda (args)
                  (mevedel-tool-web--url-host (plist-get args :url)))
    :render-transform #'mevedel-tool-web--render-transform
    :renderer #'mevedel-tool-web--render-fetch))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
