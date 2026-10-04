;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Karthik Chikmagalur
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Search, readability and YouTube handlers adapted from gptel-agent-tools.el.

;;; Commentary:

;; Native WebSearch and WebFetch tools using EWW, SHR and url-retrieve.
;; Every HTTP stage owns its timeout, response buffers and settlement.
;; Tool calls retain the mevedel permission, persistence and display pipeline.

;;; Code:

(require 'cl-lib)
(require 'eww)
(require 'mail-parse)
(require 'url-http)

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--positional-to-plist
                  "mevedel-tool-registry" (raw-args specs))
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))

;; `mevedel-view'
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
;;; Request ownership

(defvar mevedel-tool-web--timeout 30
  "Maximum seconds for one retrieval, including redirects.")

(defun mevedel-tool-web--retrieve (url parse callback)
  "Retrieve URL, parse its body with PARSE, then call CALLBACK.
CALLBACK receives (VALUE ERROR), where ERROR is nil on success or an
error string.  Each retrieval owns its timer and response buffers,
including redirects.  Cleanup precedes delivery, exactly once."
  (let ((token (make-symbol "mevedel-web-request"))
        buffer timer done)
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer))
           (let ((kill-buffer-query-functions nil))
             (dolist (candidate (buffer-list))
               (when (or (eq candidate buffer)
                         (with-current-buffer candidate
                           (memq token (bound-and-true-p url-callback-arguments))))
                 (kill-buffer candidate)))))
         (finish (value error)
           (unless done
             (setq done t)
             (cleanup)
             (funcall callback value error))))
      (setq timer (run-at-time
                   mevedel-tool-web--timeout nil
                   (lambda ()
                     (finish nil (format "Request timed out after %s seconds"
                                         mevedel-tool-web--timeout)))))
      (condition-case err
          (let ((url-request-noninteractive t)
                (inhibit-message t))
            (setq buffer
                  (url-retrieve
                   url
                   (lambda (status _token)
                     (unless done
                       (let ((parsed
                              (condition-case err
                                  (if (plist-get status :error)
                                      (cons nil (format "%S" (plist-get status :error)))
                                    (goto-char (point-min))
                                    (if (bound-and-true-p url-http-end-of-headers)
                                        (goto-char url-http-end-of-headers)
                                      (unless (re-search-forward "\r?\n\r?\n" nil t)
                                        (error "Response has no HTTP headers")))
                                    (cons t (funcall parse)))
                                (error (cons nil (error-message-string err))))))
                         (if (car parsed)
                             (finish (cdr parsed) nil)
                           (finish nil (cdr parsed))))))
                   (list token) t t))
            ;; Some URL handlers call back before returning their buffer.
            (cond (done (cleanup))
                  ((not (buffer-live-p buffer))
                   (finish nil "Retrieval did not create a response buffer"))))
        (error (finish nil (error-message-string err)))))))

(defun mevedel-tool-web--content-type ()
  "Return the response's (MIME-TYPE . CHARSET), each a string or nil."
  (let ((parsed (and (bound-and-true-p url-http-content-type)
                     (mail-header-parse-content-type url-http-content-type))))
    (cons (and (car parsed) (downcase (car parsed)))
          (cdr (assq 'charset (cdr parsed))))))

(defun mevedel-tool-web--body (&optional html-p)
  "Return the response body after point, decoded.
The Content-Type charset wins; HTML-P also consults a meta charset.
Unknown or missing charsets decode as UTF-8."
  (let* ((charset (or (cdr (mevedel-tool-web--content-type))
                      (save-excursion (eww-detect-charset html-p))))
         (coding (or (and charset
                          (ignore-errors
                            (coding-system-from-name (downcase charset))))
                     'utf-8)))
    (decode-coding-string
     (buffer-substring-no-properties (point) (point-max)) coding)))

(defun mevedel-tool-web--html-dom ()
  "Return the DOM of the HTML response body at point."
  (let ((html (mevedel-tool-web--body t)))
    (with-temp-buffer
      (insert html)
      (libxml-parse-html-region (point-min) (point-max)))))

(defun mevedel-tool-web--page-text ()
  "Return readable text from the HTML response body at point."
  (let ((dom (mevedel-tool-web--html-dom)))
    (with-temp-buffer
      (let ((shr-use-fonts nil) (shr-width 80))
        (shr-insert-document (or (eww-readable-dom dom) dom)))
      (buffer-substring-no-properties (point-min) (point-max)))))


;;
;;; Search

(defvar mevedel-tool-web--search-url "https://html.duckduckgo.com/html/?q="
  "DuckDuckGo HTML endpoint; the hexified query is appended.")

(defvar mevedel-tool-web--search-limit 10
  "Maximum number of results one search returns.")

(defvar mevedel-tool-web--search-active 0
  "Number of active web searches.")
(defvar mevedel-tool-web--search-queue nil
  "FIFO of pending (URL PARSE CALLBACK) searches.")

(defun mevedel-tool-web--class-p (node class)
  "Return non-nil when NODE's class attribute has the token CLASS."
  (and (consp node)
       (member class (split-string (or (dom-attr node 'class) "")))))

(defun mevedel-tool-web--by-class (dom class)
  "Return the elements of DOM whose class attribute has the token CLASS."
  (dom-search dom (lambda (node) (mevedel-tool-web--class-p node class))))

(defun mevedel-tool-web--node-text (node)
  "Return NODE's text with whitespace runs collapsed."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (dom-inner-text node))))

(defun mevedel-tool-web--result-url (href)
  "Return the destination of DuckDuckGo result link HREF, or nil.
DuckDuckGo wraps destinations as the `uddg' parameter of its own
redirect link; an absolute http(s) HREF is already the destination."
  (when (stringp href)
    (if-let* ((start (string-search "?" href))
              (target (car (alist-get "uddg"
                                      (url-parse-query-string
                                       (substring href (1+ start)))
                                      nil nil #'equal))))
        (url-encode-url (decode-coding-string target 'utf-8))
      (and (string-match-p "\\`https?://" href) href))))

(defun mevedel-tool-web--domains (domains)
  "Return DOMAINS, a list or vector of host suffixes, normalized."
  (mapcar (lambda (domain)
            (downcase (string-remove-prefix
                       "." (string-remove-prefix "*" (string-trim domain)))))
          (append domains nil)))

(defun mevedel-tool-web--domain-match-p (url domains)
  "Return non-nil when URL's host is one of DOMAINS or a subdomain of one."
  (when-let* ((host (mevedel-tool-web--url-host url)))
    (setq host (downcase host))
    (seq-some (lambda (domain)
                (or (string= host domain)
                    (string-suffix-p (concat "." domain) host)))
              domains)))

(defun mevedel-tool-web--search-results (allowed blocked)
  "Return formatted DuckDuckGo results from the response body at point.
When ALLOWED is non-nil, keep only results on those domains; drop
results on BLOCKED domains.  Signal an error for a bot challenge."
  (let ((dom (mevedel-tool-web--html-dom))
        (seen (make-hash-table :test #'equal))
        (found 0)
        results)
    (dolist (block (mevedel-tool-web--by-class dom "result"))
      (when-let* (((not (mevedel-tool-web--class-p block "result--ad")))
                  (link (car (mevedel-tool-web--by-class block "result__a")))
                  (url (mevedel-tool-web--result-url (dom-attr link 'href)))
                  ((not (gethash url seen))))
        (puthash url t seen)
        (cl-incf found)
        (when (and (< (length results) mevedel-tool-web--search-limit)
                   (or (null allowed)
                       (mevedel-tool-web--domain-match-p url allowed))
                   (not (mevedel-tool-web--domain-match-p url blocked)))
          (push (list (mevedel-tool-web--node-text link) url
                      (when-let* ((snippet (car (mevedel-tool-web--by-class
                                                 block "result__snippet"))))
                        (mevedel-tool-web--node-text snippet)))
                results))))
    (cond
     (results
      (let ((index 0))
        (mapconcat (pcase-lambda (`(,title ,url ,snippet))
                     (format "%d. %s\n   %s%s" (cl-incf index) title url
                             (if (and snippet (not (string-empty-p snippet)))
                                 (concat "\n   " snippet)
                               "")))
                   (nreverse results) "\n\n")))
     ((or (dom-by-id dom "\\`challenge-form\\'")
          (mevedel-tool-web--by-class dom "anomaly-modal__modal"))
      (error "DuckDuckGo refused the search (bot challenge); retry later"))
     ((> found 0) "No results on the requested domains.")
     (t "No results."))))

(defun mevedel-tool-web--start-searches ()
  "Start queued searches while fewer than two retrievals are active."
  (while (and mevedel-tool-web--search-queue
              (< mevedel-tool-web--search-active 2))
    (pcase-let ((`(,url ,parse ,callback) (pop mevedel-tool-web--search-queue)))
      (cl-incf mevedel-tool-web--search-active)
      (mevedel-tool-web--retrieve
       url parse
       (lambda (value error)
         (cl-decf mevedel-tool-web--search-active)
         (unwind-protect
             (funcall callback (if error
                                   (list :result (concat "Error: " error) :status 'error)
                                 (list :result value)))
           (mevedel-tool-web--start-searches)))))))

(defun mevedel-tool-web--websearch (callback args)
  "Search for the query in ARGS and deliver a handler result to CALLBACK.
Optional `allowed_domains' or `blocked_domains' in ARGS add `site:'
terms to the query and filter the returned results."
  (let ((allowed (mevedel-tool-web--domains (plist-get args :allowed_domains)))
        (blocked (mevedel-tool-web--domains (plist-get args :blocked_domains))))
    (if (and allowed blocked)
        (funcall callback
                 (list :result "Error: Pass allowed_domains or blocked_domains, not both."
                       :status 'error))
      (let ((query (string-join
                    (cons (plist-get args :query)
                          (if allowed
                              (list (mapconcat (lambda (domain) (concat "site:" domain))
                                               allowed " OR "))
                            (mapcar (lambda (domain) (concat "-site:" domain))
                                    blocked)))
                    " ")))
        (setq mevedel-tool-web--search-queue
              (nconc mevedel-tool-web--search-queue
                     (list (list (concat mevedel-tool-web--search-url
                                         (url-hexify-string query))
                                 (lambda ()
                                   (mevedel-tool-web--search-results allowed blocked))
                                 callback))))
        (mevedel-tool-web--start-searches)))))


;;
;;; Page and YouTube retrieval

(defun mevedel-tool-web--fetch (callback args)
  "Fetch the URL in ARGS and deliver readable text to CALLBACK."
  (let* ((url (plist-get args :url))
         done
         (finish (lambda (value error)
                   (unless done
                     (setq done t)
                     (funcall callback
                              (if error
                                  (list :result (or value (concat "Error: " error)) :status 'error)
                                (list :result value)))))))
    (if-let* ((video-id (mevedel-tool-web--yt-video-id url)))
        (mevedel-tool-web--yt-fetch finish video-id)
      (mevedel-tool-web--retrieve url #'mevedel-tool-web--page-text finish))))

(defun mevedel-tool-web--yt-fetch (callback video-id)
  "Fetch VIDEO-ID's description and captions, delivering to CALLBACK."
  (mevedel-tool-web--retrieve
   (format "https://youtube.com/watch?v=%s" video-id)
   (lambda ()
     (unless (re-search-forward "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)" nil t)
       (error "Could not extract YouTube API key"))
     (match-string 1))
   (lambda (api-key error)
     (if error (funcall callback nil error)
       (let ((url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/json") ("Accept-Language" . "en-US")))
             (url-request-data
              (encode-coding-string
               (json-encode
                `((context . ((client . ((clientName . "ANDROID")
                                        (clientVersion . "20.10.38")))))
                  (videoId . ,video-id))) 'utf-8)))
         (mevedel-tool-web--retrieve
          (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key)
          (lambda () (json-parse-buffer :object-type 'plist))
          (lambda (metadata error)
            (if error (funcall callback nil error)
              ;; Metadata used POST; caption retrieval is always a fresh GET,
              ;; even when a transport invokes its callback synchronously.
              (let ((url-request-method "GET")
                    (url-request-extra-headers nil)
                    (url-request-data nil))
                (condition-case err
                    (mevedel-tool-web--yt-captions callback metadata)
                  (error (funcall callback nil (error-message-string err)))))))))))))

(defun mevedel-tool-web--yt-captions (callback metadata)
  "Read captions from METADATA and deliver description/transcript to CALLBACK."
  (let* ((description (or (map-nested-elt metadata '(:videoDetails :shortDescription))
                          "No description available."))
         (tracks (map-nested-elt metadata
                                '(:captions :playerCaptionsTracklistRenderer :captionTracks)))
         (english (seq-find
                   (lambda (track)
                     (string-prefix-p "en" (or (plist-get track :languageCode) "")))
                   tracks))
         (render (lambda (transcript)
                   (format "# Description\n\n%s\n\n# Transcript\n\n%s"
                           description transcript))))
    (if (not english)
        (funcall callback (funcall render (if tracks "No English transcript available."
                                           "No transcript available.")) nil)
      (mevedel-tool-web--retrieve
       (replace-regexp-in-string "&fmt=srv3" "" (plist-get english :baseUrl))
       (lambda ()
         (or (mevedel-tool-web--yt-format-captions
              (mevedel-tool-web--yt-parse-captions
               (buffer-substring-no-properties (point) (point-max))))
             (error "Could not parse YouTube transcript")))
       (lambda (text error)
         (funcall callback
                  (funcall render (if error
                                      (concat "Error fetching transcript: " error)
                                    text))
                  error))))))

(defun mevedel-tool-web--yt-video-id (url)
  "Return the video ID if URL is a YouTube video URL, nil otherwise."
  (and (string-match
        (rx bol (opt "http" (opt "s") "://")
            (opt "www.") "youtu" (or ".be" "be.com") "/"
            (opt "watch?v=")
            (group (one-or-more (not (any "?&")))))
        url)
       (match-string 1 url)))

(defun mevedel-tool-web--yt-parse-captions (xml-string)
  "Parse YouTube caption XML-STRING and return DOM."
  (with-temp-buffer
    (insert xml-string)
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    ;; Clean up the XML
    (dolist (reps '(("\n" . " ")
                    ("&amp;" . "&")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")
                    ("&lt;" . "<")
                    ("&gt;" . ">")))
      (save-excursion
        (while (search-forward (car reps) nil t)
          (replace-match (cdr reps) nil t))))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun mevedel-tool-web--yt-format-captions (caption-dom &optional chunk-time)
  "Format CAPTION-DOM as paragraphs with timestamps.

CHUNK-TIME is the number of seconds per paragraph (default 30)."
  (when (and (listp caption-dom)
             (eq (car-safe caption-dom) 'transcript))
    (let ((chunk-time (or chunk-time 30))
          (result "")
          (current-para "")
          (para-start-time 0))
      (dolist (elem (cddr caption-dom)) ;; Process each text element
        (when (and (listp elem) (eq (car elem) 'text))
          (let* ((attrs (cadr elem))
                 (text (caddr elem))
                 (start (string-to-number (cdr (assoc 'start attrs))))
                 ;; Check if we've crossed into a new chunk-time boundary
                 (should-chunk (and (> (abs (- start para-start-time)) 3)
                                    (not (= (floor para-start-time chunk-time)
                                            (floor start chunk-time))))))
            (when (and should-chunk (> (length current-para) 0))
              ;; Add completed paragraph
              (setq result (concat result
                                   (format "[%d:%02d]\n%s\n\n"
                                           (floor para-start-time 60)
                                           (mod para-start-time 60)
                                           (string-trim current-para))))
              (setq current-para "")
              (setq para-start-time start))

            (when text
              (setq current-para (concat current-para " " text))))))

      ;; Add final paragraph
      (when (> (length current-para) 0)
        (setq result (concat result
                             (format "[%d:%02d]\n%s\n\n"
                                     (floor para-start-time 60)
                                     (mod para-start-time 60)
                                     (string-trim current-para)))))
      result)))


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
          :results (and query (mevedel-tool-web--result-count result))
          :lines (length (split-string result "\n" t))
          :chars (length result))))

(defun mevedel-tool-web--result-count (result)
  "Return the number of numbered search entries in RESULT."
  (let ((count 0) (start 0))
    (while (string-match "^[0-9]+\\. " result start)
      (setq count (1+ count) start (match-end 0)))
    count))

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
Header shows the query and result count; body fontifies in the data
buffer's major mode (see `mevedel-tool-web--render-fetch' for why)."
  (when (stringp result)
    (let* ((query (or (plist-get args :query) ""))
           (count (or (plist-get render-data :results)
                      (mevedel-tool-web--result-count result))))
      (list :header (format "%s: %s (%d result%s)"
                            (or name "WebSearch") query count
                            (if (= count 1) "" "s"))
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-tool-web--register ()
  "Register mevedel's native web tools."

  (mevedel-define-tool
    :name "WebSearch"
    :description "Search the web with DuckDuckGo for titled result links and snippets."
    :summary "Search the web for the top results to a query."
    :prompt-file "prompts/tools/websearch.md"
    :handler #'mevedel-tool-web--websearch
    :args ((query string :required
                  "The natural language search query, can be multiple words.")
           (allowed_domains array :optional
                            "Only return results on these domains or their subdomains."
                            :items (:type string))
           (blocked_domains array :optional
                            "Never return results on these domains or their subdomains."
                            :items (:type string)))
    :async-p t
    :category "mevedel-web"
    :groups (web)
    :read-only-p t
    :render-transform #'mevedel-tool-web--render-transform
    :renderer '((success . mevedel-tool-web--render-search)))

  (mevedel-define-tool
    :name "WebFetch"
    :description "Fetch and read the contents of a URL."
    :summary "Fetch and read the contents of a URL."
    :prompt-file "prompts/tools/webfetch.md"
    :handler #'mevedel-tool-web--fetch
    :args ((url string :required "The URL to fetch."))
    :async-p t
    :category "mevedel-web"
    :groups (web)
    :read-only-p t
    :max-result-size 50000
    :get-domain (lambda (args)
                  (mevedel-tool-web--url-host (plist-get args :url)))
    :render-transform #'mevedel-tool-web--render-transform
    :renderer '((success . mevedel-tool-web--render-fetch))))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
