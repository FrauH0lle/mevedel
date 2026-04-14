;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Web-related tool definitions: WebSearch, WebFetch, YouTube.
;; These are thin wrappers around gptel-agent functions.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `gptel-agent-tools'
(declare-function gptel-agent--read-url "ext:gptel-agent-tools" (tool-cb url))
(declare-function gptel-agent--web-search-eww "ext:gptel-agent-tools" (tool-cb query &optional count))
(declare-function gptel-agent--yt-read-url "ext:gptel-agent-tools" (callback url))



;;
;;; Handlers

(defun mevedel-tool-web--search (callback args)
  "Search the web and return top results.
CALLBACK receives the result string.  ARGS is a plist with :query
and optional :count."
  (let ((query (plist-get args :query))
        (count (plist-get args :count)))
    (unless (stringp query)
      (error "Parameter query is required"))
    (if count
        (gptel-agent--web-search-eww callback query count)
      (gptel-agent--web-search-eww callback query))))

(defun mevedel-tool-web--fetch (callback args)
  "Fetch and read the text content of a URL.
CALLBACK receives the result string.  ARGS is a plist with :url."
  (let ((url (plist-get args :url)))
    (unless (stringp url)
      (error "Parameter url is required"))
    (gptel-agent--read-url callback url)))

(defun mevedel-tool-web--youtube (callback args)
  "Fetch YouTube video description and transcript.
CALLBACK receives the result string.  ARGS is a plist with :url."
  (let ((url (plist-get args :url)))
    (unless (stringp url)
      (error "Parameter url is required"))
    (gptel-agent--yt-read-url callback url)))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-tool-web--register ()
  "Define web-related read-only tools for mevedel."

  (mevedel-define-tool
    :name "WebSearch"
    :description "Search the web and return top results with URLs and excerpts."
    :prompt-file "tools/websearch.md"
    :handler #'mevedel-tool-web--search
    :args ((query string :required
                  "The natural language search query, can be multiple words.")
           (count integer :optional
                  "Number of results to return (default 5)."))
    :async-p t
    :read-only-p t
    :groups (web))

  (mevedel-define-tool
    :name "WebFetch"
    :description "Fetch and read the text content of a URL."
    :prompt-file "tools/webfetch.md"
    :handler #'mevedel-tool-web--fetch
    :args ((url string :required "The URL to read."))
    :async-p t
    :read-only-p t
    :max-result-size 50000
    :groups (web))

  (mevedel-define-tool
    :name "YouTube"
    :description "Fetch YouTube video description and transcript."
    :prompt-file "tools/youtube.md"
    :handler #'mevedel-tool-web--youtube
    :args ((url string :required
                "The YouTube video URL, e.g. \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\"."))
    :async-p t
    :read-only-p t
    :max-result-size 50000
    :groups (web)))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
