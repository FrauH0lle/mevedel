;;; niwa-tool-web-registration.el --- Web tool catalog -*- lexical-binding: t -*-

;;; Commentary:

;; Complete discovery metadata; implementation loads at first use.

;;; Code:

(require 'niwa-tool-registry)

(autoload 'niwa-tool-web--fetch "niwa-tool-web")
(autoload 'niwa-tool-web--render-fetch "niwa-tool-web")
(autoload 'niwa-tool-web--render-search "niwa-tool-web")
(autoload 'niwa-tool-web--render-transform "niwa-tool-web")
(autoload 'niwa-tool-web--url-host "niwa-tool-web")
(autoload 'niwa-tool-web--websearch "niwa-tool-web")

(defun niwa-tool-web--register ()
  "Register niwa's native web tools."

  (niwa-define-tool
    :name "WebSearch"
    :description "Search the web with DuckDuckGo for titled result links and snippets."
    :summary "Search the web for the top results to a query."
    :prompt-file "prompts/tools/websearch.md"
    :handler #'niwa-tool-web--websearch
    :args ((query string :required
                  "The natural language search query, can be multiple words.")
           (allowed_domains array :optional
                            "Only return results on these domains or their subdomains."
                            :items (:type string))
           (blocked_domains array :optional
                            "Never return results on these domains or their subdomains."
                            :items (:type string)))
    :async-p t
    :category "niwa-web"
    :groups (web)
    :read-only-p t
    :render-transform #'niwa-tool-web--render-transform
    :renderer '((success . niwa-tool-web--render-search)))

  (niwa-define-tool
    :name "WebFetch"
    :description "Fetch a URL as readable text, or an image or PDF it serves."
    :summary "Fetch and read the contents of a URL."
    :prompt-file "prompts/tools/webfetch.md"
    :handler #'niwa-tool-web--fetch
    :args ((url string :required "The URL to fetch."))
    :async-p t
    :category "niwa-web"
    :groups (web)
    :read-only-p t
    :max-result-size 50000
    :get-domain (lambda (args)
                  (niwa-tool-web--url-host (plist-get args :url)))
    :renderer '((success . niwa-tool-web--render-fetch))))

(provide 'niwa-tool-web-registration)
;;; niwa-tool-web-registration.el ends here
