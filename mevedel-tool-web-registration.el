;;; mevedel-tool-web-registration.el --- Web tool catalog -*- lexical-binding: t -*-

;;; Commentary:

;; Complete discovery metadata; implementation loads at first use.

;;; Code:

(require 'mevedel-tool-registry)

(autoload 'mevedel-tool-web--fetch "mevedel-tool-web")
(autoload 'mevedel-tool-web--render-fetch "mevedel-tool-web")
(autoload 'mevedel-tool-web--render-search "mevedel-tool-web")
(autoload 'mevedel-tool-web--render-transform "mevedel-tool-web")
(autoload 'mevedel-tool-web--url-host "mevedel-tool-web")
(autoload 'mevedel-tool-web--websearch "mevedel-tool-web")

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
    :description "Fetch a URL as readable text, or an image or PDF it serves."
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
    :renderer '((success . mevedel-tool-web--render-fetch))))

(provide 'mevedel-tool-web-registration)
;;; mevedel-tool-web-registration.el ends here
