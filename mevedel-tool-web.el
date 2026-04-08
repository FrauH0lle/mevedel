;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;; Web-related tool definitions: WebSearch, WebFetch, YouTube.
;; These are thin wrappers around gptel-agent functions.

;;; Code:

;; `gptel-agent-tools'
(declare-function gptel-agent--read-url "ext:gptel-agent-tools" (tool-cb url))
(declare-function gptel-agent--web-search-eww "ext:gptel-agent-tools" (tool-cb query &optional count))
(declare-function gptel-agent--yt-read-url "ext:gptel-agent-tools" (callback url))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))


;;
;;; Register Tools

;;;###autoload
(defun mevedel-tool-web--register ()
  "Define web-related read-only tools for mevedel."

  (gptel-make-tool
   :name "WebSearch"
   :function #'gptel-agent--web-search-eww
   :description "Search the web and return top results with URLs and excerpts.

Returns up to 5 search results from the search engine (typically
DuckDuckGo). Each result includes `:url` and `:excerpt` keys.

Uses the Emacs web browser (eww) - no API key required.

Timeout after 30 seconds per search.

### When to use `WebSearch`

- Finding recent documentation or resources online
- Looking up current information not in training data
- Discovering URLs for packages, libraries, or tools
- Researching error messages or unfamiliar concepts

### When NOT to use `WebSearch`

- Reading known URLs -> use `WebFetch`
- Fetching YouTube transcripts -> use `YouTube`
- Information likely in local codebase -> use `Grep` or delegate
- When offline access is required

### How to use `WebSearch`

- Query can be natural language or keywords
- Returns excerpts, not full page content
- Use `WebFetch` with returned URLs to read full content
- Note: `WebFetch` may not work on JavaScript-heavy sites
- Default returns 5 results, can specify different count

### Examples of good usage

<example>
- Find official documentation:
WebSearch(query=\"Emacs gptel library documentation\")
</example>

<example>
- Research error message:
WebSearch(query=\"elisp void-function error debugging\", count=3)
</example>

<example>
- Find package repository:
WebSearch(query=\"github ripgrep rust search tool\")
</example>"
   :args '((:name "query"
            :type string
            :description "The natural language search query, can be multiple words.")
           (:name "count"
            :type integer
            :description "Number of results to return (default 5)"
            :optional t))
   :include t
   :async t
   :category "mevedel")

  (gptel-make-tool
   :function #'gptel-agent--read-url
   :name "WebFetch"
   :description "Fetch and read the text content of a URL.

Returns the text content of the URL (not raw HTML) formatted for
reading. HTML is converted to readable text.

Request times out after 30 seconds.

### When to use `WebFetch`

- Reading documentation from a known URL
- Fetching content from URLs found via `WebSearch`
- Reading blog posts, articles, or static web pages
- Accessing online resources referenced in code

### When NOT to use `WebFetch`

- Searching for URLs -> use `WebSearch` first
- YouTube videos -> use `YouTube` tool instead
- JavaScript-heavy single-page applications (may not render)
- Large files or binary content

### How to use `WebFetch`

- Provide full URL including protocol (https://)
- Works best with static HTML pages
- Content returned as formatted text, not HTML
- May fail on sites requiring JavaScript to render

### Examples of good usage

<example>
- Read documentation page:
WebFetch(url=\"https://www.gnu.org/software/emacs/manual/html_node/elisp/\")
</example>

<example>
- Fetch article content:
WebFetch(url=\"https://example.com/blog/emacs-tips\")
</example>

### Examples of bad usage

<example>
- Trying to fetch without knowing URL:
WebFetch(url=\"emacs documentation\")
<reasoning>Use WebSearch to find the URL first</reasoning>
</example>"
   :args '((:name "url"
            :type "string"
            :description "The URL to read"))
   :async t
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "YouTube"
   :function #'gptel-agent--yt-read-url
   :description "Fetch YouTube video description and transcript.

Returns markdown formatted string with two sections:
- \"description\": Video description added by uploader
- \"transcript\": Video transcript in SRT format (timestamped)

### When to use `YouTube`

- Extracting content from YouTube tutorial videos
- Getting transcripts of conference talks or presentations
- Reading video descriptions for context or links
- Analyzing spoken content from educational videos

### When NOT to use `YouTube`

- Non-YouTube video platforms -> use `WebFetch` if available
- Videos without transcripts (will fail or return empty)
- Looking for video URLs -> use `WebSearch` first

### How to use `YouTube`

- Requires full YouTube URL
- URL format: \"https://www.youtube.com/watch?v=VIDEO_ID\"
- Transcript returned in SRT format with timestamps
- May fail if video has no transcript/captions available

### Examples of good usage

<example>
- Get tutorial content:
YouTube(url=\"https://www.youtube.com/watch?v=H2qJRnV8ZGA\")
</example>

<example>
- Extract conference talk:
YouTube(url=\"https://www.youtube.com/watch?v=dQw4w9WgXcQ\")
</example>

### Examples of bad usage

<example>
- Using partial URL:
YouTube(url=\"youtube.com/watch?v=ABC123\")
<reasoning>Need full URL with protocol: https://</reasoning>
</example>

<example>
- Non-YouTube video:
YouTube(url=\"https://vimeo.com/123456\")
<reasoning>Only works with YouTube URLs</reasoning>
</example>"
   :args '((:name "url"
            :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
            :type "string"))
   :category "mevedel"
   :async t
   :include t))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
