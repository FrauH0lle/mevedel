Fetch and read the text content of a URL.

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
WebFetch(url="https://www.gnu.org/software/emacs/manual/html_node/elisp/")
</example>

### Examples of bad usage

<example>
- Trying to fetch without knowing URL:
WebFetch(url="emacs documentation")
<reasoning>Use WebSearch to find the URL first</reasoning>
</example>
