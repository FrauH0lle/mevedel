Fetch readable text from a URL, an image or PDF it serves, or a YouTube video's
description and transcript.

### When to use `WebFetch`

- Read an online source whose URL is known, including a web search result.

### When NOT to use `WebFetch`

- Discovering an unknown URL; WebSearch can find candidate sources.
- Downloading archives or other binary files, or interacting with a site's
  JavaScript application.

### How to use `WebFetch`

- Pass a complete http or https URL. HTML becomes readable text rather
  than raw markup, with links as markdown `[text](url)` you can fetch in turn;
  markdown, JSON and other text come back verbatim. An image is
  attached when the model accepts it, and a PDF becomes its extracted text,
  usually with a saved copy Read can open.
  Script-rendered or authenticated content may be unavailable.
- YouTube retrieval depends on available video metadata and captions. A missing
  transcript or failed fetch does not establish what the video says.
- Underlying fetches have a 30-second timeout; a multi-stage retrieval can take
  longer. Large returned text is persisted with a bounded preview and retrieval
  address. The extracted text may omit parts of the original page.

### Examples of good usage

<example>
WebFetch(url="https://docs.python.org/3/library/json.html")
</example>

### Examples of bad usage

<example>
WebFetch(url="Python json documentation")
<reasoning>
This is a search query, not a URL. Use WebSearch to discover the address.
</reasoning>
</example>
