# Web tools review: mevedel vs. Codex and ccs (2026-10-04)

## Status

Implemented on 2026-10-04 (commits `fix(web): Parse DuckDuckGo results
directly`, `fix(web): Restrict schemes and re-check cross-host redirects`,
`fix(web): Return fetched content by type`, `feat(web): Keep link targets in
fetched pages`): findings 1-4, the content-type dispatch, the Accept header,
domain filters, render metadata, prompt updates and markdown links. Two
deviations from the recommendations below:

- No fetch cache. Keyed by URL and shared across sessions, it would serve
  content whose final host passed a redirect check under another permission
  context, and stale pages from local dev servers.
- No binary persistence. There is no binary-safe artifact publish; other
  binary content is a tool error. PDFs use `pdftotext` with no media fallback.

Also found during implementation: every response body began with the newline
that ends url-http's headers, and WebFetch accepted `file:` URLs directly and
through redirects.

Scope: `mevedel-tool-web.el` (WebSearch, WebFetch) compared with
`~/codex` (codex-rs) and `~/ccs` (src/tools/Web*Tool).

## How the references do it

**Codex** has no local fetcher. Web access is server-side:
- hosted Responses `{"type":"web_search"}` tool (modes disabled/cached/indexed/live,
  config `allowed_domains`, `context_size`, location), or
- feature-flagged `web.run`: one multi-command tool (`search_query`, `open {ref_id|url, lineno}`,
  `click {ref_id, id}`, `find {ref_id, pattern}`, PDF `screenshot`) proxied to OpenAI's
  `alpha/search`. Results use ref ids (`turn0search0`); the server sizes output to the
  model's tool-output token budget.
- Web output is flagged as external context (memory "pollution", guardian treats it as
  untrusted). Prompt: temporal-stability rule ("if >10% chance it changed, browse"),
  primary sources for technical questions, inline markdown citations, quote word limits.

**ccs** (Claude Code style):
- WebSearch = nested model call with Anthropic server tool `web_search_20250305`
  (`max_uses: 8`, `allowed_domains`/`blocked_domains`, mutually exclusive). Result text
  ends with a "REMINDER: include sources as markdown links". Prompt injects current month/year.
- WebFetch(url, prompt): axios, `maxRedirects: 0`, manual redirects: same host (± `www.`)
  followed up to 10, **cross-host returns "REDIRECT DETECTED … call WebFetch again"** so the
  new host gets its own permission check. http→https upgrade; rejects credentials in URL,
  single-label hosts, >2000-char URLs. `Accept: text/markdown, text/html, */*`, identifiable
  User-Agent. Turndown HTML→markdown only for `text/html`; other text raw; binary saved to
  disk with MIME extension and path reported. 15-min LRU cache (50 MB). Content capped at
  100k, then a small fast model applies the caller's `prompt` (with quote-length rules),
  skipped for preapproved docs domains serving markdown. ~95 preapproved docs hosts skip
  the permission prompt. UI: "Received 12.3KB (200 OK)", "Did N searches in Xs".

## Findings in mevedel (verified)

1. **Cross-host redirects bypass domain permissions.** `:get-domain` uses the original
   URL's host, but `url-retrieve` follows up to `url-max-redirections` (30) hops to any
   host. A permitted `WebFetch(example.com)` can land anywhere.
2. **Non-HTML text is destroyed.** Every body goes through `libxml-parse-html-region` + SHR.
   Batch test: `"# Title\n\n```elisp\n(defun foo ()\n  (bar))\n```\n- a\n- b <x>\n"` →
   `"# Title ```elisp (defun foo () (bar)) ``` - a - b "` (newlines collapsed, `<x>` lost).
   Affects raw GitHub files, `llms.txt`, JSON APIs, plain-text RFCs, markdown docs.
3. **No content-type handling.** PDFs/images/binaries are fed to the HTML parser.
   `mevedel-tool-media.el` already provides a tool-result media channel that Read uses.
4. **Search results are mostly duplicates with broken URLs.** Provider is whatever
   `eww-search-prefix` holds (EWW's global defcustom, default
   `https://duckduckgo.com/html/?q=`, which 302s to `html.duckduckgo.com`). The parser
   counts every `shr-url` link run as a result, and DDG renders title, icon, display URL
   and snippet as separate links to the same page. Live run for "emacs eww": 4 of 5
   "results" were the same page, leaving 2 distinct results. Unwrapping
   `//duckduckgo.com/l/?uddg=…&amp;rut=…` by substring-from-"http" leaves
   `&rut=<hash>` appended to every URL (`…/eww.html&rut=78664e…`), so WebFetch on a
   result URL requests a wrong path. No domain filtering or recency.
5. No status/content-type/final URL in render data; no cache; Emacs default User-Agent.

Already covered (no change needed): >50k results persist to `artifact://` with exact
`Read` continuation and `Grep` recovery — this is the composable equivalent of Codex's
`open lineno` / `find`.

## Recommendations

### Fix (correctness/safety)
- **Redirects:** bind `url-max-redirections` to 0 inside `mevedel-tool-web--retrieve` and
  handle 3xx there. Follow same-host / `www.`-toggle hops (cap 10) within the existing
  timeout. Cross-host: follow when permission rules already allow the target host,
  otherwise return a non-error result naming the target URL so a new WebFetch call goes
  through the normal permission pipeline.
- **Content-type dispatch** in `--retrieve`'s parse step:
  - `text/html`, `application/xhtml+xml` → current EWW readable path;
  - other `text/*`, JSON, XML, JS → decoded body verbatim;
  - `image/*` → tool-result media (if the model is capable), else persist + path;
  - `application/pdf` → `pdftotext` if available, else persist + path;
  - other binary → persist bytes, report type/size/address.
- Send `Accept: text/markdown, text/html;q=0.9, */*;q=0.8` — many docs hosts (Mintlify,
  Cloudflare, Vercel docs) then return markdown that needs no conversion.

### Improve (capability, small interface cost)
- **WebSearch domain filters:** optional `allowed_domains` / `blocked_domains`, rewritten
  to `site:` / `-site:` operators (and post-filtered); reject both at once.
- **Search backends:** a mevedel option holding an ordered list of backend functions
  (query, filters → list of title/URL/snippet), decoupled from `eww-search-prefix`.
  Built-in DDG HTML backend parses `result__a` / `result__snippet`, decodes the `uddg`
  query parameter properly, dedups by URL, returns ~8–10 results. Optional JSON backends
  (SearXNG — itself multi-engine, Brave, Kagi, Tavily) with keys via auth-source.
  Default policy: fall back to the next backend on error, empty result or DDG's bot
  challenge page; fan-out + URL-dedup merge as an opt-in. Model-facing WebSearch
  schema stays unchanged.
- **Render metadata:** status code, content type, byte size, final URL →
  header like `WebFetch: host — 12.3 KB, 200 text/html`.
- **Short TTL cache** (~15 min, keyed by URL after normalization) for repeated fetches by
  parallel agents. Cheap; invisible to the model.
- **Prompt:** add the temporal-stability "when to use" rule and "prefer primary sources;
  link the supporting page" to `websearch.md`. Check whether the date is already in the
  system environment before adding it to the tool prompt.

### Defer / skip
- **WebFetch `prompt` + secondary model (ccs):** saves context but hides the source text
  and needs a helper-model setting; persisted output + subagents already bound context.
- **Ref-id browsing (`open`/`click`/`find`, Codex):** enlarges the interface; Read/Grep on
  the persisted artifact already give pagination and find.
- **Provider-native search:** gptel does not parse server tool blocks yet
  (`gptel-openai-responses.el` "backend tools are not supported in gptel yet");
  needs upstream gptel work first.
- **Preapproved docs-domain list:** a policy choice for the user; can be done as default
  permission rules rather than special code.
- **Taint flag for memory writes:** worth revisiting with the memory module; the guardian
  already treats tool output as untrusted.
