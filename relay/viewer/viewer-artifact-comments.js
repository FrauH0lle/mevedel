/* viewer-artifact-comments.js -- comment picker that runs inside artifacts
 *
 * The artifact frame is sandboxed without allow-same-origin, so the viewer
 * cannot reach into it. The viewer inlines this function's source into the
 * frame's srcdoc ahead of the artifact. Inside the frame it only reports
 * where the user pointed and draws highlights and markers; the comment text
 * is typed into the viewer, never into the frame, so an artifact cannot read
 * or forge what a participant writes.
 *
 * Frame -> viewer messages carry {mevedelComment: TYPE, ...}:
 *   pick    {anchor, rect, context}  a click, word, text selection or box
 *   draft   {rect}                   the picked target moved (scroll/resize)
 *   peek    {id, rect} / unpeek {}   pointer over / off a comment marker
 *   open    {id, rect}               a marker was clicked
 *   located {found, missing}         which markers could be placed
 *   exit    {}                       Escape pressed in comment mode
 * Viewer -> frame messages carry {t: TYPE, ...}:
 *   comment-mode    {on}
 *   comment-markers {markers: [{id, n, anchor, state}]}
 *   comment-draft   {active}
 *   comment-reveal  {id}
 */
'use strict';

function mevedelArtifactCommentRuntime() {
  const LIMITS = {selector: 1000, label: 128, quote: 512, words: 40,
                  text: 2000, html: 4000, markers: 200, kids: 16};
  const HEADINGS = 'h1,h2,h3,h4,h5,h6,[role=heading]';
  const SKIP = 'script,style,template,noscript,head,[data-uncommentable]';
  const CONTROLS = 'button,input,textarea,select,option,a[href],summary,label';
  const DRAG_THRESHOLD = 6;
  const SIG_DISTANCE = 20;

  let active = false;
  let hover = null;          // {kind, el, rect, node, start, end, stack}
  let drag = null;           // {x, y, box, text}
  let draft = null;          // the picked target while the viewer composes
  let markers = [];          // [{id, n, anchor, state}]
  let placed = new Map();    // id -> {el, range, region}
  let layerHost = null;
  let layer = null;
  let cursorStyle = null;
  let relocateTimer = 0;
  let frameRequest = 0;
  let segmenter;

  const post = (type, data) => {
    try { parent.postMessage(Object.assign({mevedelComment: type}, data || {}), '*'); }
    catch (_error) { /* the viewer went away */ }
  };

  // -- Geometry -----------------------------------------------------------

  const plainRect = rect => ({left: rect.left, top: rect.top,
                              width: rect.width, height: rect.height});
  const area = rect => Math.max(0, rect.width) * Math.max(0, rect.height);
  function overlap(a, b) {
    const width = Math.min(a.left + a.width, b.left + b.width) - Math.max(a.left, b.left);
    const height = Math.min(a.top + a.height, b.top + b.height) - Math.max(a.top, b.top);
    return width > 0 && height > 0 ? width * height : 0;
  }
  const inside = (rect, x, y) => x >= rect.left && x <= rect.left + rect.width
    && y >= rect.top && y <= rect.top + rect.height;
  const visible = el => {
    const rect = el.getBoundingClientRect();
    return rect.width > 0 && rect.height > 0;
  };

  // -- Target discovery ---------------------------------------------------

  const ours = node => !!layerHost && (node === layerHost || layerHost.contains(node));
  const skipped = el => !el || ours(el) || !!el.closest(SKIP);

  // The nearest box that lays out on its own, so an inline <b> or <code>
  // picks its paragraph and a word picks the text block it sits in.
  function blockOf(el) {
    for (let node = el; node && node !== document.body
         && node !== document.documentElement; node = node.parentElement) {
      if (node instanceof SVGElement) return node;
      const display = getComputedStyle(node).display;
      if (display !== 'inline' && display !== 'contents') return node;
    }
    return el;
  }

  function wordBounds(text, offset) {
    const low = Math.max(0, offset - 200);
    const slice = text.slice(low, offset + 200);
    if (typeof Intl === 'object' && typeof Intl.Segmenter === 'function') {
      segmenter ||= new Intl.Segmenter(undefined, {granularity: 'word'});
      const found = [];
      for (const part of segmenter.segment(slice)) {
        const start = low + part.index;
        const end = start + part.segment.length;
        if (part.isWordLike && offset >= start && offset <= end) found.push([start, end]);
      }
      return found;
    }
    const found = [];
    for (const match of slice.matchAll(/[\p{L}\p{N}\p{M}_'’-]+/gu)) {
      const start = low + match.index;
      const end = start + match[0].length;
      if (offset >= start && offset <= end) found.push([start, end]);
    }
    return found;
  }

  function caretAt(x, y) {
    if (document.caretPositionFromPoint) {
      const position = document.caretPositionFromPoint(x, y);
      return position && {node: position.offsetNode, offset: position.offset};
    }
    if (document.caretRangeFromPoint) {
      const range = document.caretRangeFromPoint(x, y);
      return range && {node: range.startContainer, offset: range.startOffset};
    }
    return null;
  }

  function wordAt(x, y) {
    const caret = caretAt(x, y);
    const node = caret && caret.node;
    if (!node || node.nodeType !== Node.TEXT_NODE || !node.parentElement) return null;
    const parentEl = node.parentElement;
    if (skipped(parentEl) || parentEl.closest(CONTROLS)) return null;
    const style = getComputedStyle(parentEl);
    if (style.userSelect === 'none') return null;
    for (const [start, end] of wordBounds(node.data, caret.offset)) {
      const range = document.createRange();
      range.setStart(node, start);
      range.setEnd(node, end);
      const rect = [...range.getClientRects()].find(r => inside(
        {left: r.left - 1, top: r.top - 1, width: r.width + 2, height: r.height + 2}, x, y));
      if (rect) {
        return {kind: 'word', el: blockOf(parentEl), node, start, end,
                rect: plainRect(rect)};
      }
    }
    return null;
  }

  // A page-sized container under the pointer is a background, not a
  // target: walk down to the child the pointer is actually over.
  function elementAt(x, y) {
    let el = document.elementFromPoint(x, y);
    if (!el || skipped(el)) return null;
    if (!(el instanceof SVGElement) || el instanceof SVGSVGElement) el = blockOf(el);
    const huge = node => {
      const rect = node.getBoundingClientRect();
      return node === document.body || node === document.documentElement
        || (rect.height >= innerHeight * 0.8 && rect.width >= innerWidth * 0.8);
    };
    for (let guard = 0; guard < 12 && el && huge(el); guard++) {
      const next = [...el.children].find(child => !skipped(child) && visible(child)
                                         && inside(child.getBoundingClientRect(), x, y));
      if (!next) return null;
      el = next;
    }
    if (!el || el === document.body || el === document.documentElement) return null;
    return {kind: 'element', el, rect: plainRect(el.getBoundingClientRect())};
  }

  function targetAt(x, y, preferElement) {
    return (!preferElement && wordAt(x, y)) || elementAt(x, y);
  }

  // Arrow Up widens the hovered target to its parent, Arrow Down narrows
  // it back, so a paragraph or a whole card is one key away from a word.
  function widen(target) {
    let el = target.kind === 'word' ? target.el : target.el.parentElement;
    while (el && el !== document.body && el !== document.documentElement
           && (!visible(el) || skipped(el))) el = el.parentElement;
    if (!el || el === document.body || el === document.documentElement) return null;
    return {kind: 'element', el, rect: plainRect(el.getBoundingClientRect()),
            stack: [...(target.stack || []), target]};
  }

  // -- Description --------------------------------------------------------

  const squash = text => (text || '').replace(/\s+/g, ' ').trim();
  function clip(text, limit) {
    const chars = Array.from(text);
    return chars.length <= limit ? text
      : chars.slice(0, Math.max(0, limit - 1)).join('').trimEnd() + '…';
  }
  const bytes = text => new TextEncoder().encode(text).length;

  function kindOf(el) {
    const tag = el.localName;
    if (/^h[1-6]$/.test(tag) || el.getAttribute('role') === 'heading') return 'heading';
    if (tag === 'td' || tag === 'th') return 'cell';
    if (tag === 'tr') return 'row';
    if (tag === 'li') return 'item';
    if (tag === 'table') return 'table';
    if (['img', 'picture', 'canvas', 'video'].includes(tag)) return 'image';
    if (tag === 'svg') return 'figure';
    if (el instanceof SVGElement) return 'shape';
    if (tag === 'button' || el.getAttribute('role') === 'button') return 'button';
    if (tag === 'a') return 'link';
    if (['input', 'select', 'textarea'].includes(tag)) return 'field';
    if (tag === 'figure') return 'figure';
    const own = [...el.childNodes].some(node => node.nodeType === Node.TEXT_NODE
                                        && /\S/.test(node.data));
    return own ? 'text' : 'block';
  }

  function wordsOf(el) {
    const label = el.getAttribute('aria-label') || el.getAttribute('alt')
      || el.getAttribute('title') || '';
    // innerText follows layout, so adjacent blocks read as separate words.
    const text = label || (typeof el.innerText === 'string' ? el.innerText : el.textContent);
    return clip(squash(text).replace(/"/g, '’'), LIMITS.words);
  }

  // The nearest heading before the target in document order names the
  // place; a target that contains headings is named by its first one.
  function placeOf(el) {
    const own = el.matches(HEADINGS) ? null : el.querySelector(HEADINGS);
    if (own && visible(own)) return clip(squash(own.textContent), LIMITS.words);
    let best = null;
    for (const head of document.querySelectorAll(HEADINGS)) {
      if (head === el) break;
      if (!(head.compareDocumentPosition(el) & Node.DOCUMENT_POSITION_FOLLOWING)) break;
      if (visible(head) && squash(head.textContent)) best = head;
    }
    return best ? clip(squash(best.textContent), LIMITS.words) : '';
  }

  function fitLabel(parts) {
    for (const limit of [LIMITS.words, 24, 12, 0]) {
      const words = parts.words ? clip(parts.words, limit) : '';
      const label = [parts.place, [parts.kind, words && `"${words}"`].filter(Boolean).join(' ')]
        .filter(Boolean).join(' › ');
      if (bytes(label) <= LIMITS.label) return label;
    }
    return clip(parts.kind, LIMITS.words);
  }

  function selectorOf(el) {
    const steps = [];
    for (let node = el, depth = 0; node && node.parentElement && depth < 10; depth++) {
      if (node.id && /^[A-Za-z_][\w-]{0,63}$/.test(node.id)
          && document.querySelectorAll('#' + CSS.escape(node.id)).length === 1) {
        steps.unshift('#' + CSS.escape(node.id));
        break;
      }
      let index = 1;
      for (let sibling = node.previousElementSibling; sibling;
           sibling = sibling.previousElementSibling) {
        if (sibling.localName === node.localName) index++;
      }
      steps.unshift(`${CSS.escape(node.localName)}:nth-of-type(${index})`);
      node = node.parentElement;
    }
    return steps.join(' > ').slice(0, LIMITS.selector);
  }

  // A 64-bit simhash of the element's text: small edits change few bits,
  // so a rewritten artifact still finds the element a comment was about.
  function fnv(text, seed) {
    let hash = seed >>> 0;
    for (let index = 0; index < text.length; index++) {
      hash ^= text.charCodeAt(index);
      hash = Math.imul(hash, 16777619) >>> 0;
    }
    return hash;
  }
  function simhash(text) {
    const clean = squash(text).slice(0, 4096);
    if (clean.length < 8) return '';
    const counts = new Array(64).fill(0);
    for (let index = 0; index + 4 <= clean.length; index++) {
      const gram = clean.slice(index, index + 4);
      const low = fnv(gram, 2166136261);
      const high = fnv(gram, 3735928559);
      for (let bit = 0; bit < 32; bit++) {
        counts[bit] += (low >>> bit) & 1 ? 1 : -1;
        counts[32 + bit] += (high >>> bit) & 1 ? 1 : -1;
      }
    }
    let low = 0;
    let high = 0;
    for (let bit = 0; bit < 32; bit++) {
      if (counts[bit] > 0) low |= 1 << bit;
      if (counts[32 + bit] > 0) high |= 1 << bit;
    }
    return (high >>> 0).toString(16).padStart(8, '0')
      + (low >>> 0).toString(16).padStart(8, '0');
  }
  function popcount(value) {
    let count = 0;
    for (let bits = value >>> 0; bits; bits &= bits - 1) count++;
    return count;
  }
  function distance(a, b) {
    return popcount(parseInt(a.slice(0, 8), 16) ^ parseInt(b.slice(0, 8), 16))
      + popcount(parseInt(a.slice(8), 16) ^ parseInt(b.slice(8), 16));
  }
  const signatureOf = el => {
    const hash = simhash(el.textContent || '');
    return hash ? {tag: el.localName, h: hash} : {tag: el.localName};
  };

  function textOffset(el, node, offset) {
    const range = document.createRange();
    range.selectNodeContents(el);
    range.setEnd(node, offset);
    return range.toString().length;
  }

  function excerpt(els) {
    let html = '';
    for (const el of els) {
      if (html.length >= LIMITS.html) break;
      const source = el.outerHTML || '';
      let clean = source;
      if (source.length <= 20000) {
        const copy = el.cloneNode(true);
        copy.querySelectorAll?.('script,style').forEach(node => node.remove());
        clean = copy.outerHTML || source;
      }
      html += (html ? '\n' : '') + clean;
    }
    return html.length > LIMITS.html ? html.slice(0, LIMITS.html) + '…' : html;
  }
  function textExcerpt(els) {
    return clip(squash(els.map(el => el.innerText || el.textContent || '').join('\n')),
                LIMITS.text);
  }

  function describe(target) {
    const el = target.el;
    const anchor = {selector: selectorOf(el), sig: signatureOf(el)};
    let label;
    let els = [el];
    if (target.kind === 'word') {
      const word = target.node.data.slice(target.start, target.end);
      anchor.quote = word.slice(0, LIMITS.quote);
      anchor.start = textOffset(el, target.node, target.start);
      label = fitLabel({place: placeOf(el), kind: 'word', words: word});
    } else if (target.kind === 'selection') {
      anchor.quote = target.quote;
      anchor.start = target.start;
      label = fitLabel({place: placeOf(el), kind: 'text', words: squash(target.quote)});
    } else if (target.kind === 'box') {
      anchor.region = target.region;
      anchor.count = target.kids.length;
      els = target.kids.length ? target.kids : [el];
      // One covered element is named as a click on it would name it.
      const only = target.kids.length === 1 ? target.kids[0] : null;
      label = only
        ? fitLabel({place: kindOf(only) === 'heading' ? '' : placeOf(only),
                    kind: kindOf(only), words: wordsOf(only)})
        : fitLabel({place: placeOf(el), kind: `area · ${target.kids.length} elements`});
    } else {
      label = fitLabel({place: kindOf(el) === 'heading' ? '' : placeOf(el),
                        kind: kindOf(el), words: wordsOf(el)});
    }
    anchor.kind = target.kind;
    anchor.label = label;
    return {anchor, context: {text: textExcerpt(els), html: excerpt(els)}};
  }

  // -- Box selection ------------------------------------------------------

  // The smallest element that holds most of the box, then the children it
  // actually covers, so a box around three cards names those three cards.
  function boxTarget(box) {
    const boxArea = area(box);
    if (boxArea <= 0) return null;
    let el = document.elementFromPoint(box.left + box.width / 2, box.top + box.height / 2);
    if (!el || ours(el)) return null;
    while (el && el !== document.documentElement
           && overlap(el.getBoundingClientRect(), box) < boxArea * 0.85) {
      el = el.parentElement;
    }
    if (!el || skipped(el)) return null;
    for (let guard = 0; guard < 32; guard++) {
      const covering = [...el.children].filter(child => !skipped(child) && visible(child)
        && overlap(child.getBoundingClientRect(), box) >= boxArea * 0.85);
      if (covering.length !== 1) break;
      el = covering[0];
    }
    if (el === document.documentElement) el = document.body;
    // Elements at least half inside the box count as covered; one the box
    // only crosses is searched for covered children instead, so a box over
    // two cards of a wide row finds those cards.
    // A heading or paragraph spans its column while its text may fill a
    // fraction of it; measure what is drawn, so a box around the words
    // covers the element.
    const drawn = child => {
      const outer = child.getBoundingClientRect();
      if (child instanceof SVGElement || !child.firstChild) return outer;
      const range = document.createRange();
      range.selectNodeContents(child);
      const inner = range.getBoundingClientRect();
      if (area(inner) <= 0) return outer;
      const left = Math.max(outer.left, inner.left);
      const top = Math.max(outer.top, inner.top);
      const right = Math.min(outer.right, inner.right);
      const bottom = Math.min(outer.bottom, inner.bottom);
      return right > left && bottom > top
        ? {left, top, width: right - left, height: bottom - top} : outer;
    };
    const covered = (parent, depth = 0, found = []) => {
      for (const child of parent.children) {
        if (found.length >= 64) break;
        if (skipped(child) || !visible(child)) continue;
        const rect = drawn(child);
        const shared = overlap(rect, box);
        if (!shared) continue;
        if (shared >= area(rect) * 0.5) found.push(child);
        else if (depth < 6) covered(child, depth + 1, found);
      }
      return found;
    };
    // A box drawn a little past a grid still means the grid's items, not
    // the grid as the page's one covered child.
    let kids = covered(el);
    for (let guard = 0; guard < 16 && kids.length === 1 && kids[0].children.length; guard++) {
      const inner = covered(kids[0]);
      if (!inner.length) break;
      kids = inner;
    }
    // A box inside one element that covers its drawn content, such as the
    // words of a wide heading, is about that element itself.
    if (!kids.length && el !== document.body && !skipped(el)) {
      const own = drawn(el);
      if (overlap(own, box) >= area(own) * 0.5) kids = [el];
    }
    if (kids.length && kids[0] !== el) {
      let common = kids[0].parentElement;
      while (common && common !== el && !kids.every(kid => common.contains(kid))) {
        common = common.parentElement;
      }
      if (common) el = common;
    }
    const outer = el.getBoundingClientRect();
    const fraction = value => Math.round(Math.min(1, Math.max(0, value)) * 1000) / 1000;
    const region = {
      x0: fraction((box.left - outer.left) / Math.max(1, outer.width)),
      y0: fraction((box.top - outer.top) / Math.max(1, outer.height)),
      x1: fraction((box.left + box.width - outer.left) / Math.max(1, outer.width)),
      y1: fraction((box.top + box.height - outer.top) / Math.max(1, outer.height)),
    };
    return {kind: 'box', el, kids: kids.slice(0, LIMITS.kids), region,
            rect: plainRect(box)};
  }

  function selectionTarget() {
    const selection = getSelection();
    if (!selection || selection.rangeCount === 0 || selection.isCollapsed) return null;
    const range = selection.getRangeAt(0);
    const quote = range.toString();
    if (!quote.trim()) return null;
    const common = range.commonAncestorContainer;
    const el = blockOf(common.nodeType === Node.ELEMENT_NODE ? common : common.parentElement);
    if (!el || skipped(el)) return null;
    const exact = quote.length <= LIMITS.quote;
    return {kind: 'selection', el,
            quote: exact ? quote : quote.slice(0, LIMITS.quote),
            start: exact ? textOffset(el, range.startContainer, range.startOffset) : 0,
            rect: plainRect(range.getBoundingClientRect())};
  }

  // -- Re-finding anchors -------------------------------------------------

  function matches(el, sig) {
    if (!el || el.localName !== sig.tag) return false;
    if (!sig.h) return true;
    const hash = simhash(el.textContent || '');
    return !!hash && distance(hash, sig.h) <= SIG_DISTANCE;
  }

  function locate(anchor) {
    const sig = anchor.sig || {};
    let el = null;
    try { el = document.querySelector(anchor.selector); } catch (_error) { el = null; }
    if (el && (ours(el) || !matches(el, sig))) el = null;
    if (!el && sig.h && sig.tag) {
      const candidates = document.getElementsByTagName(sig.tag);
      let best = SIG_DISTANCE + 1;
      if (candidates.length <= 2000) {
        for (const candidate of candidates) {
          if (skipped(candidate)) continue;
          const hash = simhash(candidate.textContent || '');
          if (!hash) continue;
          const gap = distance(hash, sig.h);
          if (gap < best) {
            best = gap;
            el = candidate;
          }
        }
      }
    }
    if (!el || !visible(el)) return null;
    const found = {el, range: null, region: anchor.region || null};
    if (typeof anchor.quote === 'string' && anchor.quote) {
      found.range = rangeFor(el, anchor.quote, anchor.start || 0);
    }
    return found;
  }

  function rangeFor(el, quote, start) {
    const text = el.textContent || '';
    let at = text.substr(start, quote.length) === quote ? start : -1;
    if (at < 0) {
      let best = -1;
      for (let index = text.indexOf(quote); index >= 0; index = text.indexOf(quote, index + 1)) {
        if (best < 0 || Math.abs(index - start) < Math.abs(best - start)) best = index;
      }
      at = best;
    }
    if (at < 0) return null;
    const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
    const range = document.createRange();
    let seen = 0;
    let started = false;
    for (let node = walker.nextNode(); node; node = walker.nextNode()) {
      const length = node.data.length;
      if (!started && at < seen + length) {
        range.setStart(node, at - seen);
        started = true;
      }
      if (started && at + quote.length <= seen + length) {
        range.setEnd(node, at + quote.length - seen);
        return range;
      }
      seen += length;
    }
    return null;
  }

  function rectOf(found) {
    if (found.range) {
      const rects = [...found.range.getClientRects()].filter(r => r.width > 0);
      if (rects.length) return plainRect(rects[0]);
    }
    const outer = found.el.getBoundingClientRect();
    if (found.region) {
      const r = found.region;
      return {left: outer.left + r.x0 * outer.width, top: outer.top + r.y0 * outer.height,
              width: (r.x1 - r.x0) * outer.width, height: (r.y1 - r.y0) * outer.height};
    }
    return plainRect(outer);
  }

  // -- Overlay ------------------------------------------------------------

  function ensureLayer() {
    if (layerHost && layerHost.isConnected) return;
    layerHost = document.createElement('mevedel-comment-layer');
    layerHost.setAttribute('data-uncommentable', '');
    layerHost.style.cssText = 'position:absolute;left:0;top:0;width:0;height:0;'
      + 'overflow:visible;margin:0;padding:0;border:0;z-index:2147483647;'
      + 'pointer-events:none;display:block';
    layer = layerHost.attachShadow({mode: 'closed'});
    const style = document.createElement('style');
    style.textContent = `
      .hl{position:absolute;box-sizing:border-box;border:1.5px solid #3b82f6;
        background:rgba(59,130,246,.12);border-radius:3px;pointer-events:none}
      .hl.word{border-width:0 0 2px;border-radius:2px;background:rgba(59,130,246,.18)}
      .hl.draft{border-color:#d97757;background:rgba(217,119,87,.12)}
      .hl.box{border-style:dashed}
      .pin{position:absolute;box-sizing:border-box;width:24px;height:24px;margin:0;
        padding:0;border:2px solid #fff;border-radius:12px 12px 12px 2px;
        background:#d97757;color:#fff;font:600 11px/20px system-ui,sans-serif;
        text-align:center;cursor:pointer;pointer-events:auto;
        box-shadow:0 1px 4px rgba(0,0,0,.35);transform:translate(-4px,-100%)}
      .pin[data-state=queued]{background:#6b7280}
      .pin[data-state=answered]{background:#2f855a}
      .pin[data-state=working]::after{content:"";position:absolute;inset:-6px;
        box-sizing:border-box;border-radius:50%;border:2.5px solid transparent;
        border-top-color:#d97757;border-right-color:#d97757;
        animation:mevedel-comment-spin .9s linear infinite;pointer-events:none}
      @keyframes mevedel-comment-spin{to{transform:rotate(360deg)}}
      @media (prefers-reduced-motion:reduce){.pin[data-state=working]::after{
        animation:none;border-style:dashed;border-color:#d97757}}
      .pin:focus-visible{outline:2px solid #3b82f6;outline-offset:2px}`;
    layer.append(style);
    document.documentElement.append(layerHost);
  }

  function box(className, rect) {
    const node = document.createElement('div');
    node.className = className;
    node.style.left = `${rect.left + scrollX}px`;
    node.style.top = `${rect.top + scrollY}px`;
    node.style.width = `${Math.max(0, rect.width)}px`;
    node.style.height = `${Math.max(0, rect.height)}px`;
    return node;
  }

  // Highlights are redrawn on every change; markers are kept and only
  // moved, so a pointer resting on one does not see it replaced.
  let highlights = null;
  let pins = null;
  const pinNodes = new Map();

  function paint() {
    frameRequest = 0;
    ensureLayer();
    if (!highlights || !highlights.isConnected) {
      highlights = document.createElement('div');
      pins = document.createElement('div');
      layer.append(highlights, pins);
    }
    highlights.replaceChildren();
    if (active && hover && !drag) {
      const rect = hover.kind === 'word' ? hover.rect : plainRect(hover.el.getBoundingClientRect());
      highlights.append(box(`hl ${hover.kind === 'word' ? 'word' : ''}`, rect));
    }
    if (drag && drag.box) highlights.append(box('hl box', drag.box));
    if (draft) {
      const rect = draftRect();
      if (rect) highlights.append(box(`hl draft ${draft.kind === 'box' ? 'box' : ''}`, rect));
    }
    const shown = new Set();
    for (const marker of markers) {
      const found = placed.get(marker.id);
      if (!found || !found.el.isConnected) continue;
      shown.add(marker.id);
      let pin = pinNodes.get(marker.id);
      if (!pin) {
        pin = document.createElement('button');
        pin.type = 'button';
        pin.className = 'pin';
        const report = type => post(type, {id: marker.id,
                                            rect: plainRect(pin.getBoundingClientRect())});
        pin.addEventListener('pointerenter', () => report('peek'));
        pin.addEventListener('pointerleave', () => post('unpeek'));
        pin.addEventListener('click', event => {
          event.preventDefault();
          report('open');
        });
        pinNodes.set(marker.id, pin);
        pins.append(pin);
      }
      pin.dataset.state = marker.state;
      pin.textContent = String(marker.n);
      pin.setAttribute('aria-label', marker.state === 'working'
        ? `Comment ${marker.n}, assistant working` : `Comment ${marker.n}`);
      const rect = rectOf(found);
      const onText = !!found.range;
      pin.style.left = `${(onText ? rect.left : rect.left + rect.width - 20) + scrollX}px`;
      pin.style.top = `${rect.top + scrollY}px`;
    }
    for (const [id, pin] of pinNodes) {
      if (!shown.has(id)) {
        pin.remove();
        pinNodes.delete(id);
      }
    }
  }
  const repaint = () => {
    if (!frameRequest) frameRequest = requestAnimationFrame(paint);
  };

  function draftRect() {
    if (!draft) return null;
    if (draft.kind === 'box' || draft.kind === 'selection' || draft.kind === 'word') {
      if (draft.range) {
        const rects = [...draft.range.getClientRects()].filter(r => r.width > 0);
        if (rects.length) {
          const left = Math.min(...rects.map(r => r.left));
          const top = Math.min(...rects.map(r => r.top));
          const right = Math.max(...rects.map(r => r.right));
          const bottom = Math.max(...rects.map(r => r.bottom));
          return {left, top, width: right - left, height: bottom - top};
        }
      }
      if (draft.kind === 'box') return rectOf({el: draft.el, region: draft.region});
    }
    return draft.el.isConnected ? plainRect(draft.el.getBoundingClientRect()) : null;
  }

  function relocate() {
    relocateTimer = 0;
    placed = new Map();
    const found = [];
    const missing = [];
    for (const marker of markers) {
      const hit = locate(marker.anchor);
      if (hit) {
        placed.set(marker.id, hit);
        found.push(marker.id);
      } else missing.push(marker.id);
    }
    post('located', {found, missing});
    repaint();
  }
  const scheduleRelocate = () => {
    if (!relocateTimer) relocateTimer = setTimeout(relocate, 250);
  };

  // -- Input --------------------------------------------------------------

  function setCursor(kind) {
    if (!cursorStyle) return;
    const cursor = kind === 'word' ? 'text' : 'crosshair';
    cursorStyle.textContent = `html,html *{cursor:${cursor} !important}`;
  }

  function setActive(on) {
    active = on === true;
    hover = null;
    drag = null;
    if (active && !cursorStyle) {
      cursorStyle = document.createElement('style');
      (document.head || document.documentElement).append(cursorStyle);
      setCursor('element');
    } else if (!active && cursorStyle) {
      cursorStyle.remove();
      cursorStyle = null;
    }
    repaint();
  }

  const fromLayer = event => event.target === layerHost;
  function swallow(event) {
    if (!active || fromLayer(event)) return false;
    event.stopImmediatePropagation();
    return true;
  }

  function pick(target) {
    if (!target) return;
    const {anchor, context} = describe(target);
    draft = target;
    if (target.kind === 'word') {
      draft.range = document.createRange();
      draft.range.setStart(target.node, target.start);
      draft.range.setEnd(target.node, target.end);
    }
    if (target.kind === 'selection') {
      draft.range = getSelection().rangeCount ? getSelection().getRangeAt(0).cloneRange() : null;
    }
    hover = null;
    post('pick', {anchor, rect: draftRect() || target.rect, context});
    repaint();
  }

  addEventListener('pointermove', event => {
    if (!active) return;
    if (drag) {
      const dx = event.clientX - drag.x;
      const dy = event.clientY - drag.y;
      if (!drag.text && (drag.box || Math.hypot(dx, dy) >= DRAG_THRESHOLD)) {
        drag.box = {left: Math.min(drag.x, event.clientX), top: Math.min(drag.y, event.clientY),
                    width: Math.abs(dx), height: Math.abs(dy)};
        repaint();
      }
      return;
    }
    const target = targetAt(event.clientX, event.clientY, event.altKey);
    const same = hover && target && hover.el === target.el && hover.kind === target.kind
      && hover.node === target.node && hover.start === target.start;
    if (!same) {
      hover = target;
      setCursor(target && target.kind);
      repaint();
    }
  }, true);

  addEventListener('pointerdown', event => {
    if (!swallow(event) || event.button !== 0) return;
    const overText = !event.altKey && !!wordAt(event.clientX, event.clientY);
    drag = {x: event.clientX, y: event.clientY, box: null, text: overText,
            target: hover};
  }, true);

  addEventListener('mousedown', event => {
    if (!swallow(event)) return;
    // Text keeps its native selection; everywhere else a drag is a box.
    if (!drag || !drag.text) event.preventDefault();
  }, true);

  addEventListener('pointerup', event => {
    if (!swallow(event) || !drag) return;
    const current = drag;
    drag = null;
    if (current.box && area(current.box) > 0) {
      pick(boxTarget(current.box));
      return;
    }
    const selected = current.text ? selectionTarget() : null;
    if (selected) {
      pick(selected);
      return;
    }
    pick(current.target || targetAt(event.clientX, event.clientY, event.altKey));
  }, true);

  addEventListener('pointercancel', () => {
    drag = null;
    repaint();
  }, true);

  for (const type of ['click', 'auxclick', 'dblclick', 'contextmenu', 'submit',
                      'mouseup', 'touchstart', 'touchend']) {
    addEventListener(type, event => {
      if (swallow(event) && type !== 'mouseup' && type !== 'touchstart'
          && type !== 'touchend') event.preventDefault();
    }, true);
  }

  addEventListener('keydown', event => {
    if (!active) return;
    if (event.key === 'Escape') {
      event.stopImmediatePropagation();
      post('exit');
    } else if (event.key === 'ArrowUp' && hover) {
      event.preventDefault();
      event.stopImmediatePropagation();
      hover = widen(hover) || hover;
      setCursor(hover.kind);
      repaint();
    } else if (event.key === 'ArrowDown' && hover && hover.stack && hover.stack.length) {
      event.preventDefault();
      event.stopImmediatePropagation();
      hover = hover.stack[hover.stack.length - 1];
      setCursor(hover.kind);
      repaint();
    } else if (event.key === 'Enter' && hover) {
      event.preventDefault();
      event.stopImmediatePropagation();
      pick(hover);
    }
  }, true);

  let scrollTimer = 0;
  const moved = () => {
    repaint();
    if (draft && !scrollTimer) {
      scrollTimer = setTimeout(() => {
        scrollTimer = 0;
        const rect = draftRect();
        if (rect) post('draft', {rect});
      }, 60);
    }
  };
  addEventListener('scroll', moved, true);
  addEventListener('resize', () => {
    moved();
    scheduleRelocate();
  });

  addEventListener('message', event => {
    if (event.source !== parent) return;
    const data = event.data;
    if (!data || typeof data !== 'object') return;
    if (data.t === 'comment-mode') {
      setActive(data.on === true);
    } else if (data.t === 'comment-draft') {
      if (data.active !== true) {
        draft = null;
        repaint();
      }
    } else if (data.t === 'comment-markers' && Array.isArray(data.markers)) {
      markers = data.markers.slice(0, LIMITS.markers).filter(marker =>
        marker && typeof marker.id === 'string' && marker.anchor
          && typeof marker.anchor.selector === 'string');
      relocate();
    } else if (data.t === 'comment-reveal' && typeof data.id === 'string') {
      const found = placed.get(data.id);
      if (found && found.el.isConnected) {
        found.el.scrollIntoView({block: 'center'});
        repaint();
      }
    }
  });

  const observe = () => {
    new MutationObserver(records => {
      if (records.some(record => !ours(record.target))) scheduleRelocate();
    }).observe(document.documentElement, {childList: true, subtree: true,
                                          characterData: true});
  };
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
      observe();
      scheduleRelocate();
    });
  } else observe();
  addEventListener('load', scheduleRelocate);
}

// -- Viewer-side controller ------------------------------------------------

// Owns comment mode, the composer and the marker threads for the artifact
// shown in the panel. Comments live in the host's per-artifact store; the
// transcript supplies what the assistant made of those sent to it. Frame
// messages are untrusted: only the current frame's window is heard, and
// every field is bounded before it is used or sent.
function createArtifactCommentController(options) {
  const {send, el, body, toggle, flash, renderMarkdown, reveal, canComment} = options;
  const busy = typeof options.busy === 'function' ? options.busy : () => false;
  const MAX_COMMENT_BYTES = 10000;
  const view = {frame: null, id: null, name: null, mode: false,
                draft: null, composer: null, card: null, cardPinned: false};
  let records = [];
  let queued = [];
  let stored = [];            // the host store's comments for view.name
  const pending = new Map();  // commentId -> local comment awaiting the store
  const replyDrafts = new Map();
  let requestSequence = 0;
  const inflight = new Map(); // reqId -> {resolve, reject}

  const bounded = (value, limit) => typeof value === 'string' && value.length <= limit;
  const finite = value => typeof value === 'number' && Number.isFinite(value);
  function cleanRect(rect) {
    if (!rect || typeof rect !== 'object') return null;
    const {left, top, width, height} = rect;
    if (![left, top, width, height].every(finite)) return null;
    const cap = 1e6;
    if ([left, top, width, height].some(value => Math.abs(value) > cap)) return null;
    return {left, top, width: Math.max(0, width), height: Math.max(0, height)};
  }
  function cleanAnchor(anchor) {
    if (!anchor || typeof anchor !== 'object') return null;
    if (!bounded(anchor.selector, 1000) || !anchor.selector) return null;
    if (!bounded(anchor.label, 512)) return null;
    const kinds = ['word', 'selection', 'box', 'element'];
    const clean = {kind: kinds.includes(anchor.kind) ? anchor.kind : 'element',
                   selector: anchor.selector, label: anchor.label};
    const sig = anchor.sig;
    if (sig && typeof sig === 'object' && bounded(sig.tag, 32)
        && /^[a-z][a-z0-9-]*$/.test(sig.tag)) {
      clean.sig = {tag: sig.tag};
      if (typeof sig.h === 'string' && /^[0-9a-f]{16}$/.test(sig.h)) clean.sig.h = sig.h;
    }
    if (bounded(anchor.quote, 512) && anchor.quote) {
      clean.quote = anchor.quote;
      clean.start = Number.isInteger(anchor.start) && anchor.start >= 0
        && anchor.start <= 16777216 ? anchor.start : 0;
    }
    const region = anchor.region;
    if (region && typeof region === 'object'
        && ['x0', 'y0', 'x1', 'y1'].every(key => finite(region[key])
                                         && region[key] >= 0 && region[key] <= 1)
        && region.x1 > region.x0 && region.y1 > region.y0) {
      clean.region = {x0: region.x0, y0: region.y0, x1: region.x1, y1: region.y1};
      if (Number.isInteger(anchor.count) && anchor.count >= 0 && anchor.count <= 10000) {
        clean.count = anchor.count;
      }
    }
    return clean;
  }

  function post(message) {
    const target = view.frame && view.frame.contentWindow;
    if (target && typeof target.postMessage === 'function') target.postMessage(message, '*');
  }

  function newId() {
    if (typeof crypto === 'object' && typeof crypto.randomUUID === 'function') {
      return crypto.randomUUID();
    }
    const bytes = new Uint8Array(16);
    crypto.getRandomValues(bytes);
    return [...bytes].map(byte => byte.toString(16).padStart(2, '0')).join('');
  }

  // One artifact-comment request; resolves with the host's reply fields.
  async function request(fields) {
    const reqId = ++requestSequence;
    const answer = new Promise((resolve, reject) => inflight.set(reqId, {resolve, reject}));
    const ok = await send(Object.assign({t: 'artifact-comment', reqId}, fields));
    if (!ok) {
      inflight.delete(reqId);
      throw new Error('Connection lost; try again.');
    }
    return answer;
  }

  // -- Threads ----------------------------------------------------------

  // What the assistant made of a comment's thread: its latest request in
  // the transcript or this guest's queue, and the reply that followed.
  function assistantState(id) {
    let state = '';
    let recordId = null;
    let reply = '';
    for (let index = 0; index < records.length; index++) {
      const record = records[index];
      const shared = record && record.shared;
      if (!record || record.kind !== 'user' || !shared || shared.kind !== 'artifact'
          || shared.commentId !== id) continue;
      state = 'sent';
      recordId = record.id;
      reply = '';
      for (let next = index + 1; next < records.length; next++) {
        const later = records[next];
        if (!later) continue;
        if (later.kind === 'user') break;
        if (later.kind === 'assistant' && typeof later.text === 'string' && later.text.trim()) {
          reply = later.text;
          state = 'answered';
          break;
        }
      }
    }
    if (queued.some(entry => entry && entry.shared && entry.shared.kind === 'artifact'
                    && entry.shared.commentId === id)) state = 'queued';
    // Delivered and unanswered while the session runs a turn: working on it.
    // A turn that ended without a reply leaves the thread merely sent.
    const working = state === 'queued' || (state === 'sent' && busy());
    return {state, recordId, reply, working};
  }

  function comments() {
    if (!view.name) return [];
    const list = [];
    const seen = new Set();
    for (const comment of stored) {
      if (!comment || typeof comment.id !== 'string' || seen.has(comment.id)) continue;
      seen.add(comment.id);
      pending.delete(comment.id);
      const anchor = cleanAnchor(comment.anchor);
      if (!anchor || comment.resolved === true) continue;
      list.push(Object.assign({id: comment.id, anchor, text: String(comment.text || ''),
                               actor: String(comment.actor || ''),
                               replies: Array.isArray(comment.replies) ? comment.replies : []},
                              assistantState(comment.id)));
    }
    for (const local of pending.values()) {
      if (local.name === view.name && !seen.has(local.id)) {
        const known = assistantState(local.id);
        if (local.toAssistant && !known.state) known.working = true;
        list.push(Object.assign({}, local, known));
      }
    }
    return list;
  }

  // Streaming redraws call this for every record update; the frame only
  // hears about markers when they actually changed.
  let lastMarkers = '';
  function publishMarkers(force) {
    if (!view.frame) return;
    const list = comments();
    const markers = list.map((comment, index) => ({id: comment.id, n: index + 1,
                                                   anchor: comment.anchor,
                                                   state: comment.working ? 'working'
                                                     : comment.state || 'posted'}));
    const encoded = JSON.stringify(markers);
    if (force === true || encoded !== lastMarkers) {
      lastMarkers = encoded;
      post({t: 'comment-markers', markers});
    }
    if (view.card) {
      const shown = list.find(comment => comment.id === view.card.dataset.comment);
      if (!shown) closeCard();
      else if (view.card.dataset.version !== cardVersion(shown)) refreshCard(shown);
    }
    updateToggle(list.length);
  }

  function updateToggle(count) {
    if (!toggle) return;
    const allowed = !!view.frame && canComment();
    toggle.hidden = !allowed;
    toggle.setAttribute('aria-pressed', view.mode ? 'true' : 'false');
    toggle.textContent = view.mode ? 'Commenting…' : count ? `Comment · ${count}` : 'Comment';
    toggle.title = view.mode
      ? 'Click, drag across text, or drag a box around what to comment on. Escape stops.'
      : 'Point at part of this artifact and comment on it';
  }

  // -- Placement --------------------------------------------------------

  // Frame coordinates are the frame's own viewport; the panel body is the
  // positioned parent of the composer and the thread card.
  // Thread cards open beside their marker rather than over the target.
  function placeBeside(node, rect) {
    if (!view.frame || !body) return;
    const frameBox = view.frame.getBoundingClientRect();
    const bodyBox = body.getBoundingClientRect();
    const width = Math.min(340, Math.max(240, bodyBox.width - 24));
    node.style.width = `${width}px`;
    const originLeft = frameBox.left - bodyBox.left;
    const originTop = frameBox.top - bodyBox.top + body.scrollTop;
    let left = originLeft + rect.left + rect.width + 8;
    if (left + width > bodyBox.width - 8) left = originLeft + rect.left - width - 8;
    if (left < 8) {
      place(node, rect);
      return;
    }
    node.style.left = `${left}px`;
    const height = node.offsetHeight || 160;
    const top = Math.min(originTop + rect.top - 4,
                         body.scrollTop + bodyBox.height - height - 8);
    node.style.top = `${Math.max(body.scrollTop + 8, top)}px`;
  }

  function place(node, rect) {
    if (!view.frame || !body) return;
    const frameBox = view.frame.getBoundingClientRect();
    const bodyBox = body.getBoundingClientRect();
    const left = frameBox.left - bodyBox.left + (rect ? rect.left : 0);
    const top = frameBox.top - bodyBox.top + body.scrollTop + (rect ? rect.top + rect.height : 0);
    const width = Math.min(360, Math.max(240, bodyBox.width - 24));
    const clampedLeft = Math.max(8, Math.min(left, bodyBox.width - width - 8));
    node.style.width = `${width}px`;
    node.style.left = `${clampedLeft}px`;
    let placedTop = top + 8;
    const height = node.offsetHeight || 160;
    if (rect && placedTop + height > body.scrollTop + bodyBox.height) {
      placedTop = Math.max(8, frameBox.top - bodyBox.top + body.scrollTop + rect.top - height - 8);
    }
    node.style.top = `${placedTop}px`;
  }

  // -- Message forms ----------------------------------------------------

  // A textarea with a "Send to assistant" checkbox, on by default, and one
  // submit button: posting is one action, and a people-only note is one
  // untick away.
  function messageForm(className, placeholder, submitLabel, draft) {
    const form = el('form', className);
    const input = el('textarea', 'artifact-comment-input');
    input.placeholder = placeholder;
    input.rows = 3;
    input.setAttribute('aria-label', placeholder);
    if (draft) input.value = draft;
    const status = el('p', 'artifact-comment-status', '');
    status.setAttribute('role', 'status');
    const actions = el('div', 'artifact-comment-actions');
    const option = el('label', 'artifact-comment-assistant');
    const assistant = el('input');
    assistant.type = 'checkbox';
    assistant.checked = true;
    option.append(assistant, ' Send to assistant');
    const submit = el('button', 'btn', submitLabel);
    submit.type = 'submit';
    actions.append(option, submit);
    form.append(input, status, actions);
    input.addEventListener('keydown', event => {
      if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
        event.preventDefault();
        form.requestSubmit ? form.requestSubmit() : form.dispatchEvent(new Event('submit'));
      }
    });
    return {form, input, status, actions, assistant, submit};
  }

  function checkedText(parts) {
    const text = parts.input.value.trim();
    if (!text) return null;
    if (new TextEncoder().encode(text).length > MAX_COMMENT_BYTES) {
      parts.status.textContent = 'Message too long.';
      return null;
    }
    return text;
  }

  // -- Composer ---------------------------------------------------------

  function closeComposer() {
    if (view.composer) view.composer.remove();
    view.composer = null;
    view.draft = null;
    post({t: 'comment-draft', active: false});
  }

  function openComposer(anchor, rect, context) {
    closeCard();
    if (view.composer) view.composer.remove();
    view.draft = {anchor, rect, context};
    const parts = messageForm('artifact-comment-composer', 'What should change here?', 'Post');
    const {form, input, status, actions, assistant, submit} = parts;
    form.setAttribute('role', 'dialog');
    form.setAttribute('aria-label', 'Comment on this part of the artifact');
    form.prepend(el('p', 'artifact-comment-target', anchor.label || 'Selected part'));
    if (anchor.quote && anchor.kind !== 'word') {
      form.insertBefore(el('blockquote', 'artifact-comment-quote', anchor.quote), input);
    }
    const cancel = el('button', 'btn quiet', 'Cancel');
    cancel.type = 'button';
    cancel.addEventListener('click', closeComposer);
    actions.insertBefore(cancel, submit);
    input.addEventListener('keydown', event => {
      if (event.key === 'Escape') {
        event.preventDefault();
        closeComposer();
      }
    });
    form.addEventListener('submit', async event => {
      event.preventDefault();
      const text = checkedText(parts);
      if (!text || !view.draft || !view.id) return;
      const draft = view.draft;
      const commentId = newId();
      submit.disabled = true;
      status.textContent = assistant.checked ? 'Posting and sending…' : 'Posting…';
      try {
        await request({action: 'post', id: view.id, commentId, text, anchor: draft.anchor,
                       context: draft.context, toAssistant: assistant.checked});
        if (!stored.some(comment => comment && comment.id === commentId)) {
          pending.set(commentId, {id: commentId, name: view.name, anchor: draft.anchor, text,
                                  actor: 'You', replies: [], toAssistant: assistant.checked});
        }
        if (form === view.composer) {
          closeComposer();
          setMode(false);
        }
        publishMarkers();
      } catch (error) {
        if (form === view.composer) {
          submit.disabled = false;
          status.textContent = error.message;
        }
      }
    });
    body.append(form);
    view.composer = form;
    place(form, rect);
    input.focus();
  }

  // -- Thread card ------------------------------------------------------

  const cardVersion = comment => JSON.stringify(
    [comment.text, comment.replies.map(reply => reply.id), comment.state, comment.working,
     comment.reply]);

  function closeCard() {
    if (view.card) view.card.remove();
    view.card = null;
    view.cardPinned = false;
  }

  function fillCard(card, comment) {
    const reply = view.card === card && card.querySelector
      ? card.querySelector('.artifact-comment-input') : null;
    if (reply) replyDrafts.set(comment.id, reply.value);
    card.replaceChildren();
    card.dataset.version = cardVersion(comment);
    const status = comment.working && comment.state !== 'queued' ? 'Assistant working…'
      : {queued: 'Queued for the assistant', sent: 'Sent to assistant',
         answered: 'Answered'}[comment.state] || 'Posted';
    const head = el('div', 'artifact-comment-head');
    head.append(el('span', '', [comment.actor || 'Guest', status].join(' · ')));
    const close = el('button', 'artifact-comment-close', '×');
    close.type = 'button';
    close.title = 'Close';
    close.setAttribute('aria-label', 'Close');
    close.addEventListener('click', closeCard);
    head.append(close);
    card.append(head, el('p', 'artifact-comment-target', comment.anchor.label || ''));
    const thread = el('div', 'artifact-comment-thread');
    for (const message of [comment, ...comment.replies]) {
      const item = el('p', 'artifact-comment-text');
      item.append(el('strong', '', `${message.actor || 'Guest'}: `), String(message.text || ''));
      thread.append(item);
    }
    card.append(thread);
    if (comment.reply) {
      const excerpt = comment.reply.length > 1200
        ? comment.reply.slice(0, 1200).trimEnd() + '…' : comment.reply;
      const answer = renderMarkdown(excerpt);
      answer.className = 'prose artifact-comment-reply';
      card.append(answer);
    }
    if (!view.cardPinned) return;
    const actions = el('div', 'artifact-comment-actions');
    if (comment.recordId && typeof reveal === 'function') {
      const show = el('button', 'btn quiet', 'Show in chat');
      show.type = 'button';
      show.addEventListener('click', () => reveal(comment.recordId));
      actions.append(show);
    }
    if (canComment() && stored.some(item => item && item.id === comment.id)) {
      const resolve = el('button', 'btn quiet', 'Resolve');
      resolve.type = 'button';
      resolve.title = 'Mark this comment done for everyone; its marker goes away';
      resolve.addEventListener('click', async () => {
        resolve.disabled = true;
        try {
          await request({action: 'resolve', id: view.id, commentId: comment.id, resolved: true});
        } catch (error) {
          resolve.disabled = false;
          if (flash) flash(error.message);
        }
      });
      actions.append(resolve);
      const parts = messageForm('artifact-comment-reply-form', 'Reply…', 'Post reply',
                                replyDrafts.get(comment.id));
      parts.form.addEventListener('submit', async event => {
        event.preventDefault();
        const text = checkedText(parts);
        if (!text) return;
        parts.submit.disabled = true;
        parts.status.textContent = parts.assistant.checked ? 'Posting and sending…' : 'Posting…';
        try {
          await request({action: 'reply', id: view.id, commentId: comment.id, replyId: newId(),
                         text, toAssistant: parts.assistant.checked});
          replyDrafts.delete(comment.id);
          parts.input.value = '';
          parts.status.textContent = '';
        } catch (error) {
          parts.status.textContent = error.message;
        } finally {
          parts.submit.disabled = false;
        }
      });
      card.append(parts.form);
    }
    card.append(actions);
  }

  function refreshCard(comment) {
    if (view.card) fillCard(view.card, comment);
  }

  function openCard(id, rect, pinned) {
    const comment = comments().find(item => item.id === id);
    if (!comment) return;
    if (view.card && view.card.dataset.comment === id) {
      if (pinned && !view.cardPinned) {
        view.cardPinned = true;
        fillCard(view.card, comment);
      }
      return;
    }
    if (view.cardPinned && !pinned) return;
    closeCard();
    const card = el('section', 'artifact-comment-card');
    card.dataset.comment = id;
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-label', 'Comment thread');
    view.card = card;
    view.cardPinned = pinned === true;
    fillCard(card, comment);
    body.append(card);
    placeBeside(card, rect);
  }

  // -- Mode and wiring --------------------------------------------------

  function setMode(on) {
    view.mode = on === true && !!view.frame && canComment();
    post({t: 'comment-mode', on: view.mode});
    if (!view.mode && !view.composer) post({t: 'comment-draft', active: false});
    updateToggle(comments().length);
  }

  function onMessage(event) {
    if (!view.frame || event.source !== view.frame.contentWindow) return;
    const data = event.data;
    if (!data || typeof data !== 'object' || typeof data.mevedelComment !== 'string') return;
    const type = data.mevedelComment;
    if (type === 'pick') {
      if (!view.mode) return;
      const anchor = cleanAnchor(data.anchor);
      const rect = cleanRect(data.rect);
      if (!anchor || !rect) return;
      const context = data.context && typeof data.context === 'object' ? data.context : {};
      openComposer(anchor, rect, {
        text: bounded(context.text, 4000) ? context.text : '',
        html: bounded(context.html, 8000) ? context.html : '',
      });
    } else if (type === 'draft') {
      const rect = cleanRect(data.rect);
      if (rect && view.composer) place(view.composer, rect);
    } else if (type === 'peek' || type === 'open') {
      const rect = cleanRect(data.rect);
      if (typeof data.id === 'string' && rect) openCard(data.id, rect, type === 'open');
    } else if (type === 'unpeek') {
      if (view.card && !view.cardPinned) closeCard();
    } else if (type === 'exit') {
      setMode(false);
    }
  }
  if (typeof addEventListener === 'function') addEventListener('message', onMessage);
  if (toggle) toggle.addEventListener('click', () => setMode(!view.mode));

  function load(name) {
    request({action: 'list', id: view.id}).then(reply => {
      if (view.name === name) {
        stored = Array.isArray(reply.comments) ? reply.comments : [];
        publishMarkers();
      }
    }).catch(() => {});
  }

  return Object.freeze({
    // A newly rendered HTML frame for record ID named NAME.
    attach(frame, id, name) {
      closeComposer();
      closeCard();
      view.frame = frame;
      view.id = id;
      view.name = name;
      view.mode = false;
      stored = [];
      lastMarkers = '';
      frame.addEventListener('load', () => publishMarkers(true));
      updateToggle(0);
      load(name);
    },
    detach() {
      closeComposer();
      closeCard();
      view.frame = null;
      view.id = null;
      view.name = null;
      view.mode = false;
      stored = [];
      updateToggle(0);
    },
    records(next) {
      records = Array.isArray(next) ? next : [];
      publishMarkers();
    },
    queue(entries) {
      queued = Array.isArray(entries) ? entries : [];
      publishMarkers();
    },
    // The session's activity changed; working markers follow it.
    activity() {
      publishMarkers();
    },
    // The host store changed for artifact NAME.
    stored(frame) {
      if (frame && frame.artifact === view.name && Array.isArray(frame.comments)) {
        stored = frame.comments;
        publishMarkers();
      }
    },
    // A room message about the whole artifact record ID, with attachment
    // IMAGES, into its conversation.
    discuss(id, text, images = []) {
      return request({action: 'ask', id, questionId: newId(), text,
                      ...(images.length ? {images} : {})});
    },
    // The host's answer to an artifact-comment frame.
    handle(frame) {
      const entry = inflight.get(frame.reqId);
      if (!entry) return;
      inflight.delete(frame.reqId);
      if (typeof frame.error === 'string') entry.reject(new Error(frame.error));
      else entry.resolve(frame);
    },
    onMessage,
    comments,
  });
}

window.mevedelArtifactComments = Object.freeze({
  runtime: mevedelArtifactCommentRuntime,
  // The source inlined into each artifact frame's srcdoc.
  script: () => '<script>(' + mevedelArtifactCommentRuntime.toString() + ')()<\/script>',
  create: createArtifactCommentController,
});
