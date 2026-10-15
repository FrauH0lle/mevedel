/* viewer-artifact-discussions.js -- discussion picker that runs inside artifacts
 *
 * The artifact frame is sandboxed without allow-same-origin, so the viewer
 * cannot reach into it. The viewer inlines this function's source into the
 * frame's srcdoc ahead of the artifact. Inside the frame it only reports
 * where the user pointed and draws highlights and markers; the discussion text
 * is typed into the viewer, never into the frame, so an artifact cannot read
 * or forge what a participant writes.
 *
 * Frame -> viewer messages carry {mevedelDiscussion: TYPE, ...}:
 *   pick    {anchor, rect, context}  a click, word, text selection or box
 *   peek    {id, rect} / unpeek {}   pointer over / off a discussion marker
 *   open    {id, rect}               a marker was clicked
 *   located {found, missing}         which markers could be placed
 *   exit    {}                       Escape pressed in discussion mode
 * Viewer -> frame messages carry {t: TYPE, ...}:
 *   discussion-mode    {on}
 *   discussion-markers {markers: [{id, n, anchor, state, unread}]}
 *   discussion-draft   {active}
 *   discussion-reveal  {id}
 */
'use strict';

function mevedelArtifactDiscussionRuntime() {
  const LIMITS = {selector: 1000, label: 128, quote: 512, words: 40,
                  text: 2000, html: 4000, markers: 200, kids: 16};
  const HEADINGS = 'h1,h2,h3,h4,h5,h6,[role=heading]';
  const SKIP = 'script,style,template,noscript,head,[data-undiscussable]';
  const CONTROLS = 'button,input,textarea,select,option,a[href],summary,label';
  const DRAG_THRESHOLD = 6;
  const SIG_DISTANCE = 20;

  let active = false;
  let hover = null;          // {kind, el, rect, node, start, end, stack}
  let drag = null;           // {x, y, box, text}
  let draft = null;          // the picked target while the viewer composes
  let markers = [];          // [{id, n, anchor, state, unread}]
  let placed = new Map();    // id -> {el, range, region}
  let layerHost = null;
  let layer = null;
  let cursorStyle = null;
  let relocateTimer = 0;
  let frameRequest = 0;
  let segmenter;

  const post = (type, data) => {
    try { parent.postMessage(Object.assign({mevedelDiscussion: type}, data || {}), '*'); }
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
  // so a rewritten artifact still finds the element a discussion was about.
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
    layerHost = document.createElement('mevedel-discussion-layer');
    layerHost.setAttribute('data-undiscussable', '');
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
        animation:mevedel-discussion-spin .9s linear infinite;pointer-events:none}
      @keyframes mevedel-discussion-spin{to{transform:rotate(360deg)}}
      @media (prefers-reduced-motion:reduce){.pin[data-state=working]::after{
        animation:none;border-style:dashed;border-color:#d97757}}
      .pin[data-unread]::before{content:"";position:absolute;top:-5px;right:-5px;
        width:8px;height:8px;border:2px solid #fff;border-radius:50%;background:#3b82f6}
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
      if (marker.unread === true) pin.dataset.unread = 'true';
      else delete pin.dataset.unread;
      pin.textContent = String(marker.n);
      pin.setAttribute('aria-label', `Discussion ${marker.n}`
        + (marker.state === 'working' ? ', assistant working' : '')
        + (marker.unread === true ? ', new' : ''));
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

  addEventListener('scroll', repaint, true);
  addEventListener('resize', () => {
    repaint();
    scheduleRelocate();
  });

  addEventListener('message', event => {
    if (event.source !== parent) return;
    const data = event.data;
    if (!data || typeof data !== 'object') return;
    if (data.t === 'discussion-mode') {
      setActive(data.on === true);
    } else if (data.t === 'discussion-draft') {
      if (data.active !== true) {
        draft = null;
        repaint();
      }
    } else if (data.t === 'discussion-markers' && Array.isArray(data.markers)) {
      markers = data.markers.slice(0, LIMITS.markers).filter(marker =>
        marker && typeof marker.id === 'string' && marker.anchor
          && typeof marker.anchor.selector === 'string');
      relocate();
    } else if (data.t === 'discussion-reveal' && typeof data.id === 'string') {
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

// Owns discussion mode, the markers and the discussions sidebar for the
// artifact shown in the panel. Discussions live in the host's per-artifact
// store; the transcript supplies the assistant's answers. Frame messages are
// untrusted: only the current frame's window is heard, and every field is
// bounded before it is used or sent.
function createArtifactDiscussionController(options) {
  const {send, el, toggle, sidebarToggle, side, sideRoot, count, close, peek, reveal,
         canDiscuss} = options;
  const busy = typeof options.busy === 'function' ? options.busy : () => false;
  const MAX_MESSAGE_BYTES = 10000;
  // The discussion about the whole artifact: the list's first row, without
  // a marker.
  const GENERAL = 'general';
  const NEW = 'new';
  const view = {frame: null, id: null, name: null, mode: false, draft: null};
  let records = [];
  let queued = [];
  let stored = [];            // the host store's discussions for view.name
  const pending = new Map();  // discussionId -> local discussion awaiting the store
  const drafts = new Map();   // sidebar key -> {text, toAssistant}
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

  // One discussion request; resolves with the host's reply fields.
  async function request(fields) {
    const reqId = ++requestSequence;
    const answer = new Promise((resolve, reject) => inflight.set(reqId, {resolve, reject}));
    const ok = await send(Object.assign({t: 'discussion', reqId}, fields));
    if (!ok) {
      inflight.delete(reqId);
      throw new Error('Connection lost; try again.');
    }
    return answer;
  }

  // -- Threads ----------------------------------------------------------

  const item = () => `artifact:${view.name}`;
  const mine = shared => !!shared && shared.kind === 'artifact' && shared.itemId === item()
    && typeof shared.discussionId === 'string';

  // Every discussion's latest request in the transcript, the discussion of
  // the room's latest user turn, the discussions in this guest's queue, and
  // the records about this artifact.  One pass serves every discussion:
  // streaming republishes markers for each record update.  Cleared whenever
  // records, queue, activity or the artifact change.
  let threads = null;
  function threadStates() {
    if (threads) return threads;
    const byId = new Map();
    const about = [];
    let current = null;
    let latest = null;
    for (const record of records) {
      if (!record) continue;
      if (record.kind === 'user') {
        current = mine(record.shared) ? {state: 'sent', recordId: record.id} : null;
        latest = current && record.shared.discussionId;
        if (current) byId.set(latest, current);
      } else if (current && current.state === 'sent' && record.kind === 'assistant'
                 && typeof record.text === 'string' && record.text.trim()) {
        current.state = 'answered';
      }
      if (current) about.push(record);
    }
    const queuedIds = new Set(queued.filter(entry => entry && mine(entry.shared))
                                    .map(entry => entry.shared.discussionId));
    return (threads = {byId, latest, queuedIds, about});
  }

  // What the assistant made of discussion ID: its latest request in the
  // transcript or this guest's queue.
  function assistantState(id) {
    const {byId, latest, queuedIds} = threadStates();
    const found = byId.get(id);
    const state = queuedIds.has(id) ? 'queued' : found ? found.state : '';
    // The session's running turn is the room's latest user turn: while it is
    // this thread's request the assistant is working on it, through its first
    // reply text and on to the turn's end.
    const working = state === 'queued' || (latest === id && busy());
    return {state, recordId: found ? found.recordId : null, working};
  }

  const messageView = message => ({
    id: String(message && message.id || ''),
    actor: String(message && message.actor || '').replace(/^Guest: /, ''),
    text: String(message && message.text || ''),
    alone: !!message && message.alone === true});

  // The artifact's discussions: the whole artifact's first, then those
  // anchored to a part of it, resolved ones included.
  function discussions() {
    if (!view.name) return [];
    const general = stored.find(discussion => discussion && discussion.id === GENERAL);
    const list = [Object.assign({id: GENERAL, general: true, anchor: null, resolved: false,
                                 sessionName: general && typeof general.sessionName === 'string'
                                   ? general.sessionName : '',
                                 messages: general && Array.isArray(general.messages)
                                   ? general.messages.map(messageView) : []},
                                assistantState(GENERAL))];
    const seen = new Set([GENERAL]);
    for (const discussion of stored) {
      if (!discussion || typeof discussion.id !== 'string' || seen.has(discussion.id)) continue;
      seen.add(discussion.id);
      pending.delete(discussion.id);
      const anchor = cleanAnchor(discussion.target);
      const messages = Array.isArray(discussion.messages) ? discussion.messages.map(messageView) : [];
      if (!anchor || !messages.length) continue;
      list.push(Object.assign({id: discussion.id, anchor, messages,
                               resolved: discussion.resolved === true,
                               sessionName: typeof discussion.sessionName === 'string'
                                 ? discussion.sessionName : ''},
                              assistantState(discussion.id)));
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
  const open = list => list.filter(discussion => !discussion.general && !discussion.resolved);

  // -- Sidebar ----------------------------------------------------------

  const sidebar = window.mevedelDiscussionSidebar.create({
    root: sideRoot, noun: 'artifact',
    canWrite: () => !!view.frame && canDiscuss(),
    visible: () => !side.hidden,
    draft: {
      get: key => drafts.get(key),
      set: (key, patch) => drafts.set(key, Object.assign({}, drafts.get(key), patch)),
    },
    submit: (key, message) => submit(key, message),
    // The list's foot points at a part of the artifact to discuss.
    startNew: () => {
      if (innerWidth < 800) showSide(false);
      setMode(true);
    },
    reveal: id => {
      post({t: 'discussion-reveal', id});
      if (innerWidth < 800) showSide(false);
    },
    revealLabel: 'Show in artifact',
    menu: discussion => {
      const entries = [];
      if (discussion.recordId && typeof reveal === 'function') {
        entries.push({label: 'Show in chat', run: () => reveal(discussion.recordId)});
      }
      if (!discussion.general && canDiscuss()
          && stored.some(entry => entry && entry.id === discussion.id)) {
        entries.push({label: discussion.resolved ? 'Reopen' : 'Resolve', reopen: discussion.resolved,
                      title: discussion.resolved ? '' : 'Mark this discussion done for everyone; its marker goes away',
                      run: () => resolve(discussion.id, !discussion.resolved)});
      }
      return entries;
    },
    extras: discussion => ({
      status: discussion.state === 'queued' ? 'Queued for the assistant'
        : discussion.working ? 'The assistant is working on this'
        // The session that answers this discussion, wherever it was written.
        : discussion.sessionName ? `Answered in ${discussion.sessionName}` : '',
      tone: discussion.working ? 'working' : ''}),
    selectHint: '',
    leaveNew: () => clearDraft(),
    onRender: () => publishMarkers(),
  });

  function showSide(shown) {
    side.hidden = !shown;
    if (sidebarToggle) sidebarToggle.setAttribute('aria-expanded', shown ? 'true' : 'false');
    sidebar.update();
  }

  function clearDraft() {
    view.draft = null;
    drafts.delete(NEW);
    post({t: 'discussion-draft', active: false});
  }

  // The message to send, '' for a selection sent alone, or null when there
  // is nothing to send: only the assistant takes a selection without words.
  function checkedText(text, toAssistant, alone) {
    text = text.trim();
    if (!text) {
      if (alone && toAssistant) return '';
      sidebar.notice(alone ? 'Write a message, or ask the assistant.' : 'Write a message first.', true);
      return null;
    }
    if (new TextEncoder().encode(text).length > MAX_MESSAGE_BYTES) {
      sidebar.notice('Message too long.', true);
      return null;
    }
    return text;
  }

  async function submit(key, {text: written, toAssistant}) {
    const alone = key === NEW;
    const text = checkedText(written, toAssistant, alone);
    if (text === null || !view.id || (alone && !view.draft)) return false;
    sidebar.notice(toAssistant ? 'Posting and sending…' : 'Posting…');
    try {
      if (key === NEW) {
        const draft = view.draft;
        const discussionId = newId();
        await request({action: 'post', item: view.id, discussionId, messageId: newId(), text,
                       target: draft.anchor, context: draft.context, toAssistant});
        if (!stored.some(discussion => discussion && discussion.id === discussionId)) {
          pending.set(discussionId, {id: discussionId, name: view.name, anchor: draft.anchor,
                                     resolved: false, toAssistant,
                                     messages: [{id: 'local', actor: 'You', text, alone: !text}]});
        }
        clearDraft();
        setMode(false);
        refresh();
        sidebar.notice('');
        return discussionId;
      }
      // The first message about the whole artifact starts its discussion.
      await request(key === GENERAL
        ? {action: 'post', item: view.id, target: {kind: 'whole'}, messageId: newId(),
           text, toAssistant}
        : {action: 'reply', item: view.id, discussionId: key, messageId: newId(),
           text, toAssistant});
      drafts.delete(key);
      sidebar.notice('');
      return undefined;
    } catch (error) {
      sidebar.notice(error.message, true);
      return false;
    }
  }

  async function resolve(id, resolved) {
    try {
      await request({action: 'resolve', item: view.id, discussionId: id, resolved});
    } catch (error) {
      sidebar.notice(error.message, true);
    }
  }

  // Streaming redraws call this for every record update; the frame only
  // hears about markers when they actually changed.
  let lastMarkers = '';
  function publishMarkers(force) {
    if (!view.frame) return;
    const markers = open(discussions()).map((discussion, index) => ({
      id: discussion.id, n: index + 1, anchor: discussion.anchor,
      state: discussion.working ? 'working' : discussion.state || 'posted',
      unread: sidebar.unread(discussion.id)}));
    const encoded = JSON.stringify(markers);
    if (force === true || encoded !== lastMarkers) {
      lastMarkers = encoded;
      post({t: 'discussion-markers', markers});
    }
    updateToggles();
  }

  // The sidebar's view of the discussions, then the markers it marks.
  function refresh(settled = false) {
    const list = discussions();
    const pins = new Map(open(list).map((discussion, index) => [discussion.id, index + 1]));
    sidebar.update({
      discussions: list.map(discussion => ({
        id: discussion.id, general: discussion.general === true, pin: pins.get(discussion.id),
        title: discussion.anchor ? discussion.anchor.label || 'Part of the artifact' : '',
        quote: discussion.anchor && discussion.anchor.kind !== 'word' ? discussion.anchor.quote || '' : '',
        resolved: discussion.resolved, revealable: !discussion.resolved,
        recordId: discussion.recordId, state: discussion.state, working: discussion.working,
        sessionName: discussion.sessionName,
        messages: discussion.messages})),
      records: threadStates().about,
      selection: view.frame && canDiscuss() ? {label: 'part of the artifact'} : null,
      settled,
    });
  }

  function updateToggles() {
    const shown = !!view.frame;
    const count_ = open(discussions()).length;
    if (sidebarToggle) {
      sidebarToggle.hidden = !shown;
      sidebarToggle.classList.toggle('unread', sidebar.unreadCount() > 0);
      sidebarToggle.setAttribute('aria-label', count_ ? `Discussions, ${count_} open` : 'Discussions');
    }
    if (count) {
      count.textContent = String(count_);
      count.hidden = !count_;
    }
    if (!toggle) return;
    const allowed = shown && canDiscuss();
    toggle.hidden = !allowed;
    toggle.setAttribute('aria-pressed', view.mode ? 'true' : 'false');
    toggle.textContent = view.mode ? 'Discussing…' : 'Discuss';
    toggle.title = view.mode
      ? 'Click, drag across text, or drag a box around what to discuss. Escape stops.'
      : 'Point at part of this artifact and discuss it';
  }

  // A marker under the pointer shows its latest message beside it.
  function showPeek(id, rect) {
    const discussion = discussions().find(entry => entry.id === id);
    if (!peek || !discussion || !view.frame) return;
    const latest = discussion.messages[discussion.messages.length - 1];
    peek.replaceChildren(el('small', '', latest ? latest.actor || 'Guest' : ''),
                         el('p', '', latest ? latest.alone ? 'Sent the selection as the request'
                                       : latest.text : ''));
    peek.hidden = false;
    const frameBox = view.frame.getBoundingClientRect();
    const host = peek.offsetParent ? peek.offsetParent.getBoundingClientRect() : {left: 0, top: 0, width: innerWidth, height: innerHeight};
    const left = frameBox.left - host.left + rect.left + rect.width + 6;
    peek.style.left = `${Math.max(8, Math.min(left, host.width - peek.offsetWidth - 8))}px`;
    peek.style.top = `${Math.max(8, Math.min(frameBox.top - host.top + rect.top, host.height - peek.offsetHeight - 8))}px`;
  }
  const hidePeek = () => { if (peek) peek.hidden = true; };

  // -- Mode and wiring --------------------------------------------------

  function setMode(on) {
    view.mode = on === true && !!view.frame && canDiscuss();
    post({t: 'discussion-mode', on: view.mode});
    if (!view.mode && !view.draft) post({t: 'discussion-draft', active: false});
    updateToggles();
  }

  function onMessage(event) {
    if (!view.frame || event.source !== view.frame.contentWindow) return;
    const data = event.data;
    if (!data || typeof data !== 'object' || typeof data.mevedelDiscussion !== 'string') return;
    const type = data.mevedelDiscussion;
    if (type === 'pick') {
      if (!view.mode) return;
      const anchor = cleanAnchor(data.anchor);
      if (!anchor || !cleanRect(data.rect)) return;
      const context = data.context && typeof data.context === 'object' ? data.context : {};
      view.draft = {anchor, context: {
        text: bounded(context.text, 4000) ? context.text : '',
        html: bounded(context.html, 8000) ? context.html : '',
      }};
      sidebar.update({fresh: {
        label: anchor.label || 'Part of the artifact',
        quote: anchor.kind !== 'word' ? anchor.quote || '' : '',
        placeholder: 'What should change here? Leave empty to send the selection as the request.'}});
      sidebar.show({kind: NEW});
      showSide(true);
      sidebar.focus();
    } else if (type === 'peek') {
      const rect = cleanRect(data.rect);
      if (typeof data.id === 'string' && rect) showPeek(data.id, rect);
    } else if (type === 'unpeek') {
      hidePeek();
    } else if (type === 'open') {
      if (typeof data.id !== 'string' || !discussions().some(entry => entry.id === data.id)) return;
      hidePeek();
      sidebar.show({kind: 'thread', id: data.id});
      showSide(true);
    } else if (type === 'exit') {
      setMode(false);
    }
  }
  if (typeof addEventListener === 'function') addEventListener('message', onMessage);
  if (toggle) toggle.addEventListener('click', () => setMode(!view.mode));
  if (sidebarToggle) sidebarToggle.addEventListener('click', () => showSide(side.hidden));
  if (close) close.addEventListener('click', () => showSide(false));
  // Escape drops a new discussion, or else closes the sidebar.
  side.addEventListener('keydown', event => {
    if (event.key !== 'Escape') return;
    event.preventDefault();
    if (sidebar.view().kind === NEW) sidebar.show({kind: 'list'});
    else {
      showSide(false);
      if (sidebarToggle) sidebarToggle.focus();
    }
  });

  function load(name) {
    request({action: 'list', item: view.id}).then(reply => {
      if (view.name === name) {
        stored = Array.isArray(reply.discussions) ? reply.discussions : [];
        // What the artifact opens with is not news.
        refresh(true);
      }
    }).catch(() => {});
  }

  function reset() {
    view.mode = false;
    view.draft = null;
    stored = [];
    threads = null;
    lastMarkers = '';
    drafts.clear();
    hidePeek();
    sidebar.reset();
  }

  return Object.freeze({
    // A newly rendered HTML frame for record ID named NAME.
    attach(frame, id, name) {
      view.frame = frame;
      view.id = id;
      view.name = name;
      reset();
      frame.addEventListener('load', () => publishMarkers(true));
      refresh();
      load(name);
    },
    detach() {
      view.frame = null;
      view.id = null;
      view.name = null;
      reset();
      side.hidden = true;
      updateToggles();
    },
    records(next) {
      records = Array.isArray(next) ? next : [];
      threads = null;
      refresh();
    },
    queue(entries) {
      queued = Array.isArray(entries) ? entries : [];
      threads = null;
      refresh();
    },
    // The session's activity changed; working markers follow it.
    activity() {
      threads = null;
      refresh();
    },
    // The host store changed for the item the frame names.
    stored(frame) {
      if (frame && view.name && frame.item === item() && Array.isArray(frame.discussions)) {
        stored = frame.discussions;
        refresh();
      }
    },
    // A message to the assistant in the general discussion of artifact ID
    // (a card or artifact:STORE), with attachment IMAGES.
    discuss(id, text, images = []) {
      return request({action: 'post', item: id, target: {kind: 'whole'}, messageId: newId(),
                      text, toAssistant: true, ...(images.length ? {images} : {})});
    },
    // The host's answer to a discussion frame.
    handle(frame) {
      const entry = inflight.get(frame.reqId);
      if (!entry) return;
      inflight.delete(frame.reqId);
      if (typeof frame.error === 'string') entry.reject(new Error(frame.error));
      else entry.resolve(frame);
    },
    onMessage,
    discussions,
  });
}

window.mevedelArtifactDiscussions = Object.freeze({
  runtime: mevedelArtifactDiscussionRuntime,
  // The source inlined into each artifact frame's srcdoc.
  script: () => '<script>(' + mevedelArtifactDiscussionRuntime.toString() + ')()<\/script>',
  create: createArtifactDiscussionController,
});
