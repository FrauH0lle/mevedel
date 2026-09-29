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
    return clip(squash(label || el.textContent).replace(/"/g, '’'), LIMITS.words);
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
      const place = placeOf(el);
      label = target.kids.length === 1
        ? fitLabel({place, kind: kindOf(target.kids[0]), words: wordsOf(target.kids[0])})
        : fitLabel({place, kind: `area · ${target.kids.length} elements`});
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
    const kids = [...el.children].filter(child => {
      if (skipped(child) || !visible(child)) return false;
      const rect = child.getBoundingClientRect();
      return overlap(rect, box) >= area(rect) * 0.5;
    });
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
      pin.setAttribute('aria-label', `Comment ${marker.n}`);
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
// shown in the panel. Frame messages are untrusted: only the current frame's
// window is heard, and every field is bounded before it is used or sent.
function createArtifactCommentController(options) {
  const {send, el, body, toggle, flash, renderMarkdown, reveal, canComment} = options;
  const MAX_COMMENT_BYTES = 10000;
  const HIDDEN_KEY = 'mevedel.artifactComments.hidden';
  const view = {frame: null, id: null, name: null, mode: false,
                draft: null, composer: null, card: null, cardPinned: false};
  let records = [];
  let queued = [];
  const pending = new Map();  // commentId -> local comment awaiting the host
  let requestSequence = 0;
  const inflight = new Map(); // reqId -> {commentId, done}
  let hidden = new Set();
  try {
    const saved = JSON.parse(localStorage.getItem(HIDDEN_KEY) || '[]');
    if (Array.isArray(saved)) hidden = new Set(saved.filter(id => typeof id === 'string'));
  } catch (_error) { hidden = new Set(); }

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

  // -- Comments from the transcript -------------------------------------

  // Delivered comments are guest prompts carrying artifact attribution; the
  // assistant turn that follows is the reply. Queued ones come from this
  // guest's queue, and just-sent ones from the local pending list.
  function comments() {
    if (!view.name) return [];
    const list = [];
    const seen = new Set();
    const all = records;
    for (let index = 0; index < all.length; index++) {
      const record = all[index];
      const shared = record && record.shared;
      if (!record || record.kind !== 'user' || !shared || shared.kind !== 'artifact'
          || shared.artifact !== view.name || typeof shared.questionId !== 'string'
          || seen.has(shared.questionId)) continue;
      const anchor = cleanAnchor(shared.anchor);
      if (!anchor) continue;
      seen.add(shared.questionId);
      let reply = null;
      for (let next = index + 1; next < all.length; next++) {
        const later = all[next];
        if (!later) continue;
        if (later.kind === 'user') break;
        if (later.kind === 'assistant' && typeof later.text === 'string' && later.text.trim()) {
          reply = later;
          break;
        }
      }
      list.push({id: shared.questionId, anchor, text: shared.text || '',
                 guest: record.guest || '', recordId: record.id,
                 reply: reply ? reply.text : '', state: reply ? 'answered' : 'sent'});
    }
    for (const entry of queued) {
      const shared = entry && entry.shared;
      if (!shared || shared.kind !== 'artifact' || shared.artifact !== view.name
          || typeof shared.questionId !== 'string' || seen.has(shared.questionId)) continue;
      const anchor = cleanAnchor(shared.anchor);
      if (!anchor) continue;
      seen.add(shared.questionId);
      list.push({id: shared.questionId, anchor, text: shared.text || '', guest: '',
                 queueId: entry.id, reply: '', state: 'queued'});
    }
    for (const [id, local] of pending) {
      if (seen.has(id)) {
        pending.delete(id);
        continue;
      }
      if (local.name === view.name) list.push(local);
    }
    return list.filter(comment => !hidden.has(comment.id));
  }

  // Streaming redraws call this for every record update; the frame only
  // hears about markers when they actually changed.
  let lastMarkers = '';
  function publishMarkers(force) {
    if (!view.frame) return;
    const list = comments();
    const markers = list.map((comment, index) => ({id: comment.id, n: index + 1,
                                                   anchor: comment.anchor,
                                                   state: comment.state}));
    const encoded = JSON.stringify(markers);
    if (force === true || encoded !== lastMarkers) {
      lastMarkers = encoded;
      post({t: 'comment-markers', markers});
    }
    if (view.card && !list.some(comment => comment.id === view.card.dataset.comment)) {
      closeCard();
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
    const form = el('form', 'artifact-comment-composer');
    form.setAttribute('role', 'dialog');
    form.setAttribute('aria-label', 'Comment on this part of the artifact');
    const target = el('p', 'artifact-comment-target', anchor.label || 'Selected part');
    const input = el('textarea', 'artifact-comment-input');
    input.placeholder = 'What should change here?';
    input.rows = 3;
    input.setAttribute('aria-label', 'Comment');
    const status = el('p', 'artifact-comment-status', '');
    status.setAttribute('role', 'status');
    const actions = el('div', 'artifact-comment-actions');
    const cancel = el('button', 'btn quiet', 'Cancel');
    cancel.type = 'button';
    const submit = el('button', 'btn', 'Send to assistant');
    submit.type = 'submit';
    actions.append(cancel, submit);
    form.append(target);
    if (anchor.quote && anchor.kind !== 'word') {
      form.append(el('blockquote', 'artifact-comment-quote', anchor.quote));
    }
    form.append(input, status, actions);
    cancel.addEventListener('click', closeComposer);
    input.addEventListener('keydown', event => {
      if (event.key === 'Escape') {
        event.preventDefault();
        closeComposer();
      } else if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
        event.preventDefault();
        form.requestSubmit ? form.requestSubmit() : form.dispatchEvent(new Event('submit'));
      }
    });
    form.addEventListener('submit', event => {
      event.preventDefault();
      submitComment(form, input, status, submit);
    });
    body.append(form);
    view.composer = form;
    place(form, rect);
    input.focus();
  }

  async function submitComment(form, input, status, submit) {
    const text = input.value.trim();
    if (!text || !view.draft || !view.id) return;
    if (new TextEncoder().encode(text).length > MAX_COMMENT_BYTES) {
      status.textContent = 'Comment too long.';
      return;
    }
    const reqId = ++requestSequence;
    const commentId = newId();
    const draft = view.draft;
    submit.disabled = true;
    status.textContent = 'Sending…';
    const done = result => {
      if (form !== view.composer) return;
      submit.disabled = false;
      if (result.error) {
        status.textContent = result.error;
        return;
      }
      pending.set(commentId, {id: commentId, name: view.name, anchor: draft.anchor, text,
                              guest: '', reply: '', state: 'queued'});
      closeComposer();
      setMode(false);
      publishMarkers();
    };
    inflight.set(reqId, {commentId, done});
    const ok = await send({t: 'artifact-comment', reqId, id: view.id, commentId, text,
                           anchor: draft.anchor, context: draft.context});
    if (!ok) {
      inflight.delete(reqId);
      done({error: 'Connection lost; the comment is kept here.'});
    }
  }

  // -- Thread card ------------------------------------------------------

  function closeCard() {
    if (view.card) view.card.remove();
    view.card = null;
    view.cardPinned = false;
  }

  function openCard(id, rect, pinned) {
    const comment = comments().find(item => item.id === id);
    if (!comment) return;
    if (view.card && view.card.dataset.comment === id) {
      if (pinned) view.cardPinned = true;
      return;
    }
    if (view.cardPinned && !pinned) return;
    closeCard();
    const card = el('section', 'artifact-comment-card');
    card.dataset.comment = id;
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-label', 'Comment thread');
    const status = {queued: 'Queued', sent: 'Sent to assistant',
                    answered: 'Answered'}[comment.state] || '';
    const head = el('p', 'artifact-comment-head',
                    [comment.guest || 'You', status].filter(Boolean).join(' · '));
    card.append(head);
    card.append(el('p', 'artifact-comment-target', comment.anchor.label || ''));
    card.append(el('p', 'artifact-comment-text', comment.text));
    if (comment.reply) {
      const excerpt = comment.reply.length > 1200
        ? comment.reply.slice(0, 1200).trimEnd() + '…' : comment.reply;
      const reply = renderMarkdown(excerpt);
      reply.className = 'prose artifact-comment-reply';
      card.append(reply);
    }
    const actions = el('div', 'artifact-comment-actions');
    if (comment.recordId && typeof reveal === 'function') {
      const show = el('button', 'btn quiet', 'Show in chat');
      show.type = 'button';
      show.addEventListener('click', () => reveal(comment.recordId));
      actions.append(show);
    }
    const hide = el('button', 'btn quiet', 'Hide marker');
    hide.type = 'button';
    hide.title = 'Hide this marker in this browser; the conversation keeps the comment';
    hide.addEventListener('click', () => {
      hidden.add(id);
      try { localStorage.setItem(HIDDEN_KEY, JSON.stringify([...hidden].slice(-500))); }
      catch (_error) { /* storage is a convenience */ }
      closeCard();
      publishMarkers();
    });
    const close = el('button', 'btn quiet', 'Close');
    close.type = 'button';
    close.addEventListener('click', closeCard);
    actions.append(hide, close);
    card.append(actions);
    body.append(card);
    view.card = card;
    view.cardPinned = pinned === true;
    place(card, rect);
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

  return Object.freeze({
    // A newly rendered HTML frame for record ID named NAME.
    attach(frame, id, name) {
      closeComposer();
      closeCard();
      view.frame = frame;
      view.id = id;
      view.name = name;
      view.mode = false;
      lastMarkers = '';
      frame.addEventListener('load', () => publishMarkers(true));
      updateToggle(comments().length);
    },
    detach() {
      closeComposer();
      closeCard();
      view.frame = null;
      view.id = null;
      view.name = null;
      view.mode = false;
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
    // The host's answer to an artifact-comment frame.
    handle(frame) {
      const entry = inflight.get(frame.reqId);
      if (!entry) return;
      inflight.delete(frame.reqId);
      entry.done(typeof frame.error === 'string' ? {error: frame.error} : {});
      if (typeof frame.error === 'string' && flash) flash(frame.error);
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
