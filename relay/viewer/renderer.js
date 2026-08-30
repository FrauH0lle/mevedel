/* renderer.js -- Dependency-free transcript record rendering */
'use strict';

(() => {
  function el(tag, className, text) {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (typeof text === 'string') node.textContent = text;
    return node;
  }

  /* -- Markdown (DOM-built, textContent only, XSS-safe) --------------- */

  function renderInline(target, text) {
    // `code`, **bold**, *italic* -- one pass, longest marker first.
    const pattern = /(`[^`\n]+`)|(\*\*[^*\n]+\*\*)|(\*[^*\n]+\*)/g;
    let last = 0;
    let match;
    while ((match = pattern.exec(text)) !== null) {
      if (match.index > last) {
        target.append(text.slice(last, match.index));
      }
      const token = match[0];
      if (token.startsWith('`')) {
        target.append(el('code', '', token.slice(1, -1)));
      } else if (token.startsWith('**')) {
        const strong = el('strong');
        renderInline(strong, token.slice(2, -2));
        target.append(strong);
      } else {
        const em = el('em');
        renderInline(em, token.slice(1, -1));
        target.append(em);
      }
      last = match.index + token.length;
    }
    if (last < text.length) target.append(text.slice(last));
  }

  // GitHub-style pipe tables: a header row, a delimiter row of dashes,
  // then body rows. The delimiter row is what distinguishes a table
  // from prose that happens to contain a pipe.
  function splitRow(line) {
    const trimmed = line.trim().replace(/^\|/, '').replace(/\|$/, '');
    return trimmed.split('|').map(cell => cell.trim());
  }

  function isTableStart(lines, index) {
    const header = lines[index];
    const delimiter = lines[index + 1];
    if (!header || !delimiter) return false;
    if (!header.includes('|') || !delimiter.includes('|')) return false;
    if (!/^[\s|:-]+$/.test(delimiter)) return false;
    const cells = splitRow(delimiter);
    return cells.length > 1
      && cells.every(cell => /^:?-{1,}:?$/.test(cell));
  }

  function columnAlignments(line) {
    return splitRow(line).map(cell => {
      const left = cell.startsWith(':');
      const right = cell.endsWith(':');
      if (left && right) return 'center';
      if (right) return 'right';
      return 'left';
    });
  }

  function renderTable(rows) {
    const wrap = el('div', 'tablewrap');
    const table = el('table', 'mdtable');
    const align = columnAlignments(rows[1]);
    const head = el('thead');
    const headRow = el('tr');
    splitRow(rows[0]).forEach((cell, column) => {
      const th = el('th');
      if (align[column]) th.style.textAlign = align[column];
      renderInline(th, cell);
      headRow.append(th);
    });
    head.append(headRow);
    table.append(head);
    const body = el('tbody');
    rows.slice(2).forEach(row => {
      const tr = el('tr');
      splitRow(row).forEach((cell, column) => {
        const td = el('td');
        if (align[column]) td.style.textAlign = align[column];
        renderInline(td, cell);
        tr.append(td);
      });
      body.append(tr);
    });
    table.append(body);
    wrap.append(table);
    return wrap;
  }

  function renderMarkdown(text) {
    const root = el('div', 'prose');
    const lines = String(text || '').split('\n');
    let index = 0;
    let paragraph = [];
    const flush = () => {
      if (paragraph.length) {
        const p = el('p');
        // A single newline is a real line break here, not a space.
        // Strict CommonMark would fold these into one line, but the
        // model means them -- "one number per line" arrives as forty
        // newlines -- and Emacs renders them, so the two surfaces
        // would otherwise disagree about the same answer.
        paragraph.forEach((line, index) => {
          if (index) p.append(el('br'));
          renderInline(p, line);
        });
        root.append(p);
        paragraph = [];
      }
    };
    while (index < lines.length) {
      const line = lines[index];
      // A fence closes only on a run of the same character that is at
      // least as long as the opener, so a ```elisp block nested inside
      // a ````markdown one no longer ends the outer block early.
      const fence = line.match(/^(\s{0,3})(`{3,}|~{3,})\s*(\S*)\s*$/);
      if (fence) {
        flush();
        const marker = fence[2];
        const closer = new RegExp(
          `^\\s{0,3}${marker[0] === '`' ? '`' : '~'}{${marker.length},}\\s*$`);
        const code = [];
        index++;
        while (index < lines.length && !closer.test(lines[index])) {
          code.push(lines[index]);
          index++;
        }
        index++; // closing fence
        root.append(renderCodeBlock(code.join('\n'), fence[3]));
        continue;
      }
      // A pipe table needs its delimiter row to be a table at all.
      if (isTableStart(lines, index)) {
        flush();
        const rows = [];
        while (index < lines.length && /\|/.test(lines[index])
               && lines[index].trim()) {
          rows.push(lines[index]);
          index++;
        }
        root.append(renderTable(rows));
        continue;
      }
      const heading = line.match(/^(#{1,4})\s+(.*)$/);
      if (heading) {
        flush();
        const h = el(`h${heading[1].length}`);
        renderInline(h, heading[2]);
        root.append(h);
        index++;
        continue;
      }
      const quote = line.match(/^>\s?(.*)$/);
      if (quote) {
        flush();
        const bq = el('blockquote');
        const inner = [quote[1]];
        index++;
        while (index < lines.length && /^>\s?/.test(lines[index])) {
          inner.push(lines[index].replace(/^>\s?/, ''));
          index++;
        }
        const p = el('p');
        renderInline(p, inner.join('\n'));
        bq.append(p);
        root.append(bq);
        continue;
      }
      const bullet = line.match(/^\s*([-*]|\d+\.)\s+/);
      if (bullet) {
        flush();
        const ordered = /^\s*\d+\./.test(line);
        const list = el(ordered ? 'ol' : 'ul');
        while (index < lines.length) {
          const item = lines[index].match(/^\s*(?:[-*]|\d+\.)\s+(.*)$/);
          if (!item) break;
          const li = el('li');
          renderInline(li, item[1]);
          list.append(li);
          index++;
          // Continuation lines indented under the item.
          while (index < lines.length && /^\s{2,}\S/.test(lines[index])
                 && !/^\s*(?:[-*]|\d+\.)\s+/.test(lines[index])) {
            li.append(' ' + lines[index].trim());
            index++;
          }
        }
        root.append(list);
        continue;
      }
      if (/^\s*$/.test(line)) {
        flush();
        index++;
        continue;
      }
      paragraph.push(line);
      index++;
    }
    flush();
    return root;
  }

  /* -- Fontification -------------------------------------------------- */

  const LANG_RULES = {
    lisp: [
      [/;.*$/m, 'tok-com'],
      [/"(?:[^"\\]|\\.)*"/, 'tok-str'],
      [/(?<=\()(?:defun|defmacro|defvar|defcustom|defconst|let\*?|lambda|if|when|unless|while|cond|pcase|setq|require|provide|interactive|dolist|dotimes|progn|or|and|not)\b/, 'tok-kw'],
      [/(?<=\(defun\s)[-a-zA-Z0-9_?!*/<>=]+/, 'tok-fn'],
    ],
    shell: [
      [/#.*$/m, 'tok-com'],
      [/"(?:[^"\\]|\\.)*"|'[^']*'/, 'tok-str'],
      [/\b(?:if|then|else|fi|for|do|done|while|case|esac|function|return|exit|export|local)\b/, 'tok-kw'],
    ],
    python: [
      [/#.*$/m, 'tok-com'],
      [/"""[\s\S]*?"""|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/, 'tok-str'],
      [/\b(?:def|class|return|if|elif|else|for|while|import|from|as|with|try|except|raise|lambda|None|True|False|and|or|not|in|is)\b/, 'tok-kw'],
    ],
    js: [
      [/\/\/.*$/m, 'tok-com'],
      [/`(?:[^`\\]|\\.)*`|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/, 'tok-str'],
      [/\b(?:const|let|var|function|return|if|else|for|while|class|new|await|async|import|export|null|true|false|typeof)\b/, 'tok-kw'],
    ],
    go: [
      [/\/\/.*$/m, 'tok-com'],
      [/`[^`]*`|"(?:[^"\\]|\\.)*"/, 'tok-str'],
      [/\b(?:func|return|if|else|for|range|type|struct|interface|package|import|var|const|go|defer|select|case|switch|nil|true|false)\b/, 'tok-kw'],
    ],
    // Reading a doc is as common as reading source, so markdown gets
    // its own rules: structure as keyword, code spans as string.
    markdown: [
      [/^\s{0,3}#{1,6}\s.*$/m, 'tok-kw'],
      [/^\s{0,3}(?:[-*+]|\d+\.)\s/m, 'tok-kw'],
      [/^\s{0,3}>.*$/m, 'tok-com'],
      [/`[^`\n]+`/, 'tok-str'],
      [/\*\*[^*\n]+\*\*|__[^_\n]+__/, 'tok-fn'],
      [/\[[^\]\n]*\]\([^)\n]*\)/, 'tok-fn'],
    ],
  };
  const LANG_ALIASES = {
    'emacs-lisp': 'lisp', elisp: 'lisp', lisp: 'lisp', scheme: 'lisp',
    sh: 'shell', bash: 'shell', shell: 'shell', zsh: 'shell',
    python: 'python', py: 'python',
    js: 'js', javascript: 'js', typescript: 'js', ts: 'js', json: 'js',
    go: 'go', golang: 'go',
    md: 'markdown', markdown: 'markdown', mdown: 'markdown',
  };

  function highlightInto(target, text, lang) {
    const rules = LANG_RULES[LANG_ALIASES[(lang || '').toLowerCase()] || ''];
    if (!rules) {
      target.textContent = text;
      return;
    }
    const combined = new RegExp(
      rules.map(rule => `(${rule[0].source})`).join('|'), 'gm');
    let last = 0;
    let match;
    while ((match = combined.exec(text)) !== null) {
      if (match.index > last) target.append(text.slice(last, match.index));
      let cls = '';
      for (let group = 1; group <= rules.length; group++) {
        if (match[group] !== undefined) {
          cls = rules[group - 1][1];
          break;
        }
      }
      target.append(el('span', cls, match[0]));
      last = match.index + match[0].length;
      if (match[0].length === 0) combined.lastIndex++;
    }
    if (last < text.length) target.append(text.slice(last));
  }

  // Tool results carry a path, not a fence, so the language comes from
  // the extension. Only families the highlighter actually knows are
  // mapped; anything else renders as plain text rather than guessing.
  const EXTENSION_LANG = {
    el: 'lisp', lisp: 'lisp', scm: 'lisp', clj: 'lisp',
    sh: 'shell', bash: 'shell', zsh: 'shell',
    py: 'python',
    js: 'js', mjs: 'js', cjs: 'js', ts: 'js', tsx: 'js', jsx: 'js', json: 'js',
    go: 'go',
    md: 'markdown', markdown: 'markdown', org: 'markdown', txt: '',
  };

  function langForPath(path) {
    const match = /\.([A-Za-z0-9]+)$/.exec(String(path || '').trim());
    return match ? (EXTENSION_LANG[match[1].toLowerCase()] || '') : '';
  }

  // Read prints "<right-aligned line number>TAB<source>". Keeping the
  // gutter out of the highlighter stops line numbers being coloured as
  // literals and keeps the source column aligned.
  const NUMBERED_LINE = /^(\s*\d+\t)([\s\S]*)$/;

  function renderNumberedSource(text, lang) {
    const pre = el('pre', 'result');
    String(text).split('\n').forEach((line, index, all) => {
      const match = NUMBERED_LINE.exec(line);
      if (match) {
        pre.append(el('span', 'gutter', match[1]));
        highlightInto(pre, match[2], lang);
      } else {
        pre.append(line);
      }
      if (index < all.length - 1) pre.append('\n');
    });
    return pre;
  }

  // Grep prints "path:line:match"; Xref and Imenu print
  // "path:line: text". One shape, so one renderer -- and a run can
  // span several file types, so each line picks its own language from
  // its own path.
  const GREP_LINE = /^([^\s:][^:]*):(\d+):([\s\S]*)$/;
  const LOCATED_TOOLS = new Set(
    ['Grep', 'XrefReferences', 'XrefDefinitions', 'Imenu']);

  function renderGrepResult(text) {
    const pre = el('pre', 'result');
    String(text).split('\n').forEach((line, index, all) => {
      const match = GREP_LINE.exec(line);
      if (match) {
        pre.append(el('span', 'gpath', match[1]));
        pre.append(el('span', 'gsep', ':'));
        pre.append(el('span', 'gline', match[2]));
        pre.append(el('span', 'gsep', ':'));
        highlightInto(pre, match[3], langForPath(match[1]));
      } else {
        pre.append(line);
      }
      if (index < all.length - 1) pre.append('\n');
    });
    return pre;
  }

  // Glob answers with bare paths: dimming the directory makes the
  // basenames scannable, which is the whole point of a glob.
  function renderPathList(text) {
    const pre = el('pre', 'result');
    String(text).split('\n').forEach((line, index, all) => {
      const cut = line.lastIndexOf('/');
      if (cut > 0 && line.trim()) {
        pre.append(el('span', 'gpath', line.slice(0, cut + 1)));
        pre.append(line.slice(cut + 1));
      } else {
        pre.append(line);
      }
      if (index < all.length - 1) pre.append('\n');
    });
    return pre;
  }

  function renderToolResult(record, text) {
    const name = record.name || '';
    if (LOCATED_TOOLS.has(name)) return renderGrepResult(text);
    if (name === 'Glob') return renderPathList(text);
    // Eval answers with a printed Lisp value.
    if (name === 'Eval') {
      const pre = el('pre', 'result');
      highlightInto(pre, text, 'lisp');
      return pre;
    }
    const lang = langForPath(record.detail);
    if (lang && NUMBERED_LINE.test(text)) {
      return renderNumberedSource(text, lang);
    }
    if (lang) {
      const pre = el('pre', 'result');
      highlightInto(pre, text, lang);
      return pre;
    }
    return el('pre', 'result', text);
  }

  function renderCodeBlock(code, lang) {
    if ((lang || '').toLowerCase() === 'diff') {
      const block = el('div', 'codeblock');
      block.append(el('span', 'lang', 'diff'));
      block.append(renderDiff(code));
      return block;
    }
    const block = el('div', 'codeblock');
    if (lang) block.append(el('span', 'lang', lang));
    const pre = el('pre');
    highlightInto(pre, code, lang);
    block.append(pre);
    return block;
  }

  function renderDiff(text) {
    const container = el('div', 'diff');
    for (const line of String(text || '').split('\n')) {
      let cls = 'line';
      if (/^@@/.test(line)) cls += ' hunk';
      else if (/^(\*\*\*|\+\+\+|---|diff |Index:|=== )/.test(line)) cls += ' file';
      else if (/^\+/.test(line)) cls += ' add';
      else if (/^-/.test(line)) cls += ' del';
      container.append(el('span', cls, line));
    }
    return container;
  }

  function looksLikeDiff(text) {
    return /^(@@ |\+\+\+ |--- |\*\*\* (Begin|Update|Add|Delete))/m
      .test(String(text || ''));
  }

  /* -- Ledger rendering ----------------------------------------------- */

  function roleOf(record) {
    if (record.kind === 'user') return record.guest ? 'guest' : 'you';
    return 'ai';
  }

  function whoLine(record, directiveLabel) {
    const who = el('div', 'who');
    if (record.kind === 'user') {
      who.append(el('span', 'name', record.guest || 'Host'));
      if (record.guest) who.append(el('span', 'badge', 'guest'));
    } else if (record.kind === 'assistant') {
      who.append(el('span', 'name', 'Assistant'));
    } else {
      who.append(el('span', 'name', 'Tool'));
    }
    if (record.directive) {
      who.append(el('span', 'dirchip', `◆ ${directiveLabel(record.directive)}`));
    }
    return who;
  }

  function renderContent(record) {
    if (record.kind === 'user') {
      const prose = renderMarkdown(record.text || '');
      prose.className = 'prose prompt';
      return prose;
    }
    if (record.kind === 'assistant') {
      return renderMarkdown(record.text || '');
    }
    // A tool call is one line: status glyph, name, target. Only things
    // waiting on a decision get a box, so eight finished reads cannot
    // shout as loudly as the permission holding up the run.
    const status = record.status || '';
    const details = el('details', `tool ${status}`);
    const summary = el('summary');
    const glyph = {completed: '\u2713', failed: '\u2715', running: '\u25cf'}[status]
      || '\u00b7';
    const mark = el('span', `st ${status}`, glyph);
    mark.setAttribute('role', 'img');
    mark.setAttribute('aria-label', status || 'pending');
    summary.append(mark);
    summary.append(el('span', 'tname', record.name || 'Tool'));
    summary.append(el('span', 'targ',
                      record.detail
                      || (record.summary !== record.name ? record.summary : '')
                      || ''));
    details.append(summary);
    // A patch travels as a dedicated diff field; the result text is only
    // the application summary.
    if (record.diff) {
      const body = renderDiff(record.diff);
      body.className = 'result diff';
      details.append(body);
    }
    const result = record.result || '';
    if (result) {
      if (!record.diff && looksLikeDiff(result)) {
        const body = renderDiff(result);
        body.className = 'result diff';
        details.append(body);
      } else {
        details.append(renderToolResult(record, result));
      }
      if (record.truncated) {
        details.append(el('pre', 'result', '[result truncated]'));
      }
    }
    return details;
  }

  function renderRecord(record, directiveLabel) {
    const turn = el('article', `turn ${roleOf(record)}`);
    turn.dataset.recordId = record.id;
    turn.dataset.role = roleOf(record);
    const rail = el('div', 'rail');
    const glyphText = roleOf(record) === 'you' ? 'H'
      : roleOf(record) === 'guest' ? 'G' : '◆';
    rail.append(el('div', 'glyph', glyphText));
    turn.append(rail);
    const content = el('div', 'content');
    content.append(whoLine(record, directiveLabel));
    const rendered = renderContent(record);
    content.append(rendered);
    turn.append(content);
    // Tool rows keep their disclosure state across updates; stashing the
    // details element avoids a querySelector the protocol test's fake DOM
    // does not implement.
    if (record.kind === 'tool') turn.toolDetails = rendered;
    return turn;
  }

  window.mevedelTranscriptRenderer = Object.freeze({renderRecord, renderDiff});
})();
