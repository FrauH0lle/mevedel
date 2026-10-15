/* viewer-discussions.js -- the discussions sidebar of whiteboards, documents
 * and HTML artifacts.
 *
 * The sidebar shows either the list of an item's discussions or one of them.
 * The list puts the discussion about the whole item first, then the open
 * discussions about parts of it, then the resolved ones folded into one row.
 * A discussion fills the sidebar with its messages, the assistant's answers
 * beneath the messages they answer, and the one box that replies to it. A new
 * discussion looks the same before its first message. An answer or message
 * that arrives while its discussion is not on screen marks its row and the
 * back link instead of taking the participant away from what they are
 * writing.
 *
 * The caller owns the discussions, their drafts and what posting does; this
 * view owns navigation, unread marks, answer placement and the composer.
 */
'use strict';

(() => {
  const NEW = 'new';
  // How a selection sent alone reads: its message has no text of its own.
  const ALONE = 'Sent the selection as the request';
  const firstLine = text => String(text || '').trim().split('\n')[0];

  function create(options) {
    const {root, noun, canWrite, draft, submit} = options;
    const visible = options.visible || (() => true);
    const el = (tag, className, text) => {
      const node = document.createElement(tag);
      if (className) node.className = className;
      if (text !== undefined) node.textContent = text;
      return node;
    };
    // Nodes rebuilt on each render carry a key, so focus survives the render.
    const button = (className, text, key, run) => {
      const node = el('button', className, text);
      node.type = 'button';
      if (key) node.dataset.key = key;
      if (run) node.addEventListener('click', run);
      return node;
    };

    let discussions = [];
    let records = null;        // transcript records about this item, oldest first
    let selection = null;      // {label} of what Discuss would start about
    let fresh = null;          // {label, quote, placeholder} of the new discussion
    let current = {kind: 'list', id: null};
    let awaiting = null;       // a started discussion not stored yet
    const seen = new Map();    // discussion id -> {messages, answers} last seen
    let sending = false;
    let foldOpen = false;
    let menuOpen = false;

    // -- Skeleton ---------------------------------------------------------

    const list = el('div', 'ds-view ds-list');
    const rows = el('div', 'ds-scroll ds-rows');
    rows.setAttribute('role', 'list');
    rows.setAttribute('aria-label', 'Discussions');
    const listFoot = el('div', 'ds-foot');
    list.append(rows, listFoot);

    const thread = el('div', 'ds-view ds-thread');
    thread.hidden = true;
    const bar = el('div', 'ds-bar');
    const back = button('ds-back', '', 'back', () => show({kind: 'list'}));
    const subject = el('div', 'ds-subject');
    bar.append(back, subject);
    const scroll = el('div', 'ds-scroll ds-messages');
    scroll.tabIndex = 0;
    scroll.setAttribute('role', 'log');
    scroll.setAttribute('aria-label', 'Messages');
    const foot = el('div', 'ds-foot');
    const form = el('form', 'ds-compose');
    form.autocomplete = 'off';
    const input = el('textarea', 'ds-input');
    input.id = `ds-input-${noun}`;
    input.maxLength = 10000;
    input.rows = 2;
    input.setAttribute('aria-label', 'Message');
    const files = el('div', 'attachments ds-files');
    const actions = el('div', 'ds-actions');
    const choice = el('label', 'ds-choice');
    const ask = el('input');
    ask.type = 'checkbox';
    ask.checked = true;
    choice.append(ask, ' Ask assistant');
    const attach = button('ds-attach', '');
    attach.title = 'Attach files — images, PDFs, logs and patches';
    attach.setAttribute('aria-label', 'Attach files');
    attach.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M20 11 12 19a5 5 0 0 1-7-7l8-8a3.5 3.5 0 0 1 5 5l-8 8a2 2 0 0 1-3-3l7-7"/></svg>';
    const picker = el('input');
    picker.type = 'file';
    picker.multiple = true;
    picker.hidden = true;
    picker.accept = 'image/jpeg,image/png,image/webp,application/pdf,text/plain,text/markdown,text/csv,application/json,.log,.patch,.diff,.md,.csv,.json,.txt';
    const send = el('button', 'ds-send', 'Reply');
    send.type = 'submit';
    actions.append(choice, el('span', 'ds-grow'), picker, attach, send);
    form.append(input, files, actions);
    const closed = el('div', 'ds-closed');
    foot.append(form, closed);
    thread.append(bar, scroll, foot);
    const notice = el('p', 'ds-notice');
    notice.setAttribute('role', 'status');
    root.replaceChildren(list, thread, notice);

    // -- Model ------------------------------------------------------------

    const find = id => discussions.find(d => d.id === id);
    const general = () => discussions.find(d => d.general);
    const key = () => (current.kind === NEW ? NEW : current.id);
    const whole = `Whole ${noun}`;

    // Transcript records become the answers beneath the messages they
    // answer; a record no stored message claims follows its discussion, or
    // the whole item's. A record unchanged since the last render keeps its
    // node, so text selected in an answer survives streaming elsewhere.
    let rendered = new Map();
    let placed = {byMessage: new Map(), tail: new Map(), answers: new Map(), last: new Map()};
    function place() {
      const next = new Map();
      const out = {byMessage: new Map(), tail: new Map(), answers: new Map(), last: new Map()};
      const fallback = general()?.id;
      let context = null;
      for (const record of records || []) {
        if (record.kind === 'user') context = record.shared || null;
        if (record.kind !== 'user' && record.kind !== 'assistant') continue;
        const print = JSON.stringify(record);
        const old = rendered.get(record.id);
        const node = old && old.print === print ? old.node
          : window.mevedelTranscriptRenderer.renderRecord(record, () => null, undefined, old?.node);
        next.set(record.id, {print, node});
        if (record.shared) node.dataset.questionId = record.shared.questionId;
        const d = find(context?.discussionId) || find(fallback);
        if (!d) continue;
        const message = context?.messageId && d.messages.some(m => m.id === context.messageId)
          ? context.messageId : null;
        // The message already shows the request; its turn keeps only the
        // context it carried.
        node.classList.toggle('thread-question',
          !!message && record.kind === 'user' && !!record.shared && !record.shared.edited);
        const bucket = message ? `${d.id}\n${message}` : d.id;
        const into = message ? out.byMessage : out.tail;
        if (!into.has(bucket)) into.set(bucket, []);
        into.get(bucket).push(node);
        if (record.kind === 'assistant') {
          out.answers.set(d.id, (out.answers.get(d.id) || 0) + 1);
          out.last.set(d.id, {message, text: record.text});
        }
      }
      rendered = next;
      placed = out;
    }

    const stamp = d => ({messages: d.messages.length, answers: placed.answers.get(d.id) || 0});
    const unread = d => {
      const was = seen.get(d.id);
      if (!was) return false;
      const now = stamp(d);
      return now.messages !== was.messages || now.answers !== was.answers;
    };
    const newLabel = d => ((placed.answers.get(d.id) || 0) !== seen.get(d.id)?.answers
      ? 'New answer' : 'New message');

    // The discussion on screen is seen; so is one that first appears, so
    // history loaded with the item marks nothing.
    function markSeen(all) {
      for (const d of discussions) {
        if (all || !seen.has(d.id)
            || (current.kind === 'thread' && current.id === d.id && visible())) seen.set(d.id, stamp(d));
      }
    }

    // -- List -------------------------------------------------------------

    function snippet(d) {
      const last = d.messages.at(-1), answer = placed.last.get(d.id);
      if (answer && (!last || answer.message === last.id || !answer.message)) {
        return `Assistant: ${firstLine(answer.text)}`;
      }
      if (last) return `${last.actor || 'Guest'}: ${last.alone ? ALONE : firstLine(last.text)}`;
      return d.general ? `Nothing said about the whole ${noun} yet` : '';
    }

    function rowFor(d) {
      const fresh = unread(d);
      const node = button(`ds-item${d.general ? ' general' : ''}${fresh ? ' unread' : ''}`, '',
                          `row:${d.id}`, () => show({kind: 'thread', id: d.id}));
      node.dataset.discussionId = d.id;
      node.setAttribute('role', 'listitem');
      const head = el('span', 'ds-item-head');
      if (d.pin) head.append(el('span', 'ds-pin', String(d.pin)));
      if (d.kind) head.append(el('span', 'ds-kind', d.kind));
      head.append(el('span', 'ds-title', d.general ? whole : d.title || 'Discussion'));
      const meta = el('span', 'ds-meta');
      if (fresh) meta.append(el('span', 'ds-dot'), newLabel(d));
      else if (d.messages.length) {
        meta.textContent = String(d.messages.length);
        meta.title = `${d.messages.length} message${d.messages.length === 1 ? '' : 's'}`;
      }
      const snip = el('span', 'ds-snip');
      if (d.state?.text) snip.append(el('span', `ds-state ${d.state.tone || ''}`, d.state.text));
      else snip.textContent = snippet(d);
      node.append(head, meta, snip);
      return node;
    }

    function renderList() {
      const open = discussions.filter(d => !d.general && !d.resolved);
      const resolved = discussions.filter(d => !d.general && d.resolved);
      const nodes = [];
      if (general()) nodes.push(rowFor(general()));
      nodes.push(...open.map(rowFor));
      if (!open.length && options.emptyHint && canWrite()) nodes.push(el('p', 'ds-empty', options.emptyHint));
      if (resolved.length) {
        const fold = el('details', 'ds-resolved');
        fold.open = foldOpen;
        fold.addEventListener('toggle', () => { foldOpen = fold.open; });
        const summary = el('summary', '', `Resolved · ${resolved.length}`);
        summary.dataset.key = 'resolved';
        fold.append(summary, ...resolved.map(rowFor));
        nodes.push(fold);
      }
      rows.replaceChildren(...nodes);
      listFoot.replaceChildren();
      listFoot.hidden = !canWrite() || (!selection && !options.selectHint);
      if (listFoot.hidden) return;
      if (selection) {
        const start = button('ds-start', '', 'start', () => options.startNew());
        start.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4 4h16v12H10l-5 4v-4H4Z"/></svg>';
        start.append(`Discuss ${selection.label}`);
        listFoot.append(start);
      } else listFoot.append(el('p', 'ds-hint', options.selectHint));
    }

    // -- One discussion ---------------------------------------------------

    function renderBar(d) {
      const others = discussions.filter(other => other !== d && unread(other)).length;
      back.replaceChildren('‹ All discussions');
      if (others) {
        const badge = el('span', 'ds-badge');
        badge.append(el('span', 'ds-dot'), `${others} new`);
        back.append(badge);
      }
      back.setAttribute('aria-label', others ? `All discussions, ${others} with news` : 'All discussions');
      subject.replaceChildren();
      if (d?.pin) subject.append(el('span', 'ds-pin', String(d.pin)));
      const title = !d ? 'New discussion'
        : d.general ? whole : [d.kind, d.title].filter(Boolean).join(' · ') || 'Discussion';
      const heading = el('b', 'ds-subject-title', title);
      heading.title = title;
      subject.append(heading);
      if (d && !d.general && options.reveal && d.revealable !== false) {
        subject.append(button('ds-link ds-reveal', options.revealLabel, 'reveal', () => options.reveal(d.id)));
      }
      if (!d && options.refresh && canWrite()) {
        const refresh = button('ds-link ds-refresh', 'Refresh', 'refresh', () => options.refresh(key()));
        refresh.title = 'Attach the content as it is now';
        subject.append(refresh);
      }
      const entries = d ? options.menu?.(d) || [] : [];
      if (entries.length) {
        const menu = el('details', 'ds-menu');
        menu.open = menuOpen;
        menu.addEventListener('toggle', () => { menuOpen = menu.open; });
        const summary = el('summary', '', '⋯');
        summary.dataset.key = 'menu';
        summary.setAttribute('aria-label', 'More');
        summary.title = 'More';
        const body = el('div', 'ds-menu-body');
        for (const entry of entries) {
          const item = button(entry.className || '', entry.label, `menu:${entry.label}`, () => {
            menu.open = menuOpen = false;
            entry.run();
          });
          item.disabled = entry.disabled === true;
          if (entry.title) item.title = entry.title;
          body.append(item);
        }
        menu.append(summary, body);
        subject.append(menu);
      } else menuOpen = false;
    }

    // Replace PARENT's children only when they differ, so a selection inside
    // an unchanged message survives.
    function sync(parent, nodes) {
      const now = parent.children;
      if (now.length === nodes.length && nodes.every((node, index) => now[index] === node)) return;
      parent.replaceChildren(...nodes);
    }

    const messageNodes = new Map();
    function messageNode(d, message) {
      const id = `${d.id}\n${message.id}`;
      let entry = messageNodes.get(id);
      const print = JSON.stringify([message.actor, message.text, message.alone, message.mine]);
      if (!entry || entry.print !== print) {
        const node = el('div', `ds-message${message.mine ? ' mine' : ''}`);
        node.dataset.messageId = message.id;
        node.append(el('span', 'ds-who', message.actor || 'Guest'),
                    el('p', message.alone ? 'ds-text alone' : 'ds-text', message.alone ? ALONE : message.text),
                    el('div', 'ds-answers'));
        entry = {print, node};
        messageNodes.set(id, entry);
      }
      sync(entry.node.lastChild, placed.byMessage.get(id) || []);
      return entry.node;
    }

    function renderThread() {
      const d = current.kind === NEW ? null : find(current.id);
      if (current.kind === 'thread' && !d) {
        show({kind: 'list'});
        return;
      }
      renderBar(d);
      const follow = scroll.scrollHeight - scroll.scrollTop - scroll.clientHeight < 40;
      const nodes = [];
      const quote = d ? d.quote : fresh?.quote;
      if (!d && fresh?.label) nodes.push(el('p', 'ds-target', fresh.label));
      if (quote && !d?.general) nodes.push(el('blockquote', 'ds-quote', quote));
      if (d) {
        for (const message of d.messages) nodes.push(messageNode(d, message));
        const tail = placed.tail.get(d.id) || [];
        if (tail.length) {
          const holder = el('div', 'ds-answers ds-tail');
          holder.append(...tail);
          nodes.push(holder);
        }
        if (!d.messages.length && !tail.length) {
          nodes.push(el('p', 'ds-empty', d.general
            ? `Discuss the whole ${noun} with the room and the assistant. Messages and answers are shared with everyone.`
            : 'No messages yet.'));
        }
        const extras = options.extras?.(d) || {};
        if (extras.status) nodes.push(el('p', `ds-status ${extras.tone || ''}`, extras.status));
        if (extras.quote) nodes.push(el('blockquote', 'ds-quote', extras.quote));
        if (extras.actions?.length) {
          const row = el('div', 'ds-extra');
          for (const action of extras.actions) {
            const node = button(action.className || '', action.label, `extra:${action.label}`, action.run);
            node.disabled = action.disabled === true;
            row.append(node);
          }
          nodes.push(row);
        }
      }
      sync(scroll, nodes);
      if (follow) scroll.scrollTop = scroll.scrollHeight;
      renderComposer(d);
    }

    function renderComposer(d) {
      const writable = canWrite();
      const resolved = !!d?.resolved;
      form.hidden = !writable || resolved;
      closed.replaceChildren();
      closed.hidden = !writable || !resolved;
      foot.hidden = form.hidden && closed.hidden;
      if (writable && resolved) {
        closed.append(el('span', '', 'Resolved'));
        const reopen = (options.menu?.(d) || []).find(entry => entry.reopen);
        if (reopen) closed.append(button('ds-link', 'Reopen to reply', 'reopen', reopen.run));
      }
      const posting = !d || d.general;
      attach.hidden = files.hidden = !(options.attachments && posting);
      input.placeholder = !d ? (ask.checked && fresh?.placeholder) || 'Start a discussion…'
        : d.general ? `Write about the whole ${noun}…` : 'Reply to this discussion…';
      send.textContent = !d ? 'Start discussion' : d.general ? 'Post' : 'Reply';
      send.disabled = sending;
    }

    function loadDraft() {
      const value = draft.get(key()) || {};
      if (input.value !== (value.text || '')) input.value = value.text || '';
      ask.checked = value.toAssistant !== false;
    }

    // -- Rendering and navigation -----------------------------------------

    function render() {
      const active = root.contains(document.activeElement)
        ? document.activeElement.closest('[data-key]')?.dataset.key : null;
      markSeen();
      if (current.kind === 'list') renderList();
      else renderThread();
      if (active && !root.contains(document.activeElement)) {
        const again = [...root.querySelectorAll('[data-key]')].find(node => node.dataset.key === active);
        again?.focus({preventScroll: true});
      }
      options.onRender?.();
    }

    function show(view) {
      const next = view.kind === 'list' ? {kind: 'list', id: null}
        : view.kind === NEW ? {kind: NEW, id: null} : {kind: 'thread', id: view.id};
      const changed = next.kind !== current.kind || next.id !== current.id;
      const left = current.kind === NEW && changed;
      current = next;
      awaiting = null;
      list.hidden = current.kind !== 'list';
      thread.hidden = current.kind === 'list';
      if (changed) {
        if (!view.keep) notice.textContent = '';
        menuOpen = false;
        loadDraft();
        scroll.replaceChildren();
      }
      render();
      if (changed && current.kind !== 'list') scroll.scrollTop = scroll.scrollHeight;
      if (changed) {
        if (left) options.leaveNew?.();
        options.onView?.({...current});
      }
    }

    // -- Composer ---------------------------------------------------------

    input.addEventListener('input', () => draft.set(key(), {text: input.value}));
    ask.addEventListener('change', () => {
      draft.set(key(), {toAssistant: ask.checked});
      renderComposer(current.kind === NEW ? null : find(current.id));
    });
    attach.addEventListener('click', () => picker.click());
    input.addEventListener('keydown', event => {
      if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') {
        event.preventDefault();
        form.requestSubmit();
      }
    });
    form.addEventListener('submit', async event => {
      event.preventDefault();
      if (sending || !canWrite()) return;
      const at = key(), text = input.value;
      sending = true;
      send.disabled = true;
      input.readOnly = true;
      try {
        const opened = await submit(at, {text, toAssistant: ask.checked});
        if (opened === false) return;
        if (key() === at && input.value === text) input.value = '';
        if (at === NEW && key() === NEW && typeof opened === 'string') {
          if (find(opened)) show({kind: 'thread', id: opened, keep: true});
          else awaiting = opened;
        }
      } finally {
        sending = false;
        input.readOnly = false;
        send.disabled = false;
      }
    });

    return Object.freeze({
      /* DISCUSSIONS: [{id, general?, pin?, kind?, title, resolved?, quote?,
         revealable?, messages: [{id, actor, text, alone?, mine?}],
         state?: {text, tone}}], the whole item's first.  RECORDS: the
         transcript records about the item.  SELECTION: {label} Discuss would
         start about, or null.  FRESH: {label, quote, placeholder} of the new
         discussion.  SETTLED: the discussions as loaded are all seen; the
         first records count as loaded too. */
      update(next = {}) {
        if (next.discussions) discussions = next.discussions;
        const first = next.records && records === null;
        if (next.records) records = next.records;
        if ('selection' in next) selection = next.selection;
        if ('fresh' in next) fresh = next.fresh;
        if (next.discussions || next.records) place();
        if (first || next.settled) markSeen(true);
        if (awaiting && find(awaiting)) show({kind: 'thread', id: awaiting, keep: true});
        else render();
      },
      show,
      view: () => ({...current}),
      unread: id => { const d = find(id); return !!d && unread(d); },
      unreadCount: () => discussions.filter(unread).length,
      // Start over for another item.
      reset() {
        discussions = [];
        records = null;
        seen.clear();
        rendered.clear();
        messageNodes.clear();
        foldOpen = false;
        show({kind: 'list'});
      },
      notice(text, error = false) {
        notice.textContent = text || '';
        notice.dataset.error = String(error);
      },
      // Focus the view's first control, its box, or with READING its messages.
      focus(reading = false) {
        const target = current.kind === 'list' ? rows.querySelector('button') || rows
          : form.hidden || reading ? scroll : input;
        target.focus({preventScroll: true});
      },
      composer: () => ({form, input, files, attach, picker}),
    });
  }

  window.mevedelDiscussionSidebar = Object.freeze({create});
})();
