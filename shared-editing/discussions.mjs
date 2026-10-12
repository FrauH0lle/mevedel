/* The item's Discussions panel; the host owns discussions, submissions and
   transcript truth.  One composer starts a discussion about the selection or
   writes to the discussion about the whole item; each discussion's card
   shows its messages with the assistant's answers beneath them. */
import { contentHash, GENERAL_DISCUSSION as GENERAL } from './view.mjs';
const $ = id => document.getElementById(id);
// How a selection sent alone reads: its message has no text of its own.
const ALONE = 'Sent the selection as the request';
// A board quote spans lines (area, count, objects); a card header shows it on
// one line, so the parts stay apart once whitespace collapses.
const headline = (quote) => String(quote || '').split('\n').filter(Boolean).join(' · ');
const who = (actor) => String(actor || '').replace(/^Guest: /, '');
const el = (tag, text, className) => {
  const node = document.createElement(tag);
  if (text !== undefined) node.textContent = text;
  if (className) node.className = className;
  return node;
};
const newDraft = () => ({text:'', opId:crypto.randomUUID()});
const newQuestion = () => ({text:'', attachment:null, selectionAttachment:null,
  opId:crypto.randomUUID(), discussionId:crypto.randomUUID(), toAssistant:true});
const board = () => document.body.dataset.kind === 'whiteboard';
/* Wording for what a discussion refers to on this item. */
const anchorTerms = () => board()
  ? {show:'Show objects', part:'objects',
      empty:'Select objects or drag across an area, then choose Discuss. The Discuss tool (M) does both in one step.',
      removed:'Referenced objects were removed', changed:'Referenced objects have changed',
      review:'The objects have changed. Review their current state before sending.',
      refreshed:'Current objects and discussion attached. Press Send to assistant when ready.'}
  : {show:'Show passage', part:'passage',
      empty:'Select text and choose Discuss to start a discussion about it.',
      removed:'Referenced passage was removed', changed:'Referenced passage has changed',
      review:'The passage has changed. Review its current text before sending.',
      refreshed:'Current passage and discussion attached. Press Send to assistant when ready.'};
const general = () => ({id:GENERAL, target:{kind:'whole'}, quote:'', messages:[], resolved:false,
  anchorStatus:'current'});
export class DiscussionPanel {
  constructor({ capture, save, request, changed, reveal, state, stored, restored, onConversation }) {
    Object.assign(this, { capture, save, request, changed, reveal, state, stored,
      onConversation: onConversation || (() => {}) });
    const validDrafts = restored?.format === 'discussions' && typeof restored.question?.text === 'string'
      && restored.replies && typeof restored.replies === 'object'
      && restored.requests && typeof restored.requests === 'object';
    this.drafts = validDrafts ? restored : {format:'discussions', question:newQuestion(), replies:{}, requests:{}};
    if (restored && !validDrafts) this.recoveryNotice = 'The saved discussion draft has an unsupported format. Document recovery is unaffected.';
    this.draft = this.drafts.question;
    this.discussions = [general()];
    // Discussions with a submission in flight; 'new' is the composer's.
    this.sending = new Set();
    this.conversation = { records: [], own: [], busy: false, connected: true };
    $('question').value = this.draft.text;
    $('ask-toggle').onclick = () => {
      if ($('assistant').hidden && !this.draft.attachment) this.begin('whole');
      else this.toggle($('assistant').hidden);
    };
    $('assistant-close').onclick = () => this.toggle(false);
    $('whole-question').onclick = () => this.begin('whole');
    $('selected-question').onclick = () => {
      if (!this.draft.selectionAttachment) return;
      this.draft.attachment = this.draft.selectionAttachment;
      this.draft.opId = crypto.randomUUID();
      this.notice('');
      this.toggle(true);
    };
    $('ask-assistant').onchange = () => {
      this.draft.toAssistant = $('ask-assistant').checked;
      this.draft.opId = crypto.randomUUID();
      this.renderDraft();
    };
    $('show-resolved').onchange = () => this.setDiscussions(this.discussions);
    $('refresh-context').onclick = () => {
      try {
        const a = this.draft.attachment;
        this.draft.attachment = this.capture(a?.snapshot.scope || 'whole', a);
        if (this.draft.attachment.snapshot.scope === 'selection') this.draft.selectionAttachment = this.draft.attachment;
        this.draft.opId = crypto.randomUUID();
        this.notice('Context refreshed. Review it before sending.');
        this.renderDraft();
      } catch (error) { this.notice(error.message, true); }
    };
    const handle = $('discussion-resize');
    const limits = () => {
      const min = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--discussion-min-width'));
      return {min, max:Math.max(min, Math.floor(innerWidth / 2))};
    };
    const clamp = value => { const {min,max} = limits(); return Math.round(Math.max(min, Math.min(max, value))); };
    const fitWidth = () => {
      const {min,max} = limits(), width = clamp(Number.isFinite(this.drafts.discussionWidth) ? this.drafts.discussionWidth : min);
      document.body.style.setProperty('--discussion-width', width + 'px');
      handle.hidden = innerWidth < 800;
      handle.setAttribute('aria-valuemin', min); handle.setAttribute('aria-valuemax', max);
      handle.setAttribute('aria-valuenow', width); handle.setAttribute('aria-valuetext', width + ' pixels');
      return width;
    };
    let resize;
    const finishResize = (cancel = false) => {
      if (!resize) return;
      if (cancel) this.drafts.discussionWidth = resize.preferred;
      resize = null; document.body.classList.remove('discussion-resizing');
      fitWidth(); this.changed();
    };
    handle.onpointerdown = event => {
      if (event.button !== 0 || innerWidth < 800) return;
      event.preventDefault();
      resize = {x:event.clientX, width:fitWidth(), preferred:this.drafts.discussionWidth};
      handle.setPointerCapture(event.pointerId); document.body.classList.add('discussion-resizing');
    };
    handle.onpointermove = event => {
      if (!resize) return;
      this.drafts.discussionWidth = clamp(resize.width + resize.x - event.clientX); fitWidth();
    };
    handle.onpointerup = () => finishResize();
    handle.onpointercancel = () => finishResize(true);
    handle.onlostpointercapture = () => finishResize();
    handle.ondblclick = () => { this.drafts.discussionWidth = limits().min; fitWidth(); this.changed(); };
    handle.onkeydown = event => {
      if (event.key === 'Escape' && resize) { event.stopPropagation(); finishResize(true); return; }
      const {min,max} = limits(), current = fitWidth();
      const width = {ArrowLeft:current+24, ArrowRight:current-24, Home:min, End:max}[event.key];
      if (width === undefined) return;
      event.preventDefault(); this.drafts.discussionWidth = clamp(width); fitWidth(); this.changed();
    };
    fitWidth();
    this.layout = () => {
      fitWidth();
      const container = document.querySelector($('assistant').hidden ? 'footer' : '.assistant-compose');
      if ($('context-actions').parentElement !== container) container.prepend($('context-actions'));
      if (innerHeight < 500 && document.activeElement === $('question')) $('attached-context').open = false;
      for (const node of document.body.children)
        if (node !== $('assistant')) node.inert = !$('assistant').hidden && innerWidth < 800;
    };
    $('question').onfocus = this.layout;
    window.addEventListener('resize', this.layout);
    $('question').oninput = () => {
      this.draft.text = $('question').value;
      this.draft.opId = crypto.randomUUID();
      this.changed();
    };
    $('question').onkeydown = event => {
      if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') {
        event.preventDefault(); $('ask').requestSubmit();
      }
    };
    $('ask').onsubmit = event => { event.preventDefault(); this.submit(); };
    // Files ride with the message; a changed set is a new message.
    this.files = window.mevedelAttachments.create({list:$('question-attachments'),
      notice:message => this.notice(message, true),
      onchange:() => { this.draft.opId = crypto.randomUUID(); this.changed(); }});
    window.mevedelAttachments.bind(this.files, {target:$('ask'), input:$('question'),
      button:$('question-attach'), picker:$('question-files')});
    $('assistant').addEventListener('keydown', event => {
      if (event.key === 'Escape') { event.preventDefault(); this.toggle(false); }
      if (event.key === 'Tab' && innerWidth < 800) {
        const nodes = [...$('assistant').querySelectorAll('button, textarea, input, summary, a[href]')]
          .filter(n => !n.disabled && n.getClientRects().length);
        const edge = event.shiftKey ? nodes[0] : nodes.at(-1);
        if (document.activeElement === edge) {
          event.preventDefault(); (event.shiftKey ? nodes.at(-1) : nodes[0]).focus();
        }
      }
    });
  }
  notice(text, error = false) {
    $('assistant-notice').textContent = text || this.recoveryNotice || '';
    $('assistant-notice').dataset.error = String(error);
  }
  find(id) { return this.discussions.find(d => d.id === id); }
  card(id) { return [...$('discussions').children].find(n => n.dataset.discussionId === id); }
  toggle(shown, focus = true) {
    $('assistant').hidden = !shown;
    document.body.classList.toggle('assistant-open', shown);
    $('ask-toggle').setAttribute('aria-expanded', String(shown));
    this.renderDraft();
    this.layout();
    if (!shown) $('ask-toggle').focus({preventScroll:true});
    else if (focus) (this.state().readOnly ? $('assistant-scroll') : $('question')).focus({preventScroll:true});
  }
  /* Open the composer about SCOPE: the whole item, or the current selection,
     which starts a new discussion. */
  begin(scope) {
    try {
      const draft = this.draft, attachment = this.capture(scope);
      if (scope === 'selection') {
        draft.selectionAttachment = attachment;
        draft.discussionId = crypto.randomUUID();
      } else if (draft.attachment?.snapshot.scope === 'selection') draft.selectionAttachment = draft.attachment;
      draft.attachment = attachment;
      draft.opId = crypto.randomUUID();
      this.notice('');
      this.toggle(true);
    } catch (error) {
      this.toggle(true);
      this.notice(error.message, true);
    }
  }
  renderDraft() {
    const a = this.draft.attachment, selection = a?.snapshot.scope === 'selection';
    const documentItem = document.body.dataset.kind === 'document';
    const toAssistant = this.draft.toAssistant !== false;
    const part = (selection ? a : this.draft.selectionAttachment)?.snapshot.region ? 'area' : anchorTerms().part;
    $('context-title').textContent = a
      ? `${selection ? 'New discussion · selected ' + (a.snapshot.region ? 'area' : anchorTerms().part) : 'Whole ' + a.snapshot.kind} · ${a.snapshot.title}`
      : 'Choose content to discuss';
    $('attached-context').open = selection;
    $('context-quote').textContent = a?.quote || '';
    $('context-detail').textContent = a ? JSON.stringify(a.snapshot, null, 2) : '';
    $('whole-question').textContent = documentItem ? 'Whole document' : 'Whole whiteboard';
    $('selected-question').textContent = 'Selected ' + part;
    $('whole-question').setAttribute('aria-pressed', String(a?.snapshot.scope === 'whole'));
    $('selected-question').setAttribute('aria-pressed', String(selection));
    $('selected-question').disabled = !this.draft.selectionAttachment && !selection;
    $('selected-question').title = $('selected-question').disabled ? 'Select content and choose Discuss first.' : 'Discuss the content already attached to this message.';
    $('ask-assistant').checked = toAssistant;
    $('question').placeholder = !selection ? `Write about the whole ${documentItem ? 'document' : 'whiteboard'}…`
      : toAssistant ? `Start a discussion, or leave empty to send the selected ${part} as your request…`
      : `Start a discussion about the selected ${part}…`;
    $('question-send').textContent = selection ? 'Start discussion' : 'Post';
    $('question-send').disabled = this.sending.has('new');
    $('ask').hidden = $('context-scope').hidden = this.state().readOnly;
    this.changed();
  }
  /* Post the composer's message: to the discussion about the whole item, or
     as a new discussion about the selection.  With Ask assistant, the
     assistant answers beneath it; a selection may then be sent alone, since
     what is written in it is the request. */
  async submit() {
    if (this.sending.has('new') || this.state().readOnly) return;
    const a = this.draft.attachment;
    if (!a) { this.begin('whole'); return; }
    const selection = a.snapshot.scope === 'selection', toAssistant = this.draft.toAssistant !== false;
    if (!this.draft.text.trim() && !(selection && toAssistant)) {
      this.notice(selection ? 'Write a message, or choose Ask assistant to send the selection alone.'
        : 'Write a message, or select content to send it alone.', true);
      return;
    }
    const draft = structuredClone(this.draft), id = selection ? draft.discussionId : GENERAL;
    this.sending.add('new');
    $('question').readOnly = true;
    this.renderDraft();
    try {
      this.notice(toAssistant ? 'Saving edits and sending…' : 'Saving edits and posting…');
      await this.files.settled();
      const files = this.files.items();
      await this.save();
      const result = await this.request({action:'discussion', op:'post', messageId:draft.opId,
        text:draft.text, toAssistant,
        ...(selection
          ? {discussionId:id, expected:contentHash(a.snapshot),
             target:a.range ? {kind:'passage', range:a.range}
               : {kind:'objects', selection:a.selection, ...(a.region ? {region:a.region} : {})}}
          : {target:{kind:'whole'}}),
        ...(files.length ? {images:this.files.frame(files)} : {})});
      this.files.remove(files);
      if (result?.discussions) this.stored(result.discussions);
      if (this.draft.opId === draft.opId) {
        Object.assign(this.draft, {text:'', opId:crypto.randomUUID()});
        $('question').value = '';
        // A started discussion continues in its card; the composer returns
        // to the whole item.
        if (selection) {
          Object.assign(this.draft, {discussionId:crypto.randomUUID(), selectionAttachment:null});
          try { this.draft.attachment = this.capture('whole'); } catch { this.draft.attachment = null; }
        }
      }
      this.notice(toAssistant ? 'Sent. The assistant answers in the discussion.' : 'Posted for everyone.');
      this.opening = id;
    } catch (error) { this.notice(error.message, true); }
    finally {
      this.sending.delete('new');
      $('question').readOnly = false;
      this.renderDraft();
      this.setDiscussions(this.discussions);
    }
  }
  /* The target of DISCUSSION as it now stands, for the assistant: the request
     fields that name it and what the sender reviews. */
  reviewed(discussion) {
    if (discussion.id === GENERAL) return {attachment:null, fields:{whole:true}};
    const attachment = this.capture('selection', discussion);
    return {attachment, fields:{range:attachment.range, selection:attachment.selection,
      region:attachment.region, expected:contentHash(attachment.snapshot)}};
  }
  /* Post the reply draft of discussion ID; with TOASSISTANT the assistant
     answers it.  A changed target is reviewed before it is sent. */
  async reply(id, toAssistant) {
    const source = this.drafts.replies[id], discussion = this.find(id);
    if (!source?.text.trim() || !discussion || this.state().readOnly || this.sending.has(id)) return;
    const draft = structuredClone(source);
    let ask = toAssistant && discussion.anchorStatus === 'current', review, posted = false;
    if (ask) {
      try { review = this.reviewed(discussion); } catch { ask = false; }
    }
    this.sending.add(id);
    this.setDiscussions(this.discussions);
    this.notice(ask ? 'Saving edits and sending…' : 'Posting for everyone…');
    try {
      await this.save();
      const result = await this.request({action:'discussion', op:'reply', discussionId:id,
        messageId:draft.opId, text:draft.text, toAssistant:ask, ...(ask ? review.fields : {})});
      if (source.opId === draft.opId) Object.assign(source, newDraft());
      if (result?.discussions) this.stored(result.discussions);
      // A new message is a new request: an earlier one would ask about the
      // discussion as it stood before.
      delete this.drafts.requests[id];
      this.notice(ask ? 'Reply sent. The assistant answers in this discussion.' : 'Reply posted for everyone.');
      posted = true;
    } catch (error) { this.notice(error.message, true); }
    finally { this.sending.delete(id); this.changed(); this.setDiscussions(this.discussions); }
    if (posted && toAssistant && !ask) await this.ask(id);
  }
  /* Send discussion ID as it now stands to the assistant.  With REFRESH, or
     when its target changed, the current target is attached for review first. */
  async ask(id, refresh = false) {
    const discussion = this.find(id);
    if (!discussion || this.state().readOnly || this.sending.has(id)
        || discussion.resolved || discussion.anchorStatus === 'deleted' || !discussion.messages.length) return;
    const version = discussion.messages.at(-1).id;
    let pending = this.drafts.requests[id];
    try {
      if (refresh || !pending || pending.version !== version) {
        const { attachment, fields } = this.reviewed(discussion);
        pending = this.drafts.requests[id] = {attachment, fields, opId:crypto.randomUUID(), version};
        this.changed();
        if (refresh || discussion.anchorStatus === 'changed') {
          pending.review = !refresh;
          this.setDiscussions(this.discussions);
          this.notice(refresh ? anchorTerms().refreshed : anchorTerms().review);
          return;
        }
      }
      if (pending.receipt || pending.review) return;
      this.sending.add(id);
      this.setDiscussions(this.discussions);
      this.notice('Saving edits and sending the discussion…');
      await this.save();
      const result = await this.request({action:'discussion', op:'ask', discussionId:id,
        questionId:pending.opId, version:pending.version, ...pending.fields});
      const delivered = this.conversation.records.some(r => r.shared?.questionId === pending.opId);
      pending.receipt = !delivered;
      this.notice(delivered || result?.delivered ? 'Discussion sent. The assistant answers in it.'
        : 'Discussion queued. The assistant will answer in it.');
    } catch (error) {
      if (pending) pending.failed = true;
      this.notice(error.message, true);
    } finally {
      this.sending.delete(id);
      this.changed();
      this.setDiscussions(this.discussions);
    }
  }
  async resolve(id) {
    try {
      const current = this.find(id);
      const result = await this.request({action:'discussion', op:'resolve', discussionId:id,
        resolved:!current.resolved});
      if (result?.discussions) this.stored(result.discussions);
    } catch (error) { this.notice(error.message, true); }
  }
  openDiscussion(id) {
    this.toggle(true, false);
    const card = this.card(id);
    if (card) {
      card.hidden = false;
      card.open = true;
      card.scrollIntoView({block:'nearest'});
      card.querySelector('summary').focus({preventScroll:true});
    }
  }
  buildCard(discussion) {
    const id = discussion.id, whole = id === GENERAL;
    const card = el('details', undefined, 'discussion');
    card.dataset.discussionId = id;
    card.append(el('summary'));
    if (whole) card.open = true;
    else {
      const show = el('button', anchorTerms().show, 'discussion-show'); show.type = 'button';
      show.onclick = () => {
        try { this.reveal(this.find(id)); if (innerWidth < 800) this.toggle(false); }
        catch (error) { this.notice(error.message, true); }
      };
      card.append(show);
    }
    const status = el('p', undefined, 'thread-status'); status.setAttribute('role','status');
    // Records no message claims, such as answers from before a message was
    // stored, follow the messages.
    const tail = el('div', undefined, 'thread-conversation thread-tail'); tail.dataset.version = '';
    card.append(el('div', undefined, 'thread-messages'), tail, status);
    if (whole) return card;
    const preview = el('blockquote', undefined, 'thread-context');
    const form = el('form', undefined, 'reply-form');
    form.autocomplete = 'off';
    const input = el('textarea'); input.rows = 2; input.maxLength = 10000; input.required = true;
    input.placeholder = 'Reply to this discussion…'; input.setAttribute('aria-label','Reply to discussion');
    const draft = this.drafts.replies[id] ||= newDraft();
    input.value = draft.text;
    input.oninput = () => {
      const current = this.drafts.replies[id] ||= newDraft();
      current.text = input.value; current.opId = crypto.randomUUID(); this.changed();
    };
    input.onkeydown = event => {
      if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') { event.preventDefault(); form.requestSubmit(); }
    };
    const choice = el('label', undefined, 'assistant-choice');
    const assistant = el('input'); assistant.type = 'checkbox'; assistant.checked = true;
    assistant.className = 'reply-assistant';
    choice.append(assistant, ' Ask assistant');
    const post = el('button','Reply'); post.type = 'submit';
    form.onsubmit = event => { event.preventDefault(); this.reply(id, assistant.checked); };
    const row = el('div', undefined, 'discussion-post-row');
    row.append(choice, post);
    form.append(input, row);
    const actions = el('div', undefined, 'discussion-actions');
    const ask = el('button', 'Send to assistant', 'thread-send'); ask.type = 'button';
    ask.onclick = () => this.ask(id);
    const refresh = el('button', 'Review updated context', 'thread-refresh'); refresh.type = 'button';
    refresh.onclick = () => this.ask(id, true);
    const resolve = el('button', 'Resolve', 'thread-resolve'); resolve.type = 'button';
    resolve.onclick = () => this.resolve(id);
    actions.append(ask, refresh, resolve);
    card.append(preview, form, actions);
    return card;
  }
  /* Show DISCUSSIONS, each with its target's live state; the discussion about
     the whole item comes first and always exists. */
  setDiscussions(discussions = []) {
    const known = discussions.find(d => d.id === GENERAL);
    this.discussions = [known || general(), ...discussions.filter(d => d.id !== GENERAL)];
    const list = $('discussions'), readOnly = this.state().readOnly;
    const kind = document.body.dataset.kind === 'document' ? 'document' : 'whiteboard';
    const anchored = this.discussions.filter(d => d.id !== GENERAL);
    const open = anchored.filter(d => !d.resolved).length;
    $('discussion-title').textContent = open ? `Discussions (${open})` : 'Discussions';
    $('discussions-hint').textContent = anchorTerms().empty;
    $('discussions-hint').hidden = anchored.some(d => !d.resolved || $('show-resolved').checked);
    const previous = new Map([...list.children].map(n => [n.dataset.discussionId, n]));
    for (const discussion of this.discussions) {
      const id = discussion.id, whole = id === GENERAL;
      const card = previous.get(id) || this.buildCard(discussion);
      list.append(card);
      previous.delete(id);
      card.classList.toggle('resolved', discussion.resolved);
      card.hidden = discussion.resolved && !$('show-resolved').checked;
      card.querySelector('summary').textContent = whole ? `Whole ${kind}`
        : `${discussion.resolved ? 'Resolved · ' : ''}${headline(discussion.quote)}`;
      const messages = card.querySelector('.thread-messages');
      const shown = new Set([...messages.children].map(n => n.dataset.messageId));
      for (const message of discussion.messages) {
        if (shown.has(message.id)) continue;
        const node = el('div', undefined, 'thread-message'); node.dataset.messageId = message.id;
        const answers = el('div', undefined, 'thread-conversation'); answers.dataset.version = message.id;
        node.append(el('small', who(message.actor)),
          el('p', message.alone ? ALONE : message.text, message.alone ? 'alone' : undefined), answers);
        messages.append(node);
      }
      const working = this.working(id);
      if (whole) {
        card.querySelector('.thread-status').textContent = working ? 'The assistant is working on this' : '';
        continue;
      }
      const pending = this.drafts.requests[id], sending = this.sending.has(id);
      const input = card.querySelector('textarea'), reply = this.drafts.replies[id] ||= newDraft();
      if (input.value !== reply.text) input.value = reply.text;
      card.querySelector('.reply-form').hidden = readOnly || discussion.resolved;
      card.querySelector('.reply-form button').disabled = sending;
      card.querySelector('.discussion-actions').hidden = readOnly;
      card.querySelector('.thread-resolve').textContent = discussion.resolved ? 'Reopen' : 'Resolve';
      card.querySelector('.thread-send').disabled = discussion.resolved || discussion.anchorStatus === 'deleted'
        || sending || !!pending?.receipt || !!pending?.review;
      card.querySelector('.thread-refresh').hidden = !pending?.failed && !pending?.review;
      card.querySelector('.thread-context').hidden = !pending?.attachment;
      card.querySelector('.thread-context').textContent = pending?.attachment?.quote || '';
      card.querySelector('.thread-status').textContent = sending ? 'Sending…'
        : pending?.receipt ? 'Queued for the assistant'
        : pending?.failed ? 'Not confirmed. Retry sends the same request.'
        : discussion.anchorStatus === 'deleted' ? anchorTerms().removed
        : discussion.anchorStatus === 'changed' ? anchorTerms().changed
        : working ? 'The assistant is working on this' : '';
    }
    for (const node of previous.values()) node.remove();
    // A written reply outlives a discussion not yet received after a reload.
    for (const [id, reply] of Object.entries(this.drafts.replies))
      if (!this.find(id) && !reply.text) delete this.drafts.replies[id];
    if (this.opening && this.card(this.opening)?.querySelector('.thread-message')) {
      const id = this.opening;
      this.opening = null;
      this.openDiscussion(id);
    }
    this.renderConversation();
  }
  updateConversation(data) {
    this.conversation = data;
    // A retracted queue entry can be explicitly retried with its original identity.
    for (const pending of Object.values(this.drafts.requests)) {
      if (pending.receipt && data.connected && !data.busy
          && !data.own?.some(e => e.shared?.questionId === pending.opId)
          && !data.records?.some(r => r.shared?.questionId === pending.opId)) pending.receipt = false;
    }
    this.changed();
    this.setDiscussions(this.discussions);
    this.onConversation();
  }
  // Whether the assistant is still on discussion ID: sending, queued, or the
  // session's running turn, until that turn ends.
  working(id) {
    if (this.sending.has(id) || this.drafts.requests[id]?.receipt) return true;
    const { records = [], own = [], busy, connected, active } = this.conversation;
    if (own.some(entry => entry.shared?.discussionId === id)) return true;
    if (!connected || !busy || !active) return false;
    return records.some(r => r.kind === 'user' && r.shared?.questionId === active
      && r.shared.discussionId === id);
  }
  renderConversation() {
    const { records = [], own = [], busy, paused, connected, model,
      conversationTruncated, conversationError } = this.conversation;
    const historyNotice = $('conversation-history-notice');
    historyNotice.textContent = conversationError ? `Earlier conversation unavailable: ${conversationError}`
      : conversationTruncated ? 'Showing recent conversation. Older turns remain in session history.' : '';
    historyNotice.hidden = !historyNotice.textContent;
    for (const [id, pending] of Object.entries(this.drafts.requests)) {
      if (records.some(r => r.shared?.questionId === pending.opId)) {
        delete this.drafts.requests[id];
        this.notice('');
        const card = this.card(id), discussion = this.find(id);
        if (card && discussion) {
          card.querySelector('.thread-send').disabled = this.state().readOnly
            || discussion.resolved || discussion.anchorStatus === 'deleted';
          card.querySelector('.thread-refresh').hidden = card.querySelector('.thread-context').hidden = true;
          card.querySelector('.thread-status').textContent = 'Sent to assistant';
        }
        this.changed();
      }
    }
    const scroll = $('assistant-scroll');
    const follow = scroll.scrollHeight - scroll.scrollTop - scroll.clientHeight < 40;
    const containers = [...$('discussions').querySelectorAll('.thread-conversation')];
    const previous = new Map(containers.flatMap(c => [...c.children]).map(n => [n.dataset.recordId, n]));
    for (const container of containers) container.replaceChildren();
    let context;
    for (const record of records) {
      if (record.kind === 'user') context = record.shared;
      if (!['user','assistant'].includes(record.kind)) continue;
      const node = window.mevedelTranscriptRenderer.renderRecord(record, () => null, undefined, previous.get(record.id));
      if (record.shared) node.dataset.questionId = record.shared.questionId;
      // A request belongs to the message it answers; a record no stored
      // message claims follows its discussion, or the whole item's.
      const card = this.card(context?.discussionId) || this.card(GENERAL);
      const answers = [...card.querySelectorAll('.thread-message .thread-conversation')];
      const target = answers.find(n => n.dataset.version === context?.messageId);
      // The message already shows the request; its turn keeps only who sent
      // it and the context it carried.
      if (target && record.kind === 'user' && record.shared && !record.shared.edited) node.classList.add('thread-question');
      (target || card.querySelector('.thread-tail')).append(node);
    }
    const whole = this.card(GENERAL);
    whole.querySelector('.conversation-empty')?.remove();
    if (!whole.querySelector('.thread-message, .thread-tail > *'))
      whole.querySelector('.thread-tail').before(el('p', 'Discuss this item with the room and the assistant. Messages and answers are shared with everyone.', 'conversation-empty'));
    $('conversation-state').textContent = !connected ? 'Disconnected · drafts kept'
      : busy ? `Assistant working${model ? ' · ' + model : ''}`
      : paused ? `Queue paused on host${own.length ? ' · ' + own.length + ' waiting' : ''}`
      : own.length ? `${own.length} message${own.length === 1 ? '' : 's'} queued for the assistant`
      : records.at(-1)?.status === 'failed' ? 'Assistant request failed · see the discussion' : 'Ready';
    $('conversation-state').classList.toggle('assistant-working', Boolean(connected && busy));
    $('ask-toggle').classList.toggle('assistant-working', Boolean(connected && busy));
    if (follow) scroll.scrollTop = scroll.scrollHeight;
  }
}
