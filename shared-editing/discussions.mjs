/* The item's Discussions sidebar; the host owns discussions, submissions and
   transcript truth.  The shared view (relay viewer-discussions.js) lists the
   discussions and shows one at a time; this panel supplies the item's
   discussions, captures what a new discussion is about, and sends posts,
   replies and requests to the assistant. */
import { contentHash, GENERAL_DISCUSSION as GENERAL } from './view.mjs';
const $ = id => document.getElementById(id);
const NEW = 'new';
// A board quote spans lines (area, count, objects); a title shows it on one
// line, so the parts stay apart once whitespace collapses.
const headline = (quote) => String(quote || '').split('\n').filter(Boolean).join(' · ');
const who = (actor) => String(actor || '').replace(/^Guest: /, '');
// A fresh draft keeps the Ask assistant choice of the one it replaces.
const newReply = (toAssistant = true) => ({text:'', opId:crypto.randomUUID(), toAssistant});
const newStart = (toAssistant = true) => ({text:'', attachment:null, opId:crypto.randomUUID(),
  discussionId:crypto.randomUUID(), toAssistant});
const board = () => document.body.dataset.kind === 'whiteboard';
const noun = () => board() ? 'whiteboard' : 'document';
/* Wording for what a discussion refers to on this item. */
const anchorTerms = () => board()
  ? {show:'Show on board', part:'objects',
      hint:'Select objects or drag across an area to discuss them. The Discuss tool (M) does both in one step.',
      removed:'Objects removed', changed:'Objects changed since posting',
      review:'The objects have changed. Review their current state before sending.',
      refreshed:'Current objects and discussion attached. Press Send to assistant when ready.'}
  : {show:'Show passage', part:'passage',
      hint:'Select text to discuss it.',
      removed:'Passage removed', changed:'Passage changed since posting',
      review:'The passage has changed. Review its current text before sending.',
      refreshed:'Current passage and discussion attached. Press Send to assistant when ready.'};
const general = () => ({id:GENERAL, target:{kind:'whole'}, quote:'', messages:[], resolved:false,
  anchorStatus:'current'});
export class DiscussionPanel {
  constructor({ capture, save, request, changed, reveal, state, stored, restored, onChange }) {
    Object.assign(this, { capture, save, request, changed, reveal, state, stored,
      onChange: onChange || (() => {}) });
    const validDrafts = restored?.format === 'discussions' && typeof restored.start?.text === 'string'
      && restored.replies && typeof restored.replies === 'object'
      && restored.requests && typeof restored.requests === 'object';
    this.drafts = validDrafts ? restored : {format:'discussions', start:newStart(), replies:{}, requests:{}};
    this.discussions = [general()];
    // Discussions with a submission in flight; 'new' is a starting one.
    this.sending = new Set();
    this.conversation = { records: [], own: [], busy: false, connected: true };
    this.selection = null;
    this.current = 'discussions';
    const terms = anchorTerms();
    this.sidebar = window.mevedelDiscussionSidebar.create({
      root: $('discussion-sidebar'), noun: noun(),
      canWrite: () => !this.state().readOnly,
      visible: () => !$('assistant').hidden && this.current === 'discussions',
      draft: {
        get: key => key === NEW ? this.drafts.start : this.drafts.replies[key],
        set: (key, patch) => {
          const target = key === NEW ? this.drafts.start : (this.drafts.replies[key] ||= newReply());
          Object.assign(target, patch, {opId:crypto.randomUUID()});
          this.changed();
        },
      },
      submit: (key, message) => key === NEW ? this.start(message)
        : key === GENERAL ? this.post(message) : this.reply(key, message),
      startNew: () => this.begin(),
      reveal: id => {
        try { this.reveal(this.find(id)); if (innerWidth < 800) this.toggle(false); }
        catch (error) { this.notice(error.message, true); }
      },
      revealLabel: terms.show,
      refresh: () => this.refresh(),
      menu: d => this.menu(d),
      extras: d => this.extras(d),
      attachments: true,
      selectHint: terms.hint,
      // Back from a new discussion drops it.
      leaveNew: () => { this.drafts.start = newStart(this.drafts.start.toAssistant); this.changed(); },
      onView: () => this.changed(),
      onRender: () => this.rendered(),
    });
    if (restored && !validDrafts) this.notice('The saved discussion draft has an unsupported format. Document recovery is unaffected.');
    // Files ride with a post; a changed set is a new message.
    const composer = this.sidebar.composer();
    this.files = window.mevedelAttachments.create({list:composer.files,
      notice:message => this.notice(message, true),
      onchange:() => { this.drafts.start.opId = crypto.randomUUID(); this.changed(); }});
    window.mevedelAttachments.bind(this.files, {target:composer.form, input:composer.input,
      button:composer.attach, picker:composer.picker});
    $('ask-toggle').onclick = () => {
      if (!$('assistant').hidden && this.current === 'discussions') this.toggle(false);
      else { this.tab('discussions'); this.toggle(true); }
    };
    $('assistant-close').onclick = () => this.toggle(false);
    $('discussions-tab').onclick = () => this.tab('discussions');
    $('library-tab').onclick = () => this.tab('library');
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
      for (const node of document.body.children)
        if (node !== $('assistant')) node.inert = !$('assistant').hidden && innerWidth < 800;
    };
    window.addEventListener('resize', this.layout);
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
  /* Show a new discussion restored with the drafts when the sidebar opens. */
  resume() {
    if (!this.drafts.start.attachment) return;
    this.sidebar.update({fresh:this.fresh()});
    this.sidebar.show({kind:NEW});
  }
  notice(text, error = false) { this.sidebar.notice(text, error); }
  find(id) { return this.discussions.find(d => d.id === id); }
  /* The new discussion's attachment while it is on screen, for highlighting. */
  pending() {
    return !$('assistant').hidden && this.current === 'discussions' && this.sidebar.view().kind === NEW
      ? this.drafts.start.attachment : null;
  }
  unread(id) { return this.sidebar.unread(id); }
  /* What Discuss would start about now: LABEL such as "selected passage", or
     null.  Called on every board frame, so only a change renders. */
  selectable(label) {
    if (label === this.selection) return;
    this.selection = label;
    this.sidebar.update({selection:label ? {label} : null});
  }
  /* Show sidebar tab NAME: the discussions, or the whiteboard's library. */
  tab(name) {
    this.current = name;
    for (const tab of ['discussions', 'library']) {
      $(`${tab}-tab`).setAttribute('aria-selected', String(tab === name));
      $(`${tab}-panel`).hidden = tab !== name;
    }
    this.toggles();
    if (name === 'library') this.onLibrary?.();
    else this.sidebar.update();
  }
  /* The canvas or toolbar buttons say which tab the open sidebar shows. */
  toggles() {
    const shown = !$('assistant').hidden;
    $('ask-toggle').setAttribute('aria-expanded', String(shown && this.current === 'discussions'));
    $('library-toggle')?.setAttribute('aria-expanded', String(shown && this.current === 'library'));
  }
  toggle(shown, focus = true) {
    $('assistant').hidden = !shown;
    document.body.classList.toggle('assistant-open', shown);
    this.toggles();
    this.layout();
    // Opening shows what was unread in the open discussion.
    this.sidebar.update();
    this.changed();
    if (!shown) $(this.current === 'library' ? 'library-toggle' : 'ask-toggle')?.focus({preventScroll:true});
    else if (focus && this.current === 'library') $('library-tab').focus({preventScroll:true});
    else if (focus) this.sidebar.focus();
  }
  fresh() {
    const a = this.drafts.start.attachment, part = a?.region ? 'area' : anchorTerms().part;
    return a ? {label:`Selected ${part}`, quote:a.quote || '',
      placeholder:`Start a discussion, or leave empty to send the selected ${part} as your request…`} : null;
  }
  /* Open a new discussion about the current selection. */
  begin() {
    this.tab('discussions');
    try {
      Object.assign(this.drafts.start, {attachment:this.capture('selection'),
        discussionId:crypto.randomUUID(), opId:crypto.randomUUID()});
      this.sidebar.update({fresh:this.fresh()});
      this.sidebar.show({kind:NEW});
      this.toggle(true);
    } catch (error) {
      this.toggle(true);
      this.notice(error.message, true);
    }
  }
  /* Attach the new discussion's target as it is now. */
  refresh() {
    try {
      const a = this.drafts.start.attachment;
      Object.assign(this.drafts.start, {attachment:this.capture('selection', a), opId:crypto.randomUUID()});
      this.sidebar.update({fresh:this.fresh()});
      this.notice('Context refreshed. Review it before sending.');
      this.changed();
    } catch (error) { this.notice(error.message, true); }
  }
  /* Start the new discussion with TEXT.  With TOASSISTANT the assistant
     answers beneath it, and the selection may be sent alone, since what is
     written in it is the request.  Returns the discussion's id. */
  async start({ text, toAssistant }) {
    const draft = structuredClone(this.drafts.start), a = draft.attachment;
    if (this.sending.has(NEW) || this.state().readOnly || !a) return false;
    if (!text.trim() && !toAssistant) {
      this.notice('Write a message, or choose Ask assistant to send the selection alone.', true);
      return false;
    }
    this.sending.add(NEW);
    try {
      this.notice(toAssistant ? 'Saving edits and sending…' : 'Saving edits and posting…');
      await this.files.settled();
      const files = this.files.items();
      await this.save();
      const result = await this.request({action:'discussion', op:'post', messageId:draft.opId,
        text, toAssistant, expected:contentHash(a.snapshot), discussionId:draft.discussionId,
        target:a.range ? {kind:'passage', range:a.range}
          : {kind:'objects', selection:a.selection, ...(a.region ? {region:a.region} : {})},
        ...(files.length ? {images:this.files.frame(files)} : {})});
      this.files.remove(files);
      if (this.drafts.start.opId === draft.opId) this.drafts.start = newStart(toAssistant);
      if (result?.discussions) this.stored(result.discussions);
      this.notice(toAssistant ? 'Sent. The assistant answers here.' : 'Posted for everyone.');
      return draft.discussionId;
    } catch (error) { this.notice(error.message, true); return false; }
    finally { this.sending.delete(NEW); this.changed(); }
  }
  /* Post TEXT to the discussion about the whole item. */
  async post({ text, toAssistant }) {
    const draft = structuredClone(this.drafts.replies[GENERAL] ||= newReply());
    if (this.sending.has(GENERAL) || this.state().readOnly) return false;
    if (!text.trim()) { this.notice('Write a message first.', true); return false; }
    this.sending.add(GENERAL);
    try {
      this.notice(toAssistant ? 'Saving edits and sending…' : 'Posting for everyone…');
      await this.files.settled();
      const files = this.files.items();
      await this.save();
      const result = await this.request({action:'discussion', op:'post', messageId:draft.opId,
        text, toAssistant, target:{kind:'whole'},
        ...(files.length ? {images:this.files.frame(files)} : {})});
      this.files.remove(files);
      if (this.drafts.replies[GENERAL].opId === draft.opId) this.drafts.replies[GENERAL] = newReply(toAssistant);
      if (result?.discussions) this.stored(result.discussions);
      this.notice(toAssistant ? 'Sent. The assistant answers here.' : 'Posted for everyone.');
    } catch (error) { this.notice(error.message, true); return false; }
    finally { this.sending.delete(GENERAL); this.changed(); this.sidebar.update(); }
  }
  /* The target of DISCUSSION as it now stands, for the assistant: the request
     fields that name it and what the sender reviews. */
  reviewed(discussion) {
    if (discussion.id === GENERAL) return {attachment:null, fields:{whole:true}};
    const attachment = this.capture('selection', discussion);
    return {attachment, fields:{range:attachment.range, selection:attachment.selection,
      region:attachment.region, expected:contentHash(attachment.snapshot)}};
  }
  /* Post TEXT as a reply to discussion ID; with TOASSISTANT the assistant
     answers it.  A changed target is reviewed before it is sent. */
  async reply(id, { text, toAssistant }) {
    const source = this.drafts.replies[id] ||= newReply(), discussion = this.find(id);
    if (!text.trim() || !discussion || this.state().readOnly || this.sending.has(id)) return false;
    const draft = structuredClone(source);
    let ask = toAssistant && discussion.anchorStatus === 'current', review, posted = false;
    if (ask) {
      try { review = this.reviewed(discussion); } catch { ask = false; }
    }
    this.sending.add(id);
    this.sidebar.update();
    this.notice(ask ? 'Saving edits and sending…' : 'Posting for everyone…');
    try {
      await this.save();
      const result = await this.request({action:'discussion', op:'reply', discussionId:id,
        messageId:draft.opId, text, toAssistant:ask, ...(ask ? review.fields : {})});
      if (source.opId === draft.opId) this.drafts.replies[id] = newReply(toAssistant);
      if (result?.discussions) this.stored(result.discussions);
      // A new message is a new request: an earlier one would ask about the
      // discussion as it stood before.
      delete this.drafts.requests[id];
      this.notice(ask ? 'Reply sent. The assistant answers here.' : 'Reply posted for everyone.');
      posted = true;
    } catch (error) { this.notice(error.message, true); }
    finally { this.sending.delete(id); this.changed(); this.sidebar.update(); }
    if (posted && toAssistant && !ask) await this.ask(id);
    return posted ? undefined : false;
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
          this.sidebar.update();
          this.notice(refresh ? anchorTerms().refreshed : anchorTerms().review);
          return;
        }
      }
      if (pending.receipt || pending.review) return;
      this.sending.add(id);
      this.sidebar.update();
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
      this.sidebar.update();
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
  /* Show discussion ID in the sidebar. */
  openDiscussion(id) {
    this.tab('discussions');
    this.sidebar.show({kind:'thread', id});
    this.toggle(true, false);
    // Reading comes first; a phone's keyboard stays down.
    this.sidebar.focus(true);
  }
  /* The ⋯ menu of the discussion the sidebar shows as VIEW. */
  menu(view) {
    const id = view.id, discussion = this.find(id);
    if (id === GENERAL || this.state().readOnly) return [];
    const pending = this.drafts.requests[id];
    return [
      {label:'Ask assistant again', className:'thread-send', run:() => this.ask(id),
       title:'Send this discussion as it stands to the assistant',
       disabled:discussion.resolved || discussion.anchorStatus === 'deleted' || this.sending.has(id)
         || !!pending?.receipt || !!pending?.review},
      {label:discussion.resolved ? 'Reopen' : 'Resolve', className:'thread-resolve',
       reopen:discussion.resolved, run:() => this.resolve(id)},
    ];
  }
  /* What the foot of VIEW's messages adds: the state of a request to the
     assistant, the content it attaches for review, and how to go on. */
  extras(view) {
    const id = view.id, discussion = this.find(id), pending = this.drafts.requests[id], sending = this.sending.has(id);
    const terms = anchorTerms(), working = this.working(id);
    const status = sending ? 'Sending…'
      : pending?.receipt ? 'Queued for the assistant'
      : pending?.failed ? 'Not confirmed. Retry sends the same request.'
      : id !== GENERAL && discussion.anchorStatus === 'deleted' ? terms.removed
      : id !== GENERAL && discussion.anchorStatus === 'changed' ? terms.changed
      : working ? 'The assistant is working on this' : '';
    const actions = [];
    if (!this.state().readOnly && pending && !pending.receipt) {
      if (pending.review || pending.failed)
        actions.push({label:'Review updated context', className:'thread-refresh', run:() => this.ask(id, true)});
      if (!pending.review)
        actions.push({label:pending.failed ? 'Retry' : 'Send to assistant', className:'thread-send primary',
          run:() => this.ask(id), disabled:sending});
    }
    return {status, tone:pending?.failed || /changed|removed/i.test(status) ? 'warn' : working ? 'working' : '',
      quote:pending?.attachment?.quote || '', actions};
  }
  /* A list row's state: what needs attention before reading. */
  rowState(discussion) {
    const id = discussion.id, terms = anchorTerms();
    if (this.working(id)) return {text:'Assistant working', tone:'working'};
    if (this.drafts.requests[id]?.failed) return {text:'Request not confirmed', tone:'warn'};
    if (id === GENERAL || discussion.resolved) return null;
    if (discussion.anchorStatus === 'deleted') return {text:terms.removed, tone:'warn'};
    if (discussion.anchorStatus === 'changed') return {text:terms.changed, tone:'warn'};
    return null;
  }
  /* Show DISCUSSIONS, each with its target's live state; the discussion about
     the whole item comes first and always exists. */
  setDiscussions(discussions = [], extra = {}) {
    const known = discussions.find(d => d.id === GENERAL);
    this.discussions = [known || general(), ...discussions.filter(d => d.id !== GENERAL)];
    const anchored = this.discussions.filter(d => d.id !== GENERAL);
    const open = anchored.filter(d => !d.resolved).length;
    for (const count of [$('discussion-count'), $('ask-count')]) {
      count.textContent = String(open);
      count.hidden = !open;
    }
    $('ask-toggle').setAttribute('aria-label', open ? `Discussions, ${open} open` : 'Discussions');
    // Board pins number the open discussions about objects, as drawn.
    const pins = new Map(anchored.filter(d => d.target?.kind === 'objects' && !d.resolved)
      .map((d, index) => [d.id, index + 1]));
    // A written reply outlives a discussion not yet received after a reload.
    for (const [id, reply] of Object.entries(this.drafts.replies))
      if (!this.find(id) && !reply.text) delete this.drafts.replies[id];
    this.sidebar.update({discussions:this.discussions.map(d => ({
      id:d.id, general:d.id === GENERAL, pin:pins.get(d.id), resolved:d.resolved,
      title:headline(d.quote), quote:d.quote, revealable:d.anchorStatus !== 'deleted',
      messages:d.messages.map(m => ({id:m.id, actor:who(m.actor), text:m.text, alone:m.alone})),
      state:this.rowState(d)})), ...extra});
  }
  updateConversation(data) {
    this.conversation = data;
    // A retracted queue entry can be explicitly retried with its original identity.
    let retracted = false;
    for (const pending of Object.values(this.drafts.requests)) {
      if (pending.receipt && data.connected && !data.busy
          && !data.own?.some(e => e.shared?.questionId === pending.opId)
          && !data.records?.some(r => r.shared?.questionId === pending.opId)) {
        pending.receipt = false;
        retracted = true;
      }
    }
    // A delivered request has done its work.
    let delivered = false;
    for (const [id, pending] of Object.entries(this.drafts.requests)) {
      if (data.records?.some(r => r.shared?.questionId === pending.opId)) {
        delete this.drafts.requests[id];
        delivered = true;
      }
    }
    if (delivered) this.notice('');
    // Streaming sends an update per chunk; saving drafts encodes the whole item.
    if (retracted || delivered) this.changed();
    this.renderConversation();
    this.setDiscussions(this.discussions, {records:data.records || []});
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
  /* The sidebar rendered: unread marks follow on the toggle and the pins. */
  rendered() {
    $('ask-toggle').classList.toggle('unread', this.sidebar.unreadCount() > 0);
    this.onChange();
  }
  renderConversation() {
    const { records = [], own = [], busy, paused, connected, model,
      conversationTruncated, conversationError } = this.conversation;
    const historyNotice = $('conversation-history-notice');
    historyNotice.textContent = conversationError ? `Earlier conversation unavailable: ${conversationError}`
      : conversationTruncated ? 'Showing recent conversation. Older turns remain in session history.' : '';
    historyNotice.hidden = !historyNotice.textContent;
    // The status line speaks only when there is something to say.
    $('conversation-state').textContent = !connected ? 'Disconnected · drafts kept'
      : busy ? `Assistant working${model ? ' · ' + model : ''}`
      : paused ? `Queue paused on host${own.length ? ' · ' + own.length + ' waiting' : ''}`
      : own.length ? `${own.length} message${own.length === 1 ? '' : 's'} queued for the assistant`
      : records.at(-1)?.status === 'failed' ? 'Assistant request failed · see the discussion' : '';
    $('conversation-state').hidden = !$('conversation-state').textContent;
    $('conversation-state').classList.toggle('assistant-working', Boolean(connected && busy));
    $('ask-toggle').classList.toggle('assistant-working', Boolean(connected && busy));
  }
}
