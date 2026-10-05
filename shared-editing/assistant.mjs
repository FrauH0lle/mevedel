/* Item conversation UI; the host owns submissions, comments and transcript truth. */
import { contentHash } from './view.mjs';
const $ = id => document.getElementById(id);
// A board quote spans lines (area, count, objects); a thread header shows
// it on one line, so the parts stay apart once whitespace collapses.
// Sending a thread asks with its latest human message, so the room and the
// model read the actual request; the full thread travels with its context.
const threadRequest = (comment) => (comment.replies?.at(-1) || comment).text;
const headline = (quote) => String(quote || '').split('\n').filter(Boolean).join(' · ');
const el = (tag, text, className) => {
  const node = document.createElement(tag);
  if (text !== undefined) node.textContent = text;
  if (className) node.className = className;
  return node;
};
const newDraft = () => ({text:'', attachment:null, opId:crypto.randomUUID()});
/* Wording for what a comment refers to on this item. */
const anchorTerms = () => document.body.dataset.kind === 'whiteboard'
  ? {show:'Show objects', empty:'Select objects or drag across an area, then choose Add comment. The comment tool (M) does both in one step.',
      removed:'Referenced objects were removed', changed:'Referenced objects have changed',
      review:'The objects have changed. Review their current state before sending.',
      refreshed:'Current objects and discussion attached. Press Send thread to assistant when ready.'}
  : {show:'Show passage', empty:'Select text and choose Add comment to start a discussion.',
      removed:'Referenced passage was removed', changed:'Referenced passage has changed',
      review:'The passage has changed. Review its current text before sending.',
      refreshed:'Current passage and discussion attached. Press Send thread to assistant when ready.'};
export class AssistantPanel {
  constructor({ capture, save, request, changed, reveal, state, restored, onConversation }) {
    Object.assign(this, { capture, save, request, changed, reveal, state,
      onConversation: onConversation || (() => {}) });
    const validDrafts = restored && typeof restored.question?.text === 'string'
      && typeof restored.comment?.text === 'string' && restored.replies && restored.requests;
    this.drafts = validDrafts ? restored : {question:newDraft(), comment:newDraft(), replies:{}, requests:{}, view:'assistant'};
    if (restored && !validDrafts) this.recoveryNotice = 'The saved discussion draft has an unsupported format. Document recovery is unaffected.';
    this.draft = this.drafts.question;
    this.comments = [];
    this.sendingComments = new Set();
    this.conversation = { records: [], own: [], busy: false, connected: true };
    $('question').value = this.draft.text;
    $('comment-text').value = this.drafts.comment.text;
    $('ask-toggle').onclick = () => {
      if ($('assistant').hidden && this.drafts.view === 'assistant' && !this.draft.attachment) this.begin('whole');
      else this.toggle($('assistant').hidden);
    };
    $('assistant-tab').onclick = () => this.draft.attachment ? this.toggle(true, 'assistant') : this.begin('whole');
    $('comments-tab').onclick = () => this.toggle(true, 'comments');
    $('assistant-close').onclick = () => this.toggle(false);
    $('whole-question').onclick = () => this.begin('whole');
    $('selected-question').onclick = () => {
      if (!this.draft.selectionAttachment) return;
      this.draft.attachment = this.draft.selectionAttachment;
      this.draft.opId = crypto.randomUUID();
      this.notice('');
      this.toggle(true, 'assistant');
    };
    $('show-resolved').onchange = () => this.setComments(this.comments);
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
    $('comment-refresh').onclick = () => {
      try {
        this.drafts.comment.attachment = this.capture('selection', this.drafts.comment.attachment);
        this.drafts.comment.opId = crypto.randomUUID();
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
    for (const [id, draft] of [['question',this.draft], ['comment-text',this.drafts.comment]]) {
      $(id).oninput = () => {
        draft.text = $(id).value;
        draft.opId = crypto.randomUUID();
        this.changed();
      };
      $(id).onkeydown = event => {
        if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') {
          event.preventDefault(); $(id).form.requestSubmit();
        }
      };
    }
    $('ask').onsubmit = event => { event.preventDefault(); this.submit(); };
    // Files ride with the question; a changed set is a new question.
    this.files = window.mevedelAttachments.create({list:$('question-attachments'),
      notice:message => this.notice(message, true),
      onchange:() => { this.draft.opId = crypto.randomUUID(); this.changed(); }});
    window.mevedelAttachments.bind(this.files, {target:$('ask'), input:$('question'),
      button:$('question-attach'), picker:$('question-files')});
    $('comment-form').onsubmit = event => {
      event.preventDefault();
      this.postComment(undefined, $('comment-assistant').checked);
    };
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
  toggle(shown, view = this.drafts.view) {
    this.drafts.view = view;
    $('assistant').hidden = !shown;
    document.body.classList.toggle('assistant-open', shown);
    $('ask-toggle').setAttribute('aria-expanded', String(shown));
    $('assistant-tab').setAttribute('aria-pressed', String(view === 'assistant'));
    $('comments-tab').setAttribute('aria-pressed', String(view === 'comments'));
    $('comments-section').hidden = view !== 'comments';
    $('conversation').hidden = $('queued-questions').hidden = $('question-compose').hidden = view !== 'assistant';
    this.renderDraft();
    this.layout();
    if (shown && view === 'assistant') {
      $('question').focus({preventScroll:true});
    } else if (!shown) $('ask-toggle').focus({preventScroll:true});
    else $('comments-tab').focus({preventScroll:true});
  }
  begin(scope, mode = 'question') {
    try {
      const draft = this.drafts[mode];
      const attachment = this.capture(scope);
      if (mode === 'question') {
        if (scope === 'selection') draft.selectionAttachment = attachment;
        else if (draft.attachment?.snapshot.scope === 'selection') draft.selectionAttachment = draft.attachment;
      }
      draft.attachment = attachment;
      draft.opId = crypto.randomUUID();
      this.notice('');
      this.toggle(true, mode === 'comment' ? 'comments' : 'assistant');
      $(mode === 'comment' ? 'comment-text' : 'question').focus({preventScroll:true});
    } catch (error) {
      this.toggle(true, mode === 'comment' ? 'comments' : 'assistant');
      this.notice(error.message, true);
    }
  }
  renderDraft() {
    $('comments-empty').textContent = this.drafts.comment.attachment
      ? 'Your comment will appear here after you post it.'
      : anchorTerms().empty;
    const a = this.draft.attachment;
    $('context-title').textContent = a
      ? `${a.snapshot.scope === 'whole' ? 'Whole ' + a.snapshot.kind : a.snapshot.region ? 'Selected area' : 'Selected content'} · ${a.snapshot.title}`
      : 'Choose content to discuss';
    $('attached-context').open = a?.snapshot.scope === 'selection';
    $('context-quote').textContent = a?.quote || '';
    $('context-detail').textContent = a ? JSON.stringify(a.snapshot, null, 2) : '';
    const documentItem = document.body.dataset.kind === 'document';
    $('whole-question').textContent = documentItem ? 'Whole document' : 'Whole whiteboard';
    $('comment-refresh').textContent = documentItem ? 'Refresh passage' : 'Refresh objects';
    const area = (a?.snapshot.scope === 'selection' ? a : this.draft.selectionAttachment)?.snapshot.region;
    $('selected-question').textContent = documentItem ? 'Selected passage' : area ? 'Selected area' : 'Selected objects';
    $('whole-question').setAttribute('aria-pressed', String(a?.snapshot.scope === 'whole'));
    $('selected-question').setAttribute('aria-pressed', String(a?.snapshot.scope === 'selection'));
    $('selected-question').disabled = !this.draft.selectionAttachment && a?.snapshot.scope !== 'selection';
    $('selected-question').title = $('selected-question').disabled ? 'Select content and choose Ask about selection first.' : 'Use the passage or objects already attached to this question.';
    $('ask').hidden = $('context-scope').hidden = this.state().readOnly;
    $('comment-form').hidden = this.drafts.view !== 'comments' || !this.drafts.comment.attachment || this.state().readOnly;
    $('comment-post').disabled = this.sendingComments.has('new');
    $('comment-quote').textContent = this.drafts.comment.attachment?.quote || '';
    this.changed();
  }
  async submit() {
    if (this.sending || this.state().readOnly || !this.draft.text.trim()) return;
    if (!this.draft.attachment) { this.begin('whole'); return; }
    this.sending = true;
    $('question-send').disabled = true;
    $('question').readOnly = true;
    const draft = structuredClone(this.draft), a = draft.attachment;
    try {
      this.notice('Saving edits and submitting…');
      await this.files.settled();
      const files = this.files.items();
      await this.save();
      const result = await this.request({action:'ask', opId:draft.opId, questionId:draft.opId,
        text:draft.text, expected:contentHash(a.snapshot), range:a.range, selection:a.selection, region:a.region,
        ...(files.length ? {images:this.files.frame(files)} : {})});
      this.files.remove(files);
      this.receipt = { ...result, questionId: draft.opId };
      this.notice(result.delivered ? 'This question is already in the conversation.' : 'Question queued. Your answer will appear here.');
      if (this.draft.opId === draft.opId) {
        this.draft.text = '';
        this.draft.opId = crypto.randomUUID();
        $('question').value = '';
      }
      this.changed();
      this.renderConversation();
    } catch (error) { this.notice(error.message, true); }
    finally {
      this.sending = false;
      $('question-send').disabled = false;
      $('question').readOnly = false;
    }
  }
  // Posting shares the message with everyone; with TOASSISTANT the thread is
  // then sent to the assistant, which answers in it.
  async postComment(commentId, toAssistant = false) {
    const source = commentId ? this.drafts.replies[commentId] : this.drafts.comment;
    if (!source?.text.trim() || this.state().readOnly || this.sendingComments.has(commentId || 'new')) return;
    const draft = structuredClone(source);
    let posted = null;
    this.sendingComments.add(commentId || 'new');
    this.renderDraft();
    this.setComments(this.comments);
    this.notice('Posting for everyone…');
    try {
      await this.save();
      const result = await this.request({action:commentId ? 'reply-comment' : 'comment',
        opId:draft.opId, commentId, text:draft.text,
        ...(commentId ? {} : {range:draft.attachment.range, selection:draft.attachment.selection,
          region:draft.attachment.region, expected:contentHash(draft.attachment.snapshot)})});
      if (source.opId === draft.opId) {
        Object.assign(source, newDraft());
        if (!commentId) $('comment-text').value = '';
      }
      this.setComments(result.comments);
      this.renderDraft();
      this.openComment(commentId || draft.opId);
      this.notice(commentId ? 'Reply posted for everyone.' : 'Comment posted for everyone.');
      posted = commentId || draft.opId;
    } catch (error) { this.notice(error.message, true); }
    finally { this.sendingComments.delete(commentId || 'new'); this.renderDraft(); this.setComments(this.comments); }
    // A new message is a new request: drop an earlier thread request so the
    // assistant sees the thread as it now stands.
    if (posted && toAssistant) {
      delete this.drafts.requests[posted];
      await this.sendComment(posted);
    }
  }
  openComment(id) {
    this.toggle(true, 'comments');
    const card = [...$('comments').children].find(n => n.dataset.commentId === id);
    if (card) {
      card.hidden = false;
      card.open = true;
      card.scrollIntoView({block:'nearest'});
    }
  }
  async sendComment(id, refresh = false) {
    if (this.state().readOnly || this.sendingComments.has(id)) return;
    const comment = this.comments.find(c => c.id === id);
    if (!comment || comment.resolved || comment.anchorStatus === 'deleted') return;
    let pending = this.drafts.requests[id];
    try {
      if (refresh || !pending) {
        const attachment = this.capture('selection', comment);
        pending = this.drafts.requests[id] = {attachment, opId:crypto.randomUUID(),
          version:comment.replies?.at(-1)?.id || id};
        this.changed();
        if (refresh || comment.anchorStatus === 'changed') {
          pending.review = !refresh;
          this.setComments(this.comments);
          this.notice(refresh ? anchorTerms().refreshed : anchorTerms().review);
          return;
        }
      }
      if (pending.receipt || pending.review) return;
      this.sendingComments.add(id);
      this.setComments(this.comments);
      this.notice('Saving edits and sending the discussion…');
      await this.save();
      const a = pending.attachment;
      const result = await this.request({action:'ask', opId:pending.opId, questionId:pending.opId,
        commentId:id, commentVersion:pending.version, text:threadRequest(comment),
        expected:contentHash(a.snapshot), range:a.range, selection:a.selection, region:a.region});
      const delivered = this.conversation.records.some(r => r.shared?.questionId === pending.opId);
      pending.receipt = !delivered;
      this.notice(delivered || result.delivered ? 'Discussion sent. The assistant answers in this thread.' : 'Discussion queued. The assistant will answer in this thread.');
    } catch (error) {
      if (pending) pending.failed = true;
      this.notice(error.message, true);
    } finally {
      this.sendingComments.delete(id);
      this.changed();
      this.setComments(this.comments);
    }
  }
  setComments(comments = []) {
    this.comments = comments;
    const list = $('comments');
    $('comments-tab').textContent = `Comments (${comments.filter(c => !c.resolved).length})`;
    $('comments-empty').hidden = comments.some(c => !c.resolved || $('show-resolved').checked);
    const previous = new Map([...list.children].map(n => [n.dataset.commentId,n]));
    for (const comment of comments) {
      let card = previous.get(comment.id);
      if (!card) {
        card = el('details', undefined, 'comment');
        card.dataset.commentId = comment.id;
        const summary = el('summary', headline(comment.quote));
        const passage = el('button', anchorTerms().show, 'comment-passage');
        passage.type = 'button';
        passage.onclick = () => {
          try { this.reveal(this.comments.find(c => c.id === comment.id)); if (innerWidth < 800) this.toggle(false); }
          catch (error) { this.notice(error.message, true); }
        };
        const messages = el('div', undefined, 'thread-messages');
        const status = el('p', undefined, 'thread-status'); status.setAttribute('role','status');
        const preview = el('blockquote', undefined, 'thread-context');
        const actions = el('div', undefined, 'comment-actions');
        const ask = el('button', 'Send thread to assistant', 'thread-send'); ask.type = 'button';
        ask.onclick = () => this.sendComment(comment.id);
        const refresh = el('button', 'Review updated context', 'thread-refresh'); refresh.type = 'button';
        refresh.onclick = () => this.sendComment(comment.id, true);
        const resolve = el('button', 'Resolve', 'thread-resolve'); resolve.type = 'button';
        resolve.onclick = async () => {
          try {
            const current = this.comments.find(c => c.id === comment.id);
            const result = await this.request({action:'resolve-comment', commentId:comment.id,
              resolved:!current.resolved, opId:crypto.randomUUID()});
            this.setComments(result.comments);
          } catch (error) { this.notice(error.message, true); }
        };
        actions.append(ask, refresh, resolve);
        const form = el('form', undefined, 'reply-form');
        form.autocomplete = 'off';
        const input = el('textarea'); input.rows = 2; input.maxLength = 10000; input.required = true;
        input.placeholder = 'Reply to this discussion…'; input.setAttribute('aria-label','Reply to comment');
        const draft = this.drafts.replies[comment.id] ||= newDraft();
        input.value = draft.text;
        input.oninput = () => { draft.text = input.value; draft.opId = crypto.randomUUID(); this.changed(); };
        input.onkeydown = event => {
          if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') { event.preventDefault(); form.requestSubmit(); }
        };
        const choice = el('label', undefined, 'assistant-choice');
        const assistant = el('input'); assistant.type = 'checkbox'; assistant.checked = true;
        assistant.className = 'reply-assistant';
        choice.append(assistant, ' Send to assistant');
        const post = el('button','Post reply'); post.type = 'submit';
        form.onsubmit = event => { event.preventDefault(); this.postComment(comment.id, assistant.checked); };
        const row = el('div', undefined, 'comment-post-row');
        row.append(choice, post);
        form.append(input,row);
        card.append(summary,passage,messages,status,preview,form,actions);
        list.append(card);
      }
      previous.delete(comment.id);
      card.classList.toggle('resolved', comment.resolved);
      card.hidden = comment.resolved && !$('show-resolved').checked;
      card.querySelector('summary').textContent = `${comment.resolved ? 'Resolved · ' : ''}${headline(comment.quote)}`;
      const messages = card.querySelector('.thread-messages');
      const oldMessages = new Map([...messages.children].map(n => [n.dataset.messageId,n]));
      for (const message of [comment,...(comment.replies || [])]) {
        if (oldMessages.has(message.id)) continue;
        const node = el('div',undefined,'thread-message'); node.dataset.messageId = message.id;
        const answers = el('div',undefined,'thread-conversation'); answers.dataset.version = message.id;
        node.append(el('small',message.actor.replace(/^Guest: /,'')),el('p',message.text),answers);
        messages.append(node);
      }
      const pending = this.drafts.requests[comment.id];
      const input = card.querySelector('textarea');
      if (input.value !== this.drafts.replies[comment.id].text) input.value = this.drafts.replies[comment.id].text;
      card.querySelector('.reply-form').hidden = this.state().readOnly || comment.resolved;
      card.querySelector('.reply-form button').disabled = this.sendingComments.has(comment.id);
      card.querySelector('.comment-actions').hidden = this.state().readOnly;
      card.querySelector('.thread-resolve').textContent = comment.resolved ? 'Reopen' : 'Resolve';
      card.querySelector('.thread-send').disabled = comment.resolved || comment.anchorStatus === 'deleted'
        || this.sendingComments.has(comment.id) || !!pending?.receipt || !!pending?.review;
      card.querySelector('.thread-refresh').hidden = !pending?.failed && !pending?.review;
      card.querySelector('.thread-context').hidden = !pending;
      card.querySelector('.thread-context').textContent = pending?.attachment.quote || '';
      card.querySelector('.thread-status').textContent = this.sendingComments.has(comment.id) ? 'Sending…'
        : pending?.receipt ? 'Queued for the assistant'
        : pending?.failed ? 'Not confirmed. Retry sends the same request.'
        : comment.anchorStatus === 'deleted' ? anchorTerms().removed
        : comment.anchorStatus === 'changed' ? anchorTerms().changed : '';
    }
    for (const node of previous.values()) node.remove();
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
    this.setComments(this.comments);
    this.onConversation();
  }
  // Whether the assistant is still on comment ID's request: sending, queued,
  // or the session's running turn, until that turn ends.
  working(id) {
    if (this.sendingComments.has(id) || this.drafts.requests[id]?.receipt) return true;
    const { records = [], own = [], busy, connected, active } = this.conversation;
    if (own.some(entry => entry.shared?.commentId === id)) return true;
    if (!connected || !busy || !active) return false;
    return records.some(r => r.kind === 'user' && r.shared?.questionId === active
      && r.shared.commentId === id);
  }
  renderConversation() {
    const { records = [], own = [], busy, paused, connected, model,
      conversationTruncated, conversationError } = this.conversation;
    const historyNotice = $('conversation-history-notice');
    historyNotice.textContent = conversationError ? `Earlier conversation unavailable: ${conversationError}`
      : conversationTruncated ? 'Showing recent conversation. Older turns remain in session history.' : '';
    historyNotice.hidden = !historyNotice.textContent;
    if (this.receipt && records.some(r => r.shared?.questionId === this.receipt.questionId)) {
      this.receipt = null;
      this.notice('');
    }
    for (const [id, pending] of Object.entries(this.drafts.requests)) {
      if (records.some(r => r.shared?.questionId === pending.opId)) {
        delete this.drafts.requests[id];
        this.notice('');
        const card = [...$('comments').children].find(n => n.dataset.commentId === id);
        if (card) {
          card.querySelector('.thread-send').disabled = this.state().readOnly
            || this.comments.find(c=>c.id===id)?.resolved || this.comments.find(c=>c.id===id)?.anchorStatus === 'deleted';
          card.querySelector('.thread-refresh').hidden = card.querySelector('.thread-context').hidden = true;
          card.querySelector('.thread-status').textContent = 'Sent to assistant';
        }
        this.changed();
      }
    }
    const scroll = $('assistant-scroll');
    const follow = scroll.scrollHeight - scroll.scrollTop - scroll.clientHeight < 40;
    const containers = [$('conversation'), ...$('comments').querySelectorAll('.thread-conversation')];
    const previous = new Map(containers.flatMap(c => [...c.children]).map(n => [n.dataset.recordId,n]));
    for (const container of containers) container.replaceChildren();
    let context;
    for (const record of records) {
      if (record.kind === 'user') context = record.shared;
      if (!['user','assistant'].includes(record.kind)) continue;
      const node = window.mevedelTranscriptRenderer.renderRecord(record, () => null, undefined, previous.get(record.id));
      if (context?.commentId) node.dataset.commentId = context.commentId;
      if (record.shared) node.dataset.questionId = record.shared.questionId;
      const thread = context?.commentId && [...$('comments').children].find(n => n.dataset.commentId === context.commentId);
      const target = thread && [...thread.querySelectorAll('.thread-conversation')].find(n => n.dataset.version === context.commentVersion);
      // A thread's question is its latest message, already shown above; the
      // turn keeps only who sent it and the context it carried.
      if (target && record.kind === 'user' && record.shared && !record.shared.edited) node.classList.add('thread-question');
      (target || $('conversation')).append(node);
    }
    if (!$('conversation').children.length) $('conversation').append(el('p', 'Ask about this item. Questions and answers are shared with the room.', 'conversation-empty'));
    $('conversation-state').textContent = !connected ? 'Disconnected · drafts kept'
      : busy ? `Assistant working${model ? ' · ' + model : ''}`
      : paused ? `Queue paused on host${own.length ? ' · ' + own.length + ' waiting' : ''}`
      : own.length ? `${own.length} question${own.length === 1 ? '' : 's'} queued`
      : records.at(-1)?.status === 'failed' ? 'Assistant request failed · see conversation' : 'Ready';
    $('conversation-state').classList.toggle('assistant-working', Boolean(connected && busy));
    $('ask-toggle').classList.toggle('assistant-working', Boolean(connected && busy));
    $('queued-questions').replaceChildren();
    for (const entry of own) {
      if (!entry.shared?.commentId) $('queued-questions').append(el('p', `Queued #${entry.position} · ${entry.shared?.text || entry.text}`));
    }
    if (follow) scroll.scrollTop = scroll.scrollHeight;
  }
}
