/* Item conversation UI; the host owns submissions, comments and transcript truth. */
const $ = id => document.getElementById(id);
const el = (tag, text, className) => {
  const node = document.createElement(tag);
  if (text !== undefined) node.textContent = text;
  if (className) node.className = className;
  return node;
};
export class AssistantPanel {
  constructor({ capture, save, request, changed, reveal, state, restored }) {
    Object.assign(this, { capture, save, request, changed, reveal, state });
    this.draft = restored || { text: '', mode: 'question', attachment: null, opId: crypto.randomUUID() };
    this.comments = [];
    this.conversation = { records: [], own: [], busy: false, connected: true };
    $('question').value = this.draft.text;
    $('ask-toggle').onclick = () => this.toggle($('assistant').hidden);
    $('assistant-close').onclick = () => this.toggle(false);
    $('whole-question').onclick = () => this.begin('whole');
    $('refresh-context').onclick = () => {
      try {
        const a = this.draft.attachment;
        this.draft.attachment = this.capture(a?.snapshot.scope || 'whole', a);
        this.draft.opId = crypto.randomUUID();
        this.notice('Context refreshed. Review it before sending.');
        this.renderDraft();
      } catch (error) { this.notice(error.message, true); }
    };
    const keyboardLayout = () => {
      if (innerHeight < 500 && document.activeElement === $('question')) $('attached-context').open = false;
      for (const node of document.body.children)
        if (node !== $('assistant')) node.inert = !$('assistant').hidden && innerWidth < 800;
    };
    this.layout = keyboardLayout;
    $('question').onfocus = keyboardLayout;
    window.addEventListener('resize', keyboardLayout);
    $('question').oninput = () => {
      this.draft.text = $('question').value;
      this.draft.opId = crypto.randomUUID();
      this.changed();
    };
    $('ask').onsubmit = event => { event.preventDefault(); this.submit(); };
    $('question').onkeydown = event => {
      if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') {
        event.preventDefault(); $('ask').requestSubmit();
      }
    };
    $('assistant').addEventListener('keydown', event => {
      if (event.key === 'Escape') { event.preventDefault(); this.toggle(false); }
      if (event.key === 'Tab' && innerWidth < 800) {
        const nodes = [...$('assistant').querySelectorAll('button, textarea, summary, a[href]')]
          .filter(n => !n.disabled && n.getClientRects().length);
        const edge = event.shiftKey ? nodes[0] : nodes.at(-1);
        if (document.activeElement === edge) {
          event.preventDefault(); (event.shiftKey ? nodes.at(-1) : nodes[0]).focus();
        }
      }
    });
  }
  notice(text, error = false) {
    $('assistant-notice').textContent = text;
    $('assistant-notice').dataset.error = String(error);
  }
  toggle(shown) {
    $('assistant').hidden = !shown;
    document.body.classList.toggle('assistant-open', shown);
    $('ask-toggle').setAttribute('aria-expanded', String(shown));
    this.layout();
    if (shown) {
      if (!this.draft.attachment) this.begin('whole');
      $('question').focus({preventScroll:true});
    } else $('ask-toggle').focus({preventScroll:true});
  }
  begin(scope, comment) {
    try {
      const attachment = this.capture(scope, comment);
      this.draft.attachment = attachment;
      this.draft.commentId = comment?.id;
      this.draft.mode = scope === 'selection' && attachment.snapshot.kind === 'document' && !comment?.id
        ? 'comment' : 'question';
      this.draft.opId = comment?.id ? `comment-${comment.id}` : crypto.randomUUID();
      if (comment?.id) this.draft.text = comment.text;
      $('question').value = this.draft.text;
      this.notice(comment?.anchorStatus === 'changed' ? 'This passage changed after the comment was posted. Review the current context.' : '');
      this.renderDraft();
      if ($('assistant').hidden) this.toggle(true);
      $('question').focus({preventScroll:true});
    } catch (error) {
      // Show capture failures without recapturing or broadening a stale selection.
      $('assistant').hidden = false;
      document.body.classList.add('assistant-open');
      $('ask-toggle').setAttribute('aria-expanded', 'true');
      this.layout();
      this.notice(error.message, true);
    }
  }
  renderDraft() {
    const a = this.draft.attachment;
    $('context-title').textContent = a
      ? `${a.snapshot.scope === 'whole' ? 'Whole ' + a.snapshot.kind : 'Selected content'} · ${a.snapshot.title}`
      : 'Choose content to discuss';
    $('context-quote').textContent = a?.quote || '';
    $('context-detail').textContent = a ? JSON.stringify(a.snapshot, null, 2) : '';
    $('compose-label').textContent = this.draft.mode === 'comment' ? 'New comment · private until posted' : 'Ask the assistant';
    $('question-send').textContent = this.draft.mode === 'comment' ? 'Post comment' : 'Send to assistant';
    $('ask').hidden = this.state().readOnly;
    $('whole-question').hidden = this.state().readOnly;
    this.changed();
  }
  async submit() {
    if (this.sending || this.state().readOnly || !this.draft.text.trim()) return;
    if (!this.draft.attachment) { this.begin('whole'); return; }
    this.sending = true;
    $('question-send').disabled = true;
    $('question').readOnly = true;
    // Keep one request identity and its complete draft across failed/uncertain delivery.
    const draft = structuredClone(this.draft), a = draft.attachment;
    try {
      this.notice('Saving edits and submitting…');
      await this.save();
      const comment = draft.mode === 'comment';
      const result = await this.request({
        action: comment ? 'comment' : 'ask', opId: draft.opId, questionId: draft.opId,
        commentId: draft.commentId, text: draft.text,
        expected: a.snapshot, range: a.range, selection: a.selection,
      });
      if (comment) {
        this.setComments(result.comments);
        const posted = this.comments.find(c => c.id === draft.opId);
        if (posted && this.draft.opId === draft.opId) this.begin('selection', posted);
        this.notice('Comment posted for everyone. Send it to the assistant when ready.');
      } else {
        this.receipt = { ...result, questionId: draft.opId };
        this.notice(result.delivered ? 'This question is already in the conversation.' : 'Question queued. Your answer will appear here.');
        // Follow-ups retain the chosen scope but get a new delivery identity.
        if (this.draft.opId === draft.opId) {
          this.draft.text = '';
          this.draft.opId = crypto.randomUUID();
          $('question').value = '';
        }
        this.changed();
        this.renderConversation();
      }
    } catch (error) {
      this.notice(error.message, true);
    } finally {
      this.sending = false;
      $('question-send').disabled = false;
      $('question').readOnly = false;
    }
  }
  setComments(comments = []) {
    this.comments = comments;
    const list = $('comments');
    list.replaceChildren();
    $('comments-section').hidden = !comments.length;
    $('comments-count').textContent = `Comments (${comments.filter(c => !c.resolved).length} open)`;
    for (const comment of comments) {
      const card = el('article', undefined, 'comment');
      card.dataset.commentId = comment.id;
      card.classList.toggle('resolved', comment.resolved);
      card.append(el('small', `${comment.actor.replace(/^Guest: /, '')}${comment.resolved ? ' · Resolved' : ''}`));
      const passage = el('button', comment.quote, 'comment-passage');
      passage.type = 'button';
      passage.title = 'Show referenced passage';
      passage.onclick = () => {
        try { this.reveal(comment.range); if (innerWidth < 800) this.toggle(false); }
        catch (error) { this.notice(error.message, true); }
      };
      card.append(passage, el('p', comment.text));
      if (comment.anchorStatus !== 'current')
        card.append(el('small', comment.anchorStatus === 'deleted' ? 'Referenced passage was removed' : 'Referenced passage has changed'));
      const actions = el('div', undefined, 'comment-actions');
      const ask = el('button', 'Send to assistant');
      ask.type = 'button'; ask.hidden = this.state().readOnly;
      ask.disabled = comment.anchorStatus === 'deleted';
      ask.onclick = () => this.begin('selection', comment);
      const answer = el('button', 'View conversation');
      answer.type = 'button';
      answer.onclick = () => {
        this.toggle(true);
        const node = [...$('conversation').querySelectorAll('[data-comment-id]')].find(n => n.dataset.commentId === comment.id);
        if (node) node.scrollIntoView({block:'start'});
        else this.notice('No delivered question for this comment yet. Send it explicitly to start a conversation.');
      };
      const resolve = el('button', comment.resolved ? 'Reopen' : 'Resolve');
      resolve.type = 'button'; resolve.hidden = this.state().readOnly;
      resolve.onclick = async () => {
        try {
          const result = await this.request({action:'resolve-comment', commentId:comment.id, resolved:!comment.resolved, opId:crypto.randomUUID()});
          this.setComments(result.comments);
        } catch (error) { this.notice(error.message, true); }
      };
      actions.append(ask, answer, resolve); card.append(actions); list.append(card);
    }
  }
  updateConversation(data) {
    this.conversation = data;
    this.renderConversation();
  }
  renderConversation() {
    const { records = [], own = [], busy, paused, connected, model } = this.conversation;
    if (this.receipt && records.some(r => r.shared?.questionId === this.receipt.questionId)) {
      this.receipt = null;
      this.notice('');
    }
    const scroll = $('assistant-scroll');
    const follow = scroll.scrollHeight - scroll.scrollTop - scroll.clientHeight < 40;
    const previous = new Map([...$('conversation').children].map(n => [n.dataset.recordId,n]));
    $('conversation').replaceChildren();
    let context;
    for (const record of records) {
      if (record.kind === 'user') context = record.shared;
      if (!['user','assistant'].includes(record.kind)) continue;
      const display = record.shared ? {...record, text:record.shared.text} : record;
      const node = window.mevedelTranscriptRenderer.renderRecord(display, () => '', undefined, previous.get(record.id));
      if (context?.commentId) node.dataset.commentId = context.commentId;
      if (record.shared) {
        node.dataset.questionId = record.shared.questionId;
        const details = el('details', undefined, 'sent-context');
        details.open = previous.get(record.id)?.querySelector('.sent-context')?.open || false;
        details.append(el('summary', record.shared.edited ? 'Edited on host · see delivered prompt'
          : `${record.shared.scope === 'whole' ? 'Whole item' : 'Selection'} · revision ${record.shared.revision}`),
          el('blockquote', record.shared.edited ? 'The host revised this queued question. Its delivered text above replaces the original attachment.' : record.shared.quote));
        node.append(details);
      }
      $('conversation').append(node);
    }
    if (!records.length) $('conversation').append(el('p', 'Ask about this item. Replies and follow-ups stay beside your work.', 'conversation-empty'));
    $('conversation-state').textContent = !connected ? 'Disconnected · draft kept'
      : paused ? `Queue paused on host${own.length ? ' · ' + own.length + ' waiting' : ''}`
      : busy ? `Assistant working${model ? ' · ' + model : ''}`
      : own.length ? `${own.length} question${own.length === 1 ? '' : 's'} queued`
      : records.at(-1)?.status === 'failed' ? 'Assistant request failed · see conversation' : 'Ready';
    $('queued-questions').replaceChildren();
    for (const entry of own) $('queued-questions').append(el('p', `Queued #${entry.position} · ${entry.shared?.text || entry.text}`));
    if (follow) scroll.scrollTop = scroll.scrollHeight;
  }
}
