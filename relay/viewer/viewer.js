/* viewer.js -- MevView: dependency-free sealed collaboration guest */
'use strict';

(() => {
  const transcript = document.getElementById('transcript');
  const emptyState = document.getElementById('empty-state');
  const connection = document.getElementById('connection');
  const notice = document.getElementById('notice');
  const liveButton = document.getElementById('live-button');
  const dock = document.querySelector('.dock');
  const composer = document.getElementById('composer');
  const composerInput = document.getElementById('composer-input');
  const queueState = document.getElementById('queue-state');
  const composerName = document.getElementById('composer-name');
  const stopButton = document.getElementById('stop-button');
  const filterNav = document.getElementById('filter');
  const requests = document.getElementById('requests');
  const attachments = document.getElementById('attachments');
  const attachButton = document.getElementById('attach-button');
  const imageInput = document.getElementById('image-input');
  const sessionLabel = document.getElementById('session-label');
  const notifyButton = document.getElementById('notify-button');
  const composerScope = document.getElementById('composer-scope');
  const ownQueue = document.getElementById('own-queue');
  const skillChips = document.getElementById('skill-chips');
  const commandsBox = document.getElementById('commands-box');
  const commandsSummary = document.getElementById('commands-summary');
  const skillSearch = document.getElementById('skill-search');
  const skillsButton = document.getElementById('skills-button');
  const modeline = document.getElementById('modeline');
  const sessionBox = document.getElementById('session-box');
  const sessionSummary = document.getElementById('session-summary');
  const transportApi = window.mevedelViewerTransport;
  const notificationsApi = window.mevedelViewerNotifications;
  const {base64urlDecode, base64urlEncode, importKey, parseFragment} = transportApi;

  const PROTO = 3;
  const GIVE_UP_MS = 3 * 60 * 1000;
  const MAX_PROMPT_BYTES = 256 * 1024;

  const state = {
    fragment: null,
    readOnly: true,
    records: new Map(),
    elements: new Map(),
    staging: null,
    filter: 'all',
    connected: false,
    unseen: new Set(),
    busy: null,
    roster: [],
    armed: [],
    model: null,
    // Models the host offers for a session this guest creates.
    models: [],
    mode: null,
    plan: false,
    pending: 0,
    paused: false,
    guestName: null,
    pushSubscribed: false,
    // Whether this link carried the owner token.  Cosmetic only: the
    // host re-checks the token on every owner frame, so a page that
    // sets this by hand gains nothing but buttons that get refused.
    owner: false,
    // Whether this link carried the write token; cosmetic like `owner'.
    writable: false,
  };

  let transport = null;

  function send(frame) {
    return transport ? transport.send(frame) : Promise.resolve(false);
  }

  const notifications = notificationsApi.create({
    state, button: notifyButton, send, flash: flashNotice,
    decode: base64urlDecode,
  });

  /* -- Small DOM helpers --------------------------------------------- */

  function el(tag, className, text) {
    const node = document.createElement(tag);
    if (className) node.className = className;
    if (typeof text === 'string') node.textContent = text;
    return node;
  }

  function atLiveEdge() {
    return document.documentElement.scrollHeight - window.scrollY
      - window.innerHeight < 40;
  }

  function scrollToLive() {
    window.scrollTo({top: document.documentElement.scrollHeight, behavior: 'auto'});
  }

  function setConnection(text, className) {
    connection.textContent = text;
    connection.className = `conn ${className || ''}`;
    history.connection(className === 'connected');
    if (className !== 'connected') {
      state.connected = false;
      recoverySignature = '';
      const recoveryAuth = document.getElementById('recovery-auth');
      if (recoveryAuth) recoveryAuth.replaceChildren();
      state.busy = null;
      editing.connection(false);
    }
    renderModeline();
    renderEmptyState();
  }

  function renderEmptyState() {
    emptyState.hidden = !state.connected || !!state.staging || state.records.size > 0;
  }

  function showNotice(text) {
    notice.textContent = text || '';
    notice.hidden = !text;
  }

  // Transient acknowledgements clear themselves; a later persistent
  // notice is never erased by a stale flash timer.
  let flashTimer = null;
  function flashNotice(text) {
    showNotice(text);
    if (flashTimer) clearTimeout(flashTimer);
    flashTimer = window.setTimeout(() => {
      flashTimer = null;
      if (notice.textContent === text) showNotice('');
    }, 4000);
  }

  // The dock's one menu. Commands, sub-agents, tasks, artifacts and room
  // actions each own a fragment of its summary line and a section inside
  // the disclosure, and the box shows for as long as any has something
  // to offer. Key order is display order.
  const summaryParts = {commands: '', agents: '', tasks: '', artifacts: '', room: ''};
  const summaryWarnings = {};
  function summarizeSession(key, text, warning) {
    summaryParts[key] = text || '';
    summaryWarnings[key] = warning === true;
    const bits = Object.values(summaryParts).filter(Boolean);
    sessionBox.hidden = bits.length === 0;
    sessionSummary.textContent = ['Session', ...bits].join(' · ');
    sessionSummary.dataset.warning =
      Object.values(summaryWarnings).some(Boolean) ? 'true' : 'false';
    document.getElementById('activity').hidden = !summaryParts.agents && !summaryParts.tasks;
  }
  function plural(count, noun) {
    return `${count} ${noun}${count === 1 ? '' : 's'}`;
  }

  const artifacts = window.mevedelArtifactView.create({
    send, el, flash: flashNotice, summarize: summarizeSession,
    canComment: () => state.connected && !state.readOnly,
    canDelete: () => state.connected && !state.readOnly,
    busy: () => state.connected && state.busy === true,
    // A project file opened from the lobby can seed a new session; only an
    // owner link may create one there.
    ask: {
      available: () => state.owner && lobby.active(),
      run: path => sessions.openNewSession(
        'Starts a separate session in this project.', `About \`${path}\`: `),
    },
    reveal: id => {
      const turn = state.elements.get(id);
      if (!turn) {
        flashNotice('That comment is no longer in the loaded conversation.');
        return;
      }
      if (typeof turn.scrollIntoView === 'function') {
        turn.scrollIntoView({block: 'center', behavior: 'smooth'});
      }
      if (turn.classList) {
        turn.classList.remove('turn-revealed');
        void turn.offsetWidth;
        turn.classList.add('turn-revealed');
        setTimeout(() => turn.classList.remove('turn-revealed'), 2400);
      }
    },
  });
  const history = window.mevedelHistoryView.create({
    send, el, onArtifacts:refreshFilter,
    renderRecord:record => window.mevedelTranscriptRenderer.renderRecord(
      record, scopeChip, artifacts.open, null, openExecutionResult),
  });
  const agents = window.mevedelAgentView.create({
    send, el, scopeChip, openArtifact: artifacts.open,
    summarize: summarizeSession, openExecution: openExecutionResult,
  });
  const tasks = window.mevedelTaskView.create({el, summarize: summarizeSession});
  const editing = window.mevedelEditingView.create({state, send, el, flash: flashNotice, summarize: summarizeSession,
    onVisibility: window.mevedelAppearance.editorVisible, onCatalog: refreshFilter});
  const sessions = window.mevedelSessionView.create(
    {state, send, el, encode: base64urlEncode, decode: base64urlDecode,
     summarize: summarizeSession, notice: flashNotice});
  const uploads = window.mevedelFilesView.uploader({send});
  const files = window.mevedelFilesView.create(
    {send, el, notice: flashNotice, uploads, openFile: artifacts.openFile});
  const lobby = window.mevedelLobbyView.create(
    {state, send, el, notice: flashNotice, sessions, files});

  let executionResultSequence = 0;
  let pendingExecutionResult = null;
  const executionDialog = el('dialog', 'execution-result-dialog');
  const executionTitle = el('h2');
  const executionBody = el('pre');
  const executionNote = el('p');
  const executionClose = el('button', '', 'Close');
  executionClose.type = 'button';
  executionClose.addEventListener('click', () => executionDialog.close());
  executionDialog.append(executionTitle, executionBody, executionNote, executionClose);
  document.body.append(executionDialog);

  function openExecutionResult(facts) {
    if (typeof facts?.owner !== 'string' || typeof facts.id !== 'string') return;
    const reqId = ++executionResultSequence;
    pendingExecutionResult = {reqId, owner: facts.owner, id: facts.id};
    executionTitle.textContent = facts.command || 'Bash result';
    executionBody.textContent = '';
    executionNote.textContent = 'Loading retained execution result…';
    if (!executionDialog.open) executionDialog.showModal();
    Promise.resolve(send({t: 'execution-result-get', reqId,
                          owner: facts.owner, executionId: facts.id}))
      .then(ok => {
        if (!ok && pendingExecutionResult?.reqId === reqId) {
          executionNote.textContent = 'Result unavailable while disconnected.';
        }
      });
  }

  function showExecutionResult(frame) {
    if (!pendingExecutionResult || frame.reqId !== pendingExecutionResult.reqId
        || frame.owner !== pendingExecutionResult.owner
        || frame.executionId !== pendingExecutionResult.id) return;
    executionBody.textContent = typeof frame.output === 'string' ? frame.output : '';
    executionNote.textContent = frame.error ||
      (frame.source === 'forwarded'
        ? 'Original child row unavailable; showing forwarded retained evidence.'
        : 'Execution result.') +
      (frame.truncated ? ' Output truncated for this guest view.' : '');
  }

  function setLiveButton(visible) {
    liveButton.hidden = !visible;
  }

  function updateLiveAffordance() {
    setLiveButton(!atLiveEdge());
    updateReadingMode();
  }

  // Scrolled well back from the live edge, the guest is reading, not
  // typing, and on a phone the full dock costs half the viewport: the
  // status rows fold away until they tap the composer or return to live.
  // Folding shortens the page, so the fold threshold has to clear a
  // dock's worth of scroll -- otherwise folding lands the guest back at
  // the live edge, which unfolds, which scrolls them off it again.
  function updateReadingMode() {
    const distance = document.documentElement.scrollHeight - window.scrollY
      - window.innerHeight;
    const folded = document.body.hasAttribute('data-reading');
    const next = folded ? distance >= 40 : distance > dock.offsetHeight + 80;
    if (next !== folded) document.body.toggleAttribute('data-reading', next);
  }

  // Recovery never replaces the composer or its draft. Auth challenges remain
  // in memory only and arrive exclusively on the host's owner projection.
  let recoverySignature = '';
  function recoveryAction(action, value, extra = {}) {
    send({t: 'recovery', action, value, ...extra});
  }
  function renderRecoveryIssues(issues) {
    const node = document.getElementById('recovery-issues');
    if (!node) return;
    node.replaceChildren();
    for (const issue of Array.isArray(issues) ? issues : []) {
      if (typeof issue.message === 'string') node.append(el('p', 'recovery-issue', issue.message));
    }
    node.hidden = !node.children.length;
  }
  function renderRecovery(frame) {
    if (!state.owner) return;
    const signature = JSON.stringify(frame);
    if (signature === recoverySignature) return;
    recoverySignature = signature;
    const controls = document.getElementById('recovery-controls');
    const actions = document.getElementById('recovery-actions');
    const auth = document.getElementById('recovery-auth');
    if (!controls || !actions || !auth) return;
    controls.hidden = false;
    actions.replaceChildren();
    auth.replaceChildren();
    const button = (parent, text, action) => {
      const node = el('button', 'btn quiet', text);
      node.type = 'button'; node.addEventListener('click', action); parent.append(node);
    };
    const picker = (label, values, action) => {
      const wrapper = el('label', '', label);
      const select = document.createElement('select');
      select.setAttribute('aria-label', label);
      for (const value of values || []) {
        const option = el('option', '', value); option.value = value; option.selected = action === 'model' && value === frame.model; select.append(option);
      }
      wrapper.append(select); actions.append(wrapper);
      button(actions, `Apply ${label.toLowerCase()}`, () => recoveryAction(action, select.value));
    };
    picker('Model', frame.models, 'model');
    picker('Preset', frame.presets, 'preset');
    button(actions, 'Retry retained input', () => recoveryAction('retry'));
    for (const scope of frame.histories || []) {
      button(actions, `Recover Claude history: ${scope}`, () => {
        if (window.confirm(`Continue ${scope} from a transcript excerpt in a new Claude conversation?`)) recoveryAction('history', scope);
      });
    }
    for (const entry of frame.steering || []) {
      actions.append(el('p', '', `Input requiring review: ${entry.text}`));
      button(actions, 'Queue as new message', () => recoveryAction('input-requeue', entry.id));
      button(actions, 'Discard reviewed input', () => {
        if (window.confirm('Discard this retained input?')) recoveryAction('input-discard', entry.id);
      });
    }
    button(actions, 'Check updates', () => recoveryAction('update'));
    if (frame.runtime && frame.runtime.message) actions.append(el('p', '', frame.runtime.message));
    const provider = document.createElement('select');
    provider.setAttribute('aria-label', 'Login provider');
    for (const name of frame.providers || []) {
      const option = el('option', '', name); option.value = name;
      option.selected = name === frame.provider; provider.append(option);
    }
    actions.append(provider);
    button(actions, 'Sign in', () => recoveryAction('login', null, {provider: provider.value}));
    auth.append(el('p', '', 'Signing in changes the credentials used by this Emacs host.'));
    if (frame.auth) {
      auth.append(el('p', '', frame.auth.message || ''));
      if (frame.auth.url && /^https:\/\/(auth\.openai\.com|claude\.ai|platform\.claude\.com)\//.test(frame.auth.url)) {
        const link = el('a', '', 'Open provider login');
        link.href = frame.auth.url; link.target = '_blank'; link.rel = 'noopener noreferrer'; auth.append(link);
      }
      if (frame.auth.code) auth.append(el('p', '', `One-time code: ${frame.auth.code}`));
      else if (frame.auth.status === 'login' && frame.auth.url) {
        const input = document.createElement('input');
        input.type = 'password'; input.autocomplete = 'off'; input.setAttribute('aria-label', 'Full authorization code');
        auth.append(input);
        button(auth, 'Complete login', () => {
          const value = input.value.trim(); input.value = '';
          recoveryAction('login-code', value, {provider: frame.provider, id: frame.auth.id});
        });
      }
      if (['login', 'refreshing'].includes(frame.auth.status)) {
        button(auth, 'Cancel login', () => recoveryAction('cancel-login', null, {provider: frame.provider}));
      }
    }
  }

  /* -- Status strip -------------------------------------------------- */
  // One home for session state, the way the Emacs mode line reports it,
  // instead of the same facts scattered across three corners.
  function renderModeline() {
    stopButton.hidden = !state.connected || !state.busy;
    document.getElementById('assistant-working').hidden = !state.connected || !state.busy;
    if (!modeline) return;
    modeline.replaceChildren();
    modeline.append(connection);
    const add = (text, className) => {
      if (text) modeline.append(el('span', className || 'ml', text));
    };
    add(state.model);
    if (state.owner && state.mode) modeline.append(sessions.modePicker());
    else add(state.mode);
    // Plan is a mode a guest can enter from a chip, so it has to be
    // visible afterwards -- otherwise the session silently behaves
    // differently than the transcript suggests.
    if (state.plan) add('plan', 'ml plan');
    const tail = el('span', 'ml tail');
    const bits = [];
    if (state.guestName) bits.push(state.guestName);
    if (state.pending) {
      bits.push(`${state.pending} queued${state.paused ? ' · paused' : ''}`);
    }
    tail.textContent = bits.join(' · ');
    modeline.append(tail);
  }

  function updateRecordElement(record, previous) {
    const current = state.elements.get(record.id);
    const turn = window.mevedelTranscriptRenderer.renderRecord(
      record, scopeChip, artifacts.open, previous || current, openExecutionResult);
    if (current) {
      turn.hidden = current.hidden;
      current.replaceWith(turn);
    } else {
      transcript.append(turn);
    }
    state.elements.set(record.id, turn);
    return turn;
  }

  function replaceSnapshot(records) {
    const follow = atLiveEdge();
    const previous = new Map(state.elements);
    state.records.clear();
    state.elements.clear();
    state.unseen.clear();
    transcript.replaceChildren();
    records.forEach(record => {
      if (record && typeof record.id === 'string') state.records.set(record.id, record);
    });
    state.records.forEach(record => {
      updateRecordElement(record, previous.get(record.id));
    });
    refreshFilter();
    editing.refreshConversation();
    window.mevedelTranscriptRenderer.markContinuations(transcript);
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function updateRecord(record) {
    if (!record || typeof record.id !== 'string') return;
    const follow = atLiveEdge();
    state.records.set(record.id, record);
    // Activity outside the selected filter earns its tab an unseen dot.
    if (!recordVisible(record)) state.unseen.add(scopeKey(record) || 'main');
    updateRecordElement(record);
    refreshFilter();
    window.mevedelTranscriptRenderer.markContinuations(transcript);
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function removeRecords(ids) {
    const itemHistoryChanged = (Array.isArray(ids) ? ids : [])
      .some(id => state.records.get(id)?.shared?.itemId);
    (Array.isArray(ids) ? ids : []).forEach(id => {
      state.records.delete(id);
      const turn = state.elements.get(id);
      if (turn) turn.remove();
      state.elements.delete(id);
    });
    refreshFilter();
    if (itemHistoryChanged) editing.refreshConversation();
    window.mevedelTranscriptRenderer.markContinuations(transcript);
  }

  /* -- Directive filter ---------------------------------------------- */
  // Records inside a directive turn carry its id; the menu is derived
  // client-side, so filtering is per-guest and costs no round-trips.

  // Directives and shared items (whiteboards, documents, artifacts) are
  // discussions: records carry their scope, the strip lists each one, and
  // the composer replies into the one selected.
  const scopeKey = record => window.mevedelTranscriptRenderer.scopeKey(record);
  const itemScope = key => typeof key === 'string' && key.startsWith('item:');

  function recordVisible(record) {
    if (state.filter === 'all') return true;
    if (state.filter === 'main') return !scopeKey(record);
    return scopeKey(record) === state.filter;
  }

  function itemTitle(key) {
    const id = key.slice('item:'.length);
    for (const record of state.records.values()) {
      const shared = record.shared;
      if (record.item === id && shared && typeof shared.title === 'string' && shared.title) {
        return shared.title.length > 32 ? `${shared.title.slice(0, 29)}…` : shared.title;
      }
    }
    return id.startsWith('artifact:') ? id.slice('artifact:'.length) : 'Shared item';
  }

  // The chip and tab label for a discussion scope, with its kind's mark.
  function directiveLabel(id) {
    if (itemScope(id)) return `◇ ${itemTitle(id)}`;
    for (const record of state.records.values()) {
      if (record.directive === id && record.kind === 'user' && record.text) {
        const line = record.text.split('\n', 1)[0];
        return `◆ ${line.length > 32 ? `${line.slice(0, 29)}…` : line}`;
      }
    }
    return `◆ ${id.slice(0, 8)}`;
  }

  // A deleted item's turns stay in the room, under All, but it has no
  // discussion left to show or send into: no tab, and a chip that says so.
  let deletedScopes = new Set();
  function scopeChip(scope) {
    return {label: directiveLabel(scope), gone: deletedScopes.has(scope)};
  }

  // Whether SCOPE's discussion still has its item: false once the host has
  // deleted it, null while the host has not yet listed its shared items.
  // CARDS returns the latest artifact card per name.
  function scopePresent(scope, cards) {
    if (!itemScope(scope)) return true;
    const id = scope.slice('item:'.length);
    if (!id.startsWith('artifact:')) return editing.present(id);
    return cards().get(id.slice('artifact:'.length))?.missing !== true;
  }

  // Send TEXT and attachment IMAGES into item ID's conversation; say why
  // when that fails.
  async function discussItem(id, text, images = []) {
    try {
      if (id.startsWith('artifact:')) {
        await artifacts.discuss(id.slice('artifact:'.length), text, images);
      } else {
        await editing.ask(id, text, images);
      }
      flashNotice(`Sent to ${directiveLabel(`item:${id}`)}.`);
      return true;
    } catch (error) {
      flashNotice(error && error.message ? error.message : 'The message could not be sent.');
      return false;
    }
  }

  function selectFilter(value) {
    state.filter = value;
    // Selecting a tab is looking at it; All shows everything.
    if (value === 'all') state.unseen.clear();
    else state.unseen.delete(value);
    refreshFilter();
  }
  // A turn's discussion chip switches the room into that discussion.
  if (transcript) {
    transcript.addEventListener('click', event => {
      const chip = event.target && typeof event.target.closest === 'function'
        ? event.target.closest('.dirchip[data-scope]') : null;
      if (chip) selectFilter(chip.dataset.scope);
    });
  }

  function refreshFilter() {
    renderEmptyState();
    if (filterNav) {
      const ids = [];
      const counts = {all: 0, main: 0};
      state.records.forEach(record => {
        counts.all++;
        const scope = scopeKey(record);
        if (scope) {
          if (!ids.includes(scope)) ids.push(scope);
          counts[scope] = (counts[scope] || 0) + 1;
        } else {
          counts.main++;
        }
      });
      let cards = null;
      const latestCards = () => {
        if (!cards) {
          cards = new Map();
          for (const record of [...history.artifacts(), ...state.records.values()]) {
            if (record.artifact) cards.set(record.artifact, record);
          }
        }
        return cards;
      };
      const presence = new Map(ids.map(id => [id, scopePresent(id, latestCards)]));
      const deleted = new Set(ids.filter(id => presence.get(id) === false));
      const changed = [...deleted, ...deletedScopes].filter(id => deleted.has(id) !== deletedScopes.has(id));
      deletedScopes = deleted;
      if (changed.length) {
        state.records.forEach(record => {
          if (changed.includes(scopeKey(record))) updateRecordElement(record);
        });
      }
      // A shared item's tab waits for the host's item list, so a deleted
      // item's tab does not flash up while a reload replays its turns.
      const listed = ids.filter(id => presence.get(id) === true);
      // The strip is part of the surface once connected, even with no
      // directive yet: an always-present control needs no discovering.
      filterNav.hidden = !state.connected;
      if (state.filter !== 'all' && state.filter !== 'main'
          && !listed.includes(state.filter)) {
        state.filter = 'all';
      }
      filterNav.replaceChildren();
      const add = (value, label, dir) => {
        const unseen = state.unseen.has(value);
        const button = el('button',
                          `${dir ? 'dir' : ''}${unseen ? ' unseen' : ''}`,
                          label);
        button.type = 'button';
        button.setAttribute('aria-pressed',
                            state.filter === value ? 'true' : 'false');
        button.setAttribute(
          'title',
          value === 'all' ? 'Show every turn'
            : value === 'main' ? 'Show only the main conversation'
              : `Show and reply in ${directiveLabel(value)}`);
        if (counts[value]) {
          button.append(el('span', 'cnt', String(counts[value])));
        }
        button.addEventListener('click', () => selectFilter(value));
        filterNav.append(button);
      };
      add('all', 'All');
      if (listed.length) add('main', 'Main chat');
      listed.forEach(id => add(id, directiveLabel(id), true));
    }
    state.records.forEach(record => {
      const turn = state.elements.get(record.id);
      if (turn) turn.hidden = !recordVisible(record);
    });
    artifacts.render([...history.artifacts(), ...state.records.values()]);
    editing.conversation();
    // The composer follows the filter, so say where a prompt will land.
    if (composerInput && !state.armed.length) {
      composerInput.placeholder = placeholderForFilter();
    }
    renderComposerScope();
  }

  function placeholderForFilter() {
    if (state.filter === 'all' || state.filter === 'main') {
      return 'Queue a follow-up for the session…';
    }
    return itemScope(state.filter)
      ? `Message ${directiveLabel(state.filter)}…`
      : `Discuss ${directiveLabel(state.filter)}…`;
  }

  // One line under the composer saying what the next send will do: run
  // an armed invocation, or land in a directive thread.
  function renderComposerScope() {
    if (!composerScope) return;
    composerScope.replaceChildren();
    const scoped = state.filter !== 'all' && state.filter !== 'main';
    if (state.armed.length) {
      composerScope.hidden = false;
      composerScope.className = 'composer-scope armed';
      state.armed.forEach(entry => {
        const chip = el('span', 'scope-selection',
          `${entry.kind === 'skill' ? 'Uses' : 'Runs'} ${sigilFor(entry.kind)}${entry.name}`);
        const clear = el('button', 'scope-clear', '✕');
        clear.type = 'button';
        clear.setAttribute('aria-label', `Remove ${sigilFor(entry.kind)}${entry.name}`);
        clear.addEventListener('click', () => {
          setArmedInvocation(state.armed.filter(item => item !== entry));
          composerInput.focus();
        });
        chip.append(clear);
        composerScope.append(chip);
      });
      return;
    }
    composerScope.className = 'composer-scope';
    composerScope.hidden = !scoped;
    if (scoped) {
      composerScope.append(itemScope(state.filter)
        ? `Sends to ${directiveLabel(state.filter)} · its own conversation`
        : `Sends to ${directiveLabel(state.filter)} · discuss`);
    }
  }

  /* -- Pending interactions ------------------------------------------ */
  // The host presents permission/patch/plan prompts to full-link guests;
  // the first answer (here or in Emacs) settles them everywhere.

  function removeRequest(reqId) {
    if (!requests) return;
    const cards = [...requests.children];
    const card = cards.find(c => c.dataset.reqId === String(reqId));
    if (card) card.remove();
  }

  // A questionnaire answers all questions atomically: option buttons or a
  // custom text per question, then one submit with the answers array.
  function renderQuestionnaire(card, frame) {
    const questions = frame.questions;
    const answers = questions.map(q => (typeof q.answer === 'string' ? q.answer : ''));
    const marks = [];
    questions.forEach((q, index) => {
      const block = el('div', 'question');
      block.append(el('p', 'question-text',
                      `${index + 1}. ${q.question || ''}`));
      const sample = el('pre', 'request-body question-sample');
      sample.hidden = true;
      const showSample = option => {
        sample.textContent = option && typeof option.sample === 'string'
          ? option.sample : '';
        sample.hidden = !sample.textContent;
      };
      const row = el('div', 'request-controls');
      const buttons = [];
      (Array.isArray(q.options) ? q.options : []).forEach(option => {
        const button = el('button', 'btn quiet option', option.label || '');
        button.type = 'button';
        if (option.description) button.setAttribute('title', option.description);
        button.addEventListener('click', () => {
          answers[index] = option.label || '';
          custom.value = '';
          showSample(option);
          marks[index]();
        });
        buttons.push(button);
        row.append(button);
      });
      const custom = el('input', 'request-feedback');
      custom.type = 'text';
      custom.autocomplete = 'off';
      custom.placeholder = 'Custom answer…';
      custom.setAttribute('aria-label', `Custom answer ${index + 1}`);
      custom.addEventListener('input', () => {
        answers[index] = custom.value;
        showSample(null);
        marks[index]();
      });
      if (answers[index]
          && !buttons.some(b => b.textContent === answers[index])) {
        custom.value = answers[index];
      }
      marks[index] = () => {
        buttons.forEach(button => {
          button.setAttribute('aria-pressed',
                              button.textContent === answers[index]
                              && !custom.value
                              ? 'true' : 'false');
        });
      };
      marks[index]();
      row.append(custom);
      block.append(row);
      const selected = (Array.isArray(q.options) ? q.options : [])
        .find(option => option.label === answers[index]);
      showSample(selected);
      block.append(sample);
      card.append(block);
    });
    const submitRow = el('div', 'request-controls');
    const submit = el('button', 'btn', 'Submit answers');
    submit.type = 'button';
    submit.addEventListener('click', () => {
      send({t: 'ui-response', reqId: frame.reqId, answers});
    });
    submitRow.append(submit);
    // Dismiss settles only the questionnaire; the host's run continues.
    if (frame.allowCancel === true) {
      const dismiss = el('button', 'btn quiet', 'Dismiss');
      dismiss.type = 'button';
      dismiss.setAttribute(
        'title', 'Decline the questionnaire; the turn keeps running');
      dismiss.addEventListener('click', () => {
        send({t: 'ui-response', reqId: frame.reqId, cancel: true});
      });
      submitRow.append(dismiss);
    }
    card.append(submitRow);
  }

  function renderRequest(frame) {
    if (!requests) return;
    // The host re-sends the same request on every queue redraw. Rebuilding
    // an unchanged card throws away whatever the guest had scrolled to in
    // a long guardian rationale, so an identical frame is left alone.
    const key = JSON.stringify([frame.body, frame.bodyKind, frame.options,
                                frame.questions, frame.allowFeedback,
                                frame.allowCancel]);
    const existing = [...requests.children].find(
      c => c.dataset && c.dataset.reqId === String(frame.reqId));
    if (existing && existing.frameKey === key) return;
    const scrolled = existing && existing.bodyEl
      ? existing.bodyEl.scrollTop : 0;
    removeRequest(frame.reqId);
    const card = el('section', 'request-card');
    card.frameKey = key;
    card.dataset.reqId = String(frame.reqId);
    card.append(el('span', 'rhead', 'Needs your decision'));
    if (frame.bodyKind === 'diff') {
      const body = window.mevedelTranscriptRenderer.renderDiff(frame.body || '');
      card.bodyEl = body;
      card.append(body);
    } else if (frame.body) {
      const body = el('div', 'request-body',
                      typeof frame.body === 'string' ? frame.body : '');
      card.bodyEl = body;
      card.append(body);
    }
    if (Array.isArray(frame.questions) && frame.questions.length) {
      renderQuestionnaire(card, frame);
      requests.append(card);
      if (scrolled && card.bodyEl) card.bodyEl.scrollTop = scrolled;
      return;
    }
    const controls = el('div', 'request-controls');
    (Array.isArray(frame.options) ? frame.options : []).forEach(option => {
      const button = el('button', 'btn', option.label);
      button.type = 'button';
      button.setAttribute('title', `Answer with "${option.label}"`);
      button.addEventListener('click', () => {
        send({t: 'ui-response', reqId: frame.reqId, option: option.id});
      });
      controls.append(button);
    });
    card.append(controls);
    if (frame.allowFeedback === true) {
      const feedbackRow = el('div', 'request-controls');
      const feedback = el('input', 'request-feedback');
      feedback.type = 'text';
      feedback.autocomplete = 'off';
      feedback.placeholder = 'Feedback…';
      feedback.setAttribute('aria-label', 'Feedback');
      const sendFeedback = el('button', 'btn quiet', 'Send feedback');
      sendFeedback.type = 'button';
      sendFeedback.setAttribute(
        'title', 'Answer with a comment instead of choosing an option');
      sendFeedback.addEventListener('click', () => {
        if (feedback.value.trim()) {
          send({t: 'ui-response', reqId: frame.reqId,
                feedback: feedback.value});
        }
      });
      feedbackRow.append(feedback, sendFeedback);
      const disclosure = el('details', 'decision-feedback');
      disclosure.append(el('summary', '', 'Suggest a change'), feedbackRow);
      card.append(disclosure);
    }
    requests.append(card);
    // A changed body still keeps the reader where they were.
    if (scrolled && card.bodyEl) card.bodyEl.scrollTop = scrolled;
  }

  function clearRequests() {
    if (requests) requests.replaceChildren();
  }

  /* -- Attachments --------------------------------------------------- */

  const tray = window.mevedelAttachments.create(
    {list: attachments, notice: flashNotice, shareable: true});
  // Attachments marked for the project go there as their original files,
  // beside the prompt, which carries its own copies.
  function shareToProject(items) {
    items.filter(item => item.share && item.file).forEach(item => {
      uploads.upload(item.file, '', {rename: true})
        .then(path => flashNotice(`${path} added to the project.`))
        .catch(error => flashNotice(error.message));
    });
  }
  const addFiles = files => tray.add(files);
  let submitting = false;

  /* -- Composer ------------------------------------------------------ */

  function generatedGuestName() {
    const families = [
      [['Brave', 'Bright', 'Breezy'], ['Badger', 'Bear', 'Bison']],
      [['Calm', 'Clever', 'Curious'], ['Capybara', 'Crane', 'Cat']],
      [['Daring', 'Dapper', 'Dreamy'], ['Dolphin', 'Duck', 'Deer']],
      [['Eager', 'Earnest', 'Elegant'], ['Eagle', 'Egret', 'Elephant']],
      [['Fair', 'Friendly', 'Fearless'], ['Fox', 'Finch', 'Falcon']],
      [['Gentle', 'Graceful', 'Gallant'], ['Gecko', 'Gazelle', 'Gibbon']],
      [['Happy', 'Helpful', 'Hopeful'], ['Hedgehog', 'Heron', 'Hare']],
      [['Jolly', 'Jaunty', 'Joyful'], ['Jaguar', 'Jackal', 'Jay']],
      [['Kind', 'Keen', 'Kingly'], ['Koala', 'Kestrel', 'Kiwi']],
      [['Lively', 'Lucky', 'Loyal'], ['Lynx', 'Lemur', 'Lark']],
      [['Merry', 'Mellow', 'Mindful'], ['Mongoose', 'Meerkat', 'Marmot']],
      [['Patient', 'Playful', 'Plucky'], ['Panda', 'Puffin', 'Penguin']],
      [['Quiet', 'Quick', 'Quirky'], ['Quokka', 'Quail', 'Quetzal']],
      [['Ready', 'Radiant', 'Resourceful'], ['Robin', 'Raven', 'Raccoon']],
      [['Sunny', 'Swift', 'Spirited'], ['Sparrow', 'Seal', 'Squirrel']],
      [['Warm', 'Wise', 'Witty'], ['Wombat', 'Wolf', 'Wren']],
    ];
    const picks = crypto.getRandomValues(new Uint32Array(3));
    const [adjectives, animals] = families[picks[0] % families.length];
    return `${adjectives[picks[1] % adjectives.length]} ${animals[picks[2] % animals.length]}`;
  }

  let defaultGuestName;

  function guestName(commit = false) {
    if (!defaultGuestName) {
      let stored;
      try {
        stored = localStorage.getItem('mevedel-guest-name');
        defaultGuestName = localStorage.getItem('mevedel-guest-default-name');
      } catch (_error) { /* Keep names for this page when storage is unavailable. */ }
      defaultGuestName ||= generatedGuestName();
      state.guestName = stored || defaultGuestName;
      try { localStorage.setItem('mevedel-guest-default-name', defaultGuestName); }
      catch (_error) { /* The generated fallback remains usable without storage. */ }
    }
    const name = commit ? (composerName.value.trim() || defaultGuestName) : state.guestName;
    if (composerName && (commit || !composerName.value)) composerName.value = name;
    try { localStorage.setItem('mevedel-guest-name', name); }
    catch (_error) { /* The display name remains usable without storage. */ }
    if (state.guestName !== name) {
      state.guestName = name;
      renderModeline();
    }
    return name;
  }

  // One random id per browser, minted on first use. It lets the host
  // match this guest's own queued entries across reconnects and page
  // reloads; peer numbers cannot, because the relay reassigns them.
  function guestId() {
    let id = null;
    try { id = localStorage.getItem('mevedel-guest-id'); }
    catch (_error) { /* storage unavailable */ }
    if (id && /^[A-Za-z0-9_-]{8,64}$/.test(id)) return id;
    const bytes = crypto.getRandomValues(new Uint8Array(12));
    id = base64urlEncode(bytes);
    try { localStorage.setItem('mevedel-guest-id', id); }
    catch (_error) { /* per-page id then */ }
    return id;
  }

  function setComposerVisible(visible) {
    if (composer) composer.hidden = !visible;
    // Any writable guest may ask for a session; an owner link is what
    // decides whether asking is granted outright or put to the host.
    sessions.setVisible(visible);
  }

  // The host roster is the discovery surface. Skills combine in one
  // message; a slash command is a single action. Selection never sends.
  function sigilFor(kind) {
    return kind === 'skill' ? '$' : '/';
  }

  function setArmedInvocation(entries) {
    state.armed = entries || [];
    renderComposerScope();
    if (composerInput) {
      const entry = state.armed[0];
      composerInput.placeholder = state.armed.length > 1
        ? 'Message for the selected skills…'
        : entry
          ? (entry.hint
             ? `Arguments for ${sigilFor(entry.kind)}${entry.name} — ${entry.hint}`
             : `${sigilFor(entry.kind)}${entry.name} — no arguments needed`)
          : placeholderForFilter();
    }
    renderSkillChips();
  }

  function renderSkillChips() {
    if (!skillChips) return;
    skillChips.replaceChildren();
    if (commandsBox) commandsBox.hidden = state.roster.length === 0;
    if (commandsSummary) {
      commandsSummary.textContent = plural(state.roster.length, 'command');
    }
    skillsButton.hidden = !state.roster.length;
    state.roster.forEach(entry => {
      const armed = state.armed.includes(entry);
      const chip = el('button', `skill-chip${armed ? ' armed' : ''}`,
                      `${sigilFor(entry.kind)}${entry.name}`);
      chip.type = 'button';
      chip.hidden = !`${entry.name} ${entry.hint || ''}`.toLowerCase().includes(skillSearch.value.toLowerCase().trim());
      chip.setAttribute('aria-pressed', armed ? 'true' : 'false');
      chip.setAttribute(
        'title',
        `${armed ? 'Cancel' : 'Prepare'} ${sigilFor(entry.kind)}${entry.name}`
        + (entry.hint ? ` — arguments: ${entry.hint}` : ' — takes no arguments'));
      chip.addEventListener('click', () => {
        const selected = state.armed.filter(item => item.kind === 'skill');
        if (!armed && entry.kind === 'skill' && selected.length >= 6) {
          flashNotice('Select up to six skills for one message.');
          return;
        }
        setArmedInvocation(armed
          ? state.armed.filter(item => item !== entry)
          : entry.kind === 'skill' ? [...selected, entry] : [entry]);
        skillChips.children[state.roster.indexOf(entry)].focus({preventScroll: true});
      });
      skillChips.append(chip);
    });
  }

  function showSkillChips(entries) {
    state.roster = (Array.isArray(entries) ? entries : [])
      .filter(entry => entry && typeof entry.name === 'string' && entry.name)
      .map(entry => ({
        name: entry.name,
        kind: entry.kind === 'skill' ? 'skill' : 'command',
        hint: typeof entry.hint === 'string' ? entry.hint : null,
      }));
    setArmedInvocation([]);
    summarizeSession('commands',
      state.roster.length ? plural(state.roster.length, 'command') : '');
  }

  // The guest's own pending prompts, echoed back per-peer by the host:
  // a persistent card with live position and a retract control, so a
  // queued prompt never reads as swallowed.
  function showOwnQueue(entries) {
    state.ownQueue = entries;
    artifacts.queue(entries);
    editing.conversation();
    if (!ownQueue) return;
    ownQueue.replaceChildren();
    ownQueue.hidden = entries.length === 0;
    entries.forEach(entry => {
      if (!entry || typeof entry.id !== 'number') return;
      const card = el('section', 'own-entry');
      card.append(el('span', 'rhead',
                     typeof entry.position === 'number'
                     ? `Your queued prompt · #${entry.position} in line`
                     : 'Your queued prompt'));
      card.append(el('p', 'own-text',
                     typeof entry.shared?.text === 'string' ? entry.shared.text
                       : typeof entry.text === 'string' ? entry.text : ''));
      const controls = el('div', 'request-controls');
      const retract = el('button', 'btn quiet', 'Retract');
      retract.type = 'button';
      retract.setAttribute('title', 'Take this prompt back out of the queue');
      retract.addEventListener('click', () => {
        send({t: 'retract', id: entry.id});
      });
      controls.append(retract);
      card.append(controls);
      ownQueue.append(card);
    });
  }

  // How many follow-ups are waiting, and whether the host has delivery
  // paused -- otherwise a queued prompt on a busy session looks dropped.
  function showQueueState(frame) {
    state.pending = typeof frame.pending === 'number' ? frame.pending : 0;
    state.paused = frame.paused === true;
    editing.conversation();
    renderModeline();
    if (!queueState) return;
    const pending = state.pending;
    queueState.hidden = pending === 0;
    queueState.className = `queue-state${frame.paused === true ? ' paused' : ''}`;
    if (pending === 0) return;
    queueState.textContent =
      `${pending} follow-up${pending === 1 ? '' : 's'} waiting`
      + (frame.paused === true ? ' — delivery paused in Emacs' : '');
  }

  /* -- Frame handling ------------------------------------------------ */

  // Request ids already notified about, so a re-sent card stays silent.
  const notifiedRequests = new Set();

  function showTerminal(connectionText, noticeText) {
    if (transport) transport.end();
    state.connected = false;
    notifications.forget();
    try { sessionStorage.removeItem('mevedel-tab-share'); }
    catch (_error) { /* storage unavailable */ }
    notifications.render();
    tray.clear();
    clearRequests();
    showOwnQueue([]);
    agents.close();
    artifacts.close();
    agents.show([]);
    tasks.show();
    showQueueState({pending: 0});
    showSkillChips([]);
    setComposerVisible(false);
    sessions.setInviteVisible(false);
    document.getElementById('lobby').hidden = true;
    refreshFilter();
    renderModeline();
    setConnection(connectionText, 'ended');
    showNotice('');
    document.getElementById('terminal-title').textContent = connectionText;
    document.getElementById('terminal-message').textContent = noticeText;
    document.getElementById('terminal-state').hidden = false;
    window.scrollTo({top: 0, behavior: 'instant'});
  }

  function handleFrame(frame) {
    if (!frame || typeof frame.t !== 'string') return;
    if (frame.t === 'recovery') {
      renderRecovery(frame);
    } else if (frame.t === 'welcome') {
      state.readOnly = frame.readOnly !== false;
      state.staging = {records: [], live: []};
      state.connected = true;
      state.busy = null;
      notifications.render();
      setComposerVisible(!state.readOnly);
      showSkillChips(state.readOnly ? [] : frame.commands);
      state.models = Array.isArray(frame.models) ? frame.models : [];
      sessions.setWorkspace(frame.workspace);
      // Active ui-requests are re-sent after the snapshot on every hello.
      clearRequests();
      // The host sends `queue' only when it changes, so a reconnect
      // starts empty rather than showing the previous socket's count;
      // the own-entry card is rebuilt by the hello reply's echo.
      showQueueState({pending: 0});
      showOwnQueue([]);
      // The host re-sends the roster and task list right after this
      // hello's status, so a reconnect starts clean instead of keeping
      // stale chips.
      agents.show([]);
      tasks.show();
      setConnection('Loading…', 'connected');
      editing.welcome();
    } else if (frame.t === 'editing' || frame.t === 'editing-presence') {
      editing.receive(frame);
    } else if (frame.t === 'snapshot-chunk') {
      if (!state.staging) return;
      if (Array.isArray(frame.records)) state.staging.records.push(...frame.records);
      if (frame.final === true) {
        const staged = state.staging;
        state.staging = null;
        replaceSnapshot(staged.records);
        staged.live.forEach(update => {
          if (update.t === 'record') updateRecord(update.record);
          else removeRecords(update.ids);
        });
        setConnection('Connected', 'connected');
        showNotice('');
      }
    } else if (frame.t === 'record') {
      if (state.staging) state.staging.live.push({t: 'record', record: frame.record});
      else updateRecord(frame.record);
    } else if (frame.t === 'remove') {
      if (state.staging) state.staging.live.push({t: 'remove', ids: frame.ids});
      else removeRecords(frame.ids);
    } else if (frame.t === 'queued') {
      flashNotice(typeof frame.position === 'number'
                  ? `Queued — #${frame.position} in line.`
                  : 'Follow-up queued for the session.');
    } else if (frame.t === 'queue') {
      showQueueState(frame);
      showOwnQueue(Array.isArray(frame.own) ? frame.own : []);
    } else if (frame.t === 'agents') {
      agents.show(Array.isArray(frame.agents) ? frame.agents : []);
    } else if (frame.t === 'tasks') {
      tasks.show(frame);
    } else if (frame.t === 'agent') {
      agents.handle(frame);
    } else if (frame.t === 'execution-result') {
      showExecutionResult(frame);
    } else if (frame.t === 'history-index' || frame.t === 'history') {
      history.handle(frame);
    } else if (frame.t === 'artifact') {
      artifacts.handle(frame);
    } else if (frame.t === 'artifact-comment') {
      artifacts.handleComment(frame);
    } else if (frame.t === 'artifact-delete') {
      artifacts.handleDelete(frame);
    } else if (frame.t === 'artifact-comments') {
      artifacts.storedComments(frame);
    } else if (frame.t === 'ui-request') {
      renderRequest(frame);
      // The host re-sends the same request id on every head redraw and
      // re-hello; one interaction earns one notification.
      if (!notifiedRequests.has(frame.reqId)) {
        notifiedRequests.add(frame.reqId);
        notifications.maybeNotify(
          'Pending interaction',
          typeof frame.body === 'string' ? frame.body.slice(0, 120) : '');
      }
    } else if (frame.t === 'ui-request-end') {
      removeRequest(frame.reqId);
      notifiedRequests.delete(frame.reqId);
    } else if (frame.t === 'status') {
      if (state.busy === true && frame.busy !== true) {
        notifications.maybeNotify(
          frame.outcome === 'error' ? 'Turn failed' : ['aborted', 'lost'].includes(frame.outcome) ? 'Turn interrupted' : 'Turn finished',
          frame.outcome === 'error' ? 'Open the session for the failure and recovery actions.' : 'The mevedel session is idle again.');
      }
      renderRecoveryIssues(frame.issues);
      state.busy = frame.busy === true;
      if (typeof frame.model === 'string') state.model = frame.model;
      if (typeof frame.mode === 'string') state.mode = frame.mode;
      state.plan = frame.plan === true;
      // The session's name heads the room and follows a rename, including
      // the title an unnamed session is given after its first prompt.
      if (typeof frame.name === 'string' && frame.name
          && frame.name !== state.sessionName) {
        state.sessionName = frame.name;
        if (sessionLabel) sessionLabel.textContent = frame.name;
        if (!state.editorTitle) {
          // Keep the unseen-activity marker the notifications set.
          const marker = document.title.startsWith('● ') ? '● ' : '';
          document.title = `${marker}${frame.name} · mevedel`;
        }
        sessions.rememberCurrent(frame.name);
      }
      renderModeline();
      editing.conversation();
      artifacts.activity();
    } else if (frame.t === 'lobby') {
      state.models = Array.isArray(frame.models) ? frame.models : [];
      sessions.setWorkspace(frame.workspace);
      lobby.show(frame);
      if (sessionLabel && typeof frame.project === 'string') {
        sessionLabel.textContent = `Lobby · ${frame.project}`;
      }
      setConnection('Connected', 'connected');
    } else if (frame.t === 'open-session') {
      lobby.opened(frame);
    } else if (frame.t === 'delete-session') {
      lobby.deleted(frame);
    } else if (frame.t === 'files') {
      files.listed(frame);
    } else if (frame.t === 'file') {
      artifacts.handle(frame);
    } else if (frame.t === 'file-upload') {
      uploads.handle(frame);
    } else if (frame.t === 'file-remove') {
      files.removed(frame);
    } else if (frame.t === 'files-changed') {
      files.changed(frame);
    } else if (frame.t === 'new-session') {
      sessions.showResult({
        reqId: frame.reqId, ok: frame.ok === true, message: frame.message,
        link: frame.link, name: frame.name,
      });
      lobby.created(frame);
    } else if (frame.t === 'room') {
      sessions.offerRoom({name: frame.name, link: frame.link});
    } else if (frame.t === 'bye') {
      showTerminal('Session ended', 'The host has ended this shared session. '
                   + 'Ask the host for a new invitation to continue.');
    } else if (frame.t === 'notice') {
      // Informational: the host refused one action, the room stays usable.
      if (typeof frame.message === 'string') showNotice(frame.message);
    } else if (frame.t === 'error') {
      showTerminal(
        'Rejected',
        typeof frame.message === 'string' ? frame.message
          : 'The host rejected this connection.');
    }
    // Unknown frame types from a newer host are tolerated silently.
  }

  if (composer) {
    composer.addEventListener('submit', async event => {
      event.preventDefault();
      if (submitting) return;
      submitting = true;
      const text = composerInput.value;
      const armed = state.armed;
      const filter = state.filter;
      const name = guestName(true);
      try {
        await tray.settled();
        const submittedFiles = tray.items();
        // An armed invocation may legitimately carry no arguments.
        if (!text.trim() && !submittedFiles.length && !armed.length) return;
        if (new TextEncoder().encode(text).length > MAX_PROMPT_BYTES) {
          flashNotice('Prompt too large.');
          return;
        }
        if (itemScope(filter)) {
          // An item discussion is its own conversation about that item;
          // commands belong to the main chat.
          if (armed.length) {
            flashNotice('Commands go to the main chat; select Main chat to send them.');
            return;
          }
          if (!text.trim()) return;
          const sent = await discussItem(filter.slice('item:'.length), text.trim(),
                                         tray.frame(submittedFiles));
          if (sent) {
            if (composerInput.value === text) composerInput.value = '';
            tray.remove(submittedFiles);
            shareToProject(submittedFiles);
          }
          return;
        }
        const frame = {t: 'prompt', name};
        if (armed.length) {
          // The name travels as its own field; the host resolves the
          // sigil and validates against its allowlist. Text is arguments.
          if (armed[0].kind === 'skill') frame.skills = armed.map(entry => entry.name);
          else frame.invoke = armed[0].name;
          frame.text = text.trim();
        } else {
          frame.text = text.trim() || 'See the attached file.';
          // Filtering to a directive sends into that directive's discussion,
          // so the reply lands in the thread being read. The host drops an id
          // whose directive is gone and sends to main chat instead.
          if (filter !== 'all' && filter !== 'main') {
            frame.directive = filter;
          }
        }
        if (submittedFiles.length) frame.images = tray.frame(submittedFiles);
        if (!await send(frame)) {
          flashNotice('Connection lost; prompt kept.');
          return;
        }
        if (composerInput.value === text) composerInput.value = '';
        tray.remove(submittedFiles);
        shareToProject(submittedFiles);
        // One tap, one invocation: disarm so the next send is a prompt.
        if (state.armed === armed && armed.length) setArmedInvocation(null);
      } finally {
        submitting = false;
      }
    });
    // The Send button is type="submit", so the form's submit event already
    // covers it; a click handler here would double-send every prompt.
    stopButton.addEventListener('click', () => send({t: 'abort'}));
    window.mevedelAttachments.bind(tray, {
      target: composer, input: composerInput, button: attachButton, picker: imageInput});
    if (composerName) {
      composerName.value = guestName();
      const commitName = () => {
        const previous = state.guestName;
        const name = guestName(true);
        if (name !== previous) send({t: 'set-name', name});
      };
      composerName.addEventListener('blur', commitName);
      composerName.addEventListener('keydown', event => {
        if (event.key === 'Enter' && !event.isComposing) {
          event.preventDefault();
          commitName();
        }
      });
    }
  }

  window.mevedelAppearance.bind(value => {
    artifacts.setTheme(value.theme);
    editing.setAppearance(value);
  });
  const desktopSidebar = window.matchMedia('(min-width:1100px)');
  sessionBox.open = desktopSidebar.matches;
  desktopSidebar.addEventListener('change', event => { sessionBox.open = event.matches; });
  skillSearch.addEventListener('input', renderSkillChips);
  skillsButton.addEventListener('click', () => {
    sessionBox.open = commandsBox.open = true;
    skillSearch.focus();
  });

  notifications.bind();

  liveButton.addEventListener('click', () => {
    scrollToLive();
    setLiveButton(false);
    document.body.removeAttribute('data-reading');
  });
  window.addEventListener('scroll', updateLiveAffordance, {passive: true});
  const rawFragment = notificationsApi.resolveShare(parseFragment);
  const credentials = parseFragment(`#${rawFragment}`);
  if (!credentials) {
    setConnection('Invalid link', 'ended');
    showNotice('This collaboration link is missing or malformed. '
               + 'Open the share link from the host again.');
  } else if (!(crypto && crypto.subtle)) {
    setConnection('Insecure context', 'ended');
    showNotice('This page needs HTTPS (or localhost) to unseal the session.');
  } else {
    state.fragment = rawFragment;
    state.owner = Boolean(credentials.ownerToken);
    state.writable = Boolean(credentials.writeToken);
    try { sessionStorage.setItem('mevedel-tab-share', rawFragment); }
    catch (_error) { /* storage unavailable */ }
    // Handing on access is derived from the secret this page holds, so
    // the controls need it before the socket is even open.
    sessions.useCredentials(credentials);
    // Keep an existing opt-in's persisted share pointing at the room
    // most recently opened.
    if (notifications.enabled()) notifications.persistShare();
    if (sessionLabel) sessionLabel.textContent = credentials.roomId.slice(0, 8);
    // The key remains only in this page's memory; remove it from the URL
    // and history before opening the socket.
    window.history.replaceState(null, '', `${window.location.pathname}${window.location.search}`);
    importKey(credentials.keyBytes).then(key => {
      transport = transportApi.create({
        roomId: credentials.roomId,
        key,
        giveUpMs: GIVE_UP_MS,
        hello: () => {
          const hello = {t: 'hello', proto: PROTO, name: guestName(),
                         guestId: guestId()};
          if (credentials.writeToken) {
            hello.writeToken = base64urlEncode(credentials.writeToken);
          }
          if (credentials.ownerToken) {
            hello.ownerToken = base64urlEncode(credentials.ownerToken);
          }
          return hello;
        },
        onConnection: setConnection,
        onFrame: handleFrame,
        onGiveUp: () => {
          showTerminal(
            'Room closed', 'This room is no longer available. '
              + 'Ask the host for a new invitation to continue.');
        },
        onOpen: async () => {
          if (notifications.enabled()) await notifications.syncPush();
        },
      });
      transport.connect();
    });
  }
  // A share link differs from this page's only in its fragment, which
  // the browser treats as an in-page jump, so opening another link in
  // this tab -- from the address bar, outside, or the lobby -- reloads
  // to join the room it names. The wipe above fires no hashchange.
  window.addEventListener('hashchange', () => {
    if (parseFragment(window.location.hash)) window.location.reload();
  });

  window.mevedelViewer = Object.freeze({
    parseFragment, atLiveEdge, base64urlDecode, base64urlEncode, addFiles,
  });
})();
