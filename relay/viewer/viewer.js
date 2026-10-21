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
  const roomButton = document.getElementById('room-button');
  const transportApi = window.mevedelViewerTransport;
  const notificationsApi = window.mevedelViewerNotifications;
  const {base64urlDecode, base64urlEncode, importKey, parseFragment} = transportApi;

  const PROTO = 3;
  const GIVE_UP_MS = 3 * 60 * 1000;
  // Covers the host's relay dial after it hands out a new room's link.
  const JOIN_GRACE_MS = 5 * 1000;
  const MAX_PROMPT_BYTES = 256 * 1024;

  const state = {
    fragment: null,
    readOnly: true,
    records: new Map(),
    elements: new Map(),
    staging: null,
    filter: 'main',
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
    // The project the room belongs to, from the host.
    project: null,
    // Whether the project's lobby runs: the owner link to it, true, or
    // false; undefined until the host says.
    lobby: undefined,
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
      sessions.disconnected();
      state.connected = false;
      pendingBackgroundSource = null;
      if (backgroundSourceTimer) clearTimeout(backgroundSourceTimer);
      recoveryFrame = null;
      recoverySignature = '';
      recoveryAuthSignature = '';
      recoveryProvider = '';
      // Settings act through the host; they wait for the next connection.
      const settings = document.getElementById('settings');
      if (settings?.open) settings.close();
      const recoveryAuth = document.getElementById('recovery-auth');
      if (recoveryAuth) recoveryAuth.replaceChildren();
      state.busy = null;
      editing.connection(false);
    }
    renderModeline();
    background.refresh();
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
  const summaryParts = {commands: '', agents: '', tasks: '', artifacts: ''};
  const summaryWarnings = {};
  function summarizeSession(key, text, warning) {
    summaryParts[key] = text || '';
    summaryWarnings[key] = warning === true;
    const bits = Object.values(summaryParts).filter(Boolean);
    sessionBox.hidden = bits.length === 0;
    if (roomButton) roomButton.hidden = sessionBox.hidden;
    sessionSummary.textContent = bits.join(' · ');
    sessionSummary.dataset.warning =
      Object.values(summaryWarnings).some(Boolean) ? 'true' : 'false';
    document.getElementById('activity').hidden = !summaryParts.agents && !summaryParts.tasks;
  }
  function plural(count, noun) {
    return `${count} ${noun}${count === 1 ? '' : 's'}`;
  }

  // The page is an open whiteboard, document or artifact, else the room.
  const presence = window.mevedelPresence.create(
    {send, el, page: () => editing.page() || artifacts.page()});
  const artifacts = window.mevedelArtifactView.create({
    send, el, href: destinationURL, flash: flashNotice, summarize: summarizeSession,
    openConversation: id => activeStore().conversation(id),
    onPage: () => { agents.cancelArtifactResolution(); presence.report(); },
    // A lobby has no welcome; its link tier decides instead.
    canDiscuss: () => (state.connected && !state.readOnly)
      || (lobby.active() && state.writable),
    canDelete: () => (state.connected && !state.readOnly)
      || (lobby.active() && state.writable),
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
        flashNotice('That request is no longer in the loaded conversation.');
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
      record, scopeChip, artifacts.open, null, openExecutionResult,
      path => agents.open({path}), destinationURL),
  });
  const agents = window.mevedelAgentView.create({
    send, el, scopeChip, openArtifact: artifacts.open,
    openExecution: openExecutionResult, flash: flashNotice, href: destinationURL,
  });
  const background = window.mevedelBackgroundView.create({state, send, el, href: destinationURL,
    openAgent: row => agents.open(row), openExecution: openExecutionResult,
    showConversation: locateBackground,
  });
  let backgroundSourceSequence = 0, pendingBackgroundSource = null, backgroundSourceTimer = null;

  function locateBackground(row) {
    if (backgroundSourceTimer) clearTimeout(backgroundSourceTimer);
    const reqId = ++backgroundSourceSequence;
    pendingBackgroundSource = {reqId, row};
    backgroundSourceTimer = setTimeout(() => {
      pendingBackgroundSource = null;
      flashNotice('Source navigation timed out. Reconnect or try again.');
    }, 20000);
    send(row.kind === 'agent' ? {t: 'background-locate', reqId, path: row.path}
      : {t: 'background-locate', reqId, owner: row.owner, id: row.id}).then(ok => {
      if (!ok && pendingBackgroundSource?.reqId === reqId) {
        pendingBackgroundSource = null;
        clearTimeout(backgroundSourceTimer);
        flashNotice('Source navigation is unavailable while disconnected.');
      }
    });
  }

  async function showBackgroundSource(frame) {
    if (!pendingBackgroundSource || frame.reqId !== pendingBackgroundSource.reqId) return;
    const {row} = pendingBackgroundSource;
    pendingBackgroundSource = null;
    clearTimeout(backgroundSourceTimer);
    if (!state.connected) return;
    if (frame.unavailable) {
      flashNotice(frame.unavailable);
      if (row.kind !== 'agent') openExecutionResult({owner: row.owner, id: row.id});
    } else if (frame.owner && frame.owner !== '/root') {
      agents.open({path: frame.owner, revealTool: frame.toolUseId,
        segment: frame.current ? null : frame.segment});
    } else if (frame.current) {
      selectFilter('all');
      if (!window.mevedelTranscriptRenderer.revealTool(transcript, frame.toolUseId)) {
        flashNotice('The located source has not reached the browser yet. Try again after the next update.');
      }
    } else if (!await history.reveal(frame.segment, frame.toolUseId)) {
      flashNotice('Could not load the located source. Reconnect or retry the archive.');
    }
  }
  const tasks = window.mevedelTaskView.create({el, summarize: summarizeSession});
  const goal = window.mevedelGoalView.create({el, onChange: renderModeline});
  const editing = window.mevedelEditingView.create({state, send, el, href: destinationURL, flash: flashNotice, summarize: summarizeSession,
    onVisibility: () => presence.report(),
    onCatalog: refreshFilter,
    // A docked item is the chat's subject: the chat shows and writes to its
    // discussion, and In this room steps aside behind the header's button.
    onDock: (on, id) => {
      sessionBox.open = !on && window.matchMedia('(min-width:1100px)').matches;
      if (on && id) selectFilter(`item:${id}`);
    },
    // The editor's whole-item row leads to the chat, writing to that item.
    onFocusChat: () => {
      const id = editing.dockedItem();
      if (id) selectFilter(`item:${id}`);
      document.getElementById('composer-input')?.focus();
    }});
  // The host learns a new name at once; a hello carries the current one.
  const names = window.mevedelGuestName.create({
    initials: window.mevedelPresence.initials,
    onChange: name => {
      state.guestName = name;
      send({t: 'set-name', name});
      editing.setName(name);
    },
  });
  names.ready.then(name => { state.guestName = name; });
  const sessions = window.mevedelSessionView.create(
    {state, send, el, encode: base64urlEncode, decode: base64urlDecode,
     notice: flashNotice});
  const uploads = window.mevedelFilesView.uploader({send});
  const files = window.mevedelFilesView.create(
    {send, el, notice: flashNotice, uploads, openFile: artifacts.openFile, href: destinationURL});
  // The workspace artifact store: the room's Project artifacts sheet and
  // the lobby's Artifacts tab; one is active at a time.
  const storeSheet = document.getElementById('store-sheet');
  const roomStore = window.mevedelStoreView.create({
    send, el, state, room: true, notice: flashNotice, href: destinationURL,
    navigate: link => nav.navigate(link),
    open: record => {
      storeSheet.close();
      if (record.item && !record.version) editing.openItem(record.store);
      else artifacts.open(record);
    },
    list: document.getElementById('store-list'),
    empty: document.getElementById('store-empty'),
  });
  function openStore() {
    roomStore.refresh();
    storeSheet.showModal();
    nav.note({view: 'artifacts'});
  }
  storeSheet.addEventListener('close', () => nav.note({view: null}));
  const lobbyStore = window.mevedelStoreView.create({
    send, el, state, open: artifacts.open, notice: flashNotice, href: destinationURL,
    navigate: link => nav.navigate(link),
    creation: document.getElementById('lobby-store-create'),
    list: document.getElementById('lobby-store-list'),
    empty: document.getElementById('lobby-store-empty'),
  });
  const lobby = window.mevedelLobbyView.create(
    {state, send, el, notice: flashNotice, sessions, files, store: lobbyStore,
     navigate: link => nav.navigate(link),
     onTab: tab => nav.note({tab}),
     initialTab: () => nav.entry().tab});
  const nav = window.mevedelNav.create({
    state, el, parse: parseFragment, sessions, lobby, openStore,
    openRooms: sessions.openRooms,
    panels: [
      {node: document.getElementById('artifact-panel'), describe: artifacts.describe,
       owns: page => Boolean(page.artifact || page.file),
       reopen: page => {
         if (page.file) artifacts.openFile(page.file);
         else if (page.artifact.agent) {
           artifacts.close();
           agents.resolveArtifact(page.artifact);
         } else artifacts.open(page.artifact);
       },
       close: artifacts.close},
      {node: document.getElementById('agent-panel'), describe: agents.describe,
       owns: page => Boolean(page.agent), reopen: page => agents.open(page.agent),
       close: agents.close},
    ],
  });
  const activeStore = () => (lobby.active() ? lobbyStore : roomStore);
  let storeArtifacts = null;

  // Only host-resolved identifiers travel in links, not arbitrary frame
  // payloads. Keep content selectors in the fragment, out of relay requests.
  function destinationURL(destination) {
    const params = {};
    if (destination.shared) params.shared = destination.shared;
    else if (destination.file) params.file = destination.file;
    else if (destination.agent) {
      params.agent = destination.agent.path;
      if (destination.agent.segment) params.segment = destination.agent.segment;
    } else if (destination.artifact) {
      const record = destination.artifact;
      if (record.store && record.id === `artifact:${record.store}`) params.artifact = record.store;
      else params.record = record.id;
      if (record.version) params.version = record.version;
      if (record.agent) {
        params.agent = record.agent.path;
        if (record.agent.segment) params.segment = record.agent.segment;
      }
    }
    return window.mevedelNav.roomURL(state.fragment, params);
  }

  // Import a fresh tab's closed destination vocabulary once. Ordinary panel
  // history then owns it after startup scrubs the fragment.
  function routeState(fragment) {
    const separator = fragment.indexOf('?');
    if (separator < 0) return {};
    const params = new URLSearchParams(fragment.slice(separator + 1));
    const positive = key => {
      const value = params.get(key);
      const number = Number(value);
      return /^[1-9][0-9]*$/.test(value || '') && Number.isSafeInteger(number) ? number : null;
    };
    const route = {};
    if (['sessions', 'artifacts', 'files'].includes(params.get('tab'))) route.tab = params.get('tab');
    if (params.get('view') === 'artifacts') route.view = 'artifacts';
    if (params.get('file')) route.panel = {file: params.get('file')};
    else if (params.get('artifact') || params.get('record')) {
      const store = params.get('artifact');
      route.panel = {artifact: {id: store ? `artifact:${store}` : params.get('record'),
        ...(store ? {store} : {}),
        ...(params.get('agent') ? {agent: {path: params.get('agent'),
          ...(positive('segment') ? {segment: positive('segment')} : {})}} : {}),
        ...(positive('version') ? {version: positive('version')} : {})}};
    } else if (params.get('agent')) {
      route.panel = {agent: {path: params.get('agent'), ...(positive('segment') ? {segment: positive('segment')} : {})}};
    }
    return route;
  }

  let destinationRestored = false;
  function restoreDestination() {
    nav.restore();
    if (destinationRestored) return;
    destinationRestored = true;
    if (!lobby.active() && nav.entry().view === 'artifacts' && !nav.entry().panel) openStore();
  }

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
    executionTitle.textContent = facts.command || 'Execution output';
    executionBody.textContent = '';
    executionNote.textContent = 'Loading execution output…';
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
        : frame.source === 'live' ? 'Running execution output snapshot.' : 'Execution result.') +
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

  // Session settings: one sheet for an owner's session choices and what
  // needs attention.  Recovery never replaces the composer or its draft.
  // Auth challenges remain in memory only and arrive exclusively on the
  // host's owner projection.  The login block renders separately so other
  // updates keep a typed code.  Every control explains itself in a title
  // and, since phones have no hover, in visible help text.
  const MODES = [
    ['ask', 'Ask', 'Review every file change; commands that might change something ask first.'],
    ['edits', 'Edits', 'Edit workspace files and run sandboxed commands without asking; anything more asks.'],
    ['full-auto', 'Full auto', 'No sandbox and no questions: acts with the full access of the host account.'],
  ];
  let recoveryFrame = null;
  let recoverySignature = '';
  let recoveryAuthSignature = '';
  let recoveryProvider = '';
  function recoveryAction(action, value, extra = {}) {
    send({t: 'recovery', action, value, ...extra});
  }
  function settingsButton(parent, text, title, action) {
    const node = el('button', 'btn quiet', text);
    node.type = 'button'; node.title = title; node.addEventListener('click', action); parent.append(node);
    return node;
  }
  function needsAttention(frame) {
    return Boolean(frame && (frame.paused === true || frame.issues?.length
                             || frame.steering?.length || frame.histories?.length));
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
  function openSettings() {
    const dialog = document.getElementById('settings');
    if (!dialog || !recoveryFrame) return;
    renderModeChoices();
    if (!dialog.open) {
      document.getElementById('settings-name').value = state.sessionName || '';
      dialog.showModal();
    }
  }
  function renderModeChoices() {
    const group = document.getElementById('settings-mode');
    const help = document.getElementById('settings-mode-help');
    if (!group || !help) return;
    group.replaceChildren(...MODES.map(([mode, label, text]) => {
      const node = el('button', 'btn quiet option', label);
      node.type = 'button'; node.title = text;
      node.setAttribute('aria-pressed', String(mode === state.mode));
      // The host answers with a status frame; until it does the sheet
      // keeps reporting the mode the session is actually in.
      node.addEventListener('click', () => { if (mode !== state.mode) send({t: 'set-mode', mode}); });
      return node;
    }));
    help.textContent = MODES.find(([mode]) => mode === state.mode)?.[2]
      || 'What the assistant may do without asking first.';
    const plan = document.getElementById('settings-plan');
    if (plan) {
      plan.hidden = !state.plan;
      plan.textContent = 'Plan is active: work stays read-only except for the session plan. '
        + 'Changing permissions does not leave Plan.';
    }
  }
  function renderAttention(frame) {
    const section = document.getElementById('settings-attention');
    const issues = document.getElementById('settings-issues');
    const actions = document.getElementById('recovery-actions');
    if (!section || !issues || !actions) return;
    issues.replaceChildren();
    actions.replaceChildren();
    for (const issue of frame.issues || []) {
      if (typeof issue.message === 'string') issues.append(el('p', 'settings-issue', issue.message));
    }
    if (frame.paused === true) {
      settingsButton(actions, 'Resume queue',
        'Check the provider again and send the messages that waited after the failed turn.',
        () => recoveryAction('retry'));
    }
    for (const scope of frame.histories || []) {
      const what = scope === 'root' ? 'this session' : scope;
      settingsButton(actions, scope === 'root' ? 'Continue from transcript' : `Continue ${scope} from transcript`,
        'Claude cannot resume its own conversation. Start a new one from an excerpt of the transcript; what the excerpt leaves out is lost to Claude.',
        () => {
          if (window.confirm(`Continue ${what} from a transcript excerpt in a new Claude conversation?`)) recoveryAction('history', scope);
        });
    }
    for (const entry of frame.steering || []) {
      actions.append(el('p', 'settings-input', `Waiting for review: ${entry.text}`));
      settingsButton(actions, 'Queue again', 'Send this message again, as a new message.',
        () => recoveryAction('input-requeue', entry.id));
      settingsButton(actions, 'Discard', 'Drop this message without sending it.', () => {
        if (window.confirm('Discard this retained input?')) recoveryAction('input-discard', entry.id);
      });
    }
    section.hidden = !issues.children.length && !actions.children.length;
  }
  function renderModelChoices(frame) {
    const select = document.getElementById('settings-model');
    if (!select) return;
    const models = (frame.models || []).filter(model => typeof model === 'string');
    const options = models.map(model => {
      const option = el('option', '', model); option.value = model; return option;
    });
    // A current model the host no longer offers is shown, never replaced
    // by whichever option happens to come first.
    if (typeof frame.model === 'string' && !models.includes(frame.model)) {
      const current = el('option', '', `${frame.model} (unavailable)`);
      current.value = frame.model; current.disabled = true; options.unshift(current);
    }
    select.replaceChildren(...options);
    select.value = typeof frame.model === 'string' ? frame.model : '';
  }
  function renderEffortChoices(frame) {
    const group = document.getElementById('settings-effort-group');
    const select = document.getElementById('settings-effort');
    if (!group || !select) return;
    const efforts = frame.efforts || [];
    const fallback = el('option', '', 'Default'); fallback.value = '';
    select.replaceChildren(fallback, ...efforts.map(effort => {
      const option = el('option', '', effort); option.value = effort; return option;
    }));
    select.value = efforts.includes(frame.effort) ? frame.effort : '';
    group.hidden = select.disabled = efforts.length === 0;
  }
  function renderPresetChoices(frame) {
    const select = document.getElementById('settings-preset');
    if (!select) return;
    const wanted = select.value;
    const placeholder = el('option', '', 'Choose a preset…'); placeholder.value = '';
    select.replaceChildren(placeholder, ...(frame.presets || []).map(name => {
      const option = el('option', '', name); option.value = name; return option;
    }));
    select.value = (frame.presets || []).includes(wanted) ? wanted : '';
  }
  function renderAccount(frame) {
    const section = document.getElementById('settings-account');
    const title = document.getElementById('settings-account-title');
    const actions = document.getElementById('settings-account-actions');
    if (!section || !title || !actions) return;
    actions.replaceChildren();
    const providers = frame.providers || [];
    section.hidden = !frame.provider && !frame.runtime && providers.length === 0;
    if (section.hidden) return;
    title.textContent = frame.provider ? `Account · ${frame.provider}` : 'Account';
    // The owner's choice survives re-renders; otherwise follow the host.
    const chosen = providers.includes(recoveryProvider) ? recoveryProvider : frame.provider || providers[0];
    let provider = null;
    if (providers.length > 1 || (providers.length && !frame.provider)) {
      provider = document.createElement('select');
      provider.className = 'sheet-input';
      provider.title = 'Provider to sign in to';
      provider.setAttribute('aria-label', 'Login provider');
      for (const name of providers) {
        const option = el('option', '', name); option.value = name; provider.append(option);
      }
      if (chosen) provider.value = chosen;
      provider.addEventListener('change', () => { recoveryProvider = provider.value; });
      actions.append(provider);
    }
    if (frame.provider || provider) {
      settingsButton(actions, 'Sign in',
        'Sign this Emacs host in to the provider, for example after its login expired.',
        () => recoveryAction('login', null, {provider: provider ? provider.value : frame.provider}));
    }
    if (frame.runtime) {
      settingsButton(actions, 'Check updates', 'Check whether a newer Claude Code runtime is available.',
        () => recoveryAction('update'));
      if (frame.runtime.message) actions.append(el('p', 'settings-help', frame.runtime.message));
    }
  }
  function renderRecovery(frame) {
    if (!state.owner) return;
    recoveryFrame = frame;
    const {auth: login, ...rest} = frame;
    const authSignature = JSON.stringify([frame.provider, login]);
    if (authSignature !== recoveryAuthSignature) {
      recoveryAuthSignature = authSignature;
      renderRecoveryAuth(frame);
    }
    renderModeline();
    const signature = JSON.stringify(rest);
    if (signature === recoverySignature) return;
    recoverySignature = signature;
    // Owners see the host's details in place of the status frame's
    // category-only list; other readers only the category.
    renderRecoveryIssues(frame.issues);
    renderAttention(frame);
    renderModelChoices(frame);
    renderEffortChoices(frame);
    renderPresetChoices(frame);
    renderAccount(frame);
  }
  function initSettings() {
    document.getElementById('settings-rename')?.addEventListener('click', () => {
      const name = document.getElementById('settings-name').value.trim();
      if (name) recoveryAction('rename', name);
    });
    const model = document.getElementById('settings-model');
    const preset = document.getElementById('settings-preset');
    const apply = document.getElementById('settings-preset-apply');
    // Like the mode, the model applies on pick and the sheet keeps showing
    // the current one until the host's recovery frame confirms the change.
    model?.addEventListener('change', () => {
      const wanted = model.value;
      model.value = recoveryFrame?.model || '';
      if (wanted && wanted !== recoveryFrame?.model) recoveryAction('model', wanted);
    });
    const effort = document.getElementById('settings-effort');
    effort?.addEventListener('change', () => {
      const wanted = effort.value || null;
      effort.value = recoveryFrame?.effort || '';
      const efforts = recoveryFrame?.efforts || [];
      if (state.owner && efforts.length && (wanted === null || efforts.includes(wanted))
          && wanted !== (recoveryFrame?.effort || null)) recoveryAction('effort', wanted);
    });
    apply?.addEventListener('click', () => {
      const name = preset.value;
      if (name && window.confirm(`Apply preset ${name}? It replaces the model, tools and agents; permissions stay.`)) {
        recoveryAction('preset', name);
      }
    });
  }
  function renderRecoveryAuth(frame) {
    const auth = document.getElementById('recovery-auth');
    if (!auth) return;
    const button = (parent, text, action) => settingsButton(parent, text, text, action);
    auth.replaceChildren();
    if (frame.auth) {
      auth.append(el('p', '', frame.auth.message || ''));
      if (frame.auth.url && /^https:\/\/(auth\.openai\.com|claude\.ai|claude\.com|platform\.claude\.com)\//.test(frame.auth.url)) {
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
    const add = (text, className, title) => {
      if (!text) return;
      // An owner's model and mode open the settings that change them.
      const link = Boolean(state.owner && recoveryFrame && title);
      const node = el(link ? 'button' : 'span', link ? 'ml ml-link' : className || 'ml', text);
      if (link) { node.type = 'button'; node.addEventListener('click', openSettings); }
      if (title) node.title = title;
      modeline.append(node);
    };
    add(state.model, 'ml', 'Model this session uses');
    add(state.mode, 'ml', MODES.find(([mode]) => mode === state.mode)?.[2] || 'Permission mode');
    // Plan is a mode a guest can enter from a chip, so it has to be
    // visible afterwards -- otherwise the session silently behaves
    // differently than the transcript suggests.
    if (state.plan) add('plan', 'ml plan');
    goal.renderChip(modeline);
    const tail = el('span', 'ml tail');
    const bits = [];
    if (state.pending) {
      bits.push(`${state.pending} queued${state.paused ? ' · paused' : ''}`);
    }
    tail.textContent = bits.join(' · ');
    if (state.owner && recoveryFrame) {
      const attention = needsAttention(recoveryFrame);
      const settings = el('button', `ml settings-button${attention ? ' attention' : ''}`,
                          attention ? 'Needs attention' : 'Settings');
      settings.type = 'button';
      settings.title = attention
        ? 'Something needs your decision before the session can go on'
        : 'Model, permissions, preset and sign-in for this session';
      settings.addEventListener('click', openSettings);
      modeline.append(settings);
    }
    modeline.append(tail);
    renderModeChoices();
  }

  function updateRecordElement(record, previous) {
    const current = state.elements.get(record.id);
    const turn = window.mevedelTranscriptRenderer.renderRecord(
      record, scopeChip, artifacts.open, previous || current, openExecutionResult,
      path => agents.open({path}), destinationURL);
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
    const clip = title => (title.length > 32 ? `${title.slice(0, 29)}…` : title);
    // The host's item list names a whiteboard or document as it is now,
    // including one with no turns yet.
    const listed = editing.title(id);
    if (listed) return clip(listed);
    for (const record of state.records.values()) {
      const shared = record.shared;
      if (record.item === id && shared && typeof shared.title === 'string' && shared.title) {
        return clip(shared.title);
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
  // CARDS returns the latest artifact card per store artifact.
  function scopePresent(scope, cards) {
    if (!itemScope(scope)) return true;
    const id = scope.slice('item:'.length);
    if (!id.startsWith('artifact:')) return editing.present(id);
    const artifact = id.slice('artifact:'.length);
    if (storeArtifacts) return storeArtifacts.get(artifact)?.missing === false;
    return cards().get(artifact)?.missing !== true;
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
            if (record.artifact) cards.set(record.store || record.artifact, record);
          }
        }
        return cards;
      };
      // The docked item is the chat's subject before its first turn, too,
      // and its tab stays chosen after the item closes.
      const dockedItem = editing.dockedItem();
      for (const scope of [dockedItem && `item:${dockedItem}`, itemScope(state.filter) && state.filter]) {
        if (scope && !ids.includes(scope)) ids.push(scope);
      }
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
      // All earns its tab only when it shows more than Main chat.
      const all = counts.all > counts.main;
      // The strip is part of the surface once connected, even with no
      // directive yet: an always-present control needs no discovering.
      filterNav.hidden = !state.connected;
      // Main chat is the room's home: a lost tab falls back to it.
      if (state.filter === 'all' ? !all
          : state.filter !== 'main' && !listed.includes(state.filter)) {
        state.filter = 'main';
      }
      filterNav.replaceChildren();
      const add = (value, label, dir) => {
        const unseen = state.unseen.has(value);
        const button = el('button',
                          `${dir ? 'dir' : ''}${value === 'all' ? 'all' : ''}${unseen ? ' unseen' : ''}`,
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
      add('main', 'Main chat');
      listed.forEach(id => add(id, directiveLabel(id), true));
      if (all) add('all', 'All');
    }
    state.records.forEach(record => {
      const turn = state.elements.get(record.id);
      if (turn) turn.hidden = !recordVisible(record);
    });
    window.mevedelTranscriptRenderer.groupActivity(transcript);
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

  const requestView = window.mevedelRequestView.create({requests, el, send, state});
  const renderRequest = requestView.render;
  const removeRequest = requestView.remove;
  const clearRequests = requestView.clear;

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

  // ACTION, when given, is {label, href}: somewhere this ended room leads.
  function showTerminal(connectionText, noticeText, action = null) {
    if (transport) transport.end();
    state.connected = false;
    notifications.forget();
    try { sessionStorage.removeItem('mevedel-tab-share'); }
    catch (_error) { /* storage unavailable */ }
    nav.leave();
    notifications.render();
    tray.clear();
    clearRequests();
    showOwnQueue([]);
    agents.close();
    artifacts.close();
    agents.show([]);
    background.reset();
    tasks.show();
    goal.show();
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
    const actionLink = document.getElementById('terminal-action');
    actionLink.hidden = !action;
    actionLink.textContent = action ? action.label : '';
    if (action) actionLink.href = action.href;
    else actionLink.removeAttribute('href');
    document.getElementById('terminal-state').hidden = false;
    window.scrollTo({top: 0, behavior: 'instant'});
  }

  // VALUE is the host's word on the project's lobby: an owner's link to it,
  // true while it runs, or false. A link is kept with this browser's rooms.
  function showLobbyState(value) {
    if (typeof value === 'string' && value) {
      state.lobby = value;
      sessions.rememberLobby(value, `Lobby · ${state.project || 'project'}`);
    } else if (value === true || value === false) {
      state.lobby = value;
    }
    nav.render();
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
      if (typeof frame.project === 'string' && frame.project) state.project = frame.project;
      showLobbyState(frame.lobby);
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
      goal.show();
      setConnection('Loading…', 'connected');
      editing.welcome();
      restoreDestination();
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
    } else if (frame.t === 'background-work') {
      background.show(frame);
    } else if (frame.t === 'background-source') {
      showBackgroundSource(frame);
    } else if (frame.t === 'tasks') {
      tasks.show(frame);
    } else if (frame.t === 'goal') {
      goal.show(frame);
    } else if (frame.t === 'agent') {
      agents.handle(frame);
    } else if (frame.t === 'execution-result') {
      showExecutionResult(frame);
    } else if (frame.t === 'history-index' || frame.t === 'history') {
      history.handle(frame);
    } else if (frame.t === 'artifact') {
      artifacts.handle(frame);
    } else if (frame.t === 'discussion') {
      artifacts.handleDiscussion(frame);
    } else if (frame.t === 'artifact-delete') {
      artifacts.handleDelete(frame);
    } else if (frame.t === 'discussions') {
      artifacts.storedDiscussions(frame);
    } else if (frame.t === 'store-artifacts') {
      activeStore().show(frame);
      if (!lobby.active()) {
        storeArtifacts = new Map(activeStore().rows().map(row => [row.id, row]));
        artifacts.attachedRows(frame.artifacts);
        editing.attachedItems((Array.isArray(frame.artifacts) ? frame.artifacts : [])
          .filter(row => row && row.attached === true && row.item === true).map(row => row.id));
        refreshFilter();
      }
    } else if (frame.t === 'store-action') {
      activeStore().handle(frame);
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
      // Once an owner has the detailed recovery frame, it owns the list.
      if (!state.owner || !recoverySignature) renderRecoveryIssues(frame.issues);
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
        nav.render();
      }
      renderModeline();
      editing.conversation();
      artifacts.activity();
    } else if (frame.t === 'lobby') {
      state.models = Array.isArray(frame.models) ? frame.models : [];
      sessions.setWorkspace(frame.workspace);
      if (typeof frame.project === 'string' && frame.project) state.project = frame.project;
      lobby.show(frame);
      if (sessionLabel && typeof frame.project === 'string') {
        sessionLabel.textContent = `Lobby · ${frame.project}`;
      }
      setConnection('Connected', 'connected');
      nav.render();
      restoreDestination();
    } else if (frame.t === 'lobby-state') {
      showLobbyState(frame.lobby);
    } else if (frame.t === 'presence') {
      presence.show(frame);
      lobby.counts(frame.sessions);
      lobbyStore.counts(frame.artifacts);
    } else if (frame.t === 'open-session') {
      lobby.opened(frame);
    } else if (frame.t === 'delete-session') {
      lobby.deleted(frame);
    } else if (frame.t === 'rename-session') {
      lobby.renamed(frame);
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
    } else if (frame.t === 'session-models') {
      sessions.showModels(frame.models, frame.efforts);
    } else if (frame.t === 'new-session') {
      sessions.showResult({
        reqId: frame.reqId, ok: frame.ok === true, message: frame.message,
        link: frame.link, name: frame.name,
      });
      lobby.created(frame);
    } else if (frame.t === 'room') {
      sessions.offerRoom({name: frame.name, link: frame.link});
    } else if (frame.t === 'bye' && frame.reason === 'artifact-deleted') {
      showTerminal('Artifact deleted', 'This artifact was deleted together with '
                   + 'its conversation. Its project lobby lists the others.');
    } else if (frame.t === 'bye' && frame.reason === 'session-deleted') {
      showTerminal('Session deleted', 'This session was deleted from its '
                   + 'project lobby. The lobby lists the others.');
    } else if (frame.t === 'bye' && frame.reason === 'handoff'
               && typeof frame.link === 'string') {
      // The work moved on, such as a plan accepted into a Worktree fork;
      // the host hands every guest the new room at the tier it holds.
      // An unnamed successor is known only by its ID, which says nothing.
      const name = typeof frame.name === 'string' && frame.name ? frame.name : null;
      // This page offers the room itself; a dock notice would repeat it.
      sessions.offerRoom({name: name || 'New session', link: frame.link},
                         {notice: false});
      showTerminal(name ? `Continued in ${name}` : 'Continued in a new session',
                   'The host moved this work to a new session. Open it to '
                   + 'follow along.',
                   {label: name ? `Open ${name}` : 'Open the new session',
                    href: frame.link});
    } else if (frame.t === 'bye' && frame.reason === 'handoff-failed') {
      showTerminal('Continued in Emacs', 'The host moved this work to a new '
                   + 'session but could not share it. Ask the host for an '
                   + 'invitation to follow along.');
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
      const name = names.current();
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
  }

  window.mevedelHeader.create({shared: [document.getElementById('you-menu'),
                                         document.getElementById('appearance-menu')]});
  window.mevedelAppearance.bind(value => {
    artifacts.setTheme(value.theme);
    editing.setAppearance(value);
  });
  const desktopSidebar = window.matchMedia('(min-width:1100px)');
  sessionBox.open = desktopSidebar.matches;
  desktopSidebar.addEventListener('change', event => { sessionBox.open = event.matches; });
  // On a phone In this room opens as a sheet: the header's button on a short
  // screen opens it, and a tap outside closes it.
  const phone = window.matchMedia('(max-width:640px)');
  roomButton?.addEventListener('click', () => { sessionBox.open = !sessionBox.open; });
  sessionBox.addEventListener('toggle', () => {
    roomButton?.setAttribute('aria-expanded', String(sessionBox.open));
  });
  document.addEventListener('pointerdown', event => {
    if (phone.matches && sessionBox.open && !sessionBox.contains(event.target)
        && !roomButton?.contains(event.target)) sessionBox.open = false;
  });
  skillSearch.addEventListener('input', renderSkillChips);
  initSettings();
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
  const linkFragment = notificationsApi.resolveShare(parseFragment,
                                                     window.mevedelNav.historyShare());
  const rawFragment = linkFragment.split('?')[0];
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
    // The key remains only in this page's and tab's memory; the history
    // entry names the room alone, and the URL loses the key before the
    // socket opens.
    nav.enter(credentials.roomId, rawFragment);
    if (linkFragment.includes('?')) nav.note({panel: null, tab: null, view: null, ...routeState(linkFragment)});
    nav.render();
    Promise.all([importKey(credentials.keyBytes), names.ready]).then(([key]) => {
      transport = transportApi.create({
        roomId: credentials.roomId,
        key,
        giveUpMs: GIVE_UP_MS,
        joinGraceMs: JOIN_GRACE_MS,
        hello: () => {
          const hello = {t: 'hello', proto: PROTO, name: names.current(),
                         guestId: guestId(), ...presence.hello()};
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
