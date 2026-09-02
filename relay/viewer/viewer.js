/* viewer.js -- MevView: dependency-free sealed collaboration guest */
'use strict';

(() => {
  const transcript = document.getElementById('transcript');
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
  const themeButton = document.getElementById('theme-button');
  const modeline = document.getElementById('modeline');
  const sessionBox = document.getElementById('session-box');
  const sessionSummary = document.getElementById('session-summary');
  const transportApi = window.mevedelViewerTransport;
  const notificationsApi = window.mevedelViewerNotifications;
  const {base64urlDecode, base64urlEncode, importKey, parseFragment} = transportApi;

  const PROTO = 2;
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
    armed: null,
    model: null,
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

  // Sub-agents and tasks are two reporters on one dock line: each owns a
  // fragment of the summary and its own section inside the disclosure,
  // and the box shows for as long as either has something to report.
  const summaryParts = {agents: '', tasks: ''};
  const summaryWarnings = {agents: false, tasks: false};
  function summarizeSession(key, text, warning) {
    summaryParts[key] = text || '';
    summaryWarnings[key] = warning === true;
    const bits = [summaryParts.agents, summaryParts.tasks].filter(Boolean);
    sessionBox.hidden = bits.length === 0;
    sessionSummary.textContent = ['Session', ...bits].join(' · ');
    sessionSummary.dataset.warning =
      (summaryWarnings.agents || summaryWarnings.tasks) ? 'true' : 'false';
  }

  const artifacts = window.mevedelArtifactView.create({
    send, el, flash: flashNotice,
  });
  const agents = window.mevedelAgentView.create({
    send, el, directiveLabel, openArtifact: artifacts.open,
    summarize: summarizeSession,
  });
  const tasks = window.mevedelTaskView.create({el, summarize: summarizeSession});
  const sessions = window.mevedelSessionView.create(
    {state, send, el, encode: base64urlEncode, decode: base64urlDecode});

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

  /* -- Status strip -------------------------------------------------- */
  // One home for session state, the way the Emacs mode line reports it,
  // instead of the same facts scattered across three corners.
  function renderModeline() {
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

  /* -- Colour theme -------------------------------------------------- */
  // Three states, matching the stylesheet: no stamp follows the system,
  // an explicit stamp wins over it in either direction.
  const THEMES = ['system', 'light', 'dark'];
  const THEME_GLYPH = {system: '◐', light: '☀', dark: '☾'};
  const THEME_LABEL = {
    system: 'Colour theme: follow system',
    light: 'Colour theme: light',
    dark: 'Colour theme: dark',
  };

  function storedTheme() {
    let value = null;
    try { value = localStorage.getItem('mevedel-theme'); }
    catch (_error) { /* storage unavailable */ }
    return THEMES.includes(value) ? value : 'system';
  }

  function applyTheme(theme) {
    const root = document.documentElement;
    if (theme === 'system') root.removeAttribute('data-theme');
    else root.setAttribute('data-theme', theme);
    if (themeButton) {
      themeButton.textContent = THEME_GLYPH[theme];
      themeButton.setAttribute('aria-label', THEME_LABEL[theme]);
      themeButton.setAttribute('title', `${THEME_LABEL[theme]} — click to change`);
      themeButton.className = `bell${theme === 'system' ? '' : ' on'}`;
    }
  }

  function markContinuations() {
    let previousRole = null;
    for (const turn of [...transcript.children]) {
      if (turn.hidden) continue;
      const role = turn.dataset.role || 'ai';
      const cont = role === 'ai' && previousRole === 'ai';
      turn.className = `turn ${role}${cont ? ' cont' : ''}`;
      previousRole = role;
    }
  }

  function updateRecordElement(record) {
    let turn = state.elements.get(record.id);
    if (!turn) {
      turn = window.mevedelTranscriptRenderer.renderRecord(
        record, directiveLabel, artifacts.open);
      state.elements.set(record.id, turn);
      transcript.append(turn);
    } else {
      const wasOpen = !!(turn.toolDetails && turn.toolDetails.open);
      const fresh = window.mevedelTranscriptRenderer.renderRecord(
        record, directiveLabel, artifacts.open);
      if (wasOpen && fresh.toolDetails) fresh.toolDetails.open = true;
      fresh.hidden = turn.hidden;
      turn.replaceWith(fresh);
      state.elements.set(record.id, fresh);
      turn = fresh;
    }
    return turn;
  }

  function replaceSnapshot(records) {
    const follow = atLiveEdge();
    state.records.clear();
    state.elements.clear();
    state.unseen.clear();
    transcript.replaceChildren();
    records.forEach(record => {
      if (record && typeof record.id === 'string') state.records.set(record.id, record);
    });
    state.records.forEach(record => updateRecordElement(record));
    refreshFilter();
    markContinuations();
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function updateRecord(record) {
    if (!record || typeof record.id !== 'string') return;
    const follow = atLiveEdge();
    state.records.set(record.id, record);
    // Activity outside the selected filter earns its tab an unseen dot.
    if (!recordVisible(record)) state.unseen.add(record.directive || 'main');
    updateRecordElement(record);
    refreshFilter();
    markContinuations();
    if (follow) scrollToLive();
    updateLiveAffordance();
  }

  function removeRecords(ids) {
    (Array.isArray(ids) ? ids : []).forEach(id => {
      state.records.delete(id);
      const turn = state.elements.get(id);
      if (turn) turn.remove();
      state.elements.delete(id);
    });
    refreshFilter();
    markContinuations();
  }

  /* -- Directive filter ---------------------------------------------- */
  // Records inside a directive turn carry its id; the menu is derived
  // client-side, so filtering is per-guest and costs no round-trips.

  function recordVisible(record) {
    if (state.filter === 'all') return true;
    if (state.filter === 'main') return !record.directive;
    return record.directive === state.filter;
  }

  function directiveLabel(id) {
    for (const record of state.records.values()) {
      if (record.directive === id && record.kind === 'user' && record.text) {
        const line = record.text.split('\n', 1)[0];
        return line.length > 32 ? `${line.slice(0, 29)}…` : line;
      }
    }
    return id.slice(0, 8);
  }

  function refreshFilter() {
    if (filterNav) {
      const ids = [];
      const counts = {all: 0, main: 0};
      state.records.forEach(record => {
        counts.all++;
        if (record.directive) {
          if (!ids.includes(record.directive)) ids.push(record.directive);
          counts[record.directive] = (counts[record.directive] || 0) + 1;
        } else {
          counts.main++;
        }
      });
      // The strip is part of the surface once connected, even with no
      // directive yet: an always-present control needs no discovering.
      filterNav.hidden = !state.connected;
      if (state.filter !== 'all' && state.filter !== 'main'
          && !ids.includes(state.filter)) {
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
              : `Show and reply in ◆ ${directiveLabel(value)}`);
        if (counts[value]) {
          button.append(el('span', 'cnt', String(counts[value])));
        }
        button.addEventListener('click', () => {
          state.filter = value;
          // Selecting a tab is looking at it; All shows everything.
          if (value === 'all') state.unseen.clear();
          else state.unseen.delete(value);
          refreshFilter();
        });
        filterNav.append(button);
      };
      add('all', 'All');
      if (ids.length) add('main', 'Main chat');
      ids.forEach(id => add(id, `◆ ${directiveLabel(id)}`, true));
    }
    state.records.forEach(record => {
      const turn = state.elements.get(record.id);
      if (turn) turn.hidden = !recordVisible(record);
    });
    artifacts.render(state.records);
    // The composer follows the filter, so say where a prompt will land.
    if (composerInput && !state.armed) {
      composerInput.placeholder = placeholderForFilter();
    }
    renderComposerScope();
  }

  function placeholderForFilter() {
    return (state.filter !== 'all' && state.filter !== 'main')
      ? `Discuss ◆ ${directiveLabel(state.filter)}…`
      : 'Queue a follow-up for the session…';
  }

  // One line under the composer saying what the next send will do: run
  // an armed invocation, or land in a directive thread.
  function renderComposerScope() {
    if (!composerScope) return;
    composerScope.replaceChildren();
    const scoped = state.filter !== 'all' && state.filter !== 'main';
    if (state.armed) {
      composerScope.hidden = false;
      composerScope.className = 'composer-scope armed';
      composerScope.append(
        `Runs ${sigilFor(state.armed.kind)}${state.armed.name}`);
      const clear = el('button', 'scope-clear', '✕');
      clear.type = 'button';
      clear.setAttribute('aria-label', 'Cancel this command');
      clear.addEventListener('click', () => setArmedInvocation(null));
      composerScope.append(clear);
      return;
    }
    composerScope.className = 'composer-scope';
    composerScope.hidden = !scoped;
    if (scoped) {
      composerScope.append(`Sends to ◆ ${directiveLabel(state.filter)} · discuss`);
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
    card.append(el('span', 'rhead', 'Pending interaction'));
    if (frame.bodyKind === 'diff') {
      const body = window.mevedelTranscriptRenderer.renderDiff(frame.body || '');
      card.bodyEl = body;
      card.append(body);
    } else if (frame.body) {
      const body = el('pre', 'request-body',
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
      card.append(feedbackRow);
    }
    requests.append(card);
    // A changed body still keeps the reader where they were.
    if (scrolled && card.bodyEl) card.bodyEl.scrollTop = scrolled;
  }

  function clearRequests() {
    if (requests) requests.replaceChildren();
  }

  /* -- Attachments --------------------------------------------------- */
  // Photos are downscaled client-side to fit the sealed prompt frame under
  // the relay's read limit; other files are refused when they overrun it,
  // because a log cannot be made smaller by resampling. The host enforces
  // the same allowlist and budget.

  const MAX_FILES = 3;
  // Decoded bytes, all attachments. Base64 costs a third and the prompt
  // text shares the frame, so this leaves the 2 MiB relay limit ~85 KiB
  // of headroom even with a maximum-length prompt beside it.
  const FILE_BUDGET = 1280 * 1024;
  // Mirrors the host's allowlist. Read decides text or media downstream.
  const MIME_BY_EXTENSION = {
    jpg: 'image/jpeg', jpeg: 'image/jpeg', png: 'image/png',
    webp: 'image/webp', pdf: 'application/pdf', txt: 'text/plain',
    log: 'text/plain', text: 'text/plain', md: 'text/markdown',
    csv: 'text/csv', json: 'application/json',
    patch: 'text/x-patch', diff: 'text/x-patch',
  };
  const ALLOWED_MIME = new Set(Object.values(MIME_BY_EXTENSION));
  const pendingFiles = []; // {mime, label, data (base64), bytes, url}
  let attachmentGeneration = 0;
  let attachmentWork = Promise.resolve();
  let submitting = false;

  function disposeAttachment(item) {
    if (item && item.url) {
      URL.revokeObjectURL(item.url);
      item.url = null;
    }
  }

  function clearAttachments() {
    attachmentGeneration++;
    pendingFiles.splice(0).forEach(disposeAttachment);
    renderAttachments();
  }

  function removeAttachments(items) {
    for (const item of items) {
      const index = pendingFiles.indexOf(item);
      if (index >= 0) disposeAttachment(pendingFiles.splice(index, 1)[0]);
    }
    renderAttachments();
  }

  // Browsers report "" or application/octet-stream for .log, .patch, and
  // friends, so the extension decides whenever the type is not one we take.
  function attachmentMime(file) {
    if (ALLOWED_MIME.has(file.type)) return file.type;
    const extension = (file.name || '').split('.').pop().toLowerCase();
    return MIME_BY_EXTENSION[extension] || null;
  }

  function renderAttachments() {
    if (!attachments) return;
    attachments.replaceChildren();
    pendingFiles.forEach((item, index) => {
      const chip = el('span', 'attachment');
      if (item.url) {
        const thumb = el('img', 'attachment-thumb');
        thumb.src = item.url;
        thumb.alt = `attachment ${index + 1}`;
        chip.append(thumb);
      } else {
        chip.append(el('span', 'attachment-name', item.label));
      }
      const removeButton = el('button', 'attachment-remove', '✕');
      removeButton.type = 'button';
      removeButton.setAttribute('aria-label', `Remove attachment ${index + 1}`);
      removeButton.addEventListener('click', () => {
        disposeAttachment(pendingFiles.splice(index, 1)[0]);
        renderAttachments();
      });
      chip.append(removeButton);
      attachments.append(chip);
    });
  }

  function pendingFileBytes() {
    return pendingFiles.reduce((sum, item) => sum + item.bytes, 0);
  }

  function base64OfBytes(buffer) {
    let binary = '';
    buffer.forEach(byte => { binary += String.fromCharCode(byte); });
    return btoa(binary);
  }

  async function downscaleImage(file, budget) {
    const bitmap = await createImageBitmap(file);
    try {
      const longest = Math.max(bitmap.width, bitmap.height);
      let scale = Math.min(1, 1568 / longest);
      for (let attempt = 0; attempt < 5; attempt++) {
        const canvas = document.createElement('canvas');
        canvas.width = Math.max(1, Math.round(bitmap.width * scale));
        canvas.height = Math.max(1, Math.round(bitmap.height * scale));
        canvas.getContext('2d').drawImage(bitmap, 0, 0,
                                          canvas.width, canvas.height);
        const quality = Math.max(0.4, 0.85 - attempt * 0.15);
        const blob = await new Promise(resolve =>
          canvas.toBlob(resolve, 'image/jpeg', quality));
        if (blob && blob.size <= budget) return blob;
        if (attempt >= 2) scale *= 0.7;
      }
      return null;
    } finally {
      bitmap.close();
    }
  }

  async function addFilesNow(files, generation) {
    for (const file of files) {
      if (generation !== attachmentGeneration) return;
      const mime = attachmentMime(file);
      if (!mime) {
        flashNotice(`${file.name || 'That file'} is not an accepted type.`);
        continue;
      }
      if (pendingFiles.length >= MAX_FILES) {
        flashNotice(`At most ${MAX_FILES} attachments per prompt.`);
        break;
      }
      const budget = FILE_BUDGET - pendingFileBytes();
      if (mime.startsWith('image/')) {
        const blob = await downscaleImage(file, budget).catch(() => null);
        if (generation !== attachmentGeneration) return;
        if (!blob) {
          flashNotice('Image too large for the frame budget.');
          continue;
        }
        const buffer = new Uint8Array(await blob.arrayBuffer());
        if (generation !== attachmentGeneration) return;
        pendingFiles.push({
          mime: 'image/jpeg',
          label: file.name || 'photo',
          data: base64OfBytes(buffer),
          bytes: buffer.length,
          url: URL.createObjectURL(blob),
        });
      } else {
        if (file.size > budget) {
          flashNotice(`${file.name || 'That file'} is over the `
                      + `${Math.floor(budget / 1024)} KB left in this prompt.`);
          continue;
        }
        const buffer = new Uint8Array(await file.arrayBuffer());
        if (generation !== attachmentGeneration) return;
        pendingFiles.push({
          mime,
          label: file.name || 'attachment',
          data: base64OfBytes(buffer),
          bytes: buffer.length,
          url: null,
        });
      }
    }
    renderAttachments();
  }

  function addFiles(files) {
    const generation = attachmentGeneration;
    const work = attachmentWork.then(
      () => addFilesNow(files, generation),
      () => addFilesNow(files, generation));
    attachmentWork = work.catch(() => {});
    return work;
  }

  /* -- Composer ------------------------------------------------------ */

  function guestName() {
    const name = (composerName && composerName.value.trim())
      || localStorage.getItem('mevedel-guest-name')
      || 'browser';
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

  // The welcome's host-curated roster is the whole discovery surface.
  // Tapping a chip arms the invocation and focuses the composer rather
  // than sending: most commands and skills take arguments, and an
  // immediate send gives no chance to supply them. The armed name
  // travels as its own frame field, so composer text is never parsed
  // for a sigil.
  function sigilFor(kind) {
    return kind === 'skill' ? '$' : '/';
  }

  function setArmedInvocation(entry) {
    state.armed = entry || null;
    if (composerScope) renderComposerScope();
    if (composerInput) {
      composerInput.placeholder = entry
        ? (entry.hint
           ? `Arguments for ${sigilFor(entry.kind)}${entry.name} — ${entry.hint}`
           : `${sigilFor(entry.kind)}${entry.name} — no arguments needed`)
        : placeholderForFilter();
      if (typeof composerInput.focus === 'function') composerInput.focus();
    }
    renderSkillChips();
  }

  function renderSkillChips() {
    if (!skillChips) return;
    skillChips.replaceChildren();
    skillChips.hidden = state.roster.length === 0;
    state.roster.forEach(entry => {
      const armed = state.armed && state.armed.name === entry.name;
      const chip = el('button', `skill-chip${armed ? ' armed' : ''}`,
                      `${sigilFor(entry.kind)}${entry.name}`);
      chip.type = 'button';
      chip.setAttribute('aria-pressed', armed ? 'true' : 'false');
      chip.setAttribute(
        'title',
        `${armed ? 'Cancel' : 'Prepare'} ${sigilFor(entry.kind)}${entry.name}`
        + (entry.hint ? ` — arguments: ${entry.hint}` : ' — takes no arguments'));
      chip.addEventListener('click', () => {
        setArmedInvocation(armed ? null : entry);
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
    state.armed = null;
    renderSkillChips();
  }

  // The guest's own pending prompts, echoed back per-peer by the host:
  // a persistent card with live position and a retract control, so a
  // queued prompt never reads as swallowed.
  function showOwnQueue(entries) {
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
                     typeof entry.text === 'string' ? entry.text : ''));
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
    notifications.render();
    clearAttachments();
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
    refreshFilter();
    renderModeline();
    setConnection(connectionText, 'ended');
    showNotice(noticeText);
  }

  function handleFrame(frame) {
    if (!frame || typeof frame.t !== 'string') return;
    if (frame.t === 'welcome') {
      state.readOnly = frame.readOnly !== false;
      state.staging = {records: [], live: []};
      state.connected = true;
      notifications.render();
      setComposerVisible(!state.readOnly);
      showSkillChips(state.readOnly ? [] : frame.commands);
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
    } else if (frame.t === 'artifact') {
      artifacts.handle(frame);
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
          'Turn finished', 'The mevedel session is idle again.');
      }
      state.busy = frame.busy === true;
      if (typeof frame.model === 'string') state.model = frame.model;
      if (typeof frame.mode === 'string') state.mode = frame.mode;
      state.plan = frame.plan === true;
      renderModeline();
    } else if (frame.t === 'new-session') {
      sessions.showResult({
        reqId: frame.reqId, ok: frame.ok === true, message: frame.message,
        link: frame.link, name: frame.name,
      });
    } else if (frame.t === 'room') {
      sessions.offerRoom({name: frame.name, link: frame.link});
    } else if (frame.t === 'bye') {
      showTerminal('Session ended', 'The shared session has ended.');
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
      const name = guestName();
      try {
        await attachmentWork;
        // An armed invocation may legitimately carry no arguments.
        if (!text.trim() && !pendingFiles.length && !armed) return;
        if (new TextEncoder().encode(text).length > MAX_PROMPT_BYTES) {
          flashNotice('Prompt too large.');
          return;
        }
        localStorage.setItem('mevedel-guest-name', name);
        const frame = {t: 'prompt', name};
        if (armed) {
          // The name travels as its own field; the host resolves the
          // sigil and validates against its allowlist. Text is arguments.
          frame.invoke = armed.name;
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
        const submittedFiles = pendingFiles.slice();
        if (submittedFiles.length) {
          frame.images = submittedFiles.map(
            item => ({mime: item.mime, data: item.data}));
        }
        if (!await send(frame)) {
          flashNotice('Connection lost; prompt kept.');
          return;
        }
        if (composerInput.value === text) composerInput.value = '';
        removeAttachments(submittedFiles);
        // One tap, one invocation: disarm so the next send is a prompt.
        if (state.armed === armed && armed) setArmedInvocation(null);
      } finally {
        submitting = false;
      }
    });
    // The Send button is type="submit", so the form's submit event already
    // covers it; a click handler here would double-send every prompt.
    stopButton.addEventListener('click', () => send({t: 'abort'}));
    if (attachButton && imageInput) {
      attachButton.addEventListener('click', () => imageInput.click());
      imageInput.addEventListener('change', () => {
        addFiles([...imageInput.files]);
        imageInput.value = '';
      });
    }
    if (composerInput) {
      composerInput.addEventListener('paste', event => {
        const files = [...(event.clipboardData?.items || [])]
          .filter(item => item.kind === 'file')
          .map(item => item.getAsFile())
          .filter(Boolean);
        if (files.length) addFiles(files);
      });
    }
    if (composerName) {
      composerName.value = localStorage.getItem('mevedel-guest-name') || '';
    }
  }

  applyTheme(storedTheme());
  if (themeButton) {
    themeButton.addEventListener('click', () => {
      const next = THEMES[(THEMES.indexOf(storedTheme()) + 1) % THEMES.length];
      try { localStorage.setItem('mevedel-theme', next); }
      catch (_error) { /* the choice then lasts for this page only */ }
      applyTheme(next);
    });
  }

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
            'Room closed', 'The room did not come back; the link is dead.');
        },
        onOpen: async () => {
          if (notifications.enabled()) await notifications.syncPush();
        },
      });
      transport.connect();
    });
  }

  window.mevedelViewer = Object.freeze({
    parseFragment, atLiveEdge, base64urlDecode, base64urlEncode, addFiles,
  });
})();
