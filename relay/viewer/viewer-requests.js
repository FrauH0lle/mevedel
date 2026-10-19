/* Pending host interactions. Answers remain atomic; presentation is local. */
(() => {
  'use strict';

  function create({requests, el, send}) {
    function remove(reqId) {
      const card = [...requests.children].find(c => c.dataset.reqId === String(reqId));
      if (card) {
        if (card.dispose) card.dispose();
        card.remove();
      }
    }

    function questionnaire(card, frame) {
      const questions = frame.questions;
      const answers = questions.map(q => typeof q.answer === 'string' ? q.answer : '');
      const form = el('div', 'questionnaire');
      const rows = [];
      let open = 0;
      const controls = el('div', 'request-controls question-navigation');
      const previous = el('button', 'btn quiet', 'Previous question');
      const next = el('button', 'btn quiet', 'Next question');
      const submitRow = el('div', 'request-controls');
      const submit = el('button', 'btn', 'Submit answers');
      [previous, next, submit].forEach(button => { button.type = 'button'; });

      function show(index, focus = true) {
        open = index;
        rows.forEach((row, i) => {
          row.heading.hidden = i === open;
          row.panel.inert = i !== open;
          row.panel.setAttribute('aria-hidden', i === open ? 'false' : 'true');
          row.summaryAnswer.textContent = answers[i] ? ` → ${answers[i]}` : '';
          row.summaryAnswer.hidden = !answers[i];
          row.heading.title = `${i + 1}. ${questions[i].question || ''}`
            + row.summaryAnswer.textContent;
          row.buttons.forEach((button, j) => button.setAttribute('aria-pressed',
            !row.custom.value && questions[i].options[j].label === answers[i] ? 'true' : 'false'));
        });
        previous.disabled = open === 0;
        next.disabled = open === null || open === rows.length - 1;
        if (focus) {
          if (open === null) submit.focus();
          else (rows[open].buttons[0] || rows[open].custom).focus();
        }
      }

      function advance(index) {
        const unanswered = questions.map((_, step) => (index + step + 1) % questions.length)
          .find(i => !answers[i]);
        show(unanswered === undefined ? null : unanswered);
      }

      questions.forEach((q, index) => {
        const block = el('div', 'question');
        const heading = el('button', 'question-heading');
        const summaryTitle = el('span', 'question-summary-title', `${index + 1}. ${q.question || ''}`);
        const summaryAnswer = el('span', 'question-summary-answer');
        heading.append(summaryTitle, summaryAnswer);
        heading.type = 'button';
        heading.addEventListener('click', () => show(index));
        const panel = el('div', 'question-panel');
        panel.append(el('p', 'question-text', `${index + 1}. ${q.question || ''}`));
        const options = Array.isArray(q.options) ? q.options : [];
        const sampleBox = el('div', 'question-samples');
        const samples = new Map();
        options.forEach(option => {
          if (typeof option.sample !== 'string' || !option.sample) return;
          const sample = el('div', 'question-sample-option');
          sample.append(el('div', 'question-sample-label', option.label),
            el('pre', 'request-body question-sample', option.sample));
          sample.inert = true;
          sample.setAttribute('aria-hidden', 'true');
          samples.set(option, sample);
          sampleBox.append(sample);
        });
        function inspect(option) {
          if (!samples.has(option)) return;
          samples.forEach((sample, entry) => {
            sample.inert = entry !== option;
            sample.setAttribute('aria-hidden', entry === option ? 'false' : 'true');
          });
        }
        inspect(options.find(option => option.label === answers[index] && samples.has(option))
          || [...samples.keys()][0]);
        const buttons = options.map(option => {
          const row = el('div', 'question-option');
          const button = el('button', 'btn quiet option', option.label || '');
          button.type = 'button';
          button.addEventListener('focus', () => inspect(option));
          button.addEventListener('pointerenter', () => inspect(option));
          button.addEventListener('click', () => {
            answers[index] = option.label || '';
            custom.value = '';
            advance(index);
          });
          row.append(button);
          if (option.description) row.append(el('span', 'question-description', option.description));
          // Touch users need to inspect a sample without committing an answer.
          if (samples.has(option)) {
            const preview = el('button', 'btn quiet question-preview', 'Sample');
            preview.type = 'button';
            preview.setAttribute('aria-label', `Show sample for ${option.label}`);
            preview.addEventListener('click', () => inspect(option));
            row.append(preview);
          }
          panel.append(row);
          return button;
        });
        const custom = el('input', 'request-feedback');
        custom.type = 'text';
        custom.autocomplete = 'off';
        custom.placeholder = 'Custom answer…';
        custom.setAttribute('aria-label', `Custom answer ${index + 1}`);
        if (answers[index] && !options.some(option => option.label === answers[index])) {
          custom.value = answers[index];
        }
        custom.addEventListener('input', () => {
          answers[index] = custom.value;
          buttons.forEach(button => button.setAttribute('aria-pressed', 'false'));
        });
        custom.addEventListener('keydown', event => {
          if (event.key === 'Enter' && !event.isComposing) {
            event.preventDefault();
            advance(index);
          }
        });
        panel.append(custom);
        if (samples.size) panel.append(sampleBox);
        block.append(heading, panel);
        form.append(block);
        rows.push({heading, summaryAnswer, panel, buttons, custom});
      });
      previous.addEventListener('click', () => show(open === null ? rows.length - 1 : open - 1));
      next.addEventListener('click', () => show(open + 1));
      controls.append(previous, next);
      submit.addEventListener('click', () => send({t: 'ui-response', reqId: frame.reqId, answers}));
      submitRow.append(submit);
      if (frame.allowCancel === true) {
        const dismiss = el('button', 'btn quiet', 'Dismiss');
        dismiss.type = 'button';
        dismiss.title = 'Decline the questionnaire; the turn keeps running';
        dismiss.addEventListener('click', () => send({t: 'ui-response', reqId: frame.reqId, cancel: true}));
        submitRow.append(dismiss);
      }
      card.append(form, controls, submitRow);
      show(0, false);
      // Inactive panels remain measurable but inert. Reserve the tallest open
      // question plus the other summaries once per width, never on focus/input.
      let width;
      const observer = new ResizeObserver(() => {
        const current = form.getBoundingClientRect().width;
        if (!current || current === width) return;
        width = current;
        let closed = 0;
        let extra = 0;
        rows.forEach(row => {
          const hidden = row.heading.hidden;
          row.heading.hidden = false;
          const height = row.heading.getBoundingClientRect().height;
          closed += height;
          extra = Math.max(extra, row.panel.getBoundingClientRect().height - height);
          row.heading.hidden = hidden;
        });
        form.style.height = `${Math.ceil(closed + extra)}px`;
      });
      observer.observe(form);
      card.dispose = () => observer.disconnect();
    }

    function render(frame) {
      const key = JSON.stringify([frame.body, frame.bodyKind, frame.notice, frame.options,
        frame.questions, frame.allowFeedback, frame.allowCancel]);
      const existing = [...requests.children].find(c => c.dataset.reqId === String(frame.reqId));
      if (existing && existing.frameKey === key) return;
      const scrolled = existing && existing.bodyEl ? existing.bodyEl.scrollTop : 0;
      const previousFeedback = existing && existing.feedbackEl;
      const feedbackFocused = previousFeedback && document.activeElement === previousFeedback;
      const card = el('section', 'request-card');
      card.frameKey = key;
      card.dataset.reqId = String(frame.reqId);
      card.append(el('span', 'rhead', 'Needs your decision'));
      if (typeof frame.notice === 'string' && frame.notice) {
        card.append(el('div', 'request-body request-notice', frame.notice));
      }
      if (frame.bodyKind === 'diff') {
        card.bodyEl = window.mevedelTranscriptRenderer.renderDiff(frame.body || '');
        card.append(card.bodyEl);
      } else if (frame.body) {
        card.bodyEl = el('div', 'request-body', typeof frame.body === 'string' ? frame.body : '');
        card.append(card.bodyEl);
      }
      if (Array.isArray(frame.questions) && frame.questions.length) {
        questionnaire(card, frame);
      } else {
        const controls = el('div', 'request-controls');
        (Array.isArray(frame.options) ? frame.options : []).forEach(option => {
          const button = el('button', 'btn', option.label);
          button.type = 'button';
          button.title = `Answer with "${option.label}"`;
          button.addEventListener('click', () => send({t: 'ui-response', reqId: frame.reqId, option: option.id}));
          controls.append(button);
        });
        card.append(controls);
        if (frame.allowFeedback === true) {
          const row = el('div', 'request-controls');
          const feedback = el('input', 'request-feedback');
          feedback.type = 'text';
          feedback.autocomplete = 'off';
          feedback.placeholder = 'Feedback…';
          feedback.setAttribute('aria-label', 'Feedback');
          if (previousFeedback) feedback.value = previousFeedback.value;
          card.feedbackEl = feedback;
          const button = el('button', 'btn quiet', 'Send feedback');
          button.type = 'button';
          button.title = 'Answer with a comment instead of choosing an option';
          button.addEventListener('click', () => {
            if (feedback.value.trim()) send({t: 'ui-response', reqId: frame.reqId, feedback: feedback.value});
          });
          row.append(feedback, button);
          const disclosure = el('details', 'decision-feedback');
          if (existing && existing.feedbackDisclosure) disclosure.open = existing.feedbackDisclosure.open;
          card.feedbackDisclosure = disclosure;
          disclosure.append(el('summary', '', 'Suggest a change'), row);
          card.append(disclosure);
        }
      }
      if (existing) {
        if (existing.dispose) existing.dispose();
        existing.replaceWith(card);
      } else requests.append(card);
      if (scrolled && card.bodyEl) card.bodyEl.scrollTop = scrolled;
      if (feedbackFocused && card.feedbackEl) {
        card.feedbackEl.focus({preventScroll: true});
        card.feedbackEl.setSelectionRange(previousFeedback.selectionStart,
          previousFeedback.selectionEnd, previousFeedback.selectionDirection);
      }
    }

    function clear() {
      [...requests.children].forEach(card => remove(card.dataset.reqId));
    }
    return {render, remove, clear};
  }
  window.mevedelRequestView = {create};
})();
