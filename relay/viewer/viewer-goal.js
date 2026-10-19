/* viewer-goal.js -- read-only durable Goal status and accounting */
'use strict';
(() => {
  function create({el, onChange}) {
    const sheet = document.getElementById('goal-sheet');
    const content = document.getElementById('goal-content');
    let goal = null;

    function usage() {
      return `${goal.incomplete ? '≥' : ''}${goal.tokensUsed}/${goal.tokenBudget ?? 'unbounded'}`;
    }

    function renderDetails() {
      if (!content || !goal) return;
      content.replaceChildren(
        el('p', 'goal-objective', goal.objective),
        el('p', '', `Status: ${goal.status}`),
        el('p', '', `${goal.turns} settled turns · ${goal.seconds} seconds`),
        el('p', '', `Accounted tokens: ${usage()}`));
      if (goal.reason) content.append(el('p', '', goal.reason));
      content.append(el('p', 'panel-note', goal.incomplete
        ? 'Usage is incomplete: the known total is a lower bound.'
        : 'Usage includes accounted charges; an in-flight turn may add more.'));
      if (goal.truncated) content.append(el('p', 'panel-note',
        'Long Goal text is shortened here. Read the full objective and reason in Emacs.'));
      content.append(el('p', 'panel-note', 'Goal changes stay in Emacs.'));
    }

    function show(frame) {
      goal = frame?.goal || null;
      if (!goal && sheet?.open) sheet.close();
      renderDetails();
      onChange();
    }

    function renderChip(modeline) {
      if (!goal) return;
      const chip = el('button', 'ml ml-link goal-chip',
        `Goal ${goal.status} · ${goal.turns} turns · ${usage()}`);
      chip.type = 'button';
      chip.title = 'Inspect Goal objective, status and accounted usage';
      chip.addEventListener('click', () => { renderDetails(); sheet?.showModal(); });
      modeline.append(chip);
    }

    return Object.freeze({show, renderChip});
  }
  window.mevedelGoalView = Object.freeze({create});
})();
