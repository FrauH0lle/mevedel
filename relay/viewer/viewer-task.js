/* viewer-task.js -- session task list */
'use strict';

(() => {
  const GLYPH = {'pending': '○', 'in-progress': '◐', 'completed': '✓'};

  function create({el, summarize}) {
    const list = document.getElementById('tasks-list');

    function show(frame) {
      const valid = frame && frame.t === 'tasks'
        && Array.isArray(frame.tasks)
        && [frame.total, frame.completed, frame.omitted, frame.omittedActive]
          .every(value => Number.isInteger(value) && value >= 0)
        && frame.completed <= frame.total
        && frame.omitted <= frame.total
        && frame.omittedActive <= frame.omitted
        && frame.tasks.length + frame.omitted === frame.total
        && frame.tasks.every(task => task
          && Number.isInteger(task.id)
          && typeof task.subject === 'string'
          && Object.hasOwn(GLYPH, task.status)
          && (task.owner === undefined || typeof task.owner === 'string')
          && (task.blockedBy === undefined
            || (Array.isArray(task.blockedBy)
              && task.blockedBy.every(Number.isInteger))));
      if (!valid) {
        if (list) list.replaceChildren();
        if (summarize) summarize('tasks', '', false);
        return;
      }
      const tasks = frame.tasks;
      const total = frame.total;
      const completed = frame.completed;
      const omitted = frame.omitted;
      const omittedActive = frame.omittedActive;
      if (list) list.replaceChildren();
      if (summarize) {
        let text = '';
        if (total) {
          text = `${completed}/${total} tasks`;
          if (omitted) text += ` · ${omitted} omitted`;
          if (omittedActive) text += ` · ⚠ ${omittedActive} active omitted`;
        }
        summarize('tasks', text, omittedActive > 0);
      }
      if (total === 0) return;
      if (!list) return;
      tasks.forEach(task => {
        const status = task.status;
        const item = el('li', `task ${status}`);
        const glyph = el('span', 'task-st', GLYPH[status]);
        glyph.setAttribute('aria-label', status);
        item.append(glyph, el('span', 'task-subject', task.subject));
        const bits = [];
        if (typeof task.owner === 'string' && task.owner) bits.push(task.owner);
        if (Array.isArray(task.blockedBy) && task.blockedBy.length) {
          bits.push(`blocked by #${task.blockedBy.join(' #')}`);
        }
        if (bits.length) item.append(el('span', 'task-meta', bits.join(' · ')));
        list.append(item);
      });
    }

    return Object.freeze({show});
  }

  window.mevedelTaskView = Object.freeze({create});
})();
