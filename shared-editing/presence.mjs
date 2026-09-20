/* Disposable board overlays. Samples are world coordinates, sizes are screen pixels. */

// Short transitions fill the gaps between updates without predicting motion.
// Never extrapolate: stopping and packet loss must not move the pointer past its owner.
export function positionAt(samples, time) {
  for (let i = 1; i < samples.length; i++) {
    const a = samples[i - 1], b = samples[i];
    if (b.time >= time) {
      const mix = Math.max(0, Math.min(1, (time - a.time) / Math.max(1, b.time - a.time)));
      return a.point.map((value, axis) => value + (b.point[axis] - value) * mix);
    }
  }
  return samples.at(-1).point;
}

// Quadratic midpoints keep sparse circular gestures rounded. Each piece can fade
// with its own age instead of the whole trail flashing back on with each packet.
export function trailSegments(samples, time) {
  let start = samples[0]?.point;
  return samples.slice(1).map((sample, i, rest) => {
    const next = rest[i + 1],
      end = next ? sample.point.map((v, axis) => (v + next.point[axis]) / 2) : sample.point;
    const segment = {
      path: `M${start.join(' ')} Q${sample.point.join(' ')} ${end.join(' ')}`,
      opacity: Math.max(0, 1 - (time - sample.time) / 550) ** 2,
    };
    start = end;
    return segment;
  });
}

export class BoardPresence {
  constructor(canvas, layer) {
    this.canvas = canvas;
    this.layer = layer;
    this.people = new Map();
    this.motion = matchMedia('(prefers-reduced-motion: reduce)');
    this.frame = 0;
  }
  clear(peer) {
    for (const [key, person] of this.people) {
      if (peer !== undefined && peer !== key) continue;
      clearTimeout(person.timer);
      person.group.remove();
      this.people.delete(key);
    }
    if (!this.people.size) {
      cancelAnimationFrame(this.frame);
      this.frame = 0;
    }
  }
  receive({ peer, mode, point, name, trail }) {
    if (mode === 'clear') { this.clear(peer); return; }
    if (!['laser', 'cursor'].includes(mode) || !Array.isArray(point) ||
        point.length !== 2 || !point.every(Number.isFinite)) return;
    const now = performance.now();
    let person = this.people.get(peer);
    if (!person || person.mode !== mode || now - person.last > 250) {
      this.clear(peer);
      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      group.dataset.peer = peer;
      group.dataset.mode = mode;
      group.innerHTML = `<g class="pointer-trail"></g><g class="pointer-tip"></g><g class="pointer-label"><rect y="-14" height="21" rx="5"/><text x="6" y="1"></text></g>`;
      group.style.visibility = 'hidden';
      const tip = group.querySelector('.pointer-tip');
      tip.innerHTML = mode === 'laser'
        ? '<circle r="8" fill="#f43f5e" opacity=".13"/><circle r="4" fill="#ef3555"/><circle r="1.8" fill="#fff6ed"/>'
        : '<path d="M0 0 4 17 8 11 15 10Z" fill="#a22458" stroke="white" stroke-width="1.5"/>';
      this.layer.append(group);
      person = { group, mode, samples: [{point, time:now}], trail: [], local: peer === 'self' };
      this.people.set(peer, person);
    }
    const from = positionAt(person.samples, now);
    const laser = mode === 'laser' && Array.isArray(trail) && trail.length;
    person.samples = laser
      ? trail.map(([x,y,age]) => ({point:[x,y], time:now + (person.local ? 0 : 55) - age}))
      : person.local || this.motion.matches
      ? [{ point, time: now }]
      : [{ point: from, time: now }, { point, time: now + 55 }];
    if (laser) person.trail = person.samples;
    person.sampledTrail = Boolean(laser);
    person.last = now;
    const label = person.group.querySelector('text');
    label.textContent = String(name || 'Participant').slice(0, 48);
    person.labelWidth = label.getComputedTextLength() + 12;
    person.group.querySelector('rect').setAttribute('width', person.labelWidth);
    clearTimeout(person.timer);
    person.timer = setTimeout(() => this.clear(peer), 5000);
    this.animate();
  }
  animate() {
    if (!this.frame) this.frame = requestAnimationFrame(time => this.render(time));
  }
  render(now = performance.now()) {
    this.frame = 0;
    const matrix = this.canvas.getScreenCTM();
    const scale = 1 / (matrix?.a || 1);
    const viewport = this.canvas.getBoundingClientRect();
    let moving = false;
    for (const person of this.people.values()) {
      const { group, samples, mode } = person;
      const point = this.motion.matches ? samples.at(-1).point : positionAt(samples, now);
      const laser = mode === 'laser';
      person.trail = person.trail.filter(s => now - s.time < 550).slice(-63);
      const previous = person.trail.at(-1);
      if (!person.sampledTrail && (!previous || Math.hypot(...point.map((v, i) => v - previous.point[i])) > .25 * scale))
        person.trail.push({ point, time: now });
      const visible = person.trail.filter(s => s.time <= now);
      if (visible.length && point.some((v, i) => v !== visible.at(-1).point[i]))
        visible.push({point, time:now});
      const paths = this.motion.matches || !laser ? '' : trailSegments(visible, now)
        .map(s => `<path d="${s.path}" opacity="${s.opacity.toFixed(3)}"/>`).join('');
      group.querySelector('.pointer-trail').innerHTML = paths
        ? `<g stroke="#ef3555" stroke-width="${7 * scale}" opacity=".16">${paths}</g><g stroke="#e93250" stroke-width="${3 * scale}">${paths}</g><g stroke="#fff0e9" stroke-width="${scale}" opacity=".85">${paths}</g>` : '';
      const tip = group.querySelector('.pointer-tip');
      tip.setAttribute('transform', `translate(${point.join(' ')}) scale(${scale})`);
      const screen = new DOMPoint(...point).matrixTransform(matrix || new DOMMatrix());
      const labelX = screen.x + 13 + person.labelWidth > viewport.right ? -person.labelWidth - 13 : 13;
      const labelY = screen.y + 28 > viewport.bottom ? -14 : 21;
      group.querySelector('.pointer-label').setAttribute('transform',
        `translate(${point[0] + labelX * scale} ${point[1] + labelY * scale}) scale(${scale})`);
      group.style.visibility = '';
      moving ||= now - person.last < (laser && !this.motion.matches ? 650 : 70);
    }
    if (moving) this.animate();
  }
}
