import assert from 'node:assert/strict';
import test from 'node:test';
import { recognize } from '../autoshape.mjs';

// Deterministic jitter, so hand-drawn strokes are reproducible.
let seed = 7;
const jitter = (amount) => ((seed = (seed * 16807) % 2147483647) / 2147483647 - 0.5) * amount;
const trace = (corners, steps = 20) => corners.slice(1).flatMap((to, i) => {
  const from = corners[i];
  return Array.from({ length: steps }, (_, s) => [
    from[0] + ((to[0] - from[0]) * s) / steps + jitter(3),
    from[1] + ((to[1] - from[1]) * s) / steps + jitter(3),
  ]);
});

test('hand-drawn strokes become clean shapes, scribbles stay strokes', () => {
  const rectangle = trace([[0, 0], [200, 0], [200, 120], [0, 120], [0, 2]]);
  const diamond = trace([[100, 0], [200, 60], [100, 120], [0, 60], [98, 2]]);
  const ellipse = Array.from({ length: 90 }, (_, i) => {
    const a = (i / 88) * Math.PI * 2;
    return [100 + 100 * Math.cos(a) + jitter(3), 60 + 60 * Math.sin(a) + jitter(3)];
  });
  const line = trace([[0, 0], [240, 30]]);
  const arrow = [...trace([[0, 0], [240, 0]]), ...trace([[240, 0], [215, -18], [240, 0], [215, 18]], 8)];
  const scribble = trace([[0, 0], [80, 90], [10, 140], [160, 20], [40, 200], [200, 180]]);
  assert.equal(recognize(rectangle).type, 'rectangle');
  assert.equal(recognize(diamond).type, 'diamond');
  assert.equal(recognize(ellipse).type, 'ellipse');
  assert.equal(recognize(line).type, 'line');
  const head = recognize(arrow);
  assert.equal(head.type, 'arrow');
  assert.ok(head.to[0] > 200, 'the tip is the far end, not the last drawn barb');
  assert.equal(recognize(scribble).type, 'freedraw');
  // Recognition depends on apparent size: a tiny or zoomed-out stroke stays freedraw.
  assert.equal(recognize(rectangle.map(([x, y]) => [x / 20, y / 20])).type, 'freedraw');
  assert.equal(recognize(rectangle, 0.1).type, 'freedraw');
  recognize(rectangle).box.forEach((v, i) => assert.ok(Math.abs(v - [0, 0, 200, 120][i]) <= 4));
});
