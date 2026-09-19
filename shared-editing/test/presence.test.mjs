import test from 'node:test';
import assert from 'node:assert/strict';
import { positionAt, trailSegments } from '../presence.mjs';

test('playback interpolates samples but never predicts beyond a stop or reversal', () => {
  const samples = [
    { point: [0, 0], time: 100 },
    { point: [100, 200], time: 150 },
    { point: [20, 0], time: 240 },
  ];
  assert.deepEqual(positionAt(samples, 0), [0, 0]);
  assert.deepEqual(positionAt(samples, 125), [50, 100]);
  assert.deepEqual(positionAt(samples, 195), [60, 100]);
  assert.deepEqual(positionAt(samples, 10000), [20, 0]);
  assert.deepEqual(positionAt(samples.slice(0, 1), 400), [0, 0]);
  assert.deepEqual(positionAt([{ point: [1, 2], time: 1 }, { point: [3, 4], time: 1 }], 1), [1, 2]);
});

test('curved trail meets the latest position and fades independently by sample age', () => {
  const samples = [
    { point: [0, 0], time: 0 },
    { point: [100, 100], time: 50 },
    { point: [200, 0], time: 100 },
  ];
  const segments = trailSegments(samples, 100);
  assert.equal(segments[0].path, 'M0 0 Q100 100 150 50');
  assert.equal(segments[1].path, 'M150 50 Q200 0 200 0');
  assert.ok(segments[0].opacity < segments[1].opacity);
  assert.equal(segments[1].opacity, 1);
  assert.ok(trailSegments(samples, 650).every(s => s.opacity === 0));
  assert.deepEqual(trailSegments([], 0), []);
});
