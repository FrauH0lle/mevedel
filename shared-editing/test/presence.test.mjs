import test from 'node:test';
import assert from 'node:assert/strict';
import { BoardPreviews, positionAt, trailSegments } from '../presence.mjs';

test('overlapping previews reconcile with commits and reject malformed geometry', () => {
  const previews = new BoardPreviews(() => {});
  const show = (peer, id, box, opId) => previews.receive({peer, mode:'cursor', preview:{opId, shapes:[{id,box}]}});
  try {
    show(1, 'a', [10,20,30,40], 'one');
    show(2, 'a', [20,20,30,40], 'two');
    show(3, 'b', [50,20,30,40], 'three');
    assert.equal(previews.boxes().get('a')[0], 20);
    previews.clear(2);
    assert.equal(previews.boxes().get('a')[0], 10);
    for (const box of [[0,0,-1,1], [NaN,0,1,1], [1e7,0,1,1], [0,0]]) show(4,'a',box);
    previews.receive({peer:4, mode:'cursor', preview:{shapes:[null]}});
    assert.equal(previews.people.size, 2);
    previews.reconcile([{id:'different-writer', changes:[{id:'a'}]}]);
    assert.equal(previews.boxes().has('a'), false);
    assert.equal(previews.boxes().has('b'), true);
    previews.reconcile([{id:'three', changes:[]}]);
    show(3, 'b', [60,20,30,40], 'three');
    assert.equal(previews.boxes().size, 0, 'late preview cannot override its commit');
    show(1, 'a', [10,20,30,40]);
    previews.receive({peer:1, mode:'cursor'});
    assert.equal(previews.boxes().size, 0);
  } finally { previews.clear(); }
});

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
