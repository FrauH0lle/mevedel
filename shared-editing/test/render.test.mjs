import test from 'node:test';
import assert from 'node:assert/strict';
import { boardSVG, shapeSVG } from '../render.mjs';

test('styled shapes render deterministically with bounded work', () => {
  const rough = { id: 'a', type: 'rect', box: [0, 0, 200, 120], rough: 2, fill: '#a5d8ff', pattern: 'cross' };
  const first = shapeSVG(rough, [rough]);
  assert.equal(first, shapeSVG(rough, [rough]), 'same shape, same wobble');
  assert.match(first, /<clipPath id="clip-a">/);
  assert.equal(first.match(/<path /g).length, 4, 'silhouette, hatch, and two stroke passes');
  assert.notEqual(shapeSVG({ ...rough, id: 'b' }, []), first.replaceAll('clip-a', 'clip-b'));
  const huge = shapeSVG({ ...rough, box: [0, 0, 1e6, 1e6] }, []);
  assert.ok(huge.length < 400000, 'hatching a board-sized shape stays bounded');
  const dashed = shapeSVG({ id: 'd', type: 'ellipse', box: [0, 0, 100, 50], dash: 'dotted', width: 4, opacity: 30 }, []);
  assert.match(dashed, /stroke-dasharray="1.2 10.1"/);
  assert.match(dashed, /<g data-shape="d" opacity="0.3">/);
  assert.equal(dashed.match(/<path /g).length, 1, 'a dashed stroke draws once');
  const arrow = shapeSVG({ id: 'e', type: 'arrow', box: [0, 0, 100, 100], points: [[0, 0], [100, 100]], rough: 1 }, []);
  assert.match(arrow, /<path d="M[^"]*" fill="none" [^>]*marker-end="url\(#arrowhead\)"/);
  assert.doesNotMatch(arrow, /d="C/, 'every stroke starts with a move');
  const text = shapeSVG({ id: 't', type: 'sticky', box: [0, 0, 180, 100], text: 'a\nb', fontSize: 32 }, []);
  assert.match(text, /font-size="32"/);
  assert.match(text, /dy="44"/);
  assert.match(text, /<path d="[^"]*" fill="#fff1a8" stroke="none"\/>/, 'a sticky note is solid paper by default');
  assert.match(boardSVG([rough]), /^<svg [^>]*viewBox="-30 -30 260 180">/);
});
