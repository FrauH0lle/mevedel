import test from 'node:test';
import assert from 'node:assert/strict';
import { boardSVG, elementSVG, resolveScene, extent, bounds, shapesInRegion } from '../render.mjs';
import { linearPath, complete, seedOf } from '../scene.mjs';
import { wrap, measure, layoutText, lineWidth } from '../text.mjs';

const rect = (id, x, y, w, h, extra = {}) => ({ id, type: 'rectangle', x, y, width: w, height: h, ...extra });
const svgOf = (elements, id = elements[0].id) => {
  const scene = resolveScene(elements);
  return elementSVG(scene.byId.get(id), scene, {});
};
const close = (actual, expected, label) => actual.forEach((p, i) => p.forEach((v, j) =>
  assert.ok(Math.abs(v - expected[i][j]) < 0.02, `${label}: ${JSON.stringify(actual)} != ${JSON.stringify(expected)}`)));

test('elements render as Excalidraw does: seeded roughjs strokes, fills and dashes', () => {
  const hachured = rect('a', 0, 0, 200, 120, { roughness: 2, backgroundColor: '#a5d8ff', fillStyle: 'cross-hatch' });
  const first = svgOf([hachured]);
  assert.equal(first, svgOf([hachured]), 'the same element draws the same wobble');
  assert.notEqual(svgOf([{ ...hachured, seed: 7 }]), first, 'the seed chooses the wobble');
  assert.equal(seedOf('a'), complete(hachured).seed, 'an element without a seed derives one from its id');
  assert.match(first, /stroke="#a5d8ff"/, 'cross-hatching strokes in the background colour');
  const dotted = svgOf([{ id: 'd', type: 'ellipse', x: 0, y: 0, width: 100, height: 50, strokeStyle: 'dotted', strokeWidth: 4, opacity: 30 }]);
  assert.match(dotted, /stroke-dasharray="1.5 10"/, 'Excalidraw dots: [1.5, 6 + width]');
  assert.match(dotted, /stroke-width="4.5"/, 'non-solid strokes widen by half a pixel');
  assert.match(dotted, /<g data-shape="d" opacity="0.3">/);
  assert.match(svgOf([rect('r', 10, 20, 50, 40, { angle: Math.PI / 2 })]), /transform="translate\(10 20\) rotate\(90 25 20\)"/);
  const huge = svgOf([{ ...hachured, width: 1e6, height: 1e6 }]);
  assert.ok(huge.length < 1e6, 'hachuring a board-sized shape stays bounded');
  assert.match(boardSVG([rect('a', 0, 0, 200, 120)]), /^<svg [^>]*viewBox="-30 -30 260 180"><rect [^>]*fill="#ffffff"\/>/);
});

test('arrows carry every Excalidraw arrowhead and freedraw strokes are filled outlines', () => {
  const arrow = (extra) => ({ id: 'e', type: 'arrow', x: 0, y: 0, width: 200, height: 0, points: [[0, 0], [200, 0]], roughness: 0, ...extra });
  const plain = svgOf([arrow()]);
  assert.equal(plain.match(/<path /g).length, 3, 'a shaft and two barbs by default');
  for (const head of ['triangle', 'circle', 'diamond', 'bar', 'cardinality_one_or_many', 'cardinality_zero_or_many'])
    assert.ok(svgOf([arrow({ endArrowhead: head })]) !== plain, head);
  assert.match(svgOf([arrow({ endArrowhead: 'triangle_outline' })]), /fill="#ffffff"/, 'outline heads fill with the paper');
  // On a coloured canvas they fill with that colour, never a white patch.
  const tinted = boardSVG([arrow({ endArrowhead: 'circle_outline' })], { background: '#fffce8' });
  assert.match(tinted, /<path [^>]*fill="#fffce8"/);
  assert.doesNotMatch(tinted, /fill="#ffffff"/);
  assert.equal(svgOf([arrow({ endArrowhead: null })]).match(/<path /g).length, 1);
  const line = svgOf([{ ...arrow(), type: 'line', id: 'l' }]);
  assert.match(line, /data-linear="true"/);
  assert.equal(line.match(/<path /g).length, 1, 'lines have no heads by default');
  const stroke = svgOf([{ id: 'f', type: 'freedraw', x: 10, y: 10, width: 50, height: 20, points: [[0, 0], [20, 10], [50, 20]], strokeColor: '#e03131' }]);
  assert.match(stroke, /<path d="M [^"]*Z" fill="#e03131" stroke="none"\/>/);
});

test('bound arrows meet shape outlines at the binding gap and follow moves', () => {
  const from = rect('from', 0, 0, 100, 100), to = rect('to', 300, 0, 100, 100);
  const bind = (elementId, mode = 'orbit') => ({ elementId, fixedPoint: [0.5001, 0.5001], mode });
  const arrow = { id: 'arrow', type: 'arrow', x: 0, y: 0, width: 0, height: 0, points: [[0, 0], [1, 0]],
    startBinding: bind('from'), endBinding: bind('to') };
  const path = (elements) => resolveScene(elements).byId.get('arrow').path;
  // The gap is 5 + strokeWidth / 2 outside each outline.
  close(path([from, to, arrow]), [[106, 50], [294, 50]], 'orbit');
  close(path([from, { ...to, x: 400, width: 200 }, arrow]), [[106, 50], [394, 50]], 'follows the moved target');
  close(path([from, to, { ...arrow, endBinding: bind('to', 'inside') }]), [[106, 50], [350.01, 50]], 'inside binds at the fixed point');
  close(path([from, { ...arrow, points: [[0, 0], [300, 50]] }]), [[106, 50], [300, 50]], 'a dangling binding draws unbound');
  const ellipse = { ...to, type: 'ellipse', x: 300, y: 300 };
  const diagonal = path([from, ellipse, { ...arrow, endBinding: bind('to') }]);
  const [ex, ey] = diagonal[1];
  assert.ok(Math.abs(((ex - 350) / 56) ** 2 + ((ey - 350) / 56) ** 2 - 1) < 1e-3, 'meets the inflated ellipse');
  assert.deepEqual(linearPath(complete({ ...arrow, startBinding: null, endBinding: null }), new Map()), [[0, 0], [1, 0]]);
  for (const box of [{ width: 0, height: 0 }, { width: 0, height: 100 }])
    assert.ok(path([{ ...from, ...box }, to, arrow]).flat().every(Number.isFinite));
});

test('labels wrap inside their containers and arrows cut a gap around theirs', () => {
  const container = rect('box', 0, 0, 120, 80);
  const label = { id: 'label', type: 'text', x: 0, y: 0, width: 0, height: 0, text: 'A label that wraps', containerId: 'box' };
  const scene = resolveScene([container, label]);
  const laid = scene.byId.get('label');
  assert.equal(laid.textAlign, 'center', 'bound text defaults to centred');
  assert.ok(laid.text.includes('\n'), 'long labels wrap to the container');
  assert.ok(laid.width <= 110 && laid.x >= 5 && laid.y > 0, 'and sit inside its padding');
  assert.deepEqual(scene.order.map((e) => e.id), ['box', 'label'], 'a label draws right after its container');
  assert.ok(boardSVG([container, label]).includes('font-family="Excalifont, Noto Sans, sans-serif"'));
  const arrow = { id: 'a', type: 'arrow', x: 0, y: 200, width: 200, height: 0, points: [[0, 0], [200, 0]] };
  const tag = { ...label, id: 'tag', text: 'flow', containerId: 'a' };
  const svg = svgOf([arrow, tag], 'a');
  assert.match(svg, /<mask id="mask-a" maskUnits="userSpaceOnUse"/);
  const centre = resolveScene([arrow, tag]).byId.get('tag');
  assert.ok(Math.abs(centre.x + centre.width / 2 - 100) < 1e-6, 'an arrow label sits at its midpoint');
  const orphan = resolveScene([{ ...label, containerId: 'gone', x: 40, y: 50 }]).byId.get('label');
  assert.deepEqual([orphan.x, orphan.y, orphan.textAlign], [40, 50, 'left'], 'a dangling label is free text');
});

test('text measures and wraps with the bundled fonts', () => {
  assert.ok(lineWidth('MMMM', 5, 20) > lineWidth('iiii', 5, 20));
  assert.equal(measure('one\ntwo', 5, 20).height, 50, 'Excalifont lines are 1.25 em');
  assert.equal(wrap('alpha beta gamma', 5, 20, lineWidth('alpha beta', 5, 20) + 1), 'alpha beta\ngamma');
  assert.equal(wrap('alpha\n\nbeta', 5, 20, 1000), 'alpha\n\nbeta', 'blank lines survive');
  assert.ok(wrap('x'.repeat(50), 5, 20, 100).split('\n').length > 1, 'a long word breaks between characters');
  assert.ok(wrap('漢字漢字漢字', 5, 20, 45).split('\n').length > 1, 'CJK breaks between characters');
  const free = layoutText(complete({ id: 't', type: 'text', x: 0, y: 0, width: 1, height: 1, text: 'hello' }));
  assert.equal(free.width, lineWidth('hello', 5, 20), 'free text sizes itself');
});

test('images crop, flip and round through clip paths; frames clip their children', () => {
  const files = { f: { mimeType: 'image/png', dataURL: 'data:image/png;base64,AAAA' } };
  const image = { id: 'i', type: 'image', x: 0, y: 0, width: 100, height: 50, fileId: 'f', scale: [-1, 1],
    crop: { x: 50, y: 0, width: 100, height: 50, naturalWidth: 200, naturalHeight: 100 }, roundness: { type: 3 } };
  const scene = resolveScene([image]);
  const svg = elementSVG(scene.byId.get('i'), scene, files);
  assert.match(svg, /<image href="data:image\/png;base64,AAAA" x="-50" y="0" width="200" height="100"/);
  assert.match(svg, /scale\(-1 1\)/);
  assert.match(svg, /<clipPath id="image-i"><rect width="100" height="50" rx="12.5"\/>/);
  assert.match(elementSVG(scene.byId.get('i'), scene, {}), /fill="#e7e7e7"/, 'a missing file draws a placeholder');
  const frame = { id: 'fr', type: 'frame', x: 0, y: 0, width: 300, height: 200, name: 'Area' };
  const child = rect('c', 250, 150, 100, 100, { frameId: 'fr' });
  const board = boardSVG([frame, child]);
  assert.match(board, /<clipPath id="frame-fr">/);
  assert.match(board, /<g clip-path="url\(#frame-fr\)"><g data-shape="c"/);
  assert.match(board, />Area<\/text>/);
});

test('regions and bounds follow displayed geometry; labels select with their containers', () => {
  const a = rect('a', 0, 0, 100, 100), b = rect('b', 300, 0, 100, 100);
  const label = { id: 't', type: 'text', x: 0, y: 0, width: 0, height: 0, text: 'A', containerId: 'a' };
  const arrow = { id: 'c', type: 'arrow', x: 0, y: 0, width: 1, height: 1, points: [[0, 0], [1, 1]],
    startBinding: { elementId: 'a', fixedPoint: [0.5001, 0.5001], mode: 'orbit' },
    endBinding: { elementId: 'b', fixedPoint: [0.5001, 0.5001], mode: 'orbit' } };
  const elements = [a, b, label, arrow];
  const scene = resolveScene(elements);
  assert.deepEqual(extent([a], scene), [0, 0, 100, 100]);
  assert.deepEqual(bounds([a], scene), [-30, -30, 160, 160], 'bounds keep their padding');
  assert.deepEqual(shapesInRegion(elements, [-1, -1, 102, 102], false, scene).map((s) => s.id), ['a'],
    'a label is selected through its container');
  assert.deepEqual(shapesInRegion(elements, [150, 45, 10, 10], true, scene).map((s) => s.id), ['c'],
    'a touched region follows the drawn connector, not its stale stored points');
  assert.deepEqual(shapesInRegion(elements, [-1, -1, 402, 102], false, scene).map((s) => s.id).sort(), ['a', 'b', 'c']);
  assert.deepEqual(shapesInRegion(elements, [150, 200, 50, 50], true, scene), []);
  const turned = rect('r', 0, 0, 100, 20, { angle: Math.PI / 2 });
  const [x, y, w, h] = extent([turned]);
  assert.deepEqual([x, y, w, h].map((v) => Math.round(v)), [40, -40, 20, 100], 'rotation turns the bounds');
});
