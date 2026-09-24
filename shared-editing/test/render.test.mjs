import test from 'node:test';
import assert from 'node:assert/strict';
import { boardSVG, shapeSVG, pathPoints } from '../render.mjs';

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
  assert.match(arrow, /<path d="M100 100L[^"]*Z" fill="#242424"/);
  assert.doesNotMatch(arrow, /d="C/, 'every stroke starts with a move');
  const text = shapeSVG({ id: 't', type: 'sticky', box: [0, 0, 180, 100], text: 'a\nb', fontSize: 32 }, []);
  assert.match(text, /font-size="32"/);
  assert.match(text, /y="84"/);
  assert.equal((text.match(/<text /g) || []).length, 2);
  assert.match(text, /<path d="[^"]*" fill="#fff1a8" stroke="none"\/>/, 'a sticky note is solid paper by default');
  assert.match(boardSVG([rough]), /^<svg [^>]*viewBox="-30 -30 260 180">/);
});


test('bound arrows meet silhouettes and follow moved or resized shapes', () => {
  const from = {id:'from',type:'rect',edges:'sharp',box:[0,0,100,100]};
  const to = {id:'to',type:'rect',edges:'sharp',box:[300,0,100,100]};
  const arrow = {id:'arrow',type:'arrow',box:[50,50,300,0],from:'from',to:'to'};
  const close = (actual, expected) => actual.forEach((p,i) => p.forEach((v,j) =>
    assert.ok(Math.abs(v-expected[i][j]) < 1e-6, `${JSON.stringify(actual)} != ${JSON.stringify(expected)}`)));
  close(pathPoints(arrow,[from,to]), [[100,50],[300,50]]);
  close(pathPoints(arrow,[from,{...to,box:[400,0,200,100]}]), [[100,50],[400,50]]);
  close(pathPoints({...arrow,from:undefined},[from,to]), [[50,50],[300,50]]);
  close(pathPoints({...arrow,to:'deleted'},[from,to]), [[100,50],[350,50]]);
  close(pathPoints({...arrow,from:undefined,to:undefined},[]), [[50,50],[350,50]]);
  for (const [type, expected] of [
    ['rect',100], ['diamond',75], ['ellipse',50+50/Math.sqrt(2)],
  ]) {
    close(pathPoints({...arrow,to:undefined,points:[[50,50],[200,200]]},[{...from,type}]),
      [[expected,expected],[200,200]]);
  }
  const rounded = {...from,edges:'round'};
  close(pathPoints({...arrow,to:undefined,points:[[50,50],[200,200]]},[rounded]),
    [[75+25/Math.sqrt(2),75+25/Math.sqrt(2)],[200,200]]);
  const cylinder = {...from,type:'cylinder'};
  // At x=90 the cylinder's bottom rim is y=84+16*sqrt(1-.8^2).
  const rim = [90,93.6];
  close(pathPoints({...arrow,to:undefined,points:[[50,50],[130,137.2]]},[cylinder]),
    [rim,[130,137.2]]);
  close(pathPoints({...arrow,to:undefined,points:[[50,50],[200,50]]},[cylinder]),
    [[100,50],[200,50]]);
  close(pathPoints({...arrow,to:undefined,points:[[50,50],[50,-100]]},[cylinder]),
    [[50,0],[50,-100]]);
  // A sticky note's slanted left edge runs from (1,3) to (0,100).
  close(pathPoints({...arrow,to:undefined,points:[[50,50],[-100,50]]},[{...from,type:'sticky'}]),
    [[1-47/97,50],[-100,50]]);
  for (const box of [[0,0,0,0],[0,0,0,100],[300,0,100,100]])
    assert.ok(pathPoints(arrow,[{...from,box},to]).flat().every(Number.isFinite));
  assert.deepEqual(pathPoints({type:'pen',box:[0,0,10,10],points:[[0,0],[5,3],[10,10]]},[]),
    [[0,0],[5,3],[10,10]]);
  assert.match(boardSVG([from,to,arrow]), /<path d="M300 50L286 57L286 43Z" fill="#242424"/);
});
