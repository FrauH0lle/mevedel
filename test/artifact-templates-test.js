/* artifact-templates-test.js -- bundled artifact template renderer assertions
 *
 * The dashboard and data-table skills ship templates carrying inline
 * renderers.  They run only inside a published artifact's sandbox, where
 * nothing reports a failure, so the inputs that quietly produce wrong output
 * -- degenerate chart geometry, missing cells, a non-numeric value in a
 * numeric column -- are asserted here against the fake DOM the viewer tests
 * use.
 *
 * Run: node test/artifact-templates-test.js
 */
'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const {Element} = require('./collaboration-viewer-dom');

const templateOf = skill => path.join(
  __dirname, '..', 'skills', skill, 'template.html');

/* The renderer is the one <script> in each template carrying no type. */
function rendererSource(skill) {
  const html = fs.readFileSync(templateOf(skill), 'utf8');
  const match = html.match(/<script>\n([\s\S]*?)<\/script>/);
  assert.ok(match, `${skill} template has an untyped <script> renderer`);
  return match[1];
}

class Node extends Element {
  constructor(tag) {
    super(tag);
    this.style = {};
    this.classList = {
      toggle: (name, on) => {
        const has = this.className.split(/\s+/).filter(Boolean);
        const next = has.filter(c => c !== name);
        if (on) next.push(name);
        this.className = next.join(' ');
      },
    };
  }
  descendants() {
    return this.children.flatMap(child =>
      typeof child === 'string' ? [] : [child, ...child.descendants()]);
  }
  /* Enough of the selector language for what the templates actually ask
     for: a tag, a class, or one descendant step between them. */
  querySelectorAll(selector) {
    const steps = selector.trim().split(/\s+/);
    let pool = this.descendants();
    steps.forEach((step, i) => {
      const match = node => step.startsWith('.')
        ? node.className.split(/\s+/).includes(step.slice(1))
        : node.tagName === step;
      pool = pool.filter(match);
      if (i < steps.length - 1) pool = pool.flatMap(n => n.descendants());
    });
    return pool;
  }
  querySelector(selector) { return this.querySelectorAll(selector)[0] || null; }
}

function render(spec) {
  const svg = new Node('svg');
  const legend = new Node('div');
  const specNode = new Node('script');
  specNode.textContent = typeof spec === 'string' ? spec : JSON.stringify(spec);
  const byId = {chart: svg, legend, 'chart-spec': specNode};
  const context = {
    document: {
      documentElement: new Node('html'),
      getElementById: id => byId[id] || null,
      createElement: tag => new Node(tag),
      createElementNS: (_ns, tag) => new Node(tag),
    },
    /* Real token values; the renderer reads them for stroke and fill. */
    getComputedStyle: () => ({
      getPropertyValue: name => ({
        '--accent': '#2f6f8f',
        '--rule': '#e3e6ea',
        '--ink-soft': '#646b78',
      }[name] || '#000'),
    }),
  };
  vm.runInNewContext(rendererSource('artifact-dashboard'), context);
  return {svg, legend};
}

/* Every geometry attribute the renderer emits, flattened. */
function attrValues(node) {
  return Object.values(node.attributes)
    .map(String)
    .concat(node.children.flatMap(child =>
      typeof child === 'string' ? [] : attrValues(child)));
}

function assertNoBadGeometry(svg, label) {
  const bad = attrValues(svg).filter(v => /NaN|Infinity|undefined/.test(v));
  assert.deepEqual(bad, [], `${label}: emitted unusable geometry`);
}

function tagsOf(svg, tag) {
  return svg.children.filter(child => child.tagName === tag);
}

function run() {
  const points = n => Array.from({length: n},
    (_, i) => ({x: `p${i}`, y: i * 10}));

  /* A line chart draws one polyline per series plus an endpoint dot, and
     labels a gridline at each of the five steps. */
  {
    const {svg, legend} = render({type: 'line', series: [
      {name: 'A', points: points(6)},
    ]});
    assert.equal(tagsOf(svg, 'polyline').length, 1);
    assert.equal(tagsOf(svg, 'circle').length, 1);
    assert.equal(tagsOf(svg, 'line').length, 5);
    assert.equal(
      tagsOf(svg, 'polyline')[0].attributes.points.split(' ').length, 6);
    /* A lone series names itself in the chart title, not a legend. */
    assert.equal(legend.children.length, 0);
    assertNoBadGeometry(svg, 'line');
  }

  /* Two series get distinct strokes and a legend entry each. */
  {
    const {svg, legend} = render({type: 'line', series: [
      {name: 'A', points: points(4)},
      {name: 'B', points: points(4)},
    ]});
    const strokes = tagsOf(svg, 'polyline').map(p => p.attributes.stroke);
    assert.equal(strokes.length, 2);
    assert.notEqual(strokes[0], strokes[1], 'series share a stroke color');
    assert.equal(legend.children.length, 2);
    assertNoBadGeometry(svg, 'multi-series');
  }

  /* One point has no span to divide across: it must centre, not divide by
     zero. */
  {
    const {svg} = render({type: 'line', series: [
      {name: 'A', points: [{x: 'only', y: 42}]},
    ]});
    assertNoBadGeometry(svg, 'single point');
    assert.equal(tagsOf(svg, 'circle').length, 1);
  }

  /* A flat series has no range to scale against. */
  {
    const {svg} = render({type: 'line', series: [
      {name: 'A', points: [{x: 'a', y: 7}, {x: 'b', y: 7}, {x: 'c', y: 7}]},
    ]});
    assertNoBadGeometry(svg, 'flat series');
  }

  /* Negative values must not collapse the bar baseline. */
  {
    const {svg} = render({type: 'bar', series: [
      {name: 'A', points: [{x: 'a', y: -5}, {x: 'b', y: 12}]},
    ]});
    const bars = tagsOf(svg, 'rect');
    assert.equal(bars.length, 2);
    assert.ok(Number(bars[0].attributes.height) > 1,
              'negative bar collapsed at the domain floor');
    assert.ok(Number(bars[1].attributes.height) > 1,
              'positive bar collapsed at the domain ceiling');
    assert.ok(Number(bars[0].attributes.y) > Number(bars[1].attributes.y),
              'negative bar does not extend below the zero baseline');
    assertNoBadGeometry(svg, 'bar with negatives');
  }

  /* A single donut slice covers the full ring, where one arc's start and end
     coincide and the naive path collapses. */
  {
    const {svg, legend} = render({
      type: 'donut', slices: [{name: 'Only', value: 9}],
    });
    assert.equal(tagsOf(svg, 'path').length, 1);
    assert.equal(legend.children.length, 1);
    assertNoBadGeometry(svg, 'single donut slice');
  }

  {
    const {svg, legend} = render({type: 'donut', slices: [
      {name: 'A', value: 3}, {name: 'B', value: 1}, {name: 'C', value: 6},
    ]});
    assert.equal(tagsOf(svg, 'path').length, 3);
    assert.equal(legend.children.length, 3);
    assertNoBadGeometry(svg, 'donut');
  }

  /* An explicit domain wins over the data, so a narrow band far from zero
     can be zoomed without the axis snapping back to a zero floor. */
  {
    const {svg} = render({
      type: 'line', y: {min: 90, max: 100},
      series: [{name: 'Uptime', points: [{x: 'a', y: 97}, {x: 'b', y: 99}]}],
    });
    const labels = svg.children
      .filter(child => child.tagName === 'text')
      .map(child => child.textContent);
    assert.ok(labels.includes('90'), 'y.min ignored');
    assert.ok(labels.includes('100'), 'y.max ignored');
  }

  /* Bad or empty input leaves the page standing rather than throwing into a
     sandbox where nothing would report it. */
  for (const spec of ['{ not json', '{}', {type: 'line', series: []},
                      {type: 'donut', slices: []},
                      {type: 'line', series: [{name: 'A', points: []}]}]) {
    const {svg} = render(spec);
    assert.equal(svg.children.length, 0,
                 `unusable spec drew something: ${JSON.stringify(spec)}`);
  }

  console.log('dashboard chart renderer: all assertions passed');
}

run();
runTableTests();

/* --- data-table template ------------------------------------------------ */

function renderTable(columns, rows) {
  const table = new Node('table');
  const thead = new Node('thead');
  const headRow = new Node('tr');
  const body = new Node('tbody');
  thead.append(headRow);
  table.append(thead, body);

  const filter = new Node('input');
  filter.value = '';
  const count = new Node('span');
  const columnsNode = new Node('script');
  columnsNode.textContent = typeof columns === 'string'
    ? columns : JSON.stringify(columns);
  const rowsNode = new Node('script');
  rowsNode.textContent = typeof rows === 'string' ? rows : JSON.stringify(rows);

  const byId = {
    dt: table, 'dt-filter': filter, 'dt-count': count,
    'dt-columns': columnsNode, 'dt-rows': rowsNode,
  };
  const context = {
    document: {
      getElementById: id => byId[id] || null,
      createElement: tag => new Node(tag),
      createDocumentFragment: () => new Node('#fragment'),
    },
  };
  vm.runInNewContext(rendererSource('artifact-data-table'), context);

  const type = value => {
    filter.value = value;
    filter.dispatch('input');
  };
  /* A fragment appended to tbody keeps its own children in the stub, so
     flatten one level to read the rows back. */
  const bodyRows = () => body.children.flatMap(child =>
    child.tagName === '#fragment' ? child.children : [child]);
  const cells = () => bodyRows().map(tr =>
    tr.children.map(td => td.textContent));

  return {table, headRow, body, count, type, cells,
          sortBy: label => headRow.children
            .find(th => th.textContent === label).dispatch('click')};
}

function runTableTests() {
  const columns = [
    {key: 'name', label: 'Name', type: 'text'},
    {key: 'size', label: 'Size', type: 'num'},
  ];

  /* Rows render in source order until a column is sorted. */
  {
    const t = renderTable(columns, [
      {name: 'beta', size: 2}, {name: 'alpha', size: 10},
    ]);
    assert.deepEqual(t.cells(), [['beta', '2'], ['alpha', '10']]);
    assert.equal(t.count.textContent, '2 rows');
  }

  /* A numeric column sorts by magnitude, not by string order -- the bug
     that puts 10 before 2. Clicking again reverses it. */
  {
    const t = renderTable(columns, [
      {name: 'a', size: 10}, {name: 'b', size: 2}, {name: 'c', size: 100},
    ]);
    t.sortBy('Size');
    assert.deepEqual(t.cells().map(r => r[0]), ['b', 'a', 'c']);
    t.sortBy('Size');
    assert.deepEqual(t.cells().map(r => r[0]), ['c', 'a', 'b']);
  }

  /* Missing values are absent, not extreme: they sort last whichever way
     the arrow points, and render as a blank cell rather than "null". */
  {
    const t = renderTable(columns, [
      {name: 'has', size: 5}, {name: 'null', size: null},
      {name: 'blank', size: '   '}, {name: 'gone'},
    ]);
    t.sortBy('Size');
    assert.deepEqual(t.cells().map(r => r[0]),
                     ['has', 'null', 'blank', 'gone']);
    t.sortBy('Size');
    assert.equal(t.cells()[0][0], 'has', 'missing values led the reverse sort');
    assert.deepEqual(t.cells().map(r => r[1]).slice(1), ['', '', '']);
  }

  /* A non-numeric value in a num column is shown as authored and sorts
     last, rather than being coerced to 0 and parading to the top. */
  {
    const t = renderTable(columns, [
      {name: 'good', size: 3}, {name: 'bad', size: '$1,234.50'},
    ]);
    t.sortBy('Size');
    assert.deepEqual(t.cells(), [['good', '3'], ['bad', '$1,234.50']]);
  }

  /* The filter matches text columns only, so a numeric column's digits do
     not answer a text query. */
  {
    const t = renderTable(columns, [
      {name: 'apple', size: 7}, {name: 'banana', size: 42},
    ]);
    t.type('app');
    assert.deepEqual(t.cells().map(r => r[0]), ['apple']);
    assert.equal(t.count.textContent, '1 of 2 rows');
    t.type('42');
    assert.equal(t.cells()[0][0], 'No rows match.');
    t.type('');
    assert.equal(t.cells().length, 2);
    assert.equal(t.count.textContent, '2 rows');
  }

  /* Bad or empty data leaves the page standing. */
  for (const [c, r] of [['{ not json', []], [columns, '{ not json'],
                        [{}, []], [columns, {}]]) {
    const t = renderTable(c, r);
    assert.equal(t.cells().length, 0, 'unusable data drew rows');
  }

  console.log('data-table renderer: all assertions passed');
}
