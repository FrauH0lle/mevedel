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
  get textContent() {
    return (this.text || '') + this.children.map(child =>
      typeof child === 'string' ? child : child.textContent).join('');
  }
  set textContent(value) { this.text = String(value); this.children = []; }
  append(...children) {
    super.append(...children.flatMap(child =>
      child && child.tagName === '#fragment' ? child.children : [child]));
  }
  replaceChildren(...children) { this.text = ''; super.replaceChildren(...children); }
  setAttribute(name, value) {
    super.setAttribute(name, String(value));
    if (name === 'class') this.className = String(value);
    if (name === 'tabindex') this.tabIndex = Number(value);
  }
  getAttribute(name) { return this.attributes[name] ?? null; }
  removeAttribute(name) { delete this.attributes[name]; }
  getBoundingClientRect() { return {left: 0, top: 0, width: 760, height: 300}; }
  focus() { super.focus(); this.dispatch('focus', {target: this}); }
  blur() { this.focused = false; this.dispatch('blur', {target: this}); }
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
  for (const [id, tag] of Object.entries({
    'chart-title': 'h2', 'chart-description': 'p', 'chart-status': 'p',
    'chart-values': 'details', 'chart-table': 'table', 'chart-tooltip': 'div',
  })) byId[id] = new Node(tag);
  byId['chart-tooltip'].hidden = true;
  byId['chart-values'].open = false;
  byId['chart-values'].append(byId['chart-table']);
  const document = new Node('#document');
  Object.assign(document, {
    getElementById: id => byId[id] || null,
    createElement: tag => new Node(tag),
    createElementNS: (_ns, tag) => new Node(tag),
  });
  const window = new Node('#window');
  window.innerWidth = 1024;
  window.innerHeight = 768;
  vm.runInNewContext(rendererSource('artifact-dashboard'), {document, window});
  return {svg, legend, document, window, byId,
          status: byId['chart-status'], table: byId['chart-table'],
          details: byId['chart-values'], tooltip: byId['chart-tooltip']};
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
runChartContractTests();
runTableTests();

function runChartContractTests() {
  const series = values => [{name: 'A', points: values.map((y, i) => ({x: `p${i}`, y}))}];
  const state = (spec, expected) => {
    const chart = render(spec);
    assert.equal(chart.svg.getAttribute('data-state'), expected, JSON.stringify(spec));
    assert.equal(chart.status.getAttribute('data-state'), expected);
    assert.equal(chart.svg.getAttribute('hidden'), expected === 'ready' ? null : '',
                 'SVG visibility must use the hidden attribute, not an unreflected property');
    if (expected !== 'ready') {
      assert.ok(chart.status.textContent.trim(), `${expected} state is silent`);
      assert.equal(chart.status.hidden, false, `${expected} message is hidden`);
      assert.equal(chart.svg.children.length, 0, `${expected} state drew geometry`);
    }
    return chart;
  };

  /* Invalid schema and genuinely empty datasets are visibly different. */
  for (const spec of ['{bad', 'null', '[]', {}, {type: 'pie'}, {type: false},
    {type: 'line', series: {}}, {type: 'line', series: [null]},
    {type: 'line', series: [{points: [null]}]},
    {type: 'line', series: [{name: 'A', points: [{x: 'x', y: '4'}]}]},
    {type: 'line', series: [{name: 'A', points: [{x: 'x', y: true}]}]},
    {type: 'line', series: [{name: 'A', points: [{x: 'x'}]}]},
    {type: 'line', series: [{name: 'A', points: [{x: 'same', y: 1}, {x: 'same', y: 2}]}]},
    {type: 'line', series: [...series([1, 2]), {name: 'B', points: [{x: 'p0', y: 3}]}]},
    {type: 'line', series: [...series([1]), {name: 'B', points: [{x: 'different', y: 3}]}]},
    {type: 'line', series: [...series([1, 2]), {name: 'B', points: [{x: 'p1', y: 3}, {x: 'p0', y: 4}]}]},
    {type: 'bar', series: series([null])},
    {type: 'line', series: [{name: 'A', points: [{x: 1, y: 2}]}]},
    {type: 'donut', slices: {}}, {type: 'donut', slices: [null]},
    {type: 'donut', slices: [{name: 'bad', value: -1}]},
    {type: 'donut', slices: [{name: 'bad', value: '2'}]},
    '{"type":"line","series":[{"name":"A","points":[{"x":"a","y":1e400}]}]}',
    '{"type":"donut","slices":[{"name":"a","value":1e400}]}',
    {type: 'donut', slices: [{name: 'a', value: 1e308}, {name: 'b', value: 1e308}]},
  ]) state(spec, 'invalid');
  for (const spec of [{type: 'line', series: []}, {type: 'donut', slices: []},
    {type: 'line', series: [{name: 'A', points: []}]},
    {type: 'line', series: series([null, null])},
    {type: 'donut', slices: [{name: 'zero', value: 0}]},
  ]) state(spec, 'empty');
  state({series: series([1])}, 'ready');

  /* Nulls break lines; isolated observations remain visible at their own x. */
  {
    const chart = state({type: 'line', series: series([1, null, 3, 4, null, 6])}, 'ready');
    const lines = tagsOf(chart.svg, 'polyline');
    assert.equal(lines.length, 1, 'line bridged a missing observation');
    assert.equal(lines[0].getAttribute('points').split(' ').length, 2);
    const circles = tagsOf(chart.svg, 'circle');
    assert.equal(circles.length, 3, 'each contiguous segment needs its endpoint or isolated dot');
    const marks = chart.svg.querySelectorAll('.chart-mark');
    assert.equal(marks.length, 4, 'finite line points need individual hit targets');
    assert.ok(marks.every(mark => mark.tagName === 'rect'));
    assert.ok(marks.every(mark => Number(mark.getAttribute('width')) > 0));
    assert.deepEqual(marks.map(mark => mark.getAttribute('data-index')), ['0', '2', '3', '5']);
    assert.deepEqual(chart.table.querySelectorAll('tbody tr').map(tr =>
      tr.children.map(cell => cell.textContent)), [
      ['p0', '1'], ['p1', 'Missing'], ['p2', '3'], ['p3', '4'], ['p4', 'Missing'], ['p5', '6'],
    ]);
    assert.ok(chart.table.querySelectorAll('thead th').every(th => th.getAttribute('scope') === 'col'));
    assert.ok(chart.table.querySelectorAll('tbody th').every(th => th.getAttribute('scope') === 'row'));
    assertNoBadGeometry(chart.svg, 'null gaps');
  }

  /* Domains must contain every datum and, for bars, zero; never clip quietly. */
  for (const type of ['line', 'bar']) {
    for (const y of [{min: 3, max: 2}, {min: 2, max: 2},
      {min: 2, max: 10}, {min: 0, max: 4}, {min: '0', max: 10}]) {
      state({type, y, series: series([1, 5])}, 'invalid');
    }
    state({type, y: {min: 0, max: 6}, series: series([1, 5])}, 'ready');
  }
  state({type: 'bar', y: {min: 1, max: 6}, series: series([2, 5])}, 'invalid');
  state({type: 'line', y: {min: 1, max: 6}, series: series([2, 5])}, 'ready');

  /* Group centers, labels, and baseline agree for every sign distribution. */
  for (const values of [[2, 5], [-2, -5], [-2, 5], [0, 0]]) {
    const chart = state({type: 'bar', series: series(values)}, 'ready');
    const bars = chart.svg.querySelectorAll('.chart-mark');
    assert.equal(bars.length, 2);
    const labels = tagsOf(chart.svg, 'text').filter(n => /^p\d$/.test(n.textContent));
    bars.forEach((bar, i) => {
      const center = Number(bar.getAttribute('x')) + Number(bar.getAttribute('width')) / 2;
      assert.ok(Math.abs(center - Number(labels[i].getAttribute('x'))) < 1e-9,
                'category label is not at its bar group center');
      assert.ok(Number(bar.getAttribute('height')) >= 0);
      if (values[i] === 0) {
        assert.equal(bar.getAttribute('fill'), 'transparent', 'zero became a painted nonzero bar');
        assert.ok(Number(bar.getAttribute('height')) > 0, 'zero value has no pointer hit target');
        assert.equal(Number(bar.getAttribute('y')) + Number(bar.getAttribute('height')) / 2,
                     Number(tagsOf(chart.svg, 'line')[0].getAttribute('y1')),
                     'zero hit target is not centered at baseline');
      }
    });
    assertNoBadGeometry(chart.svg, `bar ${values}`);
  }
  {
    const chart = state({type: 'bar', series: [...series([2, 5]), {name: 'B',
      points: [{x: 'p0', y: 3}, {x: 'p1', y: -4}]}]}, 'ready');
    const labels = tagsOf(chart.svg, 'text').filter(n => /^p\d$/.test(n.textContent));
    labels.forEach((label, i) => {
      const bars = chart.svg.querySelectorAll('.chart-mark').filter(mark => mark.getAttribute('data-index') === String(i));
      const left = Math.min(...bars.map(bar => Number(bar.getAttribute('x'))));
      const right = Math.max(...bars.map(bar => Number(bar.getAttribute('x')) + Number(bar.getAttribute('width'))));
      assert.ok(Math.abs((left + right) / 2 - Number(label.getAttribute('x'))) < 1e-9,
                'multi-series group and category label centers disagree');
    });
  }

  /* Tiny ticks retain useful precision and captions are actually drawn. */
  {
    const chart = state({type: 'line', x: {label: 'Elapsed'}, y: {label: 'seconds'},
      series: series([0.000001, 0.000005])}, 'ready');
    const labels = tagsOf(chart.svg, 'text');
    const ticks = chart.svg.querySelectorAll('.y-tick');
    assert.equal(new Set(ticks.map(n => n.textContent)).size, 5, 'tiny tick labels collapse');
    assert.ok(labels.some(n => n.textContent === 'Elapsed'));
    assert.ok(labels.some(n => n.textContent === 'seconds'));
    assert.ok(chart.table.textContent.includes('0.000001'), 'exact table value rounded away');
  }

  /* Zero slices stay in the data even though they have no drawn arc. */
  {
    const chart = state({type: 'donut', slices: [
      {name: 'none', value: 0}, {name: 'one', value: 1000001},
    ]}, 'ready');
    assert.equal(tagsOf(chart.svg, 'path').length, 1);
    assert.equal(chart.legend.children.length, 2);
    assert.ok(parseFloat(chart.svg.style.minWidth) >=
      Number(chart.svg.getAttribute('viewBox').split(' ')[2]),
    'donut must not shrink its 12-unit labels below 12px on narrow screens');
    assert.deepEqual(chart.table.querySelectorAll('tbody tr').map(tr =>
      tr.children.map(cell => cell.textContent)), [['1. none', '0'], ['2. one', '1000001']]);
    const empty = state({type: 'donut', slices: [{name: 'none', value: 0}]}, 'empty');
    assert.ok(empty.table.textContent.includes('none'), 'zero-total table disappeared');
  }

  /* Hostile labels are inert text in exact-value tables and both tooltip paths. */
  {
    const hostile = '<img src=x onerror=alert(1)>';
    const chart = state({type: 'line', y: {label: 'units'}, series: [{name: hostile,
      points: [{x: hostile, y: 1234567.8912345}]}]}, 'ready');
    const mark = chart.svg.querySelector('.chart-mark');
    const exact = mark.getAttribute('aria-label');
    assert.ok(exact.includes(hostile));
    assert.ok(exact.includes('1234567.8912345'));
    assert.ok(exact.includes('units'));
    assert.ok(chart.table.textContent.includes(hostile));
    assert.equal(chart.table.querySelectorAll('img').length, 0);
    assert.equal(mark.getAttribute('tabindex'), '0');
    mark.focus();
    assert.equal(chart.tooltip.hidden, false);
    assert.equal(chart.tooltip.textContent, exact);
    assert.ok(Object.values(chart.tooltip.style).every(v => !/NaN|Infinity/.test(v)));
    assert.equal(mark.getAttribute('aria-describedby'), 'chart-tooltip');
    mark.blur();
    assert.equal(chart.tooltip.hidden, true);
    assert.equal(mark.getAttribute('aria-describedby'), null);
    mark.dispatch('pointerenter', {clientX: 50, clientY: 60});
    mark.dispatch('pointermove', {clientX: 70, clientY: 80});
    assert.equal(chart.tooltip.hidden, false);
    assert.equal(chart.tooltip.textContent, exact);
    chart.document.dispatch('keydown', {key: 'Escape'});
    assert.equal(chart.tooltip.hidden, true);
    assert.equal(mark.getAttribute('aria-describedby'), null);
    mark.dispatch('pointerenter', {clientX: 50, clientY: 60});
    mark.dispatch('pointerleave');
    assert.equal(chart.tooltip.hidden, true);
    assert.equal(chart.tooltip.querySelectorAll('img').length, 0);
  }

  /* Dense charts expose a table, not hundreds of per-point tab stops. */
  {
    const chart = state({type: 'line', series: series(Array.from({length: 300}, (_, i) => i))}, 'ready');
    const marks = chart.svg.querySelectorAll('.chart-mark');
    assert.equal(marks.length, 300);
    assert.equal(marks.filter(n => n.tabIndex === 0).length, 0);
    assert.equal(chart.details.hidden, false);
    assert.equal(chart.table.querySelectorAll('tbody tr').length, 300);
    for (const wasOpen of [false, true]) {
      chart.details.open = wasOpen;
      chart.window.dispatch('beforeprint');
      assert.equal(chart.details.open, true, 'print hides exact values behind disclosure');
      chart.window.dispatch('afterprint');
      assert.equal(chart.details.open, wasOpen, 'printing changed disclosure state');
    }
  }

  /* Palette declarations stay live when the OS theme changes, and bounded. */
  {
    const chart = state({type: 'line', series: Array.from({length: 12}, (_, i) => ({
      name: `series ${i}`, points: [{x: 'a', y: i}, {x: 'b', y: i + 1}],
    }))}, 'ready');
    const colors = tagsOf(chart.svg, 'polyline').map(n => n.getAttribute('stroke'));
    assert.ok(colors.every(color => color.includes('var(--')), 'colors snapshot computed theme');
    for (const color of colors) for (const percentage of color.matchAll(/([\d.]+)%/g)) {
      assert.ok(Number(percentage[1]) <= 100, 'palette extrapolates beyond 100%');
    }
    assertNoBadGeometry(chart.svg, 'bounded palette');
  }
  /* Static accessibility wiring must exist too: the fake DOM is not an HTML parser. */
  {
    const html = fs.readFileSync(templateOf('artifact-dashboard'), 'utf8');
    assert.match(html, /<h2\b[^>]*id="chart-title"/);
    assert.match(html, /<p\b[^>]*id="chart-description"/);
    assert.match(html, /<p\b[^>]*id="chart-status"[^>]*role="status"/);
    assert.match(html, /<svg\b[^>]*aria-labelledby="chart-title"[^>]*aria-describedby="chart-description"/);
    assert.match(html, /<details\b[^>]*id="chart-values"/);
    assert.match(html, /<table\b[^>]*id="chart-table"/);
    assert.match(html, /<div\b[^>]*id="chart-tooltip"[^>]*role="tooltip"[^>]*hidden/);
  }
  console.log('dashboard chart contracts: all assertions passed');
}

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
  const filterContext = new Node('p');
  const columnsNode = new Node('script');
  columnsNode.textContent = typeof columns === 'string'
    ? columns : JSON.stringify(columns);
  const rowsNode = new Node('script');
  rowsNode.textContent = typeof rows === 'string' ? rows : JSON.stringify(rows);

  const byId = {
    dt: table, 'dt-filter': filter, 'dt-count': count,
    'dt-filter-context': filterContext,
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
  const cells = () => body.children.map(tr =>
    tr.children.map(td => td.textContent));

  return {table, headRow, body, count, filter, filterContext, type, cells,
          sortBy: label => headRow.children
            .map(th => th.querySelector('button'))
            .find(button => button.textContent.replace(/[↑↓]/g, '').trim() === label)
            .dispatch('click')};
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

  /* Invalid schemas and malformed rows are reported, never silently dropped. */
  for (const [c, r] of [['{ not json', []], [columns, '{ not json'],
    [{}, []], [columns, {}], [[], []], [[null], []], [[[]], []],
    [[{key: ''}], []], [[{key: '  '}], []], [[{key: 5}], []],
    [[{key: 'a'}, {key: 'a'}], []], [[{key: 'a', type: 'number'}], []],
    [[{key: 'a', label: {nested: true}}], []],
    [columns, [null]], [columns, [[]]], [columns, [false]],
    [columns, [{name: {nested: true}}]], [columns, [{size: [1]}]],
  ]) {
    const t = renderTable(c, r);
    assert.deepEqual(t.cells(), [['Data failed to load: invalid JSON or malformed columns or rows.']],
                     `malformed data was not reported: ${JSON.stringify([c, r])}`);
    assert.equal(t.count.textContent, 'Data unavailable');
    assert.equal(t.filter.disabled, true);
  }

  {
    const t = renderTable(columns, []);
    assert.deepEqual(t.cells(), [['No rows.']]);
    assert.equal(t.count.textContent, '0 rows');
    assert.notEqual(t.filter.disabled, true);
  }

  /* Absent and null metadata retain defaults; scalar labels are plain text. */
  {
    const t = renderTable([{key: 'a'}, {key: 'b', type: null, label: null},
      {key: 'c', label: 42}], [{a: 'A', b: 'B', c: true}]);
    assert.deepEqual(t.cells(), [['A', 'B', 'true']]);
    assert.deepEqual(t.headRow.children.map(th =>
      th.querySelector('button').textContent.replace(/[↑↓]/g, '').trim()), ['a', 'b', '42']);
  }

  /* Prototype names are legitimate keys, but inherited values are not cells. */
  {
    const t = renderTable([{key: '__proto__'}, {key: 'constructor'}, {key: 'toString'}],
      '[{}, {"__proto__":"own", "constructor":"ctor", "toString":"literal"}]');
    assert.deepEqual(t.cells(), [['', '', ''], ['own', 'ctor', 'literal']]);
    t.type('function');
    assert.deepEqual(t.cells(), [['No rows match.']]);
    t.type('ctor');
    assert.deepEqual(t.cells(), [['own', 'ctor', 'literal']]);
    t.type('');
    t.sortBy('constructor');
    assert.deepEqual(t.cells(), [['own', 'ctor', 'literal'], ['', '', '']]);
    t.sortBy('constructor');
    assert.deepEqual(t.cells(), [['own', 'ctor', 'literal'], ['', '', '']]);
  }

  /* Numeric-looking strings and booleans never gain numeric ordering. */
  {
    const t = renderTable(columns, [{name: 'string', size: '1'},
      {name: 'false', size: false}, {name: 'large', size: 20},
      {name: 'small', size: -2}, {name: 'missing'}, {name: 'null', size: null}]);
    t.sortBy('Size');
    assert.deepEqual(t.cells().map(r => r[0]), ['small', 'large', 'string', 'false', 'missing', 'null']);
    t.sortBy('Size');
    assert.deepEqual(t.cells().map(r => r[0]), ['large', 'small', 'string', 'false', 'missing', 'null']);
  }

  /* Native buttons own click activation, and column semantics remain on th. */
  {
    const t = renderTable(columns, [{name: 'b', size: 2}, {name: 'a', size: 1}]);
    const [name, size] = t.headRow.children;
    for (const th of [name, size]) {
      assert.equal(th.scope || th.getAttribute('scope'), 'col');
      assert.equal(th.getAttribute('aria-sort'), 'none');
      assert.equal(th.getAttribute('role'), null);
      assert.notEqual(th.tabIndex, 0, 'th itself must not be an extra tab stop');
      const button = th.querySelector('button');
      assert.ok(button, 'missing native sort button');
      assert.equal(button.type || button.getAttribute('type'), 'button');
      assert.equal((button.listeners.keydown || []).length, 0, 'native button double-activates on keys');
      assert.equal(th.querySelector('.arrow').getAttribute('aria-hidden'), 'true');
    }
    t.sortBy('Size');
    assert.equal(size.getAttribute('aria-sort'), 'ascending');
    t.sortBy('Size');
    assert.equal(size.getAttribute('aria-sort'), 'descending');
    t.sortBy('Name');
    assert.equal(name.getAttribute('aria-sort'), 'ascending');
    assert.equal(size.getAttribute('aria-sort'), 'none');
    const html = fs.readFileSync(templateOf('artifact-data-table'), 'utf8');
    assert.match(html, /<span\b[^>]*id="dt-count"[^>]*aria-live="polite"/);
  }

  /* Filtering is trimmed/case-insensitive text-only; context survives print. */
  {
    const hostile = '<img src=x onerror=alert(1)>';
    const t = renderTable(columns, [{name: hostile, size: 42}, {name: 'Apple', size: 7}]);
    assert.ok(!/\bfiltered\b/.test(t.filterContext.className), 'scope line shown before any filter');
    t.type('  APP  ');
    assert.deepEqual(t.cells(), [['Apple', '7']]);
    assert.match(t.filterContext.className, /\bfiltered\b/);
    assert.match(t.filterContext.textContent, /Filtered view:.*app/i);
    assert.match(t.filterContext.textContent, /printed/);
    t.type(hostile);
    assert.deepEqual(t.cells(), [[hostile, '42']]);
    assert.equal(t.table.querySelectorAll('img').length, 0);
    assert.equal(t.filterContext.querySelectorAll('img').length, 0);
    t.type('42');
    assert.deepEqual(t.cells(), [['No rows match.']]);
    assert.equal(t.count.textContent, '0 of 2 rows');
    t.type('');
    assert.match(t.filterContext.textContent, /all.*rows/i);
    assert.ok(!/\bfiltered\b/.test(t.filterContext.className), 'scope line stayed on after clearing');
  }

  console.log('data-table renderer: all assertions passed');
}
