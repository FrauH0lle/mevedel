/* The whiteboard element library: the host's personal .excalidrawlib, a
   built-in set, and the public collection at libraries.excalidraw.com,
   fetched through the host because the editor has no network access. */
import { boardSVG } from './render.mjs';
import { parseLibrary, serializeLibrary } from './excalidraw.mjs';
import { pointBounds } from './scene.mjs';

const cylinderGroup = ['builtin-cylinder'];
const line = (id, points) => ({ id, type: 'line', x: 0, y: 0, width: 0, height: 0, points, groupIds: cylinderGroup });
/* Built-in items. The database cylinder replaces the former cylinder shape. */
export const BUILTIN = [{
  id: 'builtin-database', name: 'Database', status: 'published', created: 0,
  elements: [
    { id: 'cylinder-top', type: 'ellipse', x: 0, y: 0, width: 120, height: 32, groupIds: cylinderGroup },
    line('cylinder-left', [[0, 16], [0, 124]]),
    line('cylinder-right', [[120, 16], [120, 124]]),
    { ...line('cylinder-bottom', [[0, 124], [18, 134], [60, 140], [102, 134], [120, 124]]), roundness: { type: 2 } },
  ],
}];

/* Copies of library ELEMENTS centred on CENTER with fresh ids, seeds and
   groups; references among the copies follow, others are dropped. */
export function placeElements(elements, center, indices, seed) {
  const boxes = elements.map((e) => e.points
    ? (([x1, y1, x2, y2]) => [e.x + x1, e.y + y1, e.x + x2, e.y + y2])(pointBounds(e.points))
    : [e.x, e.y, e.x + e.width, e.y + e.height]);
  const left = Math.min(...boxes.map((b) => b[0])), top = Math.min(...boxes.map((b) => b[1]));
  const width = Math.max(...boxes.map((b) => b[2])) - left, height = Math.max(...boxes.map((b) => b[3])) - top;
  const fresh = new Map(elements.map((e) => [e.id, crypto.randomUUID()]));
  const groups = new Map();
  const group = (g) => (groups.has(g) || groups.set(g, crypto.randomUUID().replace(/-/g, '')), groups.get(g));
  return elements.map((e, i) => {
    const copy = { ...e, id: fresh.get(e.id), seed: seed(), index: indices[i],
      x: e.x - left + center[0] - width / 2, y: e.y - top + center[1] - height / 2 };
    if (copy.groupIds) copy.groupIds = copy.groupIds.map(group);
    if (copy.containerId) copy.containerId = fresh.get(copy.containerId) ?? null;
    if (copy.containerId === null) delete copy.containerId;
    if (copy.frameId) copy.frameId = fresh.get(copy.frameId) ?? null;
    for (const key of ['startBinding', 'endBinding']) {
      if (!copy[key]) continue;
      if (fresh.has(copy[key].elementId)) copy[key] = { ...copy[key], elementId: fresh.get(copy[key].elementId) };
      else delete copy[key];
    }
    return copy;
  });
}

const node = (tag, props = {}, ...children) => {
  const el = Object.assign(document.createElement(tag), props);
  el.append(...children);
  return el;
};
/* An isolated preview image, so preview ids never meet the board's. */
function thumbnail(item) {
  const svg = boardSVG(item.elements, { maxEdge: 96, maxScale: 2 });
  return node('img', { src: `data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`, alt: '' });
}

/* The library popover in PARENT. REQUEST reaches the host; SELECTION
   returns the selected elements; INSERT places an item's elements. */
export function libraryPanel({ parent, request, report, selection, insert, download }) {
  const menu = node('details', { className: 'popover board-menu library', id: 'library-menu' });
  const body = node('div', { className: 'editor-menu library-body' });
  menu.append(node('summary', { textContent: 'Library' }), body);
  parent.append(menu);
  let items = [], catalog = null, loaded = false;
  const run = async (task) => {
    try { await task(); } catch (error) { report(error.message); }
  };
  const save = (action, args) => run(async () => {
    items = parseLibrary((await request({ action, ...args })).text);
    render();
  });
  const tile = (item, actions) => {
    const card = node('div', { className: 'library-item' });
    const place = node('button', { type: 'button', className: 'library-insert', title: `Insert ${item.name || 'item'}` },
      thumbnail(item), node('span', { textContent: item.name || 'Untitled' }));
    place.setAttribute('aria-label', `Insert ${item.name || 'library item'}`);
    place.onclick = () => { menu.open = false; insert(item.elements); };
    card.append(place, ...actions);
    return card;
  };
  const grid = (list, actions = () => []) => node('div', { className: 'library-grid' }, ...list.map((item) => tile(item, actions(item))));
  function render() {
    body.replaceChildren();
    if (catalog) return renderCatalog();
    const file = node('input', { type: 'file', accept: '.excalidrawlib,application/json', hidden: true });
    file.onchange = () => run(async () => {
      const [chosen] = file.files;
      file.value = '';
      if (!chosen) return;
      const added = parseLibrary(await chosen.text());
      await save('library-add', { text: serializeLibrary(added) });
    });
    const add = node('button', { type: 'button', textContent: 'Add selection' });
    add.onclick = () => {
      const elements = selection();
      if (!elements.length) return report('Select objects to add them to the library');
      save('library-add', { text: serializeLibrary([{ id: crypto.randomUUID(), status: 'unpublished',
        created: Date.now(), elements }]) });
    };
    const browse = node('button', { type: 'button', textContent: 'Browse public libraries' });
    browse.onclick = () => run(async () => {
      catalog = { entries: (await request({ action: 'library-catalog' })).libraries, filter: '', open: null };
      render();
    });
    const upload = node('button', { type: 'button', textContent: 'Import file…' });
    upload.onclick = () => file.click();
    const save_ = node('button', { type: 'button', textContent: 'Download', disabled: !items.length });
    save_.onclick = () => download(serializeLibrary(items));
    body.append(node('div', { className: 'library-actions' }, add, upload, browse, save_, file));
    body.append(node('h4', { textContent: 'My library' }));
    body.append(items.length ? grid(items, (item) => {
      const remove = node('button', { type: 'button', className: 'library-remove', textContent: '×' });
      remove.setAttribute('aria-label', `Remove ${item.name || 'item'} from the library`);
      remove.onclick = () => save('library-remove', { ids: [item.id] });
      return [remove];
    }) : node('p', { className: 'hint', textContent: 'Select objects and choose Add selection, or import an .excalidrawlib file.' }));
    body.append(node('h4', { textContent: 'Built-in' }), grid(BUILTIN));
  }
  function renderCatalog() {
    const back = node('button', { type: 'button', textContent: catalog.open ? '← Libraries' : '← My library' });
    back.onclick = () => { if (catalog.open) catalog.open = null; else catalog = null; render(); };
    body.append(node('div', { className: 'library-actions' }, back));
    if (catalog.open) {
      const { entry, items: found } = catalog.open;
      const all = node('button', { type: 'button', textContent: 'Add all to my library' });
      all.onclick = () => save('library-add', { text: serializeLibrary(found) });
      body.append(node('h4', { textContent: entry.name }), node('p', { className: 'hint', textContent: entry.description || '' }),
        node('div', { className: 'library-actions' }, all), grid(found));
      return;
    }
    const search = node('input', { type: 'search', placeholder: 'Search libraries', value: catalog.filter });
    search.setAttribute('aria-label', 'Search public libraries');
    const list = node('div', { className: 'library-catalog' });
    const show = () => {
      const needle = catalog.filter.toLowerCase();
      list.replaceChildren(...catalog.entries
        .filter((e) => `${e.name} ${e.description} ${e.authors}`.toLowerCase().includes(needle)).slice(0, 200)
        .map((entry) => {
          const open = node('button', { type: 'button', className: 'library-entry' },
            node('strong', { textContent: entry.name }), node('small', { textContent: entry.authors }),
            node('span', { textContent: entry.description || '' }));
          open.onclick = () => run(async () => {
            const { text } = await request({ action: 'library-fetch', source: entry.source });
            catalog.open = { entry, items: parseLibrary(text) };
            render();
          });
          return open;
        }));
    };
    search.oninput = () => { catalog.filter = search.value; show(); };
    body.append(search, node('p', { className: 'hint', textContent: 'From libraries.excalidraw.com, fetched by the session host.' }), list);
    show();
  }
  menu.addEventListener('toggle', () => {
    if (!menu.open || loaded) return;
    loaded = true;
    run(async () => {
      items = parseLibrary((await request({ action: 'library' })).text);
      render();
    });
  });
  render();
  return menu;
}
