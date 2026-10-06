/* The whiteboard library panel. Libraries live on the session host: the
   personal library, installed libraries and the built-in one; the public
   collection at libraries.excalidraw.com is fetched through the host because
   the editor has no network access. */
import { boardSVG } from './render.mjs';
import { parseLibrary, serializeLibrary } from './excalidraw.mjs';

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
const SORTS = {
  downloads: ['Most downloaded', (a, b) => b.downloads - a.downloads],
  updated: ['Recently updated', (a, b) => (b.updated || b.created).localeCompare(a.updated || a.created)],
  created: ['Newest', (a, b) => b.created.localeCompare(a.created)],
  name: ['Name', (a, b) => a.name.localeCompare(b.name)],
};

/* The library popover in PARENT. REQUEST reaches the host; SELECTION
   returns the selected elements; INSERT places an item's elements. */
export function libraryPanel({ parent, request, report, selection, insert, download }) {
  const menu = node('details', { className: 'popover board-menu library', id: 'library-menu' });
  const body = node('div', { className: 'editor-menu library-body' });
  menu.append(node('summary', { textContent: 'Library' }), body);
  parent.append(menu);
  let libraries = [], catalog = null, loaded = false, query = '';
  /* Which library sections the participant opened or closed; My library starts open. */
  const expanded = new Map();
  const run = async (task) => {
    try { await task(); } catch (error) { report(error.message); }
  };
  /* Apply a host reply listing every library. */
  const listing = async (args) => {
    libraries = (await request(args)).libraries.map((library) => {
      try { return { ...library, items: parseLibrary(library.text) }; }
      catch (error) { return { ...library, items: [], error: error.message }; }
    });
    render();
  };
  const tile = (item, actions = []) => {
    const card = node('div', { className: 'library-item' });
    const place = node('button', { type: 'button', className: 'library-insert', title: `Insert ${item.name || 'item'}` },
      thumbnail(item), node('span', { textContent: item.name || 'Untitled' }));
    place.setAttribute('aria-label', `Insert ${item.name || 'library item'}`);
    place.onclick = () => { menu.open = false; insert(item.elements); };
    card.append(place, ...actions);
    return card;
  };
  const grid = (items, actions) => node('div', { className: 'library-grid' }, ...items.map((item) => tile(item, actions?.(item))));
  function render() {
    body.replaceChildren();
    if (catalog) return renderCatalog();
    const file = node('input', { type: 'file', accept: '.excalidrawlib,application/json', hidden: true });
    file.onchange = () => run(async () => {
      const [chosen] = file.files;
      file.value = '';
      if (chosen) await listing({ action: 'library-add', text: serializeLibrary(parseLibrary(await chosen.text())) });
    });
    const add = node('button', { type: 'button', textContent: 'Add selection' });
    add.onclick = () => {
      const elements = selection();
      if (!elements.length) return report('Select objects to add them to the library');
      run(() => listing({ action: 'library-add', text: serializeLibrary([{ id: crypto.randomUUID(),
        status: 'unpublished', created: Date.now(), elements }]) }));
    };
    const browse = node('button', { type: 'button', textContent: 'Browse public libraries' });
    browse.onclick = () => run(async () => {
      catalog = { entries: (await request({ action: 'library-catalog' })).libraries, filter: '', sort: 'downloads', open: null };
      render();
    });
    const upload = node('button', { type: 'button', textContent: 'Import file…' });
    upload.onclick = () => file.click();
    const personal = libraries.find((library) => library.kind === 'personal');
    const save = node('button', { type: 'button', textContent: 'Download', disabled: !personal?.items.length });
    save.onclick = () => download(serializeLibrary(personal.items));
    const search = node('input', { type: 'search', placeholder: 'Search items', value: query });
    search.setAttribute('aria-label', 'Search library items');
    const sections = node('div', { className: 'library-sections' });
    search.oninput = () => { query = search.value; renderSections(sections); };
    body.append(node('div', { className: 'library-actions' }, add, upload, browse, save, file), search, sections);
    renderSections(sections);
  }
  /* One collapsible section per library. A search opens every library with a
     matching item, or whose name matches, without changing what stays open. */
  function renderSections(sections) {
    const needle = query.trim().toLowerCase();
    sections.replaceChildren();
    const personal = libraries.find((library) => library.kind === 'personal');
    if (!needle && !personal?.items.length)
      sections.append(node('h4', { textContent: 'My library' }),
        node('p', { className: 'hint', textContent: 'Select objects and choose Add selection, or import an .excalidrawlib file.' }));
    for (const library of libraries) {
      if (!library.items.length && library.kind === 'personal') continue;
      const items = !needle || library.name.toLowerCase().includes(needle) ? library.items
        : library.items.filter((item) => (item.name || '').toLowerCase().includes(needle));
      if (needle && !items.length) continue;
      const section = node('details', { className: 'library-section',
        open: Boolean(needle) || (expanded.get(library.name) ?? library.kind === 'personal') });
      section.ontoggle = () => { if (!needle) expanded.set(library.name, section.open); };
      section.append(node('summary', {}, node('h4', { textContent: library.name }),
        node('small', { textContent: String(library.items.length) })));
      section.append(library.error ? node('p', { className: 'hint', textContent: library.error })
        : grid(items, library.kind === 'personal' ? (item) => {
          const remove = node('button', { type: 'button', className: 'library-remove', textContent: '×' });
          remove.setAttribute('aria-label', `Remove ${item.name || 'item'} from the library`);
          remove.onclick = () => run(() => listing({ action: 'library-remove', ids: [item.id] }));
          return [remove];
        } : undefined));
      // Removal sits inside the opened library, so it always names what it removes.
      if (library.kind === 'installed') {
        const uninstall = node('button', { type: 'button', textContent: `Remove ${library.name}` });
        uninstall.setAttribute('aria-label', `Remove library ${library.name}`);
        uninstall.onclick = () => run(() => listing({ action: 'library-uninstall', name: library.name }));
        section.append(node('div', { className: 'library-actions' }, uninstall));
      }
      sections.append(section);
    }
    if (needle && !sections.children.length)
      sections.append(node('p', { className: 'hint', textContent: 'No library items match.' }));
  }
  const install = (entry) => run(async () => {
    await listing({ action: 'library-install', source: entry.source, name: entry.name.replace(/[/\\:*?"<>|]/g, ' ').trim().slice(0, 100) });
    report(`Installed ${entry.name}; it is available in every whiteboard`);
  });
  const installed = (entry) => libraries.some((library) => library.name === entry.name.replace(/[/\\:*?"<>|]/g, ' ').trim().slice(0, 100));
  function renderCatalog() {
    const back = node('button', { type: 'button', textContent: catalog.open ? '← Libraries' : '← My library' });
    back.onclick = () => { if (catalog.open) catalog.open = null; else catalog = null; render(); };
    body.append(node('div', { className: 'library-actions' }, back));
    if (catalog.open) {
      const { entry, items } = catalog.open;
      const add = node('button', { type: 'button', textContent: installed(entry) ? 'Installed' : 'Install library', disabled: installed(entry) });
      add.onclick = () => install(entry);
      body.append(node('h4', { textContent: entry.name }), node('p', { className: 'hint', textContent: entry.description || '' }),
        node('div', { className: 'library-actions' }, add), grid(items));
      return;
    }
    const search = node('input', { type: 'search', placeholder: 'Search libraries', value: catalog.filter });
    search.setAttribute('aria-label', 'Search public libraries');
    const sort = node('select', {}, ...Object.entries(SORTS).map(([value, [label]]) => node('option', { value, textContent: label })));
    sort.value = catalog.sort;
    sort.setAttribute('aria-label', 'Sort public libraries');
    const list = node('div', { className: 'library-catalog' });
    const show = () => {
      const needle = catalog.filter.toLowerCase();
      list.replaceChildren(...catalog.entries
        .filter((e) => `${e.name} ${e.description} ${e.authors}`.toLowerCase().includes(needle))
        .toSorted(SORTS[catalog.sort][1]).slice(0, 200)
        .map((entry) => {
          const open = node('button', { type: 'button', className: 'library-entry' },
            node('strong', { textContent: entry.name }),
            node('small', { textContent: [entry.authors, entry.downloads ? `${entry.downloads.toLocaleString()} downloads` : '',
              (entry.updated || entry.created) && `updated ${(entry.updated || entry.created).slice(0, 10)}`].filter(Boolean).join(' · ') }),
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
    sort.onchange = () => { catalog.sort = sort.value; show(); };
    body.append(node('div', { className: 'library-search' }, search, sort),
      node('p', { className: 'hint', textContent: 'From libraries.excalidraw.com, fetched by the session host. Installed libraries are available in every whiteboard.' }), list);
    show();
  }
  menu.addEventListener('toggle', () => {
    if (!menu.open || loaded) return;
    loaded = true;
    run(() => listing({ action: 'library' }));
  });
  render();
  return menu;
}
