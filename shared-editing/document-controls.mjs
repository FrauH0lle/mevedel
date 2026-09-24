/* Document menus and contextual controls use the editor's current selection. */
import {imageTools} from './image-controls.mjs';
import {validateDocument} from './document.mjs';

export function documentControls(editor, undo, readOnly, report) {
  const $ = id => document.getElementById(id), controls = [];
  const closeMenus = () => document.querySelectorAll('.document-menu[open]').forEach(el => { el.open = false; });
  const run = command => {
    if (readOnly()) return;
    closeMenus();
    undo.stopCapturing();
    command(editor.chain().focus()).run();
    undo.stopCapturing();
  };
  const menu = (parent, label) => {
    const details = document.createElement('details'), summary = document.createElement('summary');
    details.className = 'document-menu'; details.name = 'document-menu';
    summary.textContent = label;
    const body = document.createElement('div'); body.className = 'editor-menu';
    details.append(summary, body); parent.append(details);
    summary.addEventListener('click', event => {
      event.preventDefault();
      details.open = !details.open;
      if (!details.open) return;
      body.style.marginLeft = '0px';
      const bounds = body.getBoundingClientRect();
      body.style.marginLeft = `${Math.max(8 - bounds.left, Math.min(0, innerWidth - 8 - bounds.right))}px`;
    });
    details.addEventListener('keydown', event => {
      if (event.key === 'Escape') {
        event.stopPropagation(); details.open = false; summary.focus();
      }
    });
    return body;
  };
  const button = (parent, label, click) => {
    const el = document.createElement('button'); el.type = 'button'; el.textContent = label;
    el.onclick = click; parent.append(el); return el;
  };
  const action = (parent, label, command, {active, enabled, icon, danger} = {}) => {
    const el = button(parent, icon || label, () => run(command));
    el.title = label; el.setAttribute('aria-label', label);
    if (active) el.dataset.format = active;
    if (danger) el.classList.add('danger');
    controls.push({el, command, active, enabled});
    return el;
  };
  const formatting = $('formatting'), styles = document.createElement('select');
  styles.setAttribute('aria-label', 'Paragraph style');
  for (const [label, level] of [['Body',0],['Title · H1',1],['Heading · H2',2],['Subheading · H3',3],['Heading 4',4],['Heading 5',5],['Heading 6',6]])
    styles.add(new Option(label, level));
  styles.onchange = () => run(c => +styles.value ? c.setHeading({level:+styles.value}) : c.setParagraph());
  formatting.append(styles);
  action(formatting, 'Bold', c => c.toggleBold(), {active:'bold', icon:'B'});
  action(formatting, 'Italic', c => c.toggleItalic(), {active:'italic', icon:'I'});
  action(formatting, 'Underline', c => c.toggleUnderline(), {active:'underline', icon:'U'});
  const text = menu(formatting, 'Text');
  action(text, 'Strikethrough', c => c.toggleStrike(), {active:'strike'});
  action(text, 'Inline code', c => c.toggleCode(), {active:'code'});
  action(text, 'Block quote', c => c.toggleBlockquote(), {active:'blockquote'});
  action(text, 'Code block', c => c.toggleCodeBlock(), {active:'codeBlock'});
  action(text, 'Clear formatting', c => c.unsetAllMarks().clearNodes()).classList.add('menu-divider');
  const lists = menu(formatting, 'Lists');
  action(lists, 'Bullet list', c => c.toggleBulletList(), {active:'bulletList'});
  action(lists, 'Numbered list', c => c.toggleOrderedList(), {active:'orderedList'});
  action(lists, 'Indent list item', c => c.sinkListItem('listItem')).classList.add('menu-divider');
  action(lists, 'Outdent list item', c => c.liftListItem('listItem'));
  const insert = menu(formatting, 'Insert');
  button(insert, 'Link…', () => { closeMenus(); editLink(); });
  button(insert, 'Image…', () => { closeMenus(); $('image-upload').click(); });
  action(insert, 'Insert table', c => c.insertTable({rows:3, cols:3, withHeaderRow:true}), {enabled:() => !editor.isActive('table')});
  action(insert, 'Horizontal divider', c => c.setHorizontalRule());
  action(insert, 'Line break', c => c.setHardBreak());

  const table = $('table-tools');
  for (const [label, commands] of [
    ['Rows', [['Insert row above','addRowBefore'],['Insert row below','addRowAfter'],['Toggle header row','toggleHeaderRow'],['Delete row','deleteRow']]],
    ['Columns', [['Insert column left','addColumnBefore'],['Insert column right','addColumnAfter'],['Toggle header column','toggleHeaderColumn'],['Delete column','deleteColumn']]],
    ['Cells', [['Merge cells','mergeCells'],['Split cell','splitCell'],['Toggle header cell','toggleHeaderCell']]],
  ]) {
    const body = menu(table, label);
    for (const [name, command] of commands) action(body, name, c => c[command](), {danger:command.startsWith('delete')});
    if (label === 'Cells') {
      const help = document.createElement('p'); help.textContent = 'Drag across cells to select more than one.'; body.append(help);
    }
  }
  action(table, 'Delete table', c => c.deleteTable(), {danger:true});

  function editLink() {
    if (readOnly()) return;
    $('link-url').value = editor.getAttributes('link').href || '';
    $('link-url').setCustomValidity('');
    $('link-remove').hidden = !editor.isActive('link');
    $('link-dialog-title').textContent = editor.isActive('link') ? 'Edit link' : 'Insert link';
    $('link-dialog').showModal(); $('link-url').focus();
  }
  $('link-url').oninput = () => $('link-url').setCustomValidity('');
  $('link-form').onsubmit = event => {
    if (event.submitter?.value !== 'save') return;
    const url = $('link-url').value.trim();
    if (!/^(https?:\/\/|mailto:)/i.test(url)) {
      event.preventDefault(); $('link-url').setCustomValidity('Use an https://, http://, or mailto: address.'); $('link-url').reportValidity(); return;
    }
    run(c => {
      if (editor.isActive('link')) return c.extendMarkRange('link').setLink({href:url});
      return editor.state.selection.empty
        ? c.insertContent({type:'text', text:url, marks:[{type:'link',attrs:{href:url}}]})
        : c.setLink({href:url});
    });
  };
  $('link-remove').onclick = () => {
    run(c => c.extendMarkRange('link').unsetLink()); $('link-dialog').close();
  };
  button($('link-tools'), 'Edit link', editLink);
  action($('link-tools'), 'Remove link', c => c.extendMarkRange('link').unsetLink());

  let imageId, ratio = 1;
  function editImage() {
    if (readOnly() || !editor.isActive('image')) return;
    const attrs = editor.getAttributes('image'); imageId = attrs.id;
    const image = editor.view.nodeDOM(editor.state.selection.from)?.querySelector('img');
    $('image-alt').value = attrs.alt || '';
    $('image-title').value = attrs.title || '';
    $('image-width').value = Math.round(attrs.width || image?.naturalWidth || 640);
    $('image-height').value = Math.round(attrs.height || image?.naturalHeight || 480);
    ratio = +$('image-width').value / +$('image-height').value;
    $('image-proportions').checked = true;
    $('image-error').textContent = '';
    $('image-dialog').showModal(); $('image-alt').focus();
  }
  for (const dimension of ['width','height']) $('image-'+dimension).oninput = () => {
    const value = +$('image-'+dimension).value;
    if (!$('image-proportions').checked || value < 1) return;
    $('image-'+(dimension === 'width' ? 'height' : 'width')).value = Math.max(1, Math.round(dimension === 'width' ? value / ratio : value * ratio));
  };
  $('image-form').onsubmit = event => {
    if (event.submitter?.value !== 'save') return;
    let position;
    editor.state.doc.descendants((node, pos) => { if (node.type.name === 'image' && node.attrs.id === imageId) position = pos; });
    if (position === undefined) {
      event.preventDefault(); $('image-error').textContent = 'This image was removed while its properties were open.'; return;
    }
    run(c => c.setNodeSelection(position).updateAttributes('image', {
      alt:$('image-alt').value, title:$('image-title').value,
      width:+$('image-width').value, height:+$('image-height').value,
    }));
  };
  button($('image-tools'), 'Image properties', editImage);
  imageTools($('image-tools'), () => !readOnly() && editor.isActive('image')
    ? structuredClone(editor.getAttributes('image')) : null, (before, changes) => {
    let position, current;
    editor.state.doc.descendants((node,pos) => {
      if (node.type.name === 'image' && node.attrs.id === before.id) {position = pos;current = node;}
    });
    if (readOnly() || !current || ['src','imageEdit','width','height'].some(key =>
      JSON.stringify(current.attrs[key]) !== JSON.stringify(before[key])))
      throw new Error('This image changed while you were editing it. Reopen the image tools to try again.');
    const replace = node => node.type === 'image' && node.attrs.id === before.id
      ? {...node,attrs:{...node.attrs,...changes}}
      : {...node,...(node.content ? {content:node.content.map(replace)} : {})};
    validateDocument(replace(editor.getJSON()));
    run(c => c.setNodeSelection(position).updateAttributes('image',changes));
  },report);
  action($('image-tools'), 'Delete image', c => c.deleteSelection(), {danger:true});
  for (const id of ['link-dialog','image-dialog']) {
    $(id).addEventListener('keydown', event => { if (event.key === 'Escape') event.stopPropagation(); });
    $(id).addEventListener('close', () => editor.commands.focus());
  }

  const update = () => {
    const locked = readOnly();
    formatting.hidden = locked;
    styles.value = editor.isActive('heading') ? editor.getAttributes('heading').level : 0;
    table.hidden = locked || !editor.isActive('table');
    $('link-tools').hidden = locked || !editor.isActive('link');
    $('image-tools').hidden = locked || !editor.isActive('image');
    $('link-address').textContent = editor.getAttributes('link').href || '';
    const attrs = editor.getAttributes('image');
    $('image-size').textContent = attrs.width && attrs.height ? `${Math.round(attrs.width)} × ${Math.round(attrs.height)} px` : '';
    for (const {el, command, active, enabled} of controls) {
      if (active) el.setAttribute('aria-pressed', String(editor.isActive(active)));
      el.disabled = locked || (enabled ? !enabled() : !command(editor.can().chain()).run());
    }
    for (const bar of [formatting, table]) if (bar.hidden) bar.querySelectorAll('details').forEach(el => { el.open = false; });
  };
  editor.on('transaction', update); editor.on('update', update); update();
  document.addEventListener('pointerdown', event => {
    if (!event.target.closest('.document-menu')) closeMenus();
  });
}
