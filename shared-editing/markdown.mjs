/* Browser Markdown: library parsing with sanitization before DOM insertion. */
import {Marked, Renderer} from 'marked';
import DOMPurify from 'dompurify';

const parser = new Marked({
  gfm: true,
  breaks: true,
  async: false,
  renderer: {
    // Authored HTML stays literal, as in the transcript's other text surfaces.
    html({text}) {
      return text.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
    },
    table(token) {
      return '<div class="tablewrap">'
        + Renderer.prototype.table.call(this, token).replace('<table>', '<table class="mdtable">')
        + '</div>';
    },
  },
});

window.mevedelMarkdown = Object.freeze({
  render(text) {
    if (!DOMPurify.isSupported) throw new Error('Markdown sanitization is not supported by this browser');
    return DOMPurify.sanitize(parser.parse(String(text || '')), {
      ALLOWED_TAGS: [
        'a', 'p', 'br', 'hr', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
        'blockquote', 'pre', 'code', 'em', 'strong', 'del', 'ul', 'ol', 'li',
        'table', 'thead', 'tbody', 'tr', 'th', 'td', 'div', 'img', 'input',
      ],
      ALLOWED_ATTR: [
        'href', 'title', 'src', 'alt', 'class', 'align', 'start',
        'type', 'checked', 'disabled',
      ],
      ALLOW_DATA_ATTR: false,
      ALLOW_ARIA_ATTR: false,
      RETURN_TRUSTED_TYPE: false,
    });
  },
});
