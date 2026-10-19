/* Search the displayed resource, joining only output string chunks. Byte spans
   map ripgrep matches back to the physical lines ordinary Read delivers. */
export function searchDocument(address, displayed) {
  const lines = displayed.replace(/\n$/, '').split('\n');
  const spans = [];
  let text = '', bytes = 0, line = 1, at = 0;
  function append(value, sourceLine) {
    if (!value) return;
    const end = bytes + Buffer.byteLength(value, 'utf8');
    spans.push([bytes, end, sourceLine]);
    bytes = end;
    text += value;
  }
  function ordinary(value) {
    const pieces = value.split('\n');
    pieces.forEach((piece, index) => {
      const newline = index < pieces.length - 1;
      append(piece + (newline ? '\n' : ''), line);
      if (newline) line++;
    });
  }
  const chunks = /string-chunks\(\n((?:[ \t]*"(?:[^"\\\n]|\\.)*"\n)+)[ \t]*\)/g;
  for (const match of displayed.matchAll(chunks)) {
    ordinary(displayed.slice(at, match.index));
    line++; // The opening string-chunks line.
    const parts = match[1].trimEnd().split('\n');
    append('"', line);
    parts.forEach(part => {
      append(part.trim().slice(1, -1), line);
      line++;
    });
    append('"', line - 1);
    at = match.index + match[0].length;
  }
  ordinary(displayed.slice(at));
  return {address, text, lines, spans};
}
