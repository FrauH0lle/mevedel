/* QR codes for the room's invite sheet, bundled into relay/viewer/qrcode.js.
   A phone joins a room by pointing its camera at the code. */
import qrcode from 'qrcode-generator';

/* An SVG drawing TEXT as a QR code: dark modules on white with the quiet
   zone scanners need, whatever the page's theme. */
function svg(text) {
  const code = qrcode(0, 'M');
  code.addData(text, 'Byte');
  code.make();
  const count = code.getModuleCount(), size = count + 8;
  let path = '';
  for (let row = 0; row < count; row++)
    for (let column = 0; column < count; column++)
      if (code.isDark(row, column)) path += `M${column + 4} ${row + 4}h1v1h-1z`;
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${size} ${size}" shape-rendering="crispEdges">`
    + `<rect width="${size}" height="${size}" fill="#fff"/><path d="${path}" fill="#000"/></svg>`;
}

window.mevedelQr = Object.freeze({ svg });
