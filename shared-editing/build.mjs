import { build } from 'esbuild';
import { copyFile, readFile, writeFile, readdir } from 'node:fs/promises';
import { resolve } from 'node:path';
const root = new URL('./', import.meta.url).pathname;
const host = await build({
  entryPoints: [root + 'host-entry.mjs'],
  outfile: root + 'host.bundle.mjs',
  bundle: true,
  platform: 'node',
  format: 'esm',
  target: 'node22',
  supported: { 'template-literal': false },
  minify: true,
  legalComments: 'external',
  metafile: true,
});
await copyFile(root + 'node_modules/@resvg/resvg-wasm/index_bg.wasm', root + 'resvg.wasm');
const browser = await build({
  entryPoints: [root + 'editor.mjs'],
  outfile: root + '../relay/viewer/shared-editor.js',
  bundle: true,
  loader: { '.woff2': 'base64' },
  platform: 'browser',
  format: 'iife',
  target: 'es2022',
  supported: { 'template-literal': false },
  minify: true,
  legalComments: 'external',
  metafile: true,
});
const qr = await build({
  entryPoints: [root + 'qr.mjs'],
  outfile: root + '../relay/viewer/qrcode.js',
  bundle: true,
  platform: 'browser',
  format: 'iife',
  target: 'es2022',
  minify: true,
  legalComments: 'external',
  metafile: true,
});
for (const [bundle, file, title] of [
  [host, root + 'THIRD-PARTY-NOTICES.txt', 'the bundled shared editor'],
  [browser, root + '../relay/viewer/shared-editor.LICENSE.txt', 'the bundled shared editor'],
  [qr, root + '../relay/viewer/qrcode.LICENSE.txt', 'the invite QR code'],
]) {
  const packages = new Set();
  for (const input of Object.keys(bundle.metafile.inputs)) {
    const path = resolve(input),
      at = path.lastIndexOf('/node_modules/');
    if (at < 0) continue;
    const parts = path.slice(at + 14).split('/');
    packages.add(
      path.slice(0, at + 14) + parts.slice(0, parts[0].startsWith('@') ? 2 : 1).join('/'),
    );
  }
  let notices = `Third-party licenses for ${title}.\n`;
  for (const directory of [...packages].sort()) {
    const pkg = JSON.parse(await readFile(directory + '/package.json', 'utf8'));
    const files = (await readdir(directory)).filter((name) => /^licen[sc]e(?:[.-]|$)/i.test(name));
    // The package states its MIT license in its source, without a file.
    if (pkg.name === 'qrcode-generator') {
      notices += `\n--- ${pkg.name} ${pkg.version} ---\n` + (await readFile(root + 'QRCODE-LICENSE', 'utf8'));
      continue;
    }
    if (pkg.name === '@resvg/resvg-wasm') {
      notices += `\n--- ${pkg.name} ${pkg.version} ---\nUnmodified source: https://github.com/yisibl/resvg-js/tree/v${pkg.version}\n`;
      notices += await readFile(root + 'RESVG-LICENSE', 'utf8');
      continue;
    }
    if (!files.length) throw new Error(`Missing license for ${pkg.name}`);
    notices += `\n--- ${pkg.name} ${pkg.version} ---\n`;
    for (const name of files) notices += (await readFile(directory + '/' + name, 'utf8')) + '\n';
  }
  // Ported Excalidraw code, and in the editor bundle its embedded fonts.
  if (bundle !== qr) notices += '\n--- Excalidraw ---\n' + (await readFile(root + 'EXCALIDRAW-LICENSE', 'utf8'));
  if (bundle === browser) notices += '\n--- Fonts ---\n' + (await readFile(root + 'FONT-LICENSE', 'utf8'));
  await writeFile(file, notices.trimEnd() + '\n');
}
