import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';

test('direct dependencies stay exactly pinned and Tiptap schema packages stay synchronized', async () => {
  const pkg = JSON.parse(await readFile(new URL('../package.json', import.meta.url), 'utf8'));
  const schemaVersions = new Set();
  for (const [name, version] of Object.entries({...pkg.dependencies, ...pkg.devDependencies})) {
    assert.match(version, /^\d+\.\d+\.\d+$/, `${name} must use an exact release pin`);
    // The Yjs binding has its own release series, unlike the schema packages.
    if (name.startsWith('@tiptap/') && name !== '@tiptap/y-tiptap') schemaVersions.add(version);
  }
  assert.equal(schemaVersions.size, 1, 'update the Tiptap schema packages together');
});
