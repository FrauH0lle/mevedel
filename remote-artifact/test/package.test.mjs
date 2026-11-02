import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';

const root = new URL('../', import.meta.url);
const pkg = JSON.parse(readFileSync(new URL('package.json', root), 'utf8'));

test('package metadata is ready to publish', () => {
  assert.equal(pkg.license, 'GPL-3.0-or-later');
  assert.equal(pkg.engines.node, '>=22.4');
  assert.deepEqual(pkg.dependencies ?? {}, {});
  assert.ok(pkg.repository?.url && pkg.homepage && pkg.bugs?.url && pkg.keywords?.length);
  assert.ok(pkg.files.includes('LICENSE'));
  assert.ok(pkg.files.includes('NOTICE.md'));
  assert.notEqual(pkg.private, true);
});

test('the package carries its licence and a notices file', () => {
  assert.match(readFileSync(new URL('LICENSE', root), 'utf8'), /GNU GENERAL PUBLIC LICENSE\s+Version 3/);
  assert.match(readFileSync(new URL('NOTICE.md', root), 'utf8'), /bundles no third-party code/i);
  for (const file of pkg.files) assert.ok(existsSync(new URL(file, root)), file);
});
