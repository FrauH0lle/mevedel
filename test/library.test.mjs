import test from 'node:test';
import assert from 'node:assert/strict';
import { main } from '../src/cli.mjs';
import { callTool, tools } from '../src/tools.mjs';
import { fixture, startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

/* A config with a fake origin attached at TIER as snt-pipeline. */
async function attached(t, options, tier = 'edit') {
  const origin = await startFakeRoom(options);
  t.after(() => origin.close());
  const env = tempEnv(t);
  const err = sink();
  assert.equal(await main(['attach', origin.link(tier)], { env, stdout: sink(), stderr: err }), 0, err.text);
  return { origin, env };
}

const { $ID } = fixture('welcome').bindings;

/* An origin answering library and insert as the real one does. */
function libraries(body) {
  if (body.action === 'library') {
    return body.part === 'sheet'
      ? { result: { png: Buffer.from('sheet').toString('base64'), mime: 'image/png' } }
      : { result: { text: '1. My library/cube Cube' } };
  }
  return { result: { text: JSON.stringify({ id: body.id, changed: [] }) } };
}

test('the registry offers whiteboard_libraries, and insert on edit_artifact', () => {
  const library = tools.find((tool) => tool.name === 'whiteboard_libraries');
  const edit = tools.find((tool) => tool.name === 'edit_artifact');
  assert.deepEqual(library.inputSchema.required, ['name']);
  assert.ok(edit.inputSchema.properties.action.enum.includes('insert'));
  assert.ok(edit.inputSchema.properties.item);
});

test('whiteboard_libraries lists the origin user\'s libraries, for any link', async (t) => {
  const { origin, env } = await attached(t, { onEditing: libraries }, 'view');
  const result = await callTool('whiteboard_libraries', { name: 'snt-pipeline' }, { env });
  assert.ok(!result.isError, result.content[0].text);
  assert.deepEqual(result.content, [{ type: 'text', text: '1. My library/cube Cube' }]);
  assert.deepEqual(origin.received.at(-1).body, { action: 'library', id: $ID });
});

test('whiteboard_libraries previews a library as a picture', async (t) => {
  const { origin, env } = await attached(t, { onEditing: libraries }, 'view');
  const result = await callTool('whiteboard_libraries',
    { name: 'snt-pipeline', library: 'My library', preview: true }, { env });
  assert.deepEqual(result.content, [{ type: 'image', data: Buffer.from('sheet').toString('base64'),
    mimeType: 'image/png' }]);
  assert.deepEqual(origin.received.at(-1).body,
    { action: 'library', id: $ID, library: 'My library', part: 'sheet' });
});

test('edit_artifact insert places a library item with its position', async (t) => {
  const { origin, env } = await attached(t, { onEditing: libraries });
  const result = await callTool('edit_artifact',
    { name: 'snt-pipeline', action: 'insert', item: 'My library/cube', x: 300, y: 0 }, { env });
  assert.ok(!result.isError, result.content[0].text);
  assert.deepEqual(origin.received.at(-1).body,
    { action: 'insert', id: $ID, item: 'My library/cube', x: 300, y: 0 });
});

test('insert without an item fails before contacting the origin', async (t) => {
  const { origin, env } = await attached(t, { onEditing: libraries });
  const before = origin.connections;
  const result = await callTool('edit_artifact', { name: 'snt-pipeline', action: 'insert' }, { env });
  assert.ok(result.isError);
  assert.match(result.content[0].text, /insert needs item as LIBRARY\/ITEM-ID/);
  assert.equal(origin.connections, before);
});

test('an origin refusal, such as a document room, reaches the agent as an error', async (t) => {
  const { env } = await attached(t, { onEditing: () => ({ error: 'An artifact link does not permit library' }) });
  const result = await callTool('whiteboard_libraries', { name: 'snt-pipeline' }, { env });
  assert.ok(result.isError);
  assert.match(result.content[0].text, /does not permit library/);
});
