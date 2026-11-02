import test from 'node:test';
import assert from 'node:assert/strict';
import { PassThrough } from 'node:stream';
import { main } from '../src/cli.mjs';
import { callTool, tools } from '../src/tools.mjs';
import { openRoom } from '../src/room.mjs';
import { serve } from '../src/mcp.mjs';
import { fixture, startFakeRoom } from './fake-room.mjs';
import { sink, tempEnv } from './helpers.mjs';

async function room(t, options) {
  const fake = await startFakeRoom(options);
  t.after(() => fake.close());
  return fake;
}

async function attach(env, link, ...flags) {
  const err = sink();
  assert.equal(await main(['attach', link, ...flags], { env, stdout: sink(), stderr: err }), 0, err.text);
}

test('the registry offers list_artifacts and read_artifact with input schemas', () => {
  const names = tools.map((tool) => tool.name);
  assert.deepEqual(names.slice(0, 2), ['list_artifacts', 'read_artifact']);
  for (const tool of tools) {
    assert.equal(tool.inputSchema.type, 'object');
    assert.ok(tool.description.length > 20);
  }
});

test('list_artifacts shows name, title, kind, origin, tier and whether the origin is reachable', async (t) => {
  const env = tempEnv(t);
  const boardBindings = {};
  const board = await room(t, { bindings: boardBindings });
  const notes = await room(t, { bindings: { $ID: 'notes', $TITLE: 'Meeting notes', $ORIGIN: 'minutes on laptop' } });
  const gone = await room(t, { bindings: { $ID: 'old-board', $TITLE: 'Old board' } });
  // An origin whose welcome names no origin.
  const older = await room(t, { welcome: 'welcome-private', bindings: { $ID: 'older', $TITLE: 'Older' } });
  await attach(env, board.link('edit'));
  await attach(env, notes.link('view'));
  await attach(env, gone.link('edit'));
  await attach(env, older.link('view'));
  await gone.close();
  const result = await callTool('list_artifacts', {}, { env });
  assert.ok(!result.isError);
  assert.deepEqual(result.content[0].text.split('\n'), [
    'snt-pipeline: "SNT analysis pipeline" whiteboard from snt-app on mevedel-host, edit link, origin reachable',
    'notes: "Meeting notes" whiteboard from minutes on laptop, view link, origin reachable',
    // An unreachable origin is named as attach stored it.
    'old-board: "Old board" whiteboard from snt-app on mevedel-host, edit link, '
      + 'origin unavailable (revoked, deleted, or offline)',
    'older: "Older" whiteboard from 127.0.0.1, view link, origin reachable',
  ]);
  await older.close();
  const offline = await callTool('list_artifacts', {}, { env });
  assert.ok(offline.content[0].text.includes(
    'older: "Older" whiteboard from 127.0.0.1, view link, origin unavailable (revoked, deleted, or offline)'));
  boardBindings.$ORIGIN = undefined;
  const unnamed = await callTool('list_artifacts', {}, { env });
  assert.ok(unnamed.content[0].text.startsWith(
    'snt-pipeline: "SNT analysis pipeline" whiteboard from 127.0.0.1, edit link, origin reachable'));
});

test('list_artifacts says how to attach when nothing is attached', async (t) => {
  const result = await callTool('list_artifacts', {}, { env: tempEnv(t) });
  assert.match(result.content[0].text, /No remote artifacts attached.*attach/s);
});

test('read_artifact returns the lines the origin shows at shared://ID', async (t) => {
  const env = tempEnv(t);
  const origin = await room(t);
  await attach(env, origin.link('view'));
  const result = await callTool('read_artifact', { name: 'snt-pipeline' }, { env });
  assert.ok(!result.isError, result.content[0].text);
  assert.equal(result.content[0].text, fixture('read').bindings.$TEXT);
  // The request the origin received is the fixture's.
  const request = fixture('read').frames.find((f) => f.from === 'consumer').frame;
  assert.deepEqual(origin.received.at(-1).body,
    { ...request.body, id: fixture('read').bindings.$ID });
});

test('read_artifact with history returns the recent contributions, to any link', async (t) => {
  const env = tempEnv(t);
  const origin = await room(t);
  await attach(env, origin.link('view'));
  const result = await callTool('read_artifact', { name: 'snt-pipeline', history: true }, { env });
  assert.ok(!result.isError, result.content[0].text);
  assert.equal(result.content[0].text, fixture('history').bindings.$TEXT);
  const request = fixture('history').frames.find((f) => f.from === 'consumer').frame;
  assert.deepEqual(origin.received.at(-1).body,
    { ...request.body, id: fixture('history').bindings.$ID });
  // Without history, a read asks for the lines and nothing else.
  await callTool('read_artifact', { name: 'snt-pipeline', history: false }, { env });
  assert.deepEqual(origin.received.at(-1).body, { action: 'read', id: fixture('history').bindings.$ID });
});

test('tool errors are plain and actionable', async (t) => {
  const env = tempEnv(t);
  const missing = await callTool('read_artifact', { name: 'nope' }, { env });
  assert.ok(missing.isError);
  assert.match(missing.content[0].text, /No remote artifact named nope; list_artifacts shows the attached ones/);
  const origin = await room(t);
  await attach(env, origin.link());
  await origin.close();
  const offline = await callTool('read_artifact', { name: 'snt-pipeline' }, { env });
  assert.ok(offline.isError);
  assert.match(offline.content[0].text, /Origin unavailable \(revoked, deleted, or offline\)/);
  const bad = await callTool('read_artifact', {}, { env });
  assert.ok(bad.isError);
  assert.match(bad.content[0].text, /name is required/);
});

test('a room with no host reports the origin unavailable', async (t) => {
  const env = tempEnv(t);
  await attach(env, (await room(t)).link());
  // The relay is up but the room has no host: it closes the room at once.
  const absent = await room(t, { mode: 'absent' });
  const result = await callTool('read_artifact', { name: 'snt-pipeline' },
    { env, connect: (entry) => openRoom(absent.link(), { name: 'tester', guestId: entry.guestId }) });
  assert.ok(result.isError);
  assert.equal(result.content[0].text, 'Origin unavailable (revoked, deleted, or offline)');
});

for (const [reason, message] of [
  ['revoked', 'Origin unavailable: the host revoked this artifact link; ask them for a new one'],
  ['artifact-deleted', 'Origin unavailable: the artifact was deleted at its origin'],
  ['origin-stopped', 'Origin unavailable: the origin Emacs stopped; try again once it is back'],
]) {
  test(`an origin saying goodbye for ${reason} is reported as such`, async (t) => {
    const env = tempEnv(t);
    const origin = await room(t, { mode: 'goodbye', reason });
    await attach(env, origin.link());
    const result = await callTool('read_artifact', { name: 'snt-pipeline' }, { env });
    assert.ok(result.isError);
    assert.equal(result.content[0].text, message);
  });
}

test('list_artifacts says why an origin that said goodbye is unavailable', async (t) => {
  const env = tempEnv(t);
  await attach(env, (await room(t)).link());
  const result = await callTool('list_artifacts', {}, {
    env, connect: async () => { throw new Error('Origin unavailable: the artifact was deleted at its origin'); },
  });
  assert.match(result.content[0].text,
    /^snt-pipeline: .* origin unavailable: the artifact was deleted at its origin$/m);
});

test('the MCP server answers initialize, tools/list and tools/call over stdio', async (t) => {
  const env = tempEnv(t);
  const origin = await room(t);
  await attach(env, origin.link());
  const input = new PassThrough();
  const output = new PassThrough();
  const done = serve({ input, output, env });
  const replies = [];
  let buffered = '';
  output.on('data', (chunk) => {
    buffered += chunk;
    let newline;
    while ((newline = buffered.indexOf('\n')) >= 0) {
      replies.push(JSON.parse(buffered.slice(0, newline)));
      buffered = buffered.slice(newline + 1);
    }
  });
  const reply = async (id) => {
    for (let i = 0; i < 200; i++) {
      const found = replies.find((r) => r.id === id);
      if (found) return found;
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    throw new Error(`no reply ${id}`);
  };
  const send = (message) => input.write(`${JSON.stringify({ jsonrpc: '2.0', ...message })}\n`);
  send({ id: 1, method: 'initialize', params: { protocolVersion: '2025-06-18', capabilities: {},
    clientInfo: { name: 'claude-code', version: '2' } } });
  const init = await reply(1);
  assert.equal(init.result.protocolVersion, '2025-06-18');
  assert.ok(init.result.capabilities.tools);
  assert.equal(init.result.serverInfo.name, 'mevedel-remote-artifact');
  send({ method: 'notifications/initialized' });
  send({ id: 2, method: 'tools/list' });
  assert.deepEqual((await reply(2)).result.tools.map((tool) => tool.name).slice(0, 2),
    ['list_artifacts', 'read_artifact']);
  send({ id: 3, method: 'tools/call', params: { name: 'read_artifact', arguments: { name: 'snt-pipeline' } } });
  assert.equal((await reply(3)).result.content[0].text, fixture('read').bindings.$TEXT);
  send({ id: 4, method: 'tools/call', params: { name: 'attach', arguments: {} } });
  assert.equal((await reply(4)).error.code, -32602);
  send({ id: 5, method: 'resources/list' });
  assert.equal((await reply(5)).error.code, -32601);
  send({ id: 6, method: 'ping' });
  assert.deepEqual((await reply(6)).result, {});
  // The hello carried a default name from user, machine and client.
  assert.match(origin.received.at(-2).name, /^tester@.+ \(claude-code\)$/);
  input.end();
  await done;
});
