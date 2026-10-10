import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile, readFile, mkdir, rm, stat } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const script = fileURLToPath(new URL('./local.sh', import.meta.url));
test('local runner rejects prompt without starting a host', () => {
  const result = spawnSync('bash', [script, '.', 'prompt'], { encoding: 'utf8' });
  assert.equal(result.status, 2);
  assert.match(result.stderr, /prompt needs a host that drains prompts/);
});

test('local runner reaps its host and relay before removing the workspace after failure', async t => {
  const root = await mkdtemp(join(tmpdir(), 'mevedel-bench-check-'));
  t.after(async () => {
    for (const name of ['host', 'relay']) {
      try { process.kill(Number(await readFile(join(root, `${name}.pid`), 'utf8')), 'SIGKILL'); }
      catch (error) { if (!['ENOENT', 'ESRCH'].includes(error.code)) throw error; }
    }
    await rm(root, { recursive: true, force: true });
  });
  const bin = join(root, 'bin');
  await mkdir(bin);
  const commands = {
    go: `#!/bin/sh
cat > "$3" <<'RELAY'
#!/bin/sh
echo $$ > "$TEST_STATE/relay.pid"
exec sleep 300
RELAY
chmod +x "$3"
`,
    curl: '#!/bin/sh\nexit 0\n',
    emacs: `#!/bin/sh
echo $$ > "$TEST_STATE/host.pid"
echo "$MEVEDEL_BENCH_ROOT" > "$TEST_STATE/workspace"
printf '%s' '{"full":"http://localhost/#room.secret"}' > "$MEVEDEL_BENCH_ROOT/links.json"
exec sleep 300
`,
    node: '#!/bin/sh\nexit 19\n',
  };
  for (const [name, text] of Object.entries(commands))
    await writeFile(join(bin, name), text, { mode: 0o755 });
  const result = spawnSync('bash', [script, root, 'edit'], {
    encoding: 'utf8', timeout: 10000,
    env: { ...process.env, PATH: `${bin}:${process.env.PATH}`, TEST_STATE: root },
  });
  assert.equal(result.status, 19, result.stderr);
  for (const name of ['host', 'relay']) {
    const pid = Number(await readFile(join(root, `${name}.pid`), 'utf8'));
    assert.throws(() => process.kill(pid, 0), { code: 'ESRCH' });
  }
  const workspace = (await readFile(join(root, 'workspace'), 'utf8')).trim();
  await assert.rejects(stat(workspace), { code: 'ENOENT' });
});
