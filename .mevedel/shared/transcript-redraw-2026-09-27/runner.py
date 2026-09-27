"""Run one fresh child with real terminal input and owned process cleanup."""
import fcntl
import hashlib
import json
import math
import os
import pty
import select
import signal
import struct
import termios
import time
from pathlib import Path

base = Path('.scratch/transcript-redraw-2026-09-27').resolve()
label = os.environ['PROBE_LABEL']
mode = os.environ['PROBE_MODE']
trial = os.environ['PROBE_TRIAL']
root = base / 'results' / f'{label}-{mode}-{trial}'
root.mkdir()
manifest = json.loads((base / 'manifest.json').read_text())
fixture_record = next(r for r in manifest['inputs'] if r['label'] == label)
assert hashlib.sha256(Path(fixture_record['fixture']).read_bytes()).hexdigest() == fixture_record['sha256']
harness_hashes = {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                  for p in Path(__file__).parent.iterdir() if p.suffix in ('.py', '.el')}
env = os.environ.copy()
env.update(PROBE_DIRECTORY=str(root), PROBE_CAPTURE=str(base / 'fixtures' / f'{label}.org'),
           PROBE_GRAMMARS=str(base / 'grammars'), TERM='xterm-256color')
pid, fd = pty.fork()
if pid == 0:
    os.execve(env['PROBE_EMACS'], [env['PROBE_EMACS'], '-Q', '-nw', '--eval',
              env['PROBE_LOAD_PATH'], '--load',
              str(Path('.mevedel/shared/transcript-redraw-2026-09-27/driver.el').resolve())], env)
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack('HHHH', 40, 120, 0, 0))
sent, sender_lateness, terminal = [], [], bytearray()
ready = None
next_key = None
stop_at = None
reaped = False
status = None
deadline = time.monotonic() + 90
try:
    while time.monotonic() < deadline:
        now = time.time()
        if ready is None and (root / 'ready').exists():
            ready = now
            next_key = now
        if ready is not None and next_key is not None and now >= next_key:
            sender_lateness.append(1000 * (now - next_key))
            sent.append(time.time())
            os.write(fd, b'x')
            next_key = now + .01
        if stop_at is None and (root / 'finished').exists():
            stop_at = max(ready + 1.5, now + .3)
        if stop_at is not None and now >= stop_at and next_key is not None:
            next_key = None
            (root / 'sent.json').write_text(json.dumps(sent))
            os.write(fd, b'z')  # Ordinary key: C-g may quit instead of dispatching.
        if select.select([fd], [], [], .001)[0]:
            try:
                terminal.extend(os.read(fd, 65536))
                terminal = terminal[-32000:]
            except OSError:
                break
        done, status = os.waitpid(pid, os.WNOHANG)
        if done:
            reaped = True
            break
    for _ in range(500):
        if reaped:
            break
        done, status = os.waitpid(pid, os.WNOHANG)
        reaped = bool(done)
        if not reaped:
            time.sleep(.01)
    assert reaped and os.WIFEXITED(status) and os.WEXITSTATUS(status) == 0, status
    assert (root / 'result.json').exists(), f'Missing result: {root}'
    result = json.loads((root / 'result.json').read_text())
    assert not result.get('error'), result
    assert len(sent) == len(result['keys']) > 0
    expected = '> draft\nsecond line\n' + 'x' * len(sent)
    assert result['draft'] == expected
    assert result['point_at_end'] and result['history_equal'] and result['source_unchanged']
    assert result['source_map_equal']
    assert result['fixture_sha256'] == fixture_record['sha256']
    delays = [1000 * (received - written) for received, written in zip(result['keys'], sent)]
    during = [d for d, stamp in zip(delays, sent)
              if result['started'] <= stamp <= result['finished']]
    overlapping = [d for d, stamp, receipt in zip(delays, sent, result['keys'])
                   if stamp <= result['finished'] and receipt >= result['started']]
    assert during and overlapping, 'No input samples overlapping work'
    def stats(values):
        ordered = sorted(values)
        return dict(n=len(values), p50=ordered[math.ceil(len(values)*.5)-1],
                    p95=ordered[math.ceil(len(values)*.95)-1], maximum=max(values),
                    over50=sum(v > 50 for v in values), over100=sum(v > 100 for v in values)) if values else None
    result.update(sent=sent, sender_lateness_ms=sender_lateness,
                  sender_lateness=stats(sender_lateness), input_ms=delays, input_all=stats(delays),
                  input_during_work=stats(during),
                  input_overlapping_work=stats(overlapping),
                  child_exit_status=os.WEXITSTATUS(status), fixture=fixture_record,
                  harness_hashes=harness_hashes,
                  completion_ms=1000*(result['finished']-result['started']))
    (root / 'result.json').write_text(json.dumps(result, indent=2))
    print(root.name, 'completion_ms', round(result['completion_ms'], 1),
          'max_input_ms', round(max(delays), 1), 'during', stats(during))
finally:
    (root / 'terminal.log').write_bytes(terminal)
    if not reaped:
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        for _ in range(100):
            done, status = os.waitpid(pid, os.WNOHANG)
            if done:
                reaped = True
                break
            time.sleep(.01)
        if not reaped:
            os.kill(pid, signal.SIGKILL)
            os.waitpid(pid, 0)
    os.close(fd)
