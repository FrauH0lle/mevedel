#!/usr/bin/env python3
"""Measure the installed request flow in a disposable editor with a local mock."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser()
parser.add_argument('--output', required=True)
parser.add_argument('--seconds', type=float, default=8)
parser.add_argument('--screenshots', action='store_true')
parser.add_argument('--modes', nargs='+', choices=['static', 'ordinary', 'native'], default=['static', 'ordinary', 'native', 'static'])
parser.add_argument('--tool', action='store_true')
parser.add_argument('--tool-count', type=int, default=1)
parser.add_argument('--trace-native', action='store_true')
parser.add_argument('--acceptance', action='store_true')
parser.add_argument('--observe', type=float, default=0,
                    help='after sampling CPU, record ownership for this many seconds')
parser.add_argument('--stream-rate', type=float, default=0,
                    help='stream prose at this many words/s instead of holding silently')
args = parser.parse_args()
lab = Path(__file__).resolve().parent
root = lab.parents[3]
output = Path(args.output).resolve()
output.mkdir(parents=True, exist_ok=True)
results = []
with tempfile.TemporaryDirectory(prefix='mevedel-request-lab-') as name:
    temp = Path(name)
    package = temp / 'package'
    package.mkdir()
    for source in root.glob('*.el*'):
        shutil.copy(source, package / source.name)
    if not (package / 'mevedel.elc').exists():
        raise RuntimeError('Compile the worktree through Eask first')
    for directory in ('native', 'prompts', 'agents', 'scripts'):
        shutil.copytree(root / directory, package / directory)
    # Freeze the exact dependencies used by this isolated mock experiment.
    shutil.copytree(root / '.eask/31.1/elpa', temp / 'elpa')
    workspace = temp / 'workspace'
    workspace.mkdir()
    subprocess.run(['git', 'init', '-q', str(workspace)], check=True)
    (workspace / 'README.md').write_text('Animation measurement workspace.\n')
    subprocess.run(['git', '-C', str(workspace), 'add', 'README.md'], check=True)
    subprocess.run(['git', '-C', str(workspace), '-c', 'user.email=lab@local',
                    '-c', 'user.name=Renderer Lab', 'commit', '-qm', 'Initial'], check=True)
    env = dict(os.environ, HOME=str(temp), MEVEDEL_LAB_HOME=str(temp),
               MEVEDEL_LAB_ELPA=str(temp / 'elpa'), MEVEDEL_LAB_WORKSPACE=str(workspace),
               MEVEDEL_LAB_SERVER='mevedel-request-lab-' + str(os.getpid()))
    if args.trace_native:
        env['MEVEDEL_LAB_TRACE'] = str(output / 'native-invalidations.txt')
    for key in ('CACHE', 'CONFIG', 'DATA', 'STATE'):
        env['XDG_' + key + '_HOME'] = str(temp / key.lower())
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        port = sock.getsockname()[1]
    harness = (lab.parent / 'tools/cpuh-harness.el').read_text().replace('127.0.0.1:8766', f'127.0.0.1:{port}')
    (package / 'cpuh-harness.el').write_text(harness.replace(";;; Code:", ";;; Code:\n(require 'mevedel-presets)\n(require 'mevedel-telemetry)\n(require 'mevedel-turn)"))
    shutil.copy(lab / 'request.el', package / 'request.el')
    init = '(progn (require \'package) (setq package-user-dir (getenv "MEVEDEL_LAB_ELPA")) (package-initialize))'
    with (output / 'compile.log').open('w') as log:
        subprocess.run(['emacs', '-Q', '--batch', '-L', str(package), '--eval', init,
                        '-f', 'batch-byte-compile', str(package / 'request.el'),
                        str(package / 'cpuh-harness.el')], env=env, check=True, stdout=log, stderr=log)
    control = output / 'control.json'
    hold_seconds = args.seconds + (30 if args.acceptance else 8)
    control.write_text(json.dumps(dict(hold=0 if (args.tool and args.tool_count == 1) or args.stream_rate else hold_seconds,
                                      stream=dict(seconds=hold_seconds, rate=args.stream_rate) if args.stream_rate else None,
                                      tools=[dict(name='Bash', args=dict(command=f'sleep {hold_seconds}', yield_time_ms=30000)) for _ in range(args.tool_count)] if args.tool and args.tool_count == 1 else None)))
    expected_surfaces = 1 + min(args.tool_count, 5) + int(args.tool_count > 5) if args.tool else 1
    with (output / 'mock.log').open('w') as mock_log, (output / 'editor.log').open('w') as editor_log:
        mock = subprocess.Popen(['python3', '-I', str(lab.parent / 'tools/mock_server.py'),
                                 str(port), str(control), str(temp / 'body.json')], stdout=mock_log, stderr=mock_log)
        bootstrap = '(condition-case err (load ' + json.dumps(str(package / 'request.elc')) + ' nil t) (error (with-temp-file ' + json.dumps(str(output / 'startup-error.txt')) + ' (insert (error-message-string err))) (kill-emacs 3)))'
        editor = subprocess.Popen(['emacs', '-Q', '-L', str(package), '--eval', bootstrap],
                                   env=env, stdout=editor_log, stderr=editor_log)
        def call(form):
            result = subprocess.run(['emacsclient', '-s', str(temp / env['MEVEDEL_LAB_SERVER']), '--eval', form],
                                     text=True, capture_output=True, timeout=15)
            if result.returncode or result.stdout.startswith('*ERROR*'):
                raise RuntimeError((result.stdout + result.stderr)[:1500])
            return " ".join(result.stdout.split())
        def ticks(pid):
            fields = Path(f'/proc/{pid}/stat').read_text().rsplit(')', 1)[1].split()
            return int(fields[11]) + int(fields[12])
        try:
            for attempt in range(120):
                if editor.poll() is not None:
                    raise RuntimeError((output / 'editor.log').read_text()[-2000:])
                try:
                    call('t')
                    break
                except RuntimeError:
                    time.sleep(.25)
            else:
                raise RuntimeError('Editor did not start')
            time.sleep(2)
            compositor = int(subprocess.check_output(['pgrep', '-x', 'kwin_wayland']))
            for index, label in enumerate(args.modes):
                style, native = ('static' if label == 'static' else 'bounce'), label == 'native'
                start = time.monotonic()
                call(f'(request-lab-start \'{style} {"t" if native else "nil"})')
                if args.tool and args.tool_count > 1:
                    call(f'(request-lab-tools {args.tool_count})')
                first_use = time.monotonic() - start
                time.sleep(3)
                before = call('(request-lab-state)')
                if ':busy t' not in before or ':focused t' not in before:
                    raise RuntimeError('Request not visibly active: ' + before)
                # Streaming moves the label; surfaces can be settling at any instant.
                if native and not args.stream_rate and (f':native {expected_surfaces}' not in before):
                    raise RuntimeError('Native renderer did not attach: ' + before)
                call('(cpuh-start)')
                e0, k0 = ticks(editor.pid), ticks(compositor)
                start = time.monotonic()
                time.sleep(args.seconds)
                seconds = time.monotonic() - start
                e1, k1 = ticks(editor.pid), ticks(compositor)
                after = call('(request-lab-state)')
                if ':busy t' not in after or ':focused t' not in after:
                    raise RuntimeError('Request lost activity or focus during sampling: ' + after)
                call(f'(progn (cpuh-stop) (cpuh-dump {json.dumps(str(output / "timers.txt"))} {json.dumps(label)}))')
                if args.observe:
                    call(f'(request-lab-observe {args.observe} {json.dumps(str(output / (label + "-observed.txt")))})')
                    time.sleep(args.observe + 1)
                factor = 100 / os.sysconf('SC_CLK_TCK') / seconds
                row = dict(label=label, tool_count=args.tool_count if args.tool else 0, synthetic_tool_events=args.tool and args.tool_count > 1, editor_cpu=round((e1-e0)*factor, 2), compositor_cpu=round((k1-k0)*factor, 2),
                           seconds=seconds, first_use_seconds=first_use, before=before, after=after)
                results.append(row)
                (output / 'samples.json').write_text(json.dumps(results, indent=2) + '\n')
                print(json.dumps(row), flush=True)
                if args.screenshots:
                    subprocess.run(['spectacle', '-b', '-n', '-a', '-o', str(output / (str(index) + '-' + label + '.png'))],
                                    check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                if native and args.acceptance:
                    checks = []
                    for step, surfaces in [('freeze', 0), ('frozen', 0), ('thaw', expected_surfaces),
                                           ('disable-native', 0), ('enable-native', expected_surfaces),
                                           ('tool-braille', expected_surfaces), ('tool-ascii', expected_surfaces),
                                           ('tool-dots', expected_surfaces),
                                           ('tool-shimmer', expected_surfaces),
                                           ('dark', expected_surfaces), ('light', expected_surfaces),
                                           ('larger', expected_surfaces), ('split', 2 * expected_surfaces),
                                           ('hscroll', 0), ('unscroll', 2 * expected_surfaces),
                                           ('unsplit', expected_surfaces), ('typing', expected_surfaces), ('draft', expected_surfaces),
                                           ('draft-retained', expected_surfaces)]:
                        response = call(f"(request-lab-check '{step})")
                        time.sleep(.7)
                        actual = int(call('(aref (mevedel-view-native--stats) 0)'))
                        deadline = time.monotonic() + 1.5
                        while actual != surfaces and time.monotonic() < deadline:
                            time.sleep(.2)
                            actual = int(call('(aref (mevedel-view-native--stats) 0)'))
                        checks.append(dict(step=step, response=response, expected=surfaces, actual=actual))
                        (output / 'acceptance.json').write_text(json.dumps(checks, indent=2) + '\n')
                        if response != 't' or actual != surfaces:
                            raise RuntimeError(f'{step} failed: {checks[-1]}')
                        if args.screenshots and step in ('dark', 'larger', 'split'):
                            subprocess.run(['spectacle', '-b', '-n', '-a', '-o', str(output / (step + '.png'))],
                                           check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                    print('Request acceptance checks passed', flush=True)
                if args.tool and args.tool_count > 1:
                    if args.tool_count == 7:
                        overflow_checks = []
                        for count, expected_count in [(6, 7), (5, 6)]:
                            call(f'(request-lab-tools {count})')
                            time.sleep(1.2)
                            actual_count = int(call('(aref (mevedel-view-native--stats) 0)'))
                            overflow = call('(with-current-buffer (cpuh-view) (cl-some (lambda (entry) (string-prefix-p "1 more tool" (nth 1 (cadr entry)))) mevedel-view-native--entries))')
                            overflow_checks.append(dict(pending=count, native=actual_count, overflow=overflow))
                            if actual_count != expected_count or overflow != ('t' if count == 6 else 'nil'):
                                raise RuntimeError(f'Overflow update failed: {overflow_checks[-1]}')
                        (output / 'overflow.json').write_text(json.dumps(overflow_checks, indent=2) + '\n')
                    call('(request-lab-tools 0)')
                for attempt in range(60):
                    if call('(cpuh-busy-p)') == 'nil':
                        break
                    time.sleep(.5)
                else:
                    raise RuntimeError('Request did not finish')
                time.sleep(1)
                stopped = call('(request-lab-state)')
                if native and ':stats [0 ' not in stopped:
                    raise RuntimeError('Native work survived request completion: ' + stopped)
                row['stopped'] = stopped
            metadata = call('(list emacs-version (frame-pixel-width) (frame-pixel-height) (locate-library "gptel"))')
            hashes = {str(p.relative_to(package)): hashlib.sha256(p.read_bytes()).hexdigest()
                      for p in [package / 'mevedel-view-native.el', package / 'mevedel-view-stream.el', package / 'native/mevedel-view-native.c']}
            (output / 'results.json').write_text(json.dumps(dict(metadata=metadata, source_hashes=hashes, samples=results), indent=2) + '\n')
        finally:
            try:
                call('(kill-emacs)')
            except Exception:
                editor.terminate()
            editor.wait(timeout=10)
            mock.terminate()
            mock.wait(timeout=10)
