#!/usr/bin/env python3
"""Compare visible animation CPU in a disposable, isolated Emacs process."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

parser = argparse.ArgumentParser()
parser.add_argument('--emacs', default='emacs')
parser.add_argument('--modes', nargs='+', default=['idle', 'noop', 'text'])
parser.add_argument('--rate', type=float, default=30)
parser.add_argument('--seconds', type=float, default=5)
parser.add_argument('--repeat', type=int, default=1)
parser.add_argument('--output', required=True)
parser.add_argument('--assert-overhead', type=float)
parser.add_argument('--preload')
parser.add_argument('--damage-probe')
parser.add_argument('--gtk-probe')
parser.add_argument('--canvas-probe')
parser.add_argument('--native-module')
parser.add_argument('--style', default='bounce')
parser.add_argument('--screenshots', action='store_true')
parser.add_argument('--profile', action='store_true')
parser.add_argument('--lifecycle', action='store_true')
parser.add_argument('--allocation-failures', action='store_true')
args = parser.parse_args()
root = Path(__file__).resolve().parents[4]
output = Path(args.output).resolve()
output.mkdir(parents=True, exist_ok=True)
results = []
with tempfile.TemporaryDirectory(prefix='mevedel-renderer-') as temp:
    temp = Path(temp)
    sources = [root / 'mevedel-view-animation.el', Path(__file__).with_name('lab.el')]
    if args.native_module:
        sources += [root / 'mevedel-view-native.el', root / 'mevedel-structs.el']
    for source in sources:
        shutil.copy(source, temp / source.name)
    env = dict(os.environ, MEVEDEL_LAB_SERVER='mevedel-lab-' + str(os.getpid()),
               MEVEDEL_LAB_STYLE=args.style)
    if args.native_module:
        env['MEVEDEL_NATIVE_MODULE'] = str(Path(args.native_module).resolve())
    subprocess.run([args.emacs, '-Q', '--batch', '-L', str(temp), '-f',
                    'batch-byte-compile'] + [str(temp / source.name) for source in reversed(sources)],
                   check=True, env=env, stdout=subprocess.DEVNULL)
    if args.canvas_probe:
        env['MEVEDEL_CANVAS_PROBE'] = str(Path(args.canvas_probe).resolve())
    if args.gtk_probe:
        env['MEVEDEL_GTK_PROBE'] = str(Path(args.gtk_probe).resolve())
    if args.preload:
        env['LD_PRELOAD'] = str(Path(args.preload).resolve())
    if args.allocation_failures:
        env['MEVEDEL_LAB_FAILURE'] = str(temp / 'allocation-failure')
    if args.damage_probe:
        env['MEVEDEL_DAMAGE_PROBE'] = args.damage_probe
    with (output / 'editor.log').open('w') as log:
        command = [args.emacs, '-Q', '-L', str(temp), '-l', str(temp / 'lab.elc')]
        if args.profile:
            command = ['gprofng', 'collect', 'app', '-o', str(output / 'profile.er')] + command
        proc = subprocess.Popen(command, env=env, stdout=log, stderr=log)
        def call(form):
            result = subprocess.run(['emacsclient', '-s', env['MEVEDEL_LAB_SERVER'],
                                     '--eval', form], capture_output=True, text=True, timeout=10)
            if result.returncode:
                raise RuntimeError(result.stderr[:300])
            return result.stdout.strip()
        def ticks(pid):
            fields = Path(f'/proc/{pid}/stat').read_text().rsplit(')', 1)[1].split()
            return int(fields[11]) + int(fields[12])
        try:
            for attempt in range(80):
                if proc.poll() is not None:
                    raise RuntimeError('Editor exited: ' + (output / 'editor.log').read_text()[-2000:])
                try:
                    call('t')
                    break
                except RuntimeError:
                    time.sleep(.25)
            else:
                raise RuntimeError('Editor server did not start')
            editor_pid = int(call('(emacs-pid)'))
            time.sleep(2)
            compositor = subprocess.run(['pgrep', '-x', 'kwin_wayland'], capture_output=True, text=True)
            compositor = int(compositor.stdout.strip()) if compositor.returncode == 0 else None
            metadata = call('(list emacs-version window-system (frame-pixel-width) (frame-pixel-height) (frame-visible-p (selected-frame)))')
            for repeat in range(args.repeat):
                for mode in args.modes:
                    call(f'(lab-set {json.dumps(mode)} {args.rate})')
                    time.sleep(1)
                    c0 = int(call('(if (equal lab-mode "native-owned") (aref (mevedel-view-native--stats) 1) (if (and (fboundp (quote lab-native-count)) (member lab-mode (quote ("native-noop" "native-widget" "native-surface")))) (lab-native-count) lab-calls))'))
                    frame0 = None
                    if mode == 'native-surface':
                        frame0 = [int(value) for value in call('(lab-native-frames)').strip('()').split()]
                    if mode == 'native-owned':
                        frame0 = [int(value) for value in call('(mevedel-view-native--stats)').strip('[]').split()][1:3]
                    e0 = ticks(editor_pid)
                    k0 = ticks(compositor) if compositor else 0
                    start = time.monotonic()
                    time.sleep(args.seconds)
                    duration = time.monotonic() - start
                    e1 = ticks(editor_pid)
                    k1 = ticks(compositor) if compositor else 0
                    c1 = int(call('(if (equal lab-mode "native-owned") (aref (mevedel-view-native--stats) 1) (if (and (fboundp (quote lab-native-count)) (member lab-mode (quote ("native-noop" "native-widget" "native-surface")))) (lab-native-count) lab-calls))'))
                    scale = 100 / os.sysconf('SC_CLK_TCK') / duration
                    row = dict(mode=mode, repeat=repeat, rate=args.rate,
                               seconds=round(duration, 3), editor_cpu=round((e1-e0)*scale, 2),
                               compositor_cpu=round((k1-k0)*scale, 2),
                               callbacks_per_second=round((c1-c0)/duration, 2))
                    if args.gtk_probe:
                        row['native_frames'] = call('(when (fboundp (quote lab-native-frames)) (lab-native-frames))')
                    if args.native_module:
                        row['module_stats'] = call('(mevedel-view-native--stats)')
                    if frame0 is not None:
                        frame1 = ([int(value) for value in row['module_stats'].strip('[]').split()][1:3]
                                  if mode == 'native-owned' else
                                  [int(value) for value in row['native_frames'].strip('()').split()])
                        row['submitted_per_second'] = round((frame1[0]-frame0[0])/duration, 2)
                        row['released_per_second'] = round((frame1[1]-frame0[1])/duration, 2)
                    results.append(row)
                    print(json.dumps(row), flush=True)
                    if args.screenshots and mode in ('native-surface', 'native-owned'):
                        for shot in range(2):
                            subprocess.run(['spectacle','-b','-n','-a','-o',
                                            str(output / f'{mode}-{repeat}-{shot}.png')], check=True,
                                           stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
                            time.sleep(.7)
            if args.lifecycle:
                checks = []
                def active():
                    return int(call('(aref (mevedel-view-native--stats) 0)'))
                def check(label, expected):
                    actual = active()
                    checks.append(dict(check=label, expected=expected, actual=actual))
                    (output / 'lifecycle.json').write_text(json.dumps(checks, indent=2) + '\n')
                    if actual != expected:
                        raise RuntimeError(f'{label}: expected {expected} surfaces, got {actual}')
                call('(lab-set "native-owned" 30)')
                time.sleep(.4)
                check('attached', 1)
                call('(with-current-buffer "Animation experiment" (let ((inhibit-modification-hooks t)) (goto-char (point-min)) (insert "Layout moved\\n")) (goto-char (point-max)) (redisplay t) t)')
                check('inhibited edit hides old pixels before redisplay', 0)
                time.sleep(.5)
                check('reattached after inhibited edit', 1)
                call('(with-current-buffer "Animation experiment" (goto-char (marker-position (car lab-target))) (redisplay t) t)')
                check('cursor occlusion hides surface', 0)
                time.sleep(.5)
                check('cursor remains ordinary text', 0)
                call('(with-current-buffer "Animation experiment" (goto-char (point-max)) (lab-native-rearm) t)')
                check('cursor leaves label', 1)
                call('(make-frame-invisible nil t)')
                time.sleep(.2)
                check('hidden frame releases native work', 0)
                call('(progn (make-frame-visible) (select-frame-set-input-focus (selected-frame)) t)')
                time.sleep(.5)
                call('(lab-native-rearm)')
                check('shown frame reattaches', 1)
                call('(progn (set-frame-font "Aporetic Serif Mono-20" nil t) t)')
                time.sleep(.7)
                call('(lab-native-rearm)')
                check('font resize reattaches', 1)
                call('(with-current-buffer "Animation experiment" (goto-char (+ 5 (marker-position (car lab-target)))) (insert "\n") (remove-text-properties (car lab-target) (cdr lab-target) (quote (display nil))) (goto-char (point-max)) (redisplay t) t)')
                time.sleep(.5)
                check('multiline label uses ordinary text', 0)
                call('(with-current-buffer "Animation experiment" (goto-char (+ 5 (marker-position (car lab-target)))) (delete-char 1) (put-text-property (car lab-target) (cdr lab-target) (quote display) (mevedel-view-animation-frame lab-style "Working..." 0 (quote default))) (goto-char (point-max)) (redisplay t) t)')
                time.sleep(.5)
                call('(progn (lab-native-rearm) (redisplay t) t)')
                check('single line restored', 1)
                call('(with-current-buffer "Animation experiment" (transient-mark-mode 1) (goto-char (point-max)) (set-mark (point-min)) (activate-mark) (redisplay t) t)')
                time.sleep(.5)
                check('selection uses ordinary text', 0)
                call('(with-current-buffer "Animation experiment" (deactivate-mark) (lab-native-rearm) (redisplay t) t)')
                check('selection cleared', 1)
                call('(progn (defvar lab-main-frame (selected-frame)) (defvar lab-child (make-frame `((parent-frame . ,lab-main-frame) (minibuffer . nil) (width . 20) (height . 3)))) (redisplay t) t)')
                time.sleep(.5)
                check('child frame hides native labels', 0)
                call('(progn (delete-frame lab-child) (select-frame-set-input-focus lab-main-frame) t)')
                time.sleep(.5)
                call('(progn (lab-native-rearm) (redisplay t) t)')
                check('child frame dismissal restores labels', 1)
                call('(progn (switch-to-buffer "Hidden label check") (redisplay t) t)')
                time.sleep(.5)
                check('switching buffers releases work', 0)
                call('(progn (switch-to-buffer "Animation experiment") (lab-native-rearm) (redisplay t) t)')
                time.sleep(.5)
                check('buffer returns', 1)
                call('(progn (tab-bar-new-tab) (switch-to-buffer "Other tab") (redisplay t) t)')
                time.sleep(.5)
                check('inactive tab releases work', 0)
                call('(progn (tab-bar-close-tab) (lab-native-rearm) (redisplay t) t)')
                time.sleep(.5)
                check('tab returns', 1)
                call('(progn (defvar lab-other-frame (make-frame)) (select-frame-set-input-focus lab-other-frame) (switch-to-buffer "Other frame") t)')
                time.sleep(.5)
                check('unfocused parent releases work', 0)
                call('(progn (delete-frame lab-other-frame) (select-frame-set-input-focus lab-main-frame) t)')
                time.sleep(.5)
                call('(progn (lab-native-rearm) (redisplay t) t)')
                check('focus returns', 1)
                call('(with-current-buffer "Animation experiment" (defvar lab-open-args) (let ((placement (mevedel-view-native--placement lab-target (selected-window)))) (setq lab-open-args (list (frame-parameter nil (quote window-id)) (cadr placement) (nth 2 placement) "#ffffff" (mevedel-view-native--timeline lab-style "Working..." (/ 1.0 lab-rate) (quote default) (selected-frame)) 0.0))) t)')
                call('(with-current-buffer "Animation experiment" (mevedel-view-native-stop) t)')
                check('explicit stop', 0)
                if args.allocation_failures:
                    for allocation in (1, 2, 3):
                        Path(env['MEVEDEL_LAB_FAILURE']).write_text(str(allocation))
                        response = call('(null (apply (quote mevedel-view-native--open) lab-open-args))')
                        if response != 't' or Path(env['MEVEDEL_LAB_FAILURE']).read_text() != '0':
                            raise RuntimeError(f'Allocation failure {allocation} did not return nil')
                        check(f'buffer allocation {allocation} failure releases resources', 0)
                response = call('(let ((args (copy-sequence lab-open-args))) (setcar args "1") (null (apply (quote mevedel-view-native--open) args)))')
                if response != 't':
                    raise RuntimeError('Invalid parent ID did not return nil')
                check('invalid parent is rejected safely', 0)
                call('(progn (garbage-collect) t)')
                time.sleep(.3)
                before_links = {p.name: os.readlink(p) for p in Path(f'/proc/{editor_pid}/fd').iterdir()}
                before_fds = len(before_links)
                before_submissions = int(call('(aref (mevedel-view-native--stats) 1)'))
                call('(with-current-buffer "Animation experiment" (dotimes (_ 50) (lab-native-rearm) (redisplay t) (mevedel-view-native-stop)) (garbage-collect) t)')
                after_submissions = int(call('(aref (mevedel-view-native--stats) 1)'))
                if after_submissions - before_submissions < 50:
                    raise RuntimeError('Lifecycle loop did not actually create 50 surfaces')
                time.sleep(.3)
                check('50 create/close cycles leave no native work', 0)
                time.sleep(.3)
                after_links = {p.name: os.readlink(p) for p in Path(f'/proc/{editor_pid}/fd').iterdir()}
                after_fds = len(after_links)
                (output / 'descriptors.json').write_text(json.dumps(dict(before=before_links, after=after_links), indent=2) + '\n')
                if after_fds > before_fds:
                    raise RuntimeError(f'File descriptors grew from {before_fds} to {after_fds}')
                checks.append(dict(check='file descriptor stability', before=before_fds, after=after_fds))
                call('(progn (lab-native-rearm) (redisplay t) t)')
                check('active before feature unload', 1)
                call('(progn (unload-feature (quote mevedel-view-native) t) t)')
                check('feature unload releases native work', 0)
                (output / 'lifecycle.json').write_text(json.dumps(checks, indent=2) + '\n')
                print('Native lifecycle checks passed', flush=True)
            (output / 'results.json').write_text(json.dumps(dict(metadata=metadata, samples=results), indent=2)+'\n')
        finally:
            try:
                call('(kill-emacs)')
            except Exception:
                proc.terminate()
            proc.wait(timeout=10)
if args.assert_overhead is not None:
    baseline = sum(r['editor_cpu'] for r in results if r['mode']=='idle') / sum(r['mode']=='idle' for r in results)
    measured = max(r['editor_cpu'] for r in results if r['mode']!='idle') - baseline
    print(f'Animation overhead: {measured:.2f} percentage points; limit: {args.assert_overhead:.2f}', flush=True)
    cadence_ok = all(r['callbacks_per_second'] >= args.rate * .85
                     for r in results if r['mode'] != 'idle')
    presentation_ok = all(r['submitted_per_second'] >= args.rate * .85
                          and r['released_per_second'] >= args.rate * .85
                          for r in results if r['mode'] in ('native-surface', 'native-owned'))
    print(f'Callback cadence passes: {cadence_ok}; buffer delivery passes: {presentation_ok}')
    raise SystemExit(0 if measured <= args.assert_overhead and cadence_ok and presentation_ok else 1)
