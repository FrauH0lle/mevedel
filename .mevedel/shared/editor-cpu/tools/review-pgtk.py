import subprocess,time,os,json,pathlib
out=pathlib.Path('/tmp/mevedel-pgtk-probe');out.mkdir(exist_ok=True)
server='mevedel-cpu-review-isolated'
init=out/'init.el'
init.write_text('''(setq inhibit-startup-screen t)
(require 'server)
(setq server-name "mevedel-cpu-review-isolated")
(server-start)
(setq frame-title-format "mevedel CPU measurement - temporary Emacs -Q")
(switch-to-buffer "CPU measurement")
(insert "Temporary isolated Emacs -Q CPU measurement.\\nThis window will close automatically.\\n")
(defvar cpu-probe-timer nil)
''')
log=open(out/'emacs.log','w');proc=subprocess.Popen(['emacs','-Q','-l',str(init)],stdout=log,stderr=log)
def call(form):
 r=subprocess.run(['emacsclient','-s',server,'--eval',form],capture_output=True,text=True,timeout=10)
 if r.returncode:raise RuntimeError(r.stderr[:200])
 return r.stdout.strip()
def ticks():
 s=pathlib.Path(f'/proc/{proc.pid}/stat').read_text().rsplit(')',1)[1].split()
 return int(s[11])+int(s[12])
try:
 for _ in range(40):
  time.sleep(.25)
  try: call('t');break
  except RuntimeError:pass
 results=[]
 for width,height,rate in [(1536,888,0),(1536,888,10),(400,300,10),(400,300,0),(1536,888,10)]:
  call(f'(progn (when (timerp cpu-probe-timer) (cancel-timer cpu-probe-timer)) (setq cpu-probe-timer nil) (set-frame-size nil {width} {height} t) (when (> {rate} 0) (setq cpu-probe-timer (run-at-time 0.1 0.1 (quote ignore)))) t)')
  time.sleep(2)
  geom=call('(list (frame-pixel-width) (frame-pixel-height) (frame-visible-p (selected-frame)))')
  t0=time.monotonic();c0=ticks();time.sleep(10);duration=time.monotonic()-t0
  row=dict(geometry=geom,hz=rate,seconds=round(duration,3),editor_cpu=round((ticks()-c0)/os.sysconf('SC_CLK_TCK')/duration*100,2))
  results.append(row);print(json.dumps(row),flush=True)
 (out/'results.json').write_text(json.dumps(results,indent=2)+'\n')
finally:
 try:call('(kill-emacs)')
 except Exception:proc.terminate()
 proc.wait(timeout=10);log.close()
