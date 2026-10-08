import os, sys, time
root = int(sys.argv[1]); kwin = int(sys.argv[2]); dur = float(sys.argv[3]); out = sys.argv[4]
hz = os.sysconf('SC_CLK_TCK')
def stat(pid):
    try:
        with open(f'/proc/{pid}/stat') as f: s = f.read()
        r = s[s.rindex(')')+2:].split()
        return int(r[1]), int(r[11]) + int(r[12])  # ppid, utime+stime
    except Exception: return None
def ctx(pid):
    try:
        t = 0
        for l in open(f'/proc/{pid}/status'):
            if 'ctxt_switches' in l: t += int(l.split()[1])
        return t
    except Exception: return 0
def snapshot():
    procs = {}
    for d in os.listdir('/proc'):
        if d.isdigit():
            st = stat(int(d))
            if st: procs[int(d)] = st
    desc = set(); frontier = [root]
    kids = {}
    for p, (pp, _) in procs.items(): kids.setdefault(pp, []).append(p)
    while frontier:
        p = frontier.pop()
        for c in kids.get(p, []):
            if c not in desc: desc.add(c); frontier.append(c)
    return procs, desc
prev, _ = snapshot(); pctx = ctx(root); t0 = time.time()
with open(out, 'w') as f:
    f.write('t,emacs,children,kwin,ctx\n')
    while time.time() - t0 < dur:
        time.sleep(1)
        cur, desc = snapshot(); cctx = ctx(root)
        d = lambda p: (cur[p][1] - prev.get(p, (0, 0))[1]) if p in cur else 0
        e = d(root); k = d(kwin); ch = sum(d(p) for p in desc)
        f.write(f'{time.time()-t0:.0f},{e*100//hz},{ch*100//hz},{k*100//hz},{cctx-pctx}\n'); f.flush()
        prev = cur; pctx = cctx
