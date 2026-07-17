# Full-auto includes unconfined live Eval

Selecting `full-auto` is direct user authority for maximally unattended tool
execution, including live Eval without a permission prompt.  Live Eval runs
inside Emacs and therefore cannot inherit allowed-root or child-process sandbox
enforcement; explicit deny rules still win, but this mode knowingly accepts the
remaining risk in exchange for true automation.  Users who do not accept that
trade-off use `auto`, where live Eval continues to ask.
