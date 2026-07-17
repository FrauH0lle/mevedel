# Run Bash through a login shell

The Bash tool invokes model-authored source with `bash -lc` instead of the
current `bash -c`, so commands receive the user's normal login environment and
command lookup.  Login startup files execute before the requested source and
are treated as trusted user environment initialization rather than input to the
model-command classifier.  They remain inside the invocation's selected
confinement, timeout, process group, and output capture, so adopting login-shell
behavior does not create a separate execution path.
