# Git on this host

This workspace runs on a shared server; people also work on its
repositories from their own machines. The repositories here are ordinary
clones you may change, commit and push; only `refs/`, where present, is
read-only context.

- Before changing a repository or creating a branch, `git pull` it. The
  server syncs only hourly, so it may be behind what someone pushed.
- Make each coherent change on its own branch (e.g. `docs/frontend-stack`),
  never on the default branch. Push the branch and pass on the link for
  opening a pull or merge request. Do not push to a branch someone else is
  working on.
- Before committing, ask whom to commit as, unless the request says so, and
  pass it as `--author`. Never commit with the default identity alone.
  - Roland Goers <roland.goers@swisstph.ch>
  - Myroslava Volosko <myroslava.volosko@swisstph.ch>
- `git fetch` and `git pull` have network access; `git push` needs it
  requested for the command.
