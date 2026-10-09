# Let full links change project files directly

Status: accepted

ADR 0114 governs how long a lobby link stays valid; this record governs what
a full lobby link may do to the project's files.

## Current decision

A workspace lobby is the project's view, so a full or owner link can browse,
read, upload and remove the project's files from the browser, and a room's
full link can add a prompt's attachments to the project. These are direct
changes by the link holder. They do not go through the model, its permission
checks, or ApplyPatch review. A view link sees no project files at all.

Uploads land in any listed folder of the project, not in a confined drop
folder such as `work://shared/`. An upload creates a new file and never
replaces one. Removal moves a file to the trash. The project's own listing,
minus `.mevedel/`, is the only authority for which paths a browser may name;
the browser never supplies a path the listing does not show. In a Git
project that listing includes each untracked, unignored Git repository
cloned inside it, by that repository's own listing.

## Rationale

A project can be pure whiteboard and chat, with no code to protect, and still
need reference data that every session reads. Those files belong where the
sessions look for project material: the project tree. A drop folder under
`.mevedel/` would be hidden from the tree, committed as versioned working
material, and a second place for workspace files beside the project itself.

Routing every browser upload or removal through the model was the
alternative. It would keep one mutation path and an audit trail in the
transcript, but it spends a model turn, and a session, on moving bytes the
person already chose, and it fails exactly when no session is running. The
direct path keeps the model's mutation tracking intact for model edits; it
adds a second, narrow writer whose every change a human made explicitly.

Nested clones were added when a project that existed to share two cloned
reference repositories showed none of their files. Git reports an untracked
nested repository as one directory entry and `project-files` drops it, while
the model's search tools read those files; the browser then hid exactly the
material the sessions were reading. Asking each clone for its own listing
keeps that clone's ignore rules, and a clone the project ignores stays hidden.

## Consequences

A full lobby link, which ADR 0114 already makes a long-lived bearer, now also
carries write access to the project tree. Anyone it is handed on to can add
files and remove them without a permission prompt. The bounds are: no
overwrite, no folder removal, no hidden or ignored paths, no `.mevedel/`
state, at most 16 MiB per upload, and removal to the trash, which keeps the
bytes. The same bounds cover files inside nested clones; a clone the host
mounts read-only refuses the change itself. A file with unsaved changes in Emacs cannot be removed. Rotating the
lobby credentials revokes the access.

A model that read a file before a browser removed or added one sees the
change the way it sees any outside edit: on its next read.

## Decision history

The first proposal confined uploads to `work://shared/` and left changes to
existing project files to the model. It was dropped once whiteboard-and-chat
projects made the project root the natural home for reference data, and the
drop folder's costs -- invisible in the tree, committed by default, a second
location -- outweighed the narrower write surface.
