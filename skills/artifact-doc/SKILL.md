---
name: artifact-doc
description: Create a document artifact - a typeset page for a memo, proposal, plan, spec, or meeting notes that someone will read and act on. Use when the document deserves real hierarchy, callouts, tables, and print styling rather than plain prose; a Markdown artifact is the better choice when the reader only needs the text. Only for CREATING a new document; edits to an existing one modify its HTML directly.
argument-hint: "[what the document covers]"
user-invocable: true
---

$ARGUMENTS

!$artifact
!$artifact-design

# Document artifacts

A document artifact is a deliverable someone reads and acts on: a memo, a
proposal, a plan, a spec, meeting notes. It earns HTML over a Markdown artifact
when the reading experience matters - scannable hierarchy, a callout the
skimmer cannot miss, tables that hold their shape on a phone, and a page that
prints cleanly.

It is a page, not an editor. Nobody types into it: the reader reads, and the
document changes when the session changes it and the artifact is written again.
Never add a toolbar, a `contenteditable` region, or copy telling the reader how
to edit or save - the sandbox blocks every way those could persist anything, so
the controls would look real and silently lose the reader's work.

## How to use

1. Read the template:

   ```
   ${MEVEDEL_SKILL_DIR}template.html
   ```

2. Copy it as your starting point and replace each `<!-- SLOT: ... -->` marker
   with real content; the comment inside each slot says what goes there. Each
   slot also carries placeholder text after the comment - a sample title, a
   heading, a sentence. Replace that too: removing the comment markers alone
   leaves the placeholders in the finished page.
3. Take a pass on styling and content. The body structure is a default, not a
   requirement - cut the sections this document doesn't need, and retune the
   token values where the subject calls for it. Change them in **all three**
   scopes that declare them (the light `:root`, the
   `prefers-color-scheme: dark` block, and `@media print`), or the value snaps
   back in dark mode or on paper. Keep text contrast accessible in both.
4. Self-check before writing the file: no `SLOT` markers left, no placeholder
   text left, no color declared only inside the dark or print block.
5. Write the file into the session artifacts directory with ApplyPatch, per the
   artifact rules above.

**Creation only.** When updating an existing document artifact, work with its
current HTML directly - don't re-read or re-apply this template.

## Slots

| Slot | What to fill in |
| --- | --- |
| `TITLE` | The document's name alone - short and distinctive, never a `Name - explainer` compound. |
| `KIND` | What this document is: `Memo`, `Proposal`, `Spec`, `Meeting notes`. Delete the line when it adds nothing. |
| `TITLE_H1` | The same name as `TITLE`, as the page's heading. |
| `PURPOSE` | One sentence: what the document is for, and what the reader should do with it. |
| `BODY` | The document itself - `h2` sections a reader can scan, short paragraphs, lists where structure helps, a `blockquote` for the one callout a skimmer must not miss. |
| `OPEN_QUESTIONS` | Every unresolved item, each with a named owner. Delete the section when there are none. |

## Writing it

Write so people can respond. Front-load the purpose so a reader knows in one
sentence whether this concerns them. Keep paragraphs short - this is a document
someone skims before they read. Name an owner for every open item; an
unassigned question is a question nobody answers.

Don't repeat in the page what the artifact card already carries. The filename
is the label on every cockpit row and collaboration card, so name the file for
the document (`q3-migration-proposal.html`), and let the page open with the
content rather than a masthead of author, date, and version.
