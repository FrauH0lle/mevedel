# Show Ask Samples Inline Under Their Question

Status: accepted

## Current decision

An Ask option's `sample` renders inside the questionnaire, in a panel that ends
its question's option list. The panel uses the rendered source block face, so a
sample reads like a code block elsewhere in the transcript, and its title names
the option it belongs to.

Samples are wrapped into rows at the width of the narrowest window showing the
view, continuation rows
marked `↪`. The panel's height is fixed per question: the longest of that
question's samples in rows, capped by `mevedel-ask-sample-height`. Moving between
options therefore never moves anything below the panel. The panel shows the
sample-bearing option point visited last. Moving onto an option without a
sample, or out of the question, leaves the panel unchanged, and only reaching a
different sample-bearing option repaints the form. Longer samples page with
`C-v`/`M-v` while point is in their open question.

## Rationale

The sample belongs to one question and must never cover other content. Inside
the buffer, the panel scrolls, wraps and reflows with the form, and works the
same in a terminal, in a directive frame and in a shared view. It needs no
window, geometry or lifecycle code.

A child frame cannot meet the no-occlusion requirement on its own. Beside the
options it covers their descriptions; below them it covers later questions.
Avoiding that means reserving blank lines in the form for the frame and moving
the frame on every scroll, resize, window change and streamed insertion above
the form. That is more code and more redisplay work than rendering the sample
as text in the reserved space.

Fixed per-question height and the sticky shown sample are what keep
comparison still: a panel that followed each sample's length, or emptied on
options without samples, would shift the rest of the form while the user
reads. Height counts wrapped rows because Emacs soft-wraps by display width:
counting logical lines let a sample of long lines occupy several times its
line count, moving the next question by as much on an 80-column window.

## Consequences

- Long samples make the form taller, by at most `mevedel-ask-sample-height`
  rows: only the open question shows its panel
  ([ADR 0127](0127-open-one-ask-question-at-a-time.md)).
- Samples are fontified as Markdown once per form and cached.
- Wrapping depends on the view's width. When it changes, the form re-wraps
  its samples and lays itself out again at once, through the zone's
  width-change hook, instead of soft-wrapping until the next repaint.
- Samples are not shown side by side with the options, as some terminal UIs do.
  An Emacs buffer has no column layout that wraps both sides independently.

## Decision history

The sample originally rendered in a child frame parented to the top-level
Emacs frame, never to a directive frame, because a child frame is clipped at
its parent's edges and a directive frame would have left it about eight usable
lines. Placement translated anchor coordinates through nested child frames using
native and outer edges, after a PGTK run showed child-frame display origins can
be unavailable.

The frame was replaced after the user reported it looked broken: a short sample
appeared far above its question, over the composer and other questions, with no
visible border. Placement had computed the frame's position for the 20-line
maximum height and then fitted it to its content, which shrank it from the
bottom. A form near the composer flipped the frame above its anchor, leaving a
short sample hanging well above the question. Fixing placement would not have
fixed occlusion, for the reasons above. Offered both designs with mockups, the
user chose the inline panel. The frame, its placement and its three size
options were removed.
