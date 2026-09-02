---
name: artifact-diagramming
description: How to draw a diagram inside an artifact - when a picture earns its place, and the hand-authored inline SVG mechanics that keep it legible in light and dark. Attached by the artifact skills that carry visuals; read it before drawing rather than invoking it on its own.
user-invocable: false
---

# Diagramming in artifacts

Draw as the engineer who has to live with the decision, not as a decorator. A
diagram earns its place when it lets a cold reader see a mechanism they would
otherwise assemble from prose - where data flows, which components talk, what
changes between two options, what state a request moves through. If a sentence
says it faster, write the sentence.

A Markdown artifact has no diagram lane: the viewer's Markdown renderer draws
no SVG and understands no diagram fence, so a picture there is a fenced code
block at best. When a diagram carries the point, author the artifact as HTML.

## What to draw

**Depict the mechanism, not its name.** A box labeled "cache" says less than
the prose; the path a request takes through it, the two stores it sits between,
and the arrow that disappears when the cache is removed say what the words
can't. Show the parts the argument hinges on - the boundary being crossed, the
hop being added, the data that moves - and leave out the parts that don't.

**Comparing options? Draw the difference.** Two architectures side by side, a
before and an after, the one edge each option adds or removes - the reader
should be able to point at what they are choosing between. A separate labeled
box per option, with nothing connecting them to the system, is not a
comparison; it is a restated option list.

**Match complexity to the stakes.** A one-hop question is a three-box diagram;
a migration that reroutes writes through a queue needs the queue, the writer,
the reader, and the ordering arrow. Draw as much as the decision actually turns
on - no forced minimalism, and no inventory of the whole system either.

**Label the arrows.** An unlabeled arrow means "related somehow"; `writes`,
`invalidates`, `polls every 30s` is information. A legend earns its place only
when the same encoding (dashed, colored, doubled) repeats; otherwise put the
meaning on the mark itself.

## Inline SVG mechanics

Hand-author inline `<svg>` with native shapes (`rect`, `circle`, `line`,
`polyline`, `path`) and `<text>`. No libraries, no runtime, no external images -
the sandbox blocks all three, and there is no CDN to fall back to.

- **Size by `viewBox`.** Set `viewBox="0 0 W H"`, omit `width`/`height`, and let
  CSS scale it (`max-width: 100%; height: auto`). Choose W and H for the
  content, not a preset. Wide flows read left to right; layered stacks read top
  to bottom.
- **Theme through the page's tokens.** Route every color through a CSS custom
  property in a `style` attribute - `style="stroke: var(--ink)"` - because
  `var()` fails silently in a bare SVG presentation attribute like
  `stroke="var(--ink)"`. `currentColor` is the other safe choice for strokes,
  text, and arrowheads, since it inherits the page foreground in both themes.
  Never write a hardcoded hex, a named color, or `white`/`black` inside a
  diagram: near-black strokes vanish on a dark ground and light fills glare on
  it. Reserve one literal accent for the single element that carries meaning -
  the option leaned toward, the hop under discussion - and check it reads on
  both grounds.
- **Arrowheads are markers or polygons.** A `<defs><marker>` referenced by
  `marker-end="url(#arrow)"` with a fragment-internal id, or a small `<polygon>`
  at the line's end. Never an image.
- **Keep text legible.** Roughly 12-16px at the drawn scale, `text-anchor` for
  alignment, and short labels of a word or three. Explanatory sentences belong
  in the caption below the figure, not inside the drawing, and code belongs in a
  `<pre>` beside it rather than as SVG text.
- **Leave generous padding** around shapes and labels. Cramped diagrams are the
  single most common failure.
- **Align to a grid.** Shared baselines and even gaps are most of what makes a
  hand-drawn diagram read as deliberate; eyeballed offsets read as noise.
- **Keep one visual vocabulary** across a page's diagrams - boxes for things,
  arrows for movement or causality, the accent only on the current focus - so a
  sequence reads as one picture evolving rather than a new drawing each time.
- **One figure, one claim.** Wrap the `<svg>` in a `<figure>` with a
  `<figcaption>` stating what the picture shows, and give the `<svg>`
  `role="img"` plus an `aria-label` carrying the same claim for readers who
  cannot see it.
- **Stay self-contained.** No `<script>`, `<style>`, or `<foreignObject>` inside
  the SVG. Gradients, patterns, and `<use>` reference ids in the same fragment
  (`href="#id"`). Long decorative path data means the drawing wants a real
  graphics tool - simplify instead.
