---
name: artifact-design
description: Design fundamentals for session artifacts - palette, typography, layout, both themes, and how to avoid templated AI-looking pages. Attached automatically by the artifact skills; read it before writing any artifact HTML rather than invoking it on its own.
---

# Artifact design

Approach this as the design lead at a small studio known for their
versatility, giving every client a visual identity pitched at the treatment the
task actually calls for. Make deliberate choices about palette, typography, and
layout that are specific to this subject, and avoid templated designs.

## Read the request first

Calibrate the treatment, not whether to design at all.

Many requests call for a utilitarian treatment: a plan, a memo, a status page.
Make it polished - real typographic hierarchy, considered spacing, a proper
palette - but avoid over-designing. Most pages do not need a flashy, gigantic
hero. Keep flourishes tasteful and limited.

Some requests call for an editorial treatment: a pitch, a landing page, a
prototype the user will keep or show to someone.

When unsure: a well-composed page is never the wrong answer; an over-designed
visual identity sometimes is.

A Markdown artifact is the right format when the content is plain prose the
reader only needs to read. Everything below applies the moment you author HTML.

## Fundamentals

**Honor what's already there.** Look for an existing design system first -
`AGENTS.md`, a tokens or theme file, existing component styles in the
repository. When one exists, apply it; everything below fills gaps and never
overrides. Precedence is always the user's own words, then the project's
existing system, then your choices.

**Ground it in the subject.** If the subject isn't already clear, pin it: one
concrete subject, its audience, and the page's single job. The subject's own
world - its materials, instruments, vernacular - is where distinctive choices
come from. Build with real content throughout, never lorem.

**Type is system fonts, chosen deliberately.** The artifact sandbox blocks
every font host, so there is no Google Fonts link and no webfont URL - a remote
face does not fall back gracefully, it silently never loads. Build character
from the stacks that are already on the machine, and treat the choice as a real
one: a transitional serif (`Charter, "Bitstream Charter", "Sitka Text",
Cambria, Georgia, serif`), a geometric humanist sans (`Optima, Candara,
"Noto Sans", source-sans-pro, sans-serif`), a grotesque
(`Inter, Roboto, "Helvetica Neue", Arial, sans-serif`), a mono
(`ui-monospace, "SF Mono", "Cascadia Code", Menlo, Consolas, monospace`), or
the system stack (`system-ui, -apple-system, "Segoe UI", sans-serif`). Pair two
with contrast - a display role and a body role - and always declare the full
fallback chain. Embedding a face as an `@font-face` `data:` URI works but costs
real bytes against the size budget; reserve it for a page where the typeface
genuinely is the design.

Keep running text near 65 characters wide; set a type scale and stay on it;
give headings `text-wrap: balance`, body text room to breathe, and uppercase
labels a touch of letter-spacing.

**Write every line of CSS and JS into the page.** No CDN scripts, no external
stylesheets, no remote images, no runtime `fetch`. The sandbox's
Content-Security-Policy blocks all of it, so an external reference does not
degrade - it breaks, silently and completely. When a page genuinely needs
behavior, hand-write it; the libraries you would normally reach for are not
available at any URL. This is the single biggest difference from artifacts you
may have built elsewhere, and it means most pages should need no library at
all. Images embed as `data:` URIs, and every embedded byte counts against the
page's size budget.

**Choose neutrals, don't default to them.** A pure mid-grey reads as
unconsidered; a grey with a slight hue bias toward the page's accent reads as
chosen. Pure white and near-black are fine grounds when they suit the subject -
the point is that the neutral was picked, not inherited.

**Design both themes.** The artifact renders in its own sandboxed document, so
the only thing it can observe is `prefers-color-scheme` - there is no theme
attribute to read and no host stylesheet to inherit. Structure the CSS at the
token level: the bare `:root` block defines the complete light palette, and a
single `@media (prefers-color-scheme: dark)` block redefines only those tokens.
Style every component through the tokens, never with a color declared directly
inside the media block - a color whose only definition sits there never applies
in light mode, and the page renders one theme's text on the other theme's
ground. Scan the finished stylesheet for that before publishing; it is the
classic unreadable-artifact bug.

Two rules keep each theme resolving as a set. `body` must set an explicit
`background` from a token: the page is composited inside a frame whose own
ground you do not control, and a transparent body borrows it. And every element
that sets a color takes it from the same token set as the surface behind it,
never a literal that only works in one theme.

Give the second theme the same care as the first - don't naively invert; keep
contrast legible and the accent working on both grounds. A design that
deliberately commits to one visual world (a neon arcade screen, a letterpress
invitation) may stay single-theme - then skip the media query entirely, but
still paint the background and every color explicitly. Make it a choice, not an
omission.

**Let layout do the spacing.** Lay out sibling groups with flex or grid and
`gap`, not per-element margins that silently collapse or double. Wide content -
tables, code, diagrams - gets `overflow-x: auto` on its own container so the
page body never scrolls sideways. Reach for `font-variant-numeric: tabular-nums`
wherever digits line up in columns. Guests open artifacts on phones: check that
the layout holds at a narrow width.

**Avoid AI-generated design.** AI-generated design currently clusters around a
few looks: warm cream (`#F4F1EA`) with a serif display and terracotta accent;
near-black with a lone acid-green or vermilion pop; broadsheet hairline rules
with dense columns; a purple-to-blue gradient hero on white; Inter or Space
Grotesk as the "safe" face; emoji as section markers; everything centered;
`rounded-lg` everywhere; an accent bar or rail on rounded cards. Where the user
pins down a visual direction, follow it exactly - their words always win,
including when they ask for one of these looks. Where nothing is specified,
don't spend that freedom on one of these defaults.

**Build cleanly.** Watch for overlapping elements, cascade collisions, and
silent font fallbacks; visual bugs hide in the gap between source and output.
Close every non-void element, double-quote attributes, give keyboard focus a
visible state, and respect `prefers-reduced-motion`. Watch selector specificity
in particular - it is easy to generate classes that cancel each other out, a
type-based selector like `.section` fighting an element-based one like `.cta`
over the padding between sections. For generative or decorative graphics, reach
for Canvas rather than hand-authoring long SVG path data.

**Words are design material.** Write from the user's side of the screen - name
things by what people recognize, not how the system is built (a person manages
*notifications*, not *webhook config*). Active voice; a control says exactly
what happens. Errors explain what went wrong and how to fix it - no apologies,
no vagueness. Specific beats clever.

**Structure is information.** Structural devices - numbering, eyebrows,
dividers, labels - should encode something true about the content, not decorate
it. Numbered markers (01 / 02 / 03) are right only when the content actually is
a sequence, like a real process or a typed timeline where order carries
information the reader needs. Question whether such a choice makes sense before
incorporating it.

**When it's a UI, not a document.** A dashboard or tool is scanned and
operated, not read top to bottom, so the craft shifts from typography to
information design. Surface the summary before the detail; encode state in form
as well as number - a pill, a chip, a severity stripe - so what needs attention
reads at a glance. Semantic color (good / warning / critical) is separate from
the accent hue and doesn't count as your accent. Give sparklines and charts the
same care as type: an area fill, a faint grid, an emphasized endpoint. What's
interactive should look interactive.

## Process

Before writing code, sketch a short design plan - a compact token system:

- **Color**: the palette as 4-6 named hex values.
- **Type**: two or more roles - a characterful display face used with
  restraint, a complementary body face, and a utility face for captions or data
  if needed - each as a full system stack.
- **Layout**: the layout concept in one or two sentences.

Then build, deriving every color and type decision from that plan.

## When the request is editorial

The stance shifts: the client has already rejected proposals that felt
templated, and is paying for a distinctive point of view. Make opinionated
calls, and take one real aesthetic risk where it serves the work.

Review the design plan against the subject before building. If any part of it
reads like the generic default you would produce for any similar page, revise
that part and note what you changed and why. Only once you've confirmed the
plan's uniqueness do you write the code, following the revised plan exactly.

- **The hero is a thesis.** Open with the most characteristic thing in the
  subject's world - headline, image, live demo, interactive moment.
- **Typography carries the personality.** Pair the display and body roles
  deliberately, and set a clear type scale with intentional weights, widths,
  and spacing. Make the type treatment a memorable part of the design, not a
  neutral delivery vehicle.
- **Use motion deliberately.** Consider where animation serves the subject: a
  page-load sequence, a scroll-triggered reveal, hover micro-interactions. An
  orchestrated moment usually lands harder than scattered effects, and extra
  animation is itself a tell that a design was generated.
- **Match complexity to the vision.** Maximalist directions need elaborate
  execution; minimal directions need precision in spacing, type, and detail.
  Elegance is executing the chosen vision well.
- **Spend your boldness in one place** and keep everything around it quiet. If
  the accent fights the ground, shift it toward analogous or drop saturation
  rather than replacing it.
