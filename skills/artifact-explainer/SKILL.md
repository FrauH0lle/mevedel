---
name: artifact-explainer
description: Create an explainer artifact - a step-by-step walkthrough that teaches how something works, pairing short prose with a diagram at each stage. Use for concept explainers, tutorials, PR walkthroughs, and codebase tours. Only for CREATING a new explainer; edits to an existing one modify its HTML directly.
argument-hint: "[what to explain]"
user-invocable: true
---

$ARGUMENTS

!$artifact
!$artifact-design
!$artifact-diagramming

# Explainer artifacts

A teaching page: a lede stating what the reader will learn, a progression that
pairs short prose with a visual at each stage, and a recap. The format's value
is the pairing - a reader grasps structure from a picture before they parse
prose, so an explainer that is mostly text is underusing it.

## How to use

1. Read the template:

   ```
   ${MEVEDEL_SKILL_DIR}template.html
   ```

2. Pick a flavor (below) and **delete the other one entirely** - the template
   ships both, and shipping both is the most common mistake here.
3. Replace each `<!-- SLOT: ... -->` marker with real content, including the
   placeholder prose, the example diagram, and the sample code.
4. Self-check before writing the file: no `SLOT` markers left, one flavor only,
   no placeholder text, and no hardcoded color anywhere in an SVG.
5. Write the file into the session artifacts directory with ApplyPatch, per the
   artifact rules above.

**Creation only.** When updating an existing explainer, work with its current
HTML directly - don't re-read or re-apply this template.

## Flavor

- **Numbered steps** (the default): a progression the reader follows start to
  finish. Use it for concept explainers - how something works. Aim for 3-6
  steps, one idea each. Fewer and it is a report; more and it wants splitting.
- **Sections**: a tour of a system, a change, or an architecture, where reading
  order is looser and code carries more weight. Use it for PR walkthroughs,
  codebase tours, and design overviews - 2-7 sections cut at the material's
  joints, grouped rather than split mechanically. Open with one wide
  architecture or flow diagram when the subject has a structural story.

## Slots

| Slot | What to fill in |
| --- | --- |
| `TITLE` | What's being explained, phrased as the question the reader has. Appears twice - the `<title>` element and the visible `<h1>`. Fill both. |
| `LEDE` | Two or three sentences: what the reader will understand by the end, and why it matters. |
| `STEPS` | Steps flavor: one `<li class="step">` per stage - a heading, 1-3 short paragraphs, and a `.visual`. May end with a `<p class="callout">` aside. |
| `SECTIONS` | Sections flavor: `<section class="topic">` blocks with an `<h2>`, short prose, and `.visual` blocks that are usually code. |
| `RECAP` | The core takeaways, restated in the reader's new vocabulary. |

## Visuals

`.visual` is a free-form container - there is no renderer, you author what goes
in it directly. The diagramming guidance above covers how to draw; what matters
here is the balance:

- **In the steps flavor, default to a diagram.** Most steps should carry one.
  Reach for a `<pre>` code block or a small table alone only when the concept is
  genuinely symbolic - syntax, exact values, a comparison - where the code
  teaches better than a picture drawn around it. When both help, pair them in
  the same step.
- **The sections flavor inverts that balance:** the code snippet is usually the
  subject matter itself, and a diagram earns its place only where structure or
  flow genuinely needs one.
- **Code belongs in `<pre>`,** never as text inside an SVG.
- **Keep one visual vocabulary across the page** so the sequence reads as one
  picture evolving rather than a new drawing each step, and put the accent only
  on what the current step focuses on.
