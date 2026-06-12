# UI Prototype

Generate **several radically different UI variations** in the host UI, switchable from a small prototype-only control. The user flips between variants, picks one (or steals bits from each), then throws the rest away.

If the question is about logic/state rather than what something looks like — wrong branch. Use [LOGIC.md](LOGIC.md).

## When this is the right shape

- "What should this view look like?"
- "I want to see a few options for this buffer before committing."
- "Try a different layout for the interaction zone."
- Any time the user would otherwise spend a day picking between three vague mockups in their head.

## Mevedel Shape — Emacs UI

For mevedel, UI prototypes should be Emacs-native:

- Temporary commands, buffers, overlays, text properties, faces, keymaps, or minor modes.
- A variant switcher command/keybinding inside the prototype buffer, not a browser URL.
- Real mevedel data shapes where cheap: session structs, render-data, transcript snippets, queue entries, or fixture buffers.
- No persistent session writes unless the question explicitly requires persistence.

Prefer an adjustment to an existing view or scratch buffer over a freestanding mock. A prototype is easier to judge when it uses real buffer width, real faces, real keymaps, and realistic transcript density.

Good mevedel prototype shapes:

- `mevedel-prototype-FOO` interactive command that opens a scratch buffer with variants.
- A temporary branch in the existing render function guarded by a clearly named prototype variable.
- A small fixture-driven buffer renderer that imports the real renderer helpers and swaps layout variants.
- A minor mode with local keys like `n` / `p` to switch variants and `q` to quit.

## Web Shape

For web apps, use the original route shape: variants on the same route gated by `?variant=`, or a throwaway route only when there is no natural host page. Keep existing data fetching above the switcher; only the rendered subtree changes per variant.

Before creating a freestanding prototype, sanity-check whether it could be embedded in the real host view. Empty shells hide design problems that populated views expose.

## Process

### 1. State the question and pick N

Default to **3 variants**. More than 5 stops being radically different and starts being noise — cap there.

Write down the plan in one line, in the prototype's location or a top-of-file comment:

> "Three variants of the interaction-zone queue, switchable with `n`/`p`, rendered in a scratch buffer from fixture queue entries."

This works whether the user is here to push back or not.

### 2. Generate radically different variants

Draft each variant. Hold each one to:

- The view's purpose and the data it has access to.
- The project's UI primitives: in mevedel, faces, overlays, buttons, margins, text properties, keymaps, and view-buffer conventions.
- A clear function/component name, e.g. `mevedel-prototype-foo-variant-a`, `VariantA`, `VariantB`.

Variants must be **structurally different** — different layout, different information hierarchy, different primary affordance, not just different colours. Three slightly-tweaked card grids isn't a UI prototype, it's wallpaper. If two drafts come out too similar, redo one with explicit "do not use a card grid" guidance.

### 3. Wire them together

Create a single switcher. For mevedel, prefer buffer-local state and keys:

```elisp
(defvar-local mevedel-prototype--variant "A")

(defun mevedel-prototype-next-variant ()
  (interactive)
  (setq mevedel-prototype--variant
        (pcase mevedel-prototype--variant
          ("A" "B")
          ("B" "C")
          (_ "A")))
  (mevedel-prototype-render))
```

For web apps, use the route/search-param switcher:

```tsx
// pseudo-code — adapt to the project's framework
const variant = searchParams.get('variant') ?? 'A';
return (
  <>
    {variant === 'A' && <VariantA {...data} />}
    {variant === 'B' && <VariantB {...data} />}
    {variant === 'C' && <VariantC {...data} />}
    <PrototypeSwitcher variants={['A','B','C']} current={variant} />
  </>
);
```

Keep all existing data fetching or fixture setup above the switcher; only the rendered subtree changes per variant.

### 4. Build the switcher

For mevedel, expose a small prototype control surface in the buffer:

- Header line or first line showing the current variant key and label.
- Local keys for previous/next variant, such as `p` and `n`.
- `q` to bury/kill the prototype buffer.
- Optional clickable text buttons if the view being tested is mouse-oriented.

Behaviour:

- Switching variants re-renders the whole prototype buffer from the same fixture data.
- The switcher must be visually distinct from the design being evaluated.
- Prototype variables, commands, and buffers must include `prototype` in the name.
- Do not install global keybindings or mutate persistent mevedel session state.

For web apps, keep the floating bottom bar behavior: previous/next buttons, current variant label, shareable `?variant=`, keyboard arrows, and a production-build guard.

### 5. Hand it over

Surface the command or URL and the variant keys. The user will flip through whenever they get to it. The interesting feedback is usually **"I want the density from B with the status treatment from C"** — that's the actual design they want.

### 6. Capture the answer and clean up

Once a variant has won, write down which one and why (commit message, ADR, issue, or a `NOTES.md` next to the prototype if running AFK and the user hasn't responded yet). Then:

- **Embedded prototype** — delete the losing variants and the switcher; fold the winner into the existing view.
- **Scratch prototype** — port the winning idea into the real view, then delete the scratch command/buffer.
- **Web throwaway route** — promote the winning variant to a real route, delete the throwaway route and the switcher.

Don't leave variant components or the switcher lying around. They rot fast and confuse the next reader.

## Anti-patterns

- **Variants that differ only in colour or copy.** That's a tweak, not a prototype. Real variants disagree about structure.
- **Sharing too much code between variants.** A shared `<Header>` is fine; a shared `<Layout>` defeats the point. Each variant should be free to throw out the layout.
- **Wiring variants to real mutations.** Read-only prototypes are fine. If a variant needs to mutate, point it at a stub — the question is "what should this look like", not "does the backend work".
- **Promoting the prototype directly to production.** The variant code was written under prototype constraints (no tests, minimal error handling). Rewrite it properly when you fold it in.
