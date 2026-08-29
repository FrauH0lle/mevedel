# Parent Ask Sample Frames To The Top-Level Frame

An option sample renders in a child frame parented to the top-level Emacs frame,
never to the directive frame, even when the ask questionnaire is itself being
displayed inside a directive frame. A child frame is clipped at its parent's
native edges, and a directive frame is `mevedel-directive-frame-width` wide by
`mevedel-directive-frame-height` tall — so a nested sample frame would inherit a
ceiling of roughly eight usable lines at sixty percent width no matter how long
the artifact is, and `mevedel-directive-frame--fit-height' would size it to
content that then gets clipped.

Most window systems clip this way; NS builds do not, so on macOS a nested
sample would merely overflow rather than vanish.  The decision follows the
clipping majority so one layout is correct everywhere.

Parenting to the top-level frame makes the sample a sibling of the directive
frame rather than its child, so it may occlude the directive frame it belongs
to. That is the accepted cost: the sample is transient, the focused option stays
visible above it, and `mevedel-directive-frame--anchor' already flips a frame
above its anchor when there is no room below. Clipping has no such escape hatch.

The price is that anchor coordinates computed in a window inside the directive
frame must be translated into top-level frame coordinates.
`window-absolute-pixel-position' returns display coordinates while
`set-frame-position' on a child frame takes parent-relative ones, so that
translation is a subtraction of the parent's `frame-position'. It is not new
work: `mevedel-directive-frame--anchor' omits it today, which pins the directive
frame to the right edge whenever the Emacs frame does not sit at display origin.
The fix lands before sample frames are built.
