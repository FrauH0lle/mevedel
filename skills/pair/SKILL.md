---
name: pair
description: "Pair with a partner agent on a model you choose to carry out the given instructions as equals."
argument-hint: "[instructions]"
disable-model-invocation: true
allowed-tools:
  - Eval
---

# Pair

## Partner selection

The user selected the partner's model and reasoning effort:

```!el
(progn
  (require 'mevedel-models)
  (condition-case nil
      (let* ((candidates (or (mevedel-model-candidates)
                             (user-error "No registered gptel models")))
             (label (completing-read "Partner model: " candidates nil t))
             (model (plist-get (cdr (assoc label candidates)) :model))
             (efforts (mapcar #'symbol-name
                              (mevedel-model-supported-efforts model)))
             (effort (and efforts
                          (completing-read "Partner effort: "
                                           (cons "default" efforts)
                                           nil t nil nil "default"))))
        (list :model label
              :effort (and effort (not (equal effort "default"))
                           (intern effort))))
    (quit (user-error "Partner selection cancelled"))))
```

Launch the partner with the `Agent` tool: omit `role`, pass the selected
`:model` as `model`, and pass `:effort` as `effort` unless it is `nil`, in
which case omit `effort`. Give the partner the instructions and working
principles below in its `message`. Tell it that you are its partner and that
it reaches you with `SendMessage` without a target.

## Instructions

<instructions>
$ARGUMENTS
</instructions>

If the instructions block is empty, ask the user for the instructions and
wait for their answer before launching the partner.

## Working together

The partner is your equal, a second pair of eyes. Agree on a plan, divide the
work, share findings as you go, and keep the partner working throughout. Give
each of you separate files to edit, so neither overwrites the other.

When the task calls for judgement (a review, a diagnosis, a design choice),
form your views independently first and compare only after both of you are
done, so neither anchors on the other.

Settle disagreements with evidence: a test, a measurement or a source. When
evidence cannot settle one, ask the user. Verify rather than settle for the
first plausible answer.

Stay within the scope of the instructions and report what you find outside
it. Fix failing tests within that scope and report others. Preserve existing
features; removing one needs the user's confirmation.

The work is done when both of you agree the instructions are met and each has
checked the other's work. Then report the combined result to the user: what
you did or found, how it was verified, and what remains open.
