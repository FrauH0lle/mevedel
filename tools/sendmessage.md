Queue one interim plain-text message for any retained agent in this root
session tree.

`target` accepts a canonical path such as `/root` or `/root/spec_review`, or a
relative descendant path beneath the caller. Sending never starts or resumes a
turn. The recipient receives the message before its next model sample, in FIFO
order with any other unread mail.

Success returns an empty result. Use FollowupAgent instead when the target must
start or steer work. Do not resend an agent's completed response when its
canonical RESULT delivery already carries that response. MAIL is interim and
may arrive in a later root turn; agents should put their final verdict in their
terminal response so it is delivered as RESULT.
