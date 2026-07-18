Queue one plain-text message for any retained agent in this root session tree.

`target` accepts a canonical path such as `/root` or `/root/spec_review`, or a
relative descendant path beneath the caller. Sending never starts or resumes a
turn. The recipient receives the message before its next model sample, in FIFO
order with any other unread mail.

Success returns an empty result. Use FollowupAgent instead when the target must
start or steer work.
