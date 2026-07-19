# Address agents by canonical task path

Status: accepted

`Agent` requires a caller-supplied local `task_name` made from lowercase ASCII letters, digits, and underscores. Mevedel joins it to the parent's path to form a stable canonical address such as `/root/spec_review`; that path remains reserved until the root session is deleted. The existing opaque invocation ID remains the storage identity. Mevedel will not generate task names or add a separate nickname system.
