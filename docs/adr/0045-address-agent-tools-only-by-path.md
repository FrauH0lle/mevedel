# Address agent tools only by path

Status: accepted

Model-facing agent targets accept either a canonical path beginning at `/root` or a relative descendant path resolved beneath the caller. Cross-branch and ancestor addressing therefore uses canonical paths. Empty segments, `.`, `..`, malformed names, and unknown paths are rejected. Opaque invocation IDs remain internal persistence identities and are not accepted as an alternative model-facing address.
