#!/bin/sh
set -eu

if [ -n "$EVENT_BASE" ] && ! echo "$EVENT_BASE" | grep -Eq '^0+$'; then
    # A force-push can leave the previous tip outside every fetched ref.
    if ! git cat-file -e "$EVENT_BASE^{commit}" 2>/dev/null; then
        git fetch --no-tags origin "$EVENT_BASE"
    fi
    echo "CHECK_BASE=$EVENT_BASE" >> "$GITHUB_ENV"
else
    echo "CHECK_BASE=origin/$DEFAULT_BRANCH" >> "$GITHUB_ENV"
fi
