#!/bin/sh
# One fail-fast entry point for CI and local pre-commit checks.
set -eu

# Nested standalone Eask invocations must start as Eask, not plain Node.
unset PKG_EXECPATH

python3 scripts/check-standards.py
eask clean elc
eask compile --strict
# The existing runner cleans the compiled files before loading test sources.
eask run script test -- "$@"
