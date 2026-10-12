#!/usr/bin/env bash
# Build the prebuilt native module for ARCH into OUT-DIR.
#
# Runs on an older distribution (CI uses Ubuntu 22.04) so the module links
# against widely available GTK 3, Wayland and glibc versions, and against its
# older emacs-module.h (Emacs 27).  Emacs only grows the module environment and
# a module checks only that it got at least the size it was compiled for, so
# an older header's module loads into Emacs 31 and later; the module uses only
# functions Emacs 25 already had.  A newer header's would be refused by older
# Emacsen.
# Writes mevedel-native-ARCH.so and its .sha256 file.
set -euo pipefail
arch=$1
out=$2
here=$(cd "$(dirname "$0")" && pwd)
header=$(find /usr/include /usr/local/include -name emacs-module.h -print -quit 2>/dev/null || true)
if [ -z "$header" ]; then
  echo "emacs-module.h not found; install the Emacs development files" >&2
  exit 1
fi
mkdir -p "$out"
cc -shared -fPIC -O2 -Wall -Wextra -Werror \
   -I"$(dirname "$header")" \
   "$here"/*.c -o "$out/mevedel-native-$arch.so" \
   $(pkg-config --cflags --libs gtk+-3.0 wayland-client) -lm
strip --strip-unneeded "$out/mevedel-native-$arch.so"
(cd "$out" && sha256sum "mevedel-native-$arch.so" > "mevedel-native-$arch.so.sha256")
# The hash `mevedel-native--source-hash' computes: all sources in byte order.
echo "source $(cd "$here" && LC_ALL=C ls ./*.c | LC_ALL=C sort | xargs cat | sha256sum | cut -c1-16)"
cat "$out/mevedel-native-$arch.so.sha256"
