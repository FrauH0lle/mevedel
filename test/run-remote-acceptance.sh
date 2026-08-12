#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture="$root/test/remote-target/Containerfile"
image=localhost/mevedel-remote-target:test
suffix=$$
docker_container=mevedel-remote-docker-$suffix
podman_container=mevedel-remote-podman-$suffix
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mevedel-remote-acceptance.XXXXXX")
client_home=$HOME
client_cache_home=${XDG_CACHE_HOME:-$client_home/.cache}
client_config_home=${XDG_CONFIG_HOME:-$client_home/.config}
client_data_home=${XDG_DATA_HOME:-$client_home/.local/share}
client_state_home=${XDG_STATE_HOME:-$client_home/.local/state}
grammar_file=

cleanup() {
    docker rm --force "$docker_container" >/dev/null 2>&1 || true
    podman rm --force "$podman_container" >/dev/null 2>&1 || true
    if [ -n "$grammar_file" ]; then
        rm -f -- "$grammar_file"
    fi
    rm -rf "$scratch"
}
trap cleanup EXIT HUP INT TERM

for command in docker podman ssh ssh-keygen; do
    command -v "$command" >/dev/null || {
        echo "Missing required command: $command" >&2
        exit 1
    }
done
ssh_path=$(command -v ssh)
docker_path=$(command -v docker)
podman_path=$(command -v podman)

ssh-keygen -q -t ed25519 -N '' -f "$scratch/id_ed25519"
cat >"$scratch/ssh-config" <<EOF
Host *
    BatchMode yes
    IdentityFile $scratch/id_ed25519
    StrictHostKeyChecking no
    UserKnownHostsFile /dev/null
    LogLevel ERROR
EOF
mkdir "$scratch/bin"
cat >"$scratch/bin/docker" <<EOF
#!/bin/sh
export HOME="$client_home"
export XDG_CACHE_HOME="$client_cache_home"
export XDG_CONFIG_HOME="$client_config_home"
export XDG_DATA_HOME="$client_data_home"
export XDG_STATE_HOME="$client_state_home"
exec "$docker_path" "\$@"
EOF
cat >"$scratch/bin/podman" <<EOF
#!/bin/sh
export HOME="$client_home"
export XDG_CACHE_HOME="$client_cache_home"
export XDG_CONFIG_HOME="$client_config_home"
export XDG_DATA_HOME="$client_data_home"
export XDG_STATE_HOME="$client_state_home"
exec "$podman_path" "\$@"
EOF
chmod +x "$scratch/bin/docker" "$scratch/bin/podman"

docker build --tag "$image" --file "$fixture" "$root"
docker save --output "$scratch/image.tar" "$image"
podman load --input "$scratch/image.tar"

docker run --detach \
    --name "$docker_container" \
    --publish 127.0.0.1::22 \
    --volume "$scratch/id_ed25519.pub:/home/mevedel/.ssh/authorized_keys:ro" \
    "$image" >/dev/null
podman run --detach \
    --name "$podman_container" \
    "$image" >/dev/null

port=$(docker port "$docker_container" 22/tcp)
port=${port##*:}
i=0
until "$ssh_path" -F "$scratch/ssh-config" \
    -p "$port" mevedel@127.0.0.1 true 2>/dev/null; do
    i=$((i + 1))
    if [ "$i" -ge 30 ]; then
        echo "SSH target did not become ready" >&2
        exit 1
    fi
    sleep 1
done

for target in "docker $docker_container" "podman $podman_container"; do
    set -- $target
    "$1" exec "$2" bwrap --new-session --die-with-parent \
        --ro-bind / / --dev /dev --unshare-user --unshare-pid --unshare-net \
        -- true
done

cd "$root"
if ! npx @emacs-eask/cli emacs --batch --eval \
    "(kill-emacs (if (treesit-language-available-p 'bash) 0 1))"; then
    npx @emacs-eask/cli emacs --batch --eval \
        "(progn (setq treesit-language-source-alist '((bash \"https://github.com/tree-sitter/tree-sitter-bash\" \"v0.25.1\"))) (treesit-install-language-grammar 'bash))"
    grammar_file=$(find "$root/.eask" -type f -name 'libtree-sitter-bash.so' -print -quit)
    test -n "$grammar_file"
fi
npx @emacs-eask/cli clean elc
PATH="$scratch/bin:$PATH" \
MEVEDEL_TEST_SSH_CONFIG="$scratch/ssh-config" \
MEVEDEL_TEST_SSH_CONTAINER="$docker_container" \
MEVEDEL_TEST_SSH_ROOT="/ssh:mevedel@127.0.0.1#$port:/workspace/" \
MEVEDEL_TEST_DOCKER_ROOT="/docker:$docker_container:/workspace/" \
MEVEDEL_TEST_PODMAN_ROOT="/podman:$podman_container:/workspace/" \
npx @emacs-eask/cli test ert test/test-mevedel-execution-remote.el
