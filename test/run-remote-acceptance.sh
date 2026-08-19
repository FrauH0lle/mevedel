#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture="$root/test/remote-target/Containerfile"
image=localhost/mevedel-remote-target:test
suffix=$$
ssh_container=mevedel-remote-ssh-$suffix
podman_container=mevedel-remote-podman-$suffix
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mevedel-remote-acceptance.XXXXXX")
ssh_workspace_volume="$scratch/ssh-workspace"
podman_workspace_volume="$scratch/podman-workspace"
client_home=$HOME
client_cache_home=${XDG_CACHE_HOME:-$client_home/.cache}
client_config_home=${XDG_CONFIG_HOME:-$client_home/.config}
client_data_home=${XDG_DATA_HOME:-$client_home/.local/share}
client_state_home=${XDG_STATE_HOME:-$client_home/.local/state}
grammar_file=

cleanup() {
    podman exec "$ssh_container" sh -c \
        'rm -rf /workspace/.[!.]* /workspace/*' >/dev/null 2>&1 || true
    podman exec "$podman_container" sh -c \
        'rm -rf /workspace/.[!.]* /workspace/*' >/dev/null 2>&1 || true
    podman rm --force "$ssh_container" >/dev/null 2>&1 || true
    podman rm --force "$podman_container" >/dev/null 2>&1 || true
    if [ -n "$grammar_file" ]; then
        rm -f -- "$grammar_file"
    fi
    rm -rf "$scratch"
}
trap cleanup EXIT HUP INT TERM

for command in podman ssh ssh-keygen; do
    command -v "$command" >/dev/null || {
        echo "Missing required command: $command" >&2
        exit 1
    }
done
ssh_path=$(command -v ssh)
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
mkdir "$ssh_workspace_volume" "$podman_workspace_volume"
cat >"$scratch/bin/podman" <<EOF
#!/bin/sh
export HOME="$client_home"
export XDG_CACHE_HOME="$client_cache_home"
export XDG_CONFIG_HOME="$client_config_home"
export XDG_DATA_HOME="$client_data_home"
export XDG_STATE_HOME="$client_state_home"
exec "$podman_path" "\$@"
EOF
chmod +x "$scratch/bin/podman"

podman build --tag "$image" --file "$fixture" "$root"

# The targets run Bubblewrap inside themselves.  Neither the default AppArmor
# profile nor the default seccomp filter lets a container create an
# unprivileged user namespace, which is the whole point of these targets, so
# both come off.
podman run --detach \
    --name "$ssh_container" \
    --security-opt apparmor=unconfined \
    --security-opt seccomp=unconfined \
    --publish 127.0.0.1::22 \
    --volume "$scratch/id_ed25519.pub:/home/mevedel/.ssh/authorized_keys:ro" \
    --volume "$ssh_workspace_volume:/workspace" \
    "$image" >/dev/null
podman run --detach \
    --name "$podman_container" \
    --security-opt apparmor=unconfined \
    --security-opt seccomp=unconfined \
    --volume "$podman_workspace_volume:/workspace" \
    "$image" >/dev/null

port=$(podman port "$ssh_container" 22/tcp)
port=${port##*:}
cat >>"$scratch/ssh-config" <<EOF

Host mevedel-acceptance-alias-a
    HostName 127.0.0.1
    Port $port

Host mevedel-acceptance-alias-b
    HostName 127.0.0.1
    Port $port
EOF
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

for container in "$ssh_container" "$podman_container"; do
    podman exec "$container" bwrap --new-session --die-with-parent \
        --ro-bind / / --dev /dev --unshare-user --unshare-pid --unshare-net \
        -- true
done

cd "$root"
npx @emacs-eask/cli clean elc
if ! npx @emacs-eask/cli emacs --batch --eval \
    "(kill-emacs (if (treesit-language-available-p 'bash) 0 1))"; then
    npx @emacs-eask/cli emacs --batch --eval \
        "(progn (setq treesit-language-source-alist '((bash \"https://github.com/tree-sitter/tree-sitter-bash\" \"v0.25.1\"))) (treesit-install-language-grammar 'bash))"
    grammar_file=$(find "$root/.eask" -type f -name 'libtree-sitter-bash.so' -print -quit)
    test -n "$grammar_file"
fi
export PATH="$scratch/bin:$PATH"
export MEVEDEL_TEST_SSH_CONFIG="$scratch/ssh-config"
export MEVEDEL_TEST_SSH_ROOT="/ssh:mevedel@127.0.0.1#$port:/workspace/"
export MEVEDEL_TEST_SSH_ALIAS_A_ROOT="/ssh:mevedel@mevedel-acceptance-alias-a:/workspace/"
export MEVEDEL_TEST_SSH_ALIAS_B_ROOT="/ssh:mevedel@mevedel-acceptance-alias-b:/workspace/"
export MEVEDEL_TEST_PODMAN_ROOT="/podman:$podman_container:/workspace/"
export MEVEDEL_TEST_PODMAN_IMAGE="$image"
export MEVEDEL_TEST_PODMAN_VOLUME="$podman_workspace_volume"
echo "mevedel: remote acceptance diagnostic: the Docker selector is not provisioned here; its journeys skip, and genuine Docker Engine evidence is external."
echo "mevedel: remote acceptance diagnostic: all aliases and independent clients share this host and container route; physical second-host and distinct-route evidence is external."
echo "mevedel: remote acceptance diagnostic: ordinary entry and Plan Worktree selection use callable command seams; rendered UI keypress automation is external."
if [ -n "${MEVEDEL_TEST_REMOTE_TEST:-}" ]; then
    selector_home="$scratch/selector-home"
    mkdir -p "$selector_home/.cache" "$selector_home/.config" \
        "$selector_home/.local/share" "$selector_home/.local/state"
    HOME="$selector_home" \
    XDG_CACHE_HOME="$selector_home/.cache" \
    XDG_CONFIG_HOME="$selector_home/.config" \
    XDG_DATA_HOME="$selector_home/.local/share" \
    XDG_STATE_HOME="$selector_home/.local/state" \
    npx @emacs-eask/cli emacs --batch \
        -l test/test-mevedel-execution-remote.el \
        --eval '(progn (message "mevedel: running selected remote journey")
                       (test-mevedel-execution-remote-run-selector))'
else
    npx @emacs-eask/cli test ert test/test-mevedel-execution-remote.el
fi
