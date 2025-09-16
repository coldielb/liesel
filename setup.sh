#!/usr/bin/env bash
set -euo pipefail

# Styled logging helpers
info()  { printf '\033[1;34m[info]\033[0m %s\n' "$1"; }
warn()  { printf '\033[1;33m[warn]\033[0m %s\n' "$1"; }
error() { printf '\033[1;31m[fail]\033[0m %s\n' "$1" >&2; }
success(){ printf '\033[1;32m[done]\033[0m %s\n' "$1"; }

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"

require_file() {
    if [[ ! -f "$1" ]]; then
        error "Required file '$1' missing. Run from the repository root."
        exit 1
    fi
}

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

find_compiler() {
    if [[ -n "${CC:-}" ]] && command_exists "$CC"; then
        printf '%s' "$CC"
        return
    fi
    for candidate in cc clang gcc; do
        if command_exists "$candidate"; then
            printf '%s' "$candidate"
            return
        fi
    done
    error "No C compiler found (cc/clang/gcc). Install one and rerun."
    exit 1
}

build_interpreter() {
    mkdir -p "$PROJECT_ROOT/build"

    if command_exists make; then
        info "Building via make"
        if ! (cd "$PROJECT_ROOT" && make clean >/dev/null 2>&1); then
            warn "make clean failed (continuing with fresh build)"
        fi
        (cd "$PROJECT_ROOT" && make)
    else
        warn "'make' not found. Falling back to direct compilation."
        local cc
        cc=$(find_compiler)
        info "Compiling with $cc"
        local cflags=(-std=c17 -Wall -Wextra -Werror -pedantic -g -I"$PROJECT_ROOT/include")
        local ldlibs=(-lm)
        local sources=()
        local file
        shopt -s nullglob
        for file in "$PROJECT_ROOT"/src/*.c; do
            sources+=("$file")
        done
        shopt -u nullglob
        if [[ ${#sources[@]} -eq 0 ]]; then
            error "No source files found under src/."
            exit 1
        fi
        "$cc" "${cflags[@]}" "${sources[@]}" "${ldlibs[@]}" -o "$PROJECT_ROOT/build/liesel"
    fi
    success "Interpreter built"
}

select_install_paths() {
    INSTALL_BIN_DIR="/usr/local/bin"
    INSTALL_SHARE_DIR="/usr/local/share/liesel"

    if [[ ! -w "$INSTALL_BIN_DIR" || ! -w "/usr/local/share" ]]; then
        warn "Cannot write to /usr/local without elevated permissions."
        INSTALL_BIN_DIR="$HOME/.local/bin"
        INSTALL_SHARE_DIR="$HOME/.local/share/liesel"
        info "Using user-level installation at $HOME/.local"
    fi
}

prompt_yes_no() {
    local prompt="$1"
    local default="$2"
    local reply
    while true; do
        read -r -p "$prompt" reply || { printf '%s' "$default"; return; }
        if [[ -z "$reply" ]]; then
            reply="$default"
        fi
        local lowered
        lowered=$(printf '%s' "$reply" | tr '[:upper:]' '[:lower:]')
        case "$lowered" in
            y|yes) printf 'yes'; return ;;
            n|no) printf 'no'; return ;;
            *) echo "Please answer yes or no." ;;
        esac
    done
}

copy_runtime_assets() {
    local bin_dir="$1"
    local share_dir="$2"

    mkdir -p "$bin_dir" "$share_dir"

    if [[ -n "$share_dir" && "$share_dir" != "/" && -d "$share_dir" ]]; then
        info "Refreshing runtime assets under $share_dir"
        rm -rf "$share_dir"
        mkdir -p "$share_dir"
    fi

    cp "$PROJECT_ROOT/build/liesel" "$share_dir/liesel.bin"
    cp -R "$PROJECT_ROOT/libs" "$share_dir/"
    cp -R "$PROJECT_ROOT/docs" "$share_dir/docs"
    success "Assets installed to $share_dir"

    cat > "$bin_dir/liesel" <<EOFWRAP
#!/usr/bin/env bash
set -euo pipefail
INSTALL_BASE="$share_dir"
EXEC="\$INSTALL_BASE/liesel.bin"
if [[ ! -x "\$EXEC" ]]; then
    echo "Liesel binary missing at \$EXEC" >&2
    exit 1
fi
export LIESEL_HOME="\$INSTALL_BASE"
if [[ \$# -ge 1 ]] && { [[ "\$1" == "--help" ]] || [[ "\$1" == "-h" ]]; }; then
    cd "\$INSTALL_BASE"
    exec "\$EXEC" "\$@"
fi
if [[ \$# -lt 1 ]]; then
    echo "Usage: liesel <script.ls>" >&2
    exit 64
fi
ORIGINAL_PWD="\$(pwd)"
SCRIPT_PATH="\$1"
shift || true
if [[ "\$SCRIPT_PATH" != /* ]]; then
    SCRIPT_PATH="\$ORIGINAL_PWD/\$SCRIPT_PATH"
fi
cd "\$INSTALL_BASE"
exec "\$EXEC" "\$SCRIPT_PATH" "\$@"
EOFWRAP
    chmod +x "$bin_dir/liesel"
    success "Wrapper installed to $bin_dir/liesel"
}

detect_shell_rc() {
    local shell_name
    shell_name=$(basename "${SHELL:-}")
    case "$shell_name" in
        bash) printf '%s' "$HOME/.bashrc" ;;
        zsh) printf '%s' "$HOME/.zshrc" ;;
        fish) printf '%s' "$HOME/.config/fish/config.fish" ;;
        *) printf '%s' "" ;;
    esac
}

append_alias() {
    local bin_dir="$1"

    if [[ ! -t 0 ]]; then
        warn "Non-interactive shell detected; skipping alias setup."
        return
    fi

    local add_alias
    add_alias=$(prompt_yes_no "Would you like to add a short alias for Liesel? [y/N] " "n")
    if [[ "$add_alias" != "yes" ]]; then
        return
    fi

    read -r -p "Alias name (default: lsoul): " alias_name || true
    if [[ -z "$alias_name" ]]; then
        alias_name="lsoul"
    fi

    local rc_file
    rc_file=$(detect_shell_rc)
    if [[ -z "$rc_file" ]]; then
        warn "Could not determine shell configuration file. Add 'alias $alias_name="$bin_dir/liesel"' manually."
        return
    fi

    mkdir -p "$(dirname "$rc_file")"
    if [[ ! -f "$rc_file" ]]; then
        touch "$rc_file"
    fi

    if grep -F "alias $alias_name=" "$rc_file" >/dev/null 2>&1; then
        warn "Alias '$alias_name' already defined in $rc_file; leaving untouched."
        return
    fi

    case "$(basename "${SHELL:-}")" in
        fish)
            printf '\nalias %s "%s/liesel"\n' "$alias_name" "$bin_dir" >> "$rc_file"
            ;;
        *)
            printf '\nalias %s="%s/liesel"\n' "$alias_name" "$bin_dir" >> "$rc_file"
            ;;
    esac

    success "Alias '$alias_name' appended to $rc_file. Reload your shell to apply."
}

main() {
    require_file "$PROJECT_ROOT/Makefile"
    require_file "$PROJECT_ROOT/src/liesel.c"

    info "Starting Liesel setup"
    build_interpreter

    select_install_paths
    local bin_dir="$INSTALL_BIN_DIR"
    local share_dir="$INSTALL_SHARE_DIR"

    local bin_parent
    bin_parent=$(dirname "$bin_dir")
    local bin_needs_priv=0
    if [[ -d "$bin_dir" ]]; then
        if [[ ! -w "$bin_dir" ]]; then
            bin_needs_priv=1
        fi
    else
        if [[ -d "$bin_parent" && ! -w "$bin_parent" ]]; then
            bin_needs_priv=1
        fi
    fi

    if [[ $bin_needs_priv -eq 1 ]]; then
        warn "No write access to $bin_dir."
        if [[ $EUID -ne 0 ]]; then
            local choice
            choice=$(prompt_yes_no "Attempt to create $bin_dir with sudo? [y/N] " "n")
            if [[ "$choice" == "yes" ]]; then
                if ! command_exists sudo; then
                    error "sudo not available; rerun with elevated privileges or choose a writable directory."
                    exit 1
                fi
                sudo mkdir -p "$bin_dir"
                sudo chown "$USER" "$bin_dir"
            else
                read -r -p "Provide alternative bin directory: " alt_bin || true
                if [[ -z "$alt_bin" ]]; then
                    error "No bin directory provided. Aborting installation."
                    exit 1
                fi
                bin_dir="$alt_bin"
            fi
        fi
    fi

    local share_parent
    share_parent=$(dirname "$share_dir")
    local share_needs_priv=0
    if [[ -d "$share_dir" ]]; then
        if [[ ! -w "$share_dir" ]]; then
            share_needs_priv=1
        fi
    else
        if [[ -d "$share_parent" && ! -w "$share_parent" ]]; then
            share_needs_priv=1
        fi
    fi

    if [[ $share_needs_priv -eq 1 ]]; then
        if [[ -d "$share_dir" ]]; then
            warn "Insufficient permissions to overwrite $share_dir"
        fi
        if [[ $EUID -ne 0 ]]; then
            local choice
            choice=$(prompt_yes_no "Attempt to adjust permissions using sudo? [y/N] " "n")
            if [[ "$choice" == "yes" ]]; then
                if ! command_exists sudo; then
                    error "sudo not available; rerun with elevated privileges or choose a writable directory."
                    exit 1
                fi
                sudo mkdir -p "$share_dir"
                sudo chown "$USER" "$share_dir"
            else
                read -r -p "Provide alternative share directory: " alt_share || true
                if [[ -z "$alt_share" ]]; then
                    error "No share directory provided. Aborting installation."
                    exit 1
                fi
                share_dir="$alt_share"
            fi
        fi
    fi

    mkdir -p "$bin_dir" "$share_dir"
    copy_runtime_assets "$bin_dir" "$share_dir"
    append_alias "$bin_dir"

    success "Liesel installation complete. Binary available at $bin_dir/liesel"
    info "Ensure $bin_dir is on your PATH."
}

main "$@"
