export NVM_DIR="$HOME/.nvm"

## Lazy-load nvm: sourcing nvm.sh eagerly runs its "auto use default version"
## logic on every shell startup, which is slow (pure-shell version resolution).
## Defer that cost to the first time nvm/node/npm/npx is actually invoked.

_nvm_lazy_load() {
    unset -f nvm node npm npx
    source "$HOMEBREW_PREFIX/opt/nvm/nvm.sh"
}

for _nvm_cmd in nvm node npm npx; do
    eval "${_nvm_cmd}() { _nvm_lazy_load; ${_nvm_cmd} \"\$@\"; }"
done
unset _nvm_cmd
