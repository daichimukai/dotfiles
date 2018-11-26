# do not read /etc/zprofile, /etc/zshrc, /etc/zlogin
# setopt no_global_rcs

has() {
    type "$1" > /dev/null 2>&1
    return $?
}

export GOPATH=$HOME/.go
source $HOME/.cargo/env
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

path=(
    $HOME/bin(N-/)
    $HOME/.cask/bin(N-/)
    $HOME/.local/bin(N-/)
    /usr/local/bin(N-/)
    $GOPATH/bin(N-/)
    $path
)

if has ruby && [ ! $(has rbenv) ] ; then
    local gembin="$(ruby -e 'print Gem.user_dir')"
    path=(
        $gembin/bin(N-/)
        $path
)
fi
if [ -z $TMUX ]; then
    if [[ "${OSTYPE}" =~ darwin* ]]; then path=( /Library/TeX/texbin(N-/) $path ) fi
    export EDITOR=vim

    if [[ "${OSTYPE}" =~ darwin* ]]; then
        export LANG=en_US.UTF-8
        export LC_ALL=$LANG
    fi

    # env
    if has pyenv; then eval "$(pyenv init -)"; fi
    if has pyenv-virtualenv-init; then eval "$(pyenv virtualenv-init -)"; fi
    if has rbenv; then eval "$(rbenv init -)"; fi
fi
