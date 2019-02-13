# /etc/skel/.bash_profile
# This file is sourced by bash for login shells.

# General settings
export PATH=$HOME/bin:$HOME/.local/bin:$PATH
if [ "$(uname)" == "Darwin" ]; then
    export PATH=$HOME/Library/Python/3.7/bin:$PATH
fi

# For python encoding issue
# LC_CTYPE is sufficient?
export LANG=en_US.UTF-8

# clang
if $(type llvm-config > /dev/null 2>&1); then
    export LIBCLANG_PATH=$(llvm-config --libdir)
fi

# Rust settings
if [ -f $HOME/.cargo/env ]; then
    source $HOME/.cargo/env
fi
if $(type rustc > /dev/null 2>&1); then
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
    if [ ! -d $RUST_SRC_PATH ]; then
        unset RUST_SRC_PATH
    fi
fi

# Go settings
if [ -d $HOME/.go ]; then
    export GOPATH=$HOME/.go
    export PATH=$PATH:$GOPATH/bin
fi

# node.js
export PATH=~/.npm-global/bin:~/.nodebrew/current/bin:$PATH

# opam configuration
source $HOME/.opam/opam-init/init.sh > /dev/null 2>&1 || true

# The following line runs your .bashrc and is recommended by the bash
# info pages.
# If this shell is interactive mode, the following lines never return!
if [[ -f ~/.bashrc ]] ; then
	. ~/.bashrc
fi

