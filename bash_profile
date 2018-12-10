# /etc/skel/.bash_profile
# This file is sourced by bash for login shells.

# General settings
export PATH=$HOME/bin:$PATH

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
export PATH=~/.npm-global/bin:$PATH

# The following line runs your .bashrc and is recommended by the bash
# info pages.
# If this shell is interactive mode, the following lines never return!
if [[ -f ~/.bashrc ]] ; then
	. ~/.bashrc
fi

