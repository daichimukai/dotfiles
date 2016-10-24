# do not read /etc/zprofile, /etc/zshrc, /etc/zlogin
setopt no_global_rcs

has() {
    type "$1" > /dev/null 2>&1
    return $?
}

if [ -z $TMUX ]; then
    path=(
	$HOME/bin(N-/)
	$HOME/.cask/bin(N-/)
	/usr/local/bin(N-/)
	$path
    )
    # env
    if has pyenv; then eval "$(pyenv init -)"; fi
    if has pyenv-virtualenv-init; then eval "$(pyenv virtualenv-init -)"; fi
    if has rbenv; then eval "$(rbenv init -)"; fi
    if has ruby && [ ! $(has rbenv) ] ; then
	local gembin="$(ruby -e 'print Gem.user_dir')"
	path=(
	    $gembin/bin(N-/)
	    $path
	)
    fi
fi
