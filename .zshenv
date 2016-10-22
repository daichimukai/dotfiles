has() {
    type "$1" > /dev/null 2>&1
    return $?
}

if [ -z $TMUX ]; then
    path=(
	$HOME/bin(N-/)
	$HOME/.cask/bin(N-/)
	$path
    )
    if has ruby && [ ! $(has rbenv) ] ; then
	local gembin="$(ruby -e 'print Gem.user_dir')"
	path=(
	    $gembin/bin(N-/)
	    $path
	)
    fi
fi
