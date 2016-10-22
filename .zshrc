bindkey -e

autoload -Uz compinit; compinit
autoload -Uz colors; colors
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

setopt auto_cd
setopt auto_pushd
setopt extended_glob
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt share_history
setopt prompt_subst

export SAVEHIST=100000
export HISTSIZE=1000
export HISTFILE=${HOME}/.zsh_history

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

zstyle ':completion:*:default' menu select=1

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b]' '%m' '<!%a>'
zstyle ':vcs_info:git:*' formats '(%s)-[%b]' '%c%u%m'
zstyle ':vcs_info:git:*' actionformats '(%s)-[%b]' '%c%u%m' '<!%a>'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "!"
zstyle ':vcs_info:git:*' unstagedstr "+"
zstyle ':vcs_info:git+set-message:*' hooks \
       git-hook-begin \
       git-untracked \
       git-push-status \
       git-nomerge-branch \
       git-stash-count

PROMPT="%{${fg[magenta]}%}[%n@%M, %y, %h]%{${reset_color}%}
%{${fg[cyan]}%}%~/%f%{${reset_color}%} %# "

function +vi-git-hook-begin() {
    if [[ $(command git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
        return 1
    fi

    return 0
}

function +vi-git-untracked() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi

    if command git status --porcelain 2> /dev/null \
            | awk '{print $1}' \
            | command grep -F '??' > /dev/null 2>&1 ; then
        hook_com[unstaged]+='?'
    fi
}

function +vi-git-push-status() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi

    if [[ "${hook_com[branch]}" != "master" ]]; then
        return 0
    fi

    local ahead
    ahead=$(command git rev-list origin/master..master 2>/dev/null \
                   | wc -l \
                   | tr -d ' ')

    if [[ "$ahead" -gt 0 ]]; then
        hook_com[misc]+="(p${ahead})"
    fi
}

function +vi-git-nomerge-branch() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi

    if [[ "${hook_com[branch]}" == "master" ]]; then
        return 0
    fi

    local nomerged
    nomerged=$(command git rev-list master..${hook_com[branch]} 2>/dev/null | wc -l | tr -d ' ')

    if [[ "$nomerged" -gt 0 ]] ; then
        hook_com[misc]+="(m${nomerged})"
    fi
}


function +vi-git-stash-count() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi

    local stash
    stash=$(command git stash list 2>/dev/null | wc -l | tr -d ' ')
    if [[ "${stash}" -gt 0 ]]; then
        hook_com[misc]+=":S${stash}"
    fi
}

function _update_vcs_info_msg() {
    local -a messages
    local prompt

    LANG=en_US.UTF-8 vcs_info

    if [[ -z ${vcs_info_msg_0_} ]]; then
        prompt=""
    else
        [[ -n "$vcs_info_msg_0_" ]] && messages+=( "%F{green}${vcs_info_msg_0_}%f" )
        [[ -n "$vcs_info_msg_1_" ]] && messages+=( "%F{yellow}${vcs_info_msg_1_}%f" )
        [[ -n "$vcs_info_msg_2_" ]] && messages+=( "%F{red}${vcs_info_msg_2_}%f" )
    fi

    prompt="${(j: :)messages}"

    RPROMPT="${prompt}"
}
add-zsh-hook precmd _update_vcs_info_msg

# hash
hash -d db=$HOME/Dropbox
hash -d dl=$HOME/Downloads

# alias
alias be="bundle exec"
alias ls="ls -G"

# env
#if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
