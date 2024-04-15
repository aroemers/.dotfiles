## Add sbin path used by brew

export PATH="/usr/local/sbin:$PATH"

## Load jenv, install using brew

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

## Git prompt

autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats       '%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:*' enable git

vcs_info_wrapper() {
    vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
    fi
}

## Set clean custom prompt

setopt prompt_subst
NEWLINE=$'\n'
PROMPT='$NEWLINE%F{yellow}%2~%f $(vcs_info_wrapper)$NEWLINE%(?.🚀.💥) > '

## Colors in ls

export CLICOLOR=1

## Aliases

alias ll='ls -l'

## Use own bin directory

export PATH="$HOME/bin:$PATH"

## Don't send analytics with brew

export HOMEBREW_NO_ANALYTICS="true"

## Fix GPG "inappropriate ioctl for device"

export GPG_TTY=$(tty)

## Simple notification for long running tasks

function nd() {
    osascript -e "display notification \"Your task $1 is done\" with title \"Task done\" sound name \"Glass\""
}

## Save more than default history

export HISTSIZE=10000
export SAVEHIST=10000
setopt SHARE_HISTORY

## Load z, installed with root Brewfile
source /usr/local/etc/profile.d/z.sh

## Load fzf, installed with root Brewfile
export FZF_TMUX_OPTS='-p70%'
eval "$(fzf --zsh)"

## Load scripts in .zsh.d

for file in ~/.zsh.d/*.sh(N); do
    source "$file"
done
