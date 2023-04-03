## Uncomment line below and line at bottom of this file to enable profiling.

#zmodload zsh/zprof


## Add sbin path used by brew

export PATH="/usr/local/sbin:$PATH"


## Load nvm, install using brew

function load_nvm() {
  export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
}

## Load jenv, install using brew

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"


## Load rbenv, install using brew

function load_rbenv() {
  eval "$(rbenv init -)"
}


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
PROMPT='%F{yellow}%3~%f $(vcs_info_wrapper)%(?.🚀.💥) '


## Load z, install using brew

. /usr/local/etc/profile.d/z.sh


## Load atuin, install using brew

export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"
bindkey '^r' _atuin_search_widget


## Aliases

# Install exa using brew
alias ll='exa -l --git'
alias cat='bat'
alias dotenv='env $(grep -v '^#' .env | xargs)'


## Load project specifics

for file in ~/.zsh.d/projects/*.zshrc; do
    source "$file"
done


## Use own bin directory

export PATH="$HOME/bin:$PATH"


## Don't send analytics with brew

export HOMEBREW_NO_ANALYTICS="true"


## Fix GPG "inappropriate ioctl for device"
export GPG_TTY=$(tty)


## Uncomment line below and line at top of this file to enable profiling

# zprof
