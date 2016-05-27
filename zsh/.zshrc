#Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git urltools archlinux bower cabal github pip python redis-cli screen stack thefuck web-search)

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
SOLARIZED_THEME=light
ZSH_THEME="blinks"

source $ZSH/oh-my-zsh.sh

unsetopt AUTO_CD

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

setopt BRACE_CCL

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Customize to your needs...
#export PYTHONPATH=/usr/lib/python3.3/site-packages

# Some stuff
bindkey -v
#bindkey -a u undo
#bindkey -a '^R' redo

# Fix home and end keys.
bindkey "[7~" beginning-of-line
bindkey "[8~" end-of-line
bindkey "[3~" delete-char

# History management
export HISTSIZE=1000
export SAVEHIST=1000
export HISTCONTROL=ignorespace:erasedups

function fuck() {
    if killall -9 "$2" ; then
        echo
        echo " (╯°□°）╯︵$(echo "$2"|toilet -f term -F rotate)"
        echo
    fi
}

source $HOME/.zsh_aliases

# enable neoman integration
source $HOME/.vim/bundle/neoman.vim/scripts/neovim.zsh

command -v fortune >/dev/null 2>&1 && fortune
if test -e ~/todo.txt
then
    echo
    cat ~/todo.txt
fi
