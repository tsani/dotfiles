LOCAL_ZSHENV="$HOME/.zshenv-local"

# Source machine-dependent configuration if any
test -e "$LOCAL_ZSHENV" && source "$LOCAL_ZSHENV"

export PATH=$HOME/bin:$HOME/.cabal/bin:/usr/lib/smlnj/bin:$PATH
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
