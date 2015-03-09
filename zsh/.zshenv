LOCAL_ZSHENV="$HOME/.zshenv-local"

# Source machine-dependent configuration if any
test -e "$LOCAL_ZSHENV" && source "$LOCAL_ZSHENV"

export PATH=$HOME/.cabal/bin:/usr/lib/smlnj/bin:$PATH
