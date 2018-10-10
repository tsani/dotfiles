LOCAL_ZSHENV="$HOME/.zshenv-local"

# Source machine-dependent configuration if any
test -e "$LOCAL_ZSHENV" && source "$LOCAL_ZSHENV"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export MAGNET_DIR="$HOME/torrents/.watch"
export DOWNLOAD_DIR="$HOME/downloads"

export DMENU_OPTIONS="-dim 0.3 -fn 'DejaVu' -x 340 -y 192 -w 683 -l 15 -r -p '$ '"

which vimb > /dev/null && export BROWSER=vimb || export BROWSER=firefox

export PATH=$HOME/bin:$HOME/.local/bin:/usr/bin/vendor_perl:HOME/.gem/ruby/2.2.0/bin:$HOME/.cabal/bin:/opt/android-sdk/platform-tools:$PATH
export EDITOR=e
