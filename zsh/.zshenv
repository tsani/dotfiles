LOCAL_ZSHENV="$HOME/.zshenv-local"

# Source machine-dependent configuration if any
test -e "$LOCAL_ZSHENV" && source "$LOCAL_ZSHENV"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export MAGNET_DIR="$HOME/torrents/.watch"
export DMENU_OPTIONS="-dim 0.3 -fn 'DejaVu' -x 480 -y 270 -w 960 -l 15 -r -p '$ '"
export DOWNLOAD_DIR="$HOME/Downloads"
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

export VIMB_USER_AGENT="Mozilla/5.0 (X11; Linux x86_64; rv:50.0) Gecko/50.0 Firefox/50.0"

which vimb > /dev/null && export BROWSER=vimb || export BROWSER=firefox

export PATH="$(/usr/local/bin/brew --prefix)/opt/python/libexec/bin:$(/usr/local/bin/brew --prefix)/opt/coreutils/libexec/gnubin:$HOME/bin:$HOME/.local/bin:/usr/bin/vendor_perl:HOME/.gem/ruby/2.2.0/bin:$HOME/.cabal/bin:/opt/android-sdk/platform-tools:$PATH"
export EDITOR=e

export SSDNS_PASSWORD_CMD='pass show drw/main'
. "$HOME/.cargo/env"
