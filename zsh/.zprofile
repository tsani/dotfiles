export PATH=/usr/bin/vendor_perl:$HOME/bin:$HOME/.gem/ruby/2.2.0/bin:$HOME/.cabal/bin:/opt/android-sdk/platform-tools:$PATH
which vimb > /dev/null && export BROWSER=vimb || export BROWSER=firefox

if which nvim > /dev/null ; then
    if test -n "$NVIM_LISTEN_ADDRESS" ; then
        export EDITOR="nvr --remote"

        preexec () { nvr -c ":file \"$3\"" ; }
    else
        export EDITOR=nvim
    fi
else
    export EDITOR=vim
fi

export _JAVA_AWT_WM_NONREPARENTING=1
export MAGNET_DIR="$HOME/torrents/.watch"
export DMENU_OPTIONS="-dim 0.3 -fn 'DejaVu' -x 480 -y 270 -w 960 -l 15 -r -p '$ '"
export DOWNLOAD_DIR="$HOME/Downloads"
