#!/usr/bin/zsh -l

function trywallpaper () {
    while (( $# ))
    do
        test -e ~/.wallpaper/$1 && hsetroot -center ~/.wallpaper/$1 && return 0
        shift
    done

    hsetroot -solid black
}

xrdb -merge $HOME/.Xresources &
xmodmap "$HOME/.Xmodmap" &

# start some nice programs
mpd &
trywallpaper "835.png" "stallman.png" "ycombinator.png" "sofybakni.jpg" &
steam_wishlist_watcher.sh &
redshift &
zeno &
xsetroot -cursor_name left_ptr &
dunst &
(   which xautolock slock >/dev/null &&
    xautolock -time 15 -locker slock -notify 30 \
        -notifier 'notify-send "slock" "screen will soon lock"' -detectsleep
)

# run the window manager
if which ssh-agent >/dev/null ; then
    exec ssh-agent xmonad
else
    exec xmonad
fi
