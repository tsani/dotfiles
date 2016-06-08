#!/usr/bin/zsh -l

function trywallpaper () {
    while (( $# ))
    do
        test -e ~/.wallpaper/$1 && hsetroot -center ~/.wallpaper/$1 && return 0
        shift
    done

    hsetroot -solid black
}

xrdb -merge $HOME/.Xresources

usermodmap=$HOME/.Xmodmap # swap escape and caps

xmodmap "$usermodmap" &

xsetroot -cursor_name left_ptr &

# start some nice programs
mpd &
trywallpaper "835.png" "stallman.png" "ycombinator.png" "sofybakni.jpg" &
compton --no-fading-openclose &
steam_wishlist_watcher.sh &
redshift &
dunst &
(   which xautolock slock >/dev/null &&
    xautolock -time 15 -locker slock -notify 30 \
        -notifier 'notify-send "slock" "screen will soon lock"' -detectsleep
) &
eval $('ssh-agent')

# run the window manager
exec xmonad
