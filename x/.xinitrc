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
trywallpaper "stallman.png" "ycombinator.png" "sofybakni.jpg" &
compton &
steam_wishlist_watcher.sh &
xscreensaver -nosplash &
redshift &
dunst &
eval $('ssh-agent')

# run the window manager
exec xmonad
