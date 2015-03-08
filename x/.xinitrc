#!/bin/bash

xrdb -merge $HOME/.Xresources

usermodmap=$HOME/.Xmodmap # swap escape and caps

xmodmap "$usermodmap" &

xsetroot -cursor_name left_ptr &

# start some nice programs
mpd &
hsetroot -full ~/.wallpaper/moon_by_kpx_beatrix-d7pmpj0.png
compton &
steam_wishlist_watcher.sh &
xscreensaver -nosplash &
redshift &

# run the window manager
exec xmonad
