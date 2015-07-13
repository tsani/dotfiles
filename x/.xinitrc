#!/bin/zsh

xrdb -merge $HOME/.Xresources

usermodmap=$HOME/.Xmodmap # swap escape and caps

xmodmap "$usermodmap" &

xsetroot -cursor_name left_ptr &

# start some nice programs
mpd &
hsetroot -full ~/.wallpaper/ycombinator.png
compton &
steam_wishlist_watcher.sh &
xscreensaver -nosplash &
redshift &
eval 'ssh-agent -c'

# run the window manager
exec xmonad
