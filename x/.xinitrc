#!/usr/bin/zsh -l

xrdb -merge $HOME/.Xresources &
xmodmap "$HOME/.Xmodmap" &

# start some nice programs
hsetroot -solid '#002b36' &
xsetroot -cursor_name left_ptr &
xset -b &
setxkbmap -option compose:ralt &

# await startup processes to collect their deaths
wait

which ssh-agent >/dev/null && eval $(ssh-agent -s)
exec $HOME/.local/bin/xmonad
