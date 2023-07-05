#!/usr/bin/zsh -l

xrdb -merge $HOME/.Xresources &
xmodmap "$HOME/.Xmodmap" &

# start some nice programs
hsetroot -solid '#002b36' &
xsetroot -cursor_name left_ptr &
xset -b -dpms s off s noblank &
setxkbmap -option compose:ralt &
tv off

# await startup processes to collect their deaths
wait

tv mirror

systemctl --user import-environment DISPLAY

## Uncomment to enable japanese language input via ibus
# export GTK_IM_MODULE=ibus
# export QT_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# ibus-daemon -drx

# Make Qt applications honor the dpi setting configured in Xresources
export QT_AUTO_SCREEN_SCALE_FACTOR=1

# $HOME/start_jack.sh

which ssh-agent >/dev/null && eval $(ssh-agent -s)

exec $HOME/.local/bin/xmonad
