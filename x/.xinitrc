#!/usr/bin/zsh -l

function trywallpaper () {
    if ! test -e ~/.wallpaper ; then
        return 0
    fi

    find ~/.wallpaper -type f | shuf | head -n 1 | xargs hsetroot -center ||
    hsetroot -solid black
}

xrdb -merge $HOME/.Xresources &
xmodmap "$HOME/.Xmodmap" &

# start some nice programs
trywallpaper &
xsetroot -cursor_name left_ptr &
xset -b &

# await startup processes to collect their deaths
wait

which ssh-agent >/dev/null && eval $(ssh-agent -s)

exec $HOME/.local/bin/xmonad
