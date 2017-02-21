#!/bin/bash

P=${XDG_DATA_HOME:-$HOME/.local/share}/pb-notify/notify-sfx.wav
exec paplay "$P"
