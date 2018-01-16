#!/bin/bash

set -e

# use BROWSER hack to avoid error about BROWSER being set
BROWSER= xdg-settings set default-web-browser vimb.desktop
xdg-mime default feh.desktop 'image/*'
