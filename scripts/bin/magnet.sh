#!/bin/bash

# Script to convert magnet links into torrents, saving them into a directory.
# The directory is configured by the environment variable MAGNET_DIR
# If this variable is not set, then the resulting torrents are saved to the
# current user's home.

set -e

if test -z "$MAGNET_DIR" ; then
    MAGNET_DIR="$HOME"
fi

[[ "$1" =~ xt=urn:btih:([^&/]+) ]] || \
    eval 'echo "not a magnet link >&2" ; exit 1'
echo "d10:magnet-uri${#1}:${1}e" > \
    "$MAGNET_DIR/meta-${BASH_REMATCH[1]}.torrent"
