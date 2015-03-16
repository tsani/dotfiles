#!/bin/bash

# When copying with the cursor from vim, a lot of whitespace can get captured
# undesirably by indentation. This script kills duplicate whitespace and line
# breaks from the clipboard.

xclip -o | sed 's/ \+/ /g' | xargs | xclip -i
