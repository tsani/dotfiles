#!/bin/bash

mailcheck -bs | grep new | xargs -I "{}" notify-send "Mail" "{}"
