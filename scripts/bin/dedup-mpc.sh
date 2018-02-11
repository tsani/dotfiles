#!/bin/bash

mpc playlist | nl | sort -k 2 | uniq -f 1 -d | sort -nr | cut -f 1 | xargs mpc del
