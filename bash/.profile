#!/usr/bin/env bash

# Configure the screens available
if [ -f ~/shell-scripts/screen_layout.sh ]; then
  ~/scripts/screen_layout.sh
fi

if [ "${XDG_SESSION_DESKTOP}" == "awesome" ]; then
  nm-applet &
fi
