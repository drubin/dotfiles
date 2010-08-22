#!/bin/bash

xrdb -merge "$HOME/.Xresources"

xsetroot -solid black

trayer --edge top --align right \
	--widthtype percent --width 10 \
	--heighttype pixel --height 17 \
	--SetDockType true --SetPartialStrut false \
	--transparent true --alpha 0 --tint 000000 \
	--expand true &

xscreensaver &

xmonad &

# Useful applications
nautilus --no-default-window &
gnome-power-manager &
gnome-settings-daemon &
/usr/lib/gnome-session/helpers/gnome-settings-daemon-helper &
seahorse-daemon &
gnome-keyring-daemon &
nm-applet &
synergyc waffles &
dropbox start &
xterm &

export GNOME-KEYRING-SOCKET
export GNOME-KEYRING-PID
export `gnome-keyring-daemon -s -c ssh,pkcs11`
wait %xmonad
