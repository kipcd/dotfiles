#!/bin/sh

# Fire it up
# -mm : maximize Emacs window on start
# Wrapping into dbus session, to have access to Linux desktop messages that allows programs to communicate between each other (IPC bus), etc.
# -l means after loading emacs execute specified script, in our case the configuration of exwm package
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/dotfiles/emacs/.emacs.default/exwm/desktop.el
