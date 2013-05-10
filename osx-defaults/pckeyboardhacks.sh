#!/usr/bin/env bash

# Map caps lock to escape
#
# Note that when remapping caps lock, we should also remove the corresponding modifier 
# key under System Preferences/Modifier Keys... (i.e. set Caps Lock to "No Action").
defaults write org.pqrs.PCKeyboardHack sysctl -dict enable_capslock -bool TRUE keycode_capslock -int 53
