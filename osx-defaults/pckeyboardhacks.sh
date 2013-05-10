#!/usr/bin/env bash

# Map caps lock to escape
defaults write org.pqrs.PCKeyboardHack sysctl -dict enable_capslock -bool TRUE keycode_capslock -int 53
