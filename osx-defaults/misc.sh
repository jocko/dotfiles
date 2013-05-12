#!/usr/bin/env bash

# Trackpad: enable tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true

# Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true

# Empirical studies have shown that these settings are optimal key repeat values
defaults write -g InitialKeyRepeat -int 14
defaults write -g KeyRepeat -int 2

# Use a 24-hour clock
defaults write com.apple.menuextra.clock DateFormat -string "EEE HH:mm"
