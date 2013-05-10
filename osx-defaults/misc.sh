#!/usr/bin/env bash

# Trackpad: enable tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true

# Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true
