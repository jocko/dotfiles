#!/usr/bin/env bash

# New tabs open with empty page
defaults write com.apple.Safari NewTabBehavior -int 1

# Set Safari’s home page
defaults write com.apple.Safari HomePage -string "www.google.com"

# Prevent Safari from opening ‘safe’ files automatically after downloading
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

# Enable Safari’s debug menu
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

# Enable the Develop menu and the Web Inspector in Safari
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

# Show status bar
defaults write com.apple.Safari ShowStatusBar -bool TRUE