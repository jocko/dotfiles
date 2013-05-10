#!/usr/bin/env bash

# No confirmation when quitting
defaults write com.googlecode.iterm2 PromptOnQuit -bool FALSE
# Not even if multiple sessions are open
defaults write com.googlecode.iterm2 OnlyWhenMoreTabs -bool FALSE
