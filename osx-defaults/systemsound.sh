#!/usr/bin/env bash

defaults write com.apple.systemsound com.apple.sound.beep.volume -float 0
#defaults write com.apple.systemsound com.apple.sound.uiaudio.enabled -int 0
defaults write -g com.apple.sound.beep.feedback -int 0
