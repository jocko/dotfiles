#!/usr/bin/env bash

# Disable: Mission Control, ctrl+up arrow
#/usr/libexec/PlistBuddy -c "Set :AppleSymbolicHotKeys:32:enabled bool false" ~/Library/Preferences/com.apple.symbolichotkeys.plist

# Disable: Show Help menu, shift+cmd+/
/usr/libexec/PlistBuddy -c "Set :AppleSymbolicHotKeys:98:enabled bool false" ~/Library/Preferences/com.apple.symbolichotkeys.plist > /dev/null 2>&1 || /usr/libexec/PlistBuddy -c "Add :AppleSymbolicHotKeys:98:enabled bool false" ~/Library/Preferences/com.apple.symbolichotkeys.plist

# Custom: Move focus to next window, cmd+§
/usr/libexec/PlistBuddy -c "Delete :AppleSymbolicHotKeys:27" ~/Library/Preferences/com.apple.symbolichotkeys.plist > /dev/null 2>&1
/usr/libexec/PlistBuddy -c "Add :AppleSymbolicHotKeys:27 dict" ~/Library/Preferences/com.apple.symbolichotkeys.plist
/usr/libexec/PlistBuddy -c "Merge cmd-paragraph-hotkey.plist :AppleSymbolicHotKeys:27 dict" ~/Library/Preferences/com.apple.symbolichotkeys.plist
