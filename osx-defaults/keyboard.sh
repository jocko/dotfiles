#!/usr/bin/env bash

# Disables the caps lock key (I want this mapped to escape instead!)
#
# Caveat:
# The last part of the modifiermapping key below consist of the vendor id (1452 is Apple) 
# and product id of the keyboard (595 is Apple Internal Keyboard / Trackpad). These values
# might obviously differ between different computers. The vendor and product id for your 
# keyboard can be obtained via ioreg -n IOHIDKeyboard -r'.
defaults -currentHost write -g com.apple.keyboard.modifiermapping.1452-595-0 -array '<dict><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer><key>HIDKeyboardModifierMappingDst</key><integer>-1</integer></dict>'