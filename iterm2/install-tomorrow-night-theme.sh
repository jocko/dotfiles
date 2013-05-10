#!/usr/bin/env bash

# Install the theme (Tomorrow Night)
/usr/libexec/PlistBuddy -c "Add :'Custom Color Presets':'Tomorrow Night' dict" ~/Library/Preferences/com.googlecode.iterm2.plist
/usr/libexec/PlistBuddy -c "Merge 'Tomorrow Night.itermcolors' :'Custom Color Presets':'Tomorrow Night'" ~/Library/Preferences/com.googlecode.iterm2.plist

# Apply the theme (for the first profile)
#
# How iTerm handles this is fubar. Instead of having some kind of reference to
# the previously installed theme, we must copy each color key to the profile. 
# Also, as far as I know, there's no way to get the plist merge to overwrite
# existing keys which means that we first have to delete all the current color
# keys before we actually can apply the theme.
for color in "Ansi 0 Color" "Ansi 1 Color" "Ansi 2 Color" "Ansi 3 Color" "Ansi 4 Color" "Ansi 5 Color" "Ansi 6 Color" "Ansi 7 Color" "Ansi 8 Color" "Ansi 9 Color" "Ansi 10 Color" "Ansi 11 Color" "Ansi 12 Color" "Ansi 13 Color" "Ansi 14 Color" "Ansi 15 Color" "Background Color" "Bold Color" "Cursor Color" "Cursor Text Color" "Foreground Color" "Selected Text Color" "Selection Color"; do
  /usr/libexec/PlistBuddy -c "Delete :'New Bookmarks':0:'$color'" ~/Library/Preferences/com.googlecode.iterm2.plist
done

/usr/libexec/PlistBuddy -c "Merge 'Tomorrow Night.itermcolors' :'New Bookmarks':0" ~/Library/Preferences/com.googlecode.iterm2.plist