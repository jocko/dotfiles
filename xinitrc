. /etc/X11/xinit/xinitrc-common

xset r rate 200 30

#if [ -d /etc/X11/xinit/xinitrc.d ]; then
  #for f in /etc/X11/xinit/xinitrc.d/*; do
    #[ -x "$f" ] && . "$f"
  #done
  #unset f
#fi

exec i3
