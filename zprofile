[[ -z $DISPLAY && $XDG_VTNR -eq 1 && -f ~/.xinitrc ]] && exec startx
