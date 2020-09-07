XINITRC_LOCAL="$HOME/.xinitrc_local"
if [ -e "$XINITRC_LOCAL" ] ; then
    source $XINITRC_LOCAL
fi

exec xmonad
