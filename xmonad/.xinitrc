# Set theme to Arc-Dark only if it is installed
if [ -d "/usr/share/themes/Arc"] ; then
    export GTK_THEME=Arc-Dark
fi

XINITRC_LOCAL="$HOME/.xinitrc_local"
if [ -e "$XINITRC_LOCAL" ] ; then
    source $XINITRC_LOCAL
fi

exec xmonad
