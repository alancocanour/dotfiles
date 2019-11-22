# Set theme to Arc-Dark only if it is installed
if [ -d "/usr/share/themes/Arc"] ; then
    export GTK_THEME=Arc-Dark
fi

exec xmonad
