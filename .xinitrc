xrandr --setprovideroutputsource modesetting NVIDIA-0

xrandr --newmode 1920x1080_60.00 173.00 1920 2048 2248 2576 1080 1083 1088 1120 -hsync +vsync
xrandr --addmode eDP-1-1 1920x1080_60.00
xrandr --output eDP-1-1 --mode 1920x1080_60.00

xrandr --dpi 96

xrandr --auto

xrdb ~/.Xresources

# necessary to use windows key as prefix key (which is set in stumpwm config)
#xmodmap -e "clear mod4"
#xmodmap -e "keycode 133 = F20"
#
#stumpwm

exec bspwm

#setxkbmap -option 'ctrl:nocaps'

#exec gnome-session
