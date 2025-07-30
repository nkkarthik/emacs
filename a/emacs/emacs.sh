#!/bin/bash

# find docker group and add container user to that group
SOCK=/var/run/docker.sock
if [ -S "$SOCK" ]; then
    GID=$(stat -c '%g' $SOCK)
    groupadd -g "$GID" dockerhost || true
    usermod -aG dockerhost knannuru
fi

chown -R knannuru:knannuru /home/knannuru/quicklisp

export DISPLAY=:100
export QT_X11_NO_MITSHM=1
export WEBKIT_DISABLE_COMPOSITING_MODE=1

# Start Xvfb for headless operation
# Xvfb :100 -screen 0 1920x1080x24 &
Xvfb :100 -screen 0 6016x3384x24 &
sleep 2

# Start Xpra server with Emacs + EAF
export USER=knannuru
export HOME=/home/knannuru
export SHELL=/usr/bin/fish
cd $HOME
#    exec gosu knannuru gotty --port 1111 -w emacs 
exec gosu knannuru xpra start :100 \
    --sharing=yes \
    --desktop-scaling=2 \
    --dpi=218 \
    --video-encoders=none \
    --encodings=jpeg,rgb \
    --min-quality=90  --min-speed=30 \
    --start-child="emacs --load /emacs/emacs.el" \
    --bind-tcp=0.0.0.0:1111 \
    --html=on \
    --keyboard-layout=us \
    --keyboard-layouts=us \
    --swap-keys=no \
    --daemon=no \
    --exit-with-children \
    --no-pulseaudio \
    --no-mdns \
    --no-notifications \
    --no-system-tray \
    --no-cursors \
    --tray=no \
    --notifications=no \
    --system-tray=no \
    --bell=no \
    --cursors=no \
    --pulseaudio=no \
    --mdns=no \

#    --clipboard=no \
#    --desktop-scaling=off &

# Wait for Xpra to start
# sleep 5

# Keep container running
echo "What???"
echo "Emacs with EAF is now available at http://localhost:1111"
echo "What???"
# wait

