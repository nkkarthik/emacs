
# user 
USERNAME=knannuru
USER_UID=1001
USER_GID=1001

groupmod --gid $USER_GID $USERNAME \
    && usermod --uid $USER_UID --gid $USER_GID $USERNAME \
    && chown -R $USER_UID:$USER_GID /home/$USERNAME \
    && chown -R $USER_UID:$USER_GID /home/.openvscode-server

# find docker group and add container user to that group
SOCK=/var/run/docker.sock
if [ -S "$SOCK" ]; then
    GID=$(stat -c '%g' $SOCK)
    groupadd -g "$GID" dockerhost || true
    usermod -aG dockerhost devuser
fi

PS1='\w: '

PATH=$HOME/bin:$PATH
PATH=$HOME/go/bin:$PATH

alias v=nvim
# start
export SHELL=fish
if [[ -v EMACS ]]; then
    exec gosu knannuru gotty --port 10003 -w emacs 
else
    exec gosu knannuru /home/.openvscode-server/bin/openvscode-server --port 30001 --connection-secret=/k/.openvscode-server-token
fi
