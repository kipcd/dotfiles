# To enable the keyring for applications run through the terminal, such as SSH
if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

export SCRIPTS=$HOME/.config/scripts
export SCANNER=escl:https://192.168.0.14:443
timestamp() {
    date +%F-%H-%M-%S
}

# Emacs daemon instance name from systemd --user service
# export EMACS_SOCKET_NAME=instance1
