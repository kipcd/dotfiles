# Follow best practice by setting your environment variables
# so that they are available to both interactive and non-interactive shells.
# In practical terms, for most people this means
# setting them in ~/.profile, ~/.bash_profile, ~/.zshenv
# instead of ~/.bashrc and ~/.zshrc
export PATH="$HOME/.sdkman/candidates/maven/current/bin:$HOME/.sdkman/candidates/java/current/bin:/home/denis/.nvm/versions/node/v12.18.3/bin:/usr/local/bin:/usr/bin:/bin:$HOME/.local/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/local/go/bin:$HOME/dev/jmeter/bin:$HOME/wed/selenium:$HOME/doom-emacs/bin"

export DEVELOPER_KIT_HOME="$HOME/wed/data/ws/developer-kit"

# To enable the keyring for applications run through the terminal, such as SSH
# if [ -n "$DESKTOP_SESSION" ];then
#     eval $(gnome-keyring-daemon --start)
#     export SSH_AUTH_SOCK
# fi

export SCRIPTS=$HOME/.config/scripts
export SCANNER=escl:https://192.168.0.14:443
timestamp() {
    date +%F-%H-%M-%S
}

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR='emacs'
   # export EDITOR='emacsclient --no-wait --create-frame'
 fi

# Run terminal Emacs client for editing git messages
# export VISUAL='emacsclient --tty'
# export VISUAL='emacsclient --create-frame'
 export VISUAL='emacs'

# Emacs daemon instance name from systemd --user service
# export EMACS_SOCKET_NAME=instance1

export JAVA_HOME="$HOME/.sdkman/candidates/java/current"
