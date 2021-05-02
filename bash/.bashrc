#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
alias config='usr/bin/git --git-dir=/home/denis/.dotfiles --work-tree=/home/denis'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/denis/.sdkman"
[[ -s "/home/denis/.sdkman/bin/sdkman-init.sh" ]] && source "/home/denis/.sdkman/bin/sdkman-init.sh"
