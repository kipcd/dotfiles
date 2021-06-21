# This file is read when zsh is started in interactive shell

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    vi-mode
)

source $ZSH/oh-my-zsh.sh

# Fish-like syntax highlighting for shell input
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Init Node Version Manager
source /usr/share/nvm/init-nvm.sh

# set shortcut for reverse incremental search
bindkey "^R" history-incremental-pattern-search-backward

# start xbindkeys if installed
type -p xbindkeys > /dev/null && xbindkeys -f $HOME/.config/xbindkeys/.xbindkeysrc

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# An SSH agent prevents to enter the SSH private key passphrase each time the ssh command is invoqued.
#ssh_env=${XDG_CACHE_HOME:-$HOME}/.ssh-agent
#[ -f $ssh_env ] && . $ssh_env
#if ! ssh-add -l >/dev/null 2>&1 ;then
#  (umask 066; ssh-agent | sed '$ d' >! $ssh_env)
#  . $ssh_env
#  ssh-add
#fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases

# Verbose copy
alias cp='cp -v'

# dotfile management with bare git repository
alias dots='/usr/bin/git --git-dir=$HOME/.dotfiles --work-tree=$HOME $@'

# colored ls
alias lc='colorls -lA --sd'

# Use Emacs client instead of starting a new instance
# alias e='emacsclient --no-wait --create-frame'
alias e='emacs'
# alias et='emacsclient --tty'

# Scan a document
alias scan='scanimage --device="$SCANNER" --mode=color --resolution=300 --format=jpeg --output-file ~/Documents/scans/scan-$(timestamp).jpg --progress --verbose'

#ALIASES SDKMAN
alias sdklist='sdk list java'
alias sdkuse='sdk use java'
alias sdkcurrent='sdk current java'
alias sdkuse8='sdk use java 8.0.202-zulu'
alias sdkuse11='sdk use java 11.0.5.hs-adpt'
alias sdkusegrl='sdk use java 19.0.0-grl'

#ALIASES - K8s RELATED ALIASES
alias kx='kubectx'
alias klogs='kubectl logs -f'
alias kpods='kubectl get pods'
alias ksecrets='kubectl get secrets'
alias keditsecret='kubectl edit secret'

# WED aliases
[ -f $HOME/.wed-aliases ] && source $HOME/.wed-aliases

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
