# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    ant
    cabal
    cargo
    cp
    docker
    git
    gitignore
    gradle
    rsync
    rust
    stack
    sudo
    systemd
    z
)

source $ZSH/oh-my-zsh.sh

# User configuration

export EDITOR="emacs -nw"
export BROWSER="firefox"
export LESS="-iSR"

add_path_dir() {
  if [ -d "$1" ] ; then
    PATH="$1:$PATH"
  fi
}
add_path_dir "$HOME/bin"
add_path_dir "$HOME/.cabal/bin"
add_path_dir "$HOME/.cargo/bin"

alias e="$EDITOR"
alias ga="git annex"
alias pu="pushd"
alias po="popd"

if [ -d "/usr/share/info/" ] ; then
    export INFOPATH=/usr/share/info
fi

if [ -e "$HOME/.dir_colors" ] ; then
    eval `dircolors ~/.dir_colors`
fi

set HISTCONTROL=ignoreboth

if [ -e "$HOME/.zshrc_local" ] ; then
    source "$HOME/.zshrc_local"
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . /home/alan/.nix-profile/etc/profile.d/nix.sh
fi

if [ `whence -p direnv` ] ; then
    eval "$(direnv hook zsh)"
fi
