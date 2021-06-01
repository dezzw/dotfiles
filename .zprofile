### Manage JDK Verions
export JAVA_11_HOME=/Library/Java/JavaVirtualMachines/jdk-11.0.10.jdk/Contents/Home
export JAVA_8_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_281.jdk/Contents/Home

export JAVA_HOME=$JAVA_8_HOME

alias jdk8="export JAVA_HOME=$JAVA_8_HOME"
alias jdk11="export JAVA_HOME=$JAVA_11_HOME"

# alias c as clear
alias c="clear"

# alias p as python3
alias p="python3"

# alias ls as ls -a -l
alias ls="ls -a -l"

# alias v/vim as nivm
alias vim="nvim"
alias v="nvim"

# alias for emacs
# alias emacs='emacs -nw'
alias kille="emacsclient -e '(kill-emacs)'"
alias ec='emacsclient -c -a ""'
alias sec='sudo emacsclient -a ""'
alias dump="~/Documents/dotfiles/dump.sh"

# alias to delet extra latex output file
alias clean_file="python ~/Documents/Projects/Python/Del_aditional_tex_file/main.py"

# alias e as exit
alias e="exit"

# alias shell script
alias update="~/Documents/dotfiles/update.sh"

# Hide Desktop files
alias hideDesktop="defaults write com.apple.finder CreateDesktop -bool FALSE; killall Finder"
alias showDesktop="defaults write com.apple.finder CreateDesktop -bool true; killall Finder"

# suggestted by homebrew
export PATH="/usr/local/opt/ncurses/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/ncurses/lib"
export CPPFLAGS="-I/usr/local/opt/ncurses/include"
export PATH="/usr/local/sbin:$PATH"

# Pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# Pyenv-virtualenv
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# fix the problem of pyenv about uninstallable in Big Sur
export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"

# nvm
export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# setting for vterm in emacs
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

