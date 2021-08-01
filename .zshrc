### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

### Plugings Management
# syntax highlighting
zinit ice lucid wait='0' atinit='zpcompinit'
zinit light zdharma/fast-syntax-highlighting

# auto suggestion
zinit ice lucid wait="0" atload='_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions

# completions
zinit ice lucid wait='0'
zinit light zsh-users/zsh-completions

# 加载 OMZ 框架及部分插件
zinit snippet OMZ::lib/completion.zsh
zinit snippet OMZ::lib/history.zsh
zinit snippet OMZ::lib/key-bindings.zsh
zinit snippet OMZ::lib/theme-and-appearance.zsh
zinit snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh
zinit snippet OMZ::plugins/sudo/sudo.plugin.zsh
zinit snippet OMZ::plugins/command-not-found/command-not-found.plugin.zsh


zinit ice svn
zinit snippet OMZ::plugins/extract

zinit ice lucid wait='1'
zinit snippet OMZ::plugins/git/git.plugin.zsh

# lazy load pyenv
zinit ice wait='!0'
zinit light davidparsson/zsh-pyenv-lazy

# lazy load jenv
zinit ice wait='!0'
zinit light shihyuho/zsh-jenv-lazy

# defer load
zinit ice wait='!0'
zinit light romkatv/zsh-defer

zinit ice wait='!0'
zinit light paulirish/git-open
zinit ice wait='!0'
zinit snippet OMZ::plugins/autojump/autojump.plugin.zsh

# load theme -- pure
zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure

### End of Plugings Management

### Manage JDK Verions
# jdk versons
#export JAVA_11_HOME=/Library/Java/JavaVirtualMachines/jdk-11.0.10.jdk/Contents/Home
#export JAVA_8_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_281.jdk/Contents/Home

# set jek8 as default
#export JAVA_HOME=$JAVA_8_HOME

export PATH="$HOME/.jenv/bin:$PATH"
#eval "$(jenv init -)" # Managed by zsh-jenv-load now

# hotkeys to change jdk versions
#alias jdk8="export JAVA_HOME=$JAVA_8_HOME"
#alias jdk11="export JAVA_HOME=$JAVA_11_HOME"
### End of JDK Management

# suggestted by homebrew
export PATH="/usr/local/opt/ncurses/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/ncurses/lib"
export CPPFLAGS="-I/usr/local/opt/ncurses/include"
export PATH="/usr/local/sbin:$PATH"

### Config Pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

# Pyenv-virtualenv
# eval "$(pyenv init -)" # Managed by zsh-pyenv-lazy now
# eval "$(pyenv virtualenv-init -)"
export ZSH_PYENV_LAZY_VIRTUALENV=true

# fix the problem of pyenv about uninstallable in Big Sur
export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"

### End of Config Pyenv


### Cofig NVM
# Add default node to path
export PATH=$HOME/.nvm/versions/node/v14.17.3/bin:$PATH

# set path of nvm
export NVM_DIR="$HOME/.nvm"
# This loads nvm
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh" --no-use
# This loads nvm bash_completion
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm" --no-use

# place this after nvm initialization!
# autoload -U add-zsh-hook
# load-nvmrc() {
#   local node_version="$(nvm version)"
#   local nvmrc_path="$(nvm_find_nvmrc)"

#   if [ -n "$nvmrc_path" ]; then
#     local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

#     if [ "$nvmrc_node_version" = "N/A" ]; then
#       nvm install
#     elif [ "$nvmrc_node_version" != "$node_version" ]; then
#       nvm use
#     fi
#   elif [ "$node_version" != "$(nvm version default)" ]; then
#     echo "Reverting to nvm default version"
#     nvm use default
#   fi
# }
# add-zsh-hook chpwd load-nvmrc
# load-nvmrc

### End of Config NVM

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

### Manage Alias

# alias cls as clear
alias cls="clear"

# alias p as python3
alias p="python3"


# alias v/vim as nivm
alias vim="vim"
alias v="nvim"

# alias to delet extra latex output file
alias clean_file="python ~/Documents/Projects/Python/Del_aditional_tex_file/main.py"

# alias e as exit
alias e="exit"

# ls alias based on exa
alias l="exa"

# rm alias based on trash
alias rm="trash"

# alias shell script
alias update="sh ~/.dotfiles/Shells/update.sh"
alias backup="sh ~/.dotfiles/Shells/backup.sh"

# Hide Desktop files
alias hideDesktop="defaults write com.apple.finder CreateDesktop -bool FALSE; killall Finder"
alias showDesktop="defaults write com.apple.finder CreateDesktop -bool true; killall Finder"

### End of Alias Management

### Source Other Shells Files
source ~/.dotfiles/Shells/emacs-cmds.sh
