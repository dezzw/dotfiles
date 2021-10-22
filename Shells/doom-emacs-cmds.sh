#!/bin/bash

# Set up $DOOMDIR environment
export DOOMDIR="~/.dotfiles/Emacs/emacs-configs/doom-emacs"

# alais doom CLI path
alias doom="~/.dotfiles/Emacs/emacs-configs/doom-core/bin/doom"

# interact with `emacs-cmds.sh` to control emacsclients
# emacsclient: doom (doom-emacs configurations)
function edm() {
    if [[ $(uname) == "Linux" ]]; then
        EMACSCLIENT="/usr/bin/emacsclient"
    else
        EMACSCLIENT="/Users/dez/.nix-profile/bin/emacsclient"
    fi

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nc --socket-name=doom
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nc  --socket-name=doom $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "coding" server;'
        echo '  - 1: run emacsclient to open FILES.'
    fi
}

function edt() {
    if [[ $(uname) == "Linux" ]]; then
        EMACSCLIENT="/usr/bin/emacsclient"
    else
        EMACSCLIENT="/Users/dez/.nix-profile/bin/emacsclient"
    fi

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nw --socket-name=doom
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nw  --socket-name=doom $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "coding" server;'
        echo '  - 1: run emacsclient to open FILES.'
    fi
}
