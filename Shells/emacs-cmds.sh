#!/bin/bash

if [[ $(uname) == "Linux" ]]; then
    EMACS="/usr/bin/emacs"
else
    EMACS="/etc/profiles/per-user/dez/bin/emacs"
fi
if [[ $(uname) == "Linux" ]]; then
    EMACSCLIENT="/usr/bin/emacsclient"
else
    EMACSCLIENT="/etc/profiles/per-user/dez/bin/emacsclient"
fi

function es() {
    tmpfile="$HOME/.tmp.stdout"


    if [[ $# -eq 0 ]] || [[ "$1" == "list" ]]; then
        ps aux | grep -i 'emacs.* --bg-daemon' | grep -v 'grep' \
            | awk '{print $2 "\t" $9 "\tEmacs " $12}' > $tmpfile
        while read line; do
            echo "$line" | sed 's/\0123,4\012//'
        done < $tmpfile
        rm -f $tmpfile

    elif [[ "$1" == "kill" ]]; then
        if [[ -z $2 ]]; then
            kill -9 $(ps aux | grep -i 'emacs.* --bg-daemon' \
                          | grep -v 'grep' | awk '{print $2}')
        else
            case $2 in
                m)
                    kill $(ps aux | grep -i 'emacs.* --bg-daemon' \
                               | grep "main" | grep -v 'grep' | awk '{print $2}')
                    ;;
                c)
                    kill $(ps aux | grep -i 'emacs.* --bg-daemon' \
                               | grep "coding" | grep -v 'grep' | awk '{print $2}')
                    ;;
                t)
                    kill $(ps aux | grep -i 'emacs.* --bg-daemon' \
                               | grep "tty" | grep -v 'grep' | awk '{print $2}')
                    ;;
                *)
                    kill $(ps aux | grep -i 'emacs.* --bg-daemon' \
                               | grep "$2" | grep -v 'grep' | awk '{print $2}')
                    ;;
            esac
        fi

    elif [[ "$1" == "start" ]]; then
        if [[ -z $2 ]]; then
            $EMACS --daemon=main
	    $EMACS --daemon=tty
        else
            case $2 in
                m)
                    $EMACS --daemon=main
                    ;;
                c)
                    $EMACS --daemon=coding
                    ;;
                t)
                    $EMACS --daemon=tty
                    ;;
		dm)
		    $EMACS --with-profile doom --daemon=doom
		    ;;
                *)
                    $EMACS --daemon="$2"
                    ;;
            esac
        fi

    elif [[ "$1" == "stop" ]]; then
        if [[ -z $2 ]]; then

	    $EMACSCLIENT -n -e '(kill-emacs)'
	    
	    ps aux | grep -i 'emacs.* --bg-daemon' | grep -v 'grep' \
                | awk '{print $2 "\t" $9 "\tEmacs " $12}' > $tmpfile
            while read line; do
                servername=$(echo "$line" | sed 's/\0123,4\012//' | sed 's/.*=//')
		$EMACSCLIENT -n --socket-name=$servername   -e '(kill-emacs)'
            done < $tmpfile
            rm -f $tmpfile
        else
            case $2 in
                m)
                    $EMACSCLIENT -n --socket-name=main   -e '(kill-emacs)'
                    ;;
                c)
                    $EMACSCLIENT -n --socket-name=coding -e '(kill-emacs)'
                    ;;
                t)
                    $EMACSCLIENT -n --socket-name=tty -e '(kill-emacs)'
                    ;;
		dm)
                    $EMACSCLIENT -n --with-profile doom --socket-name=doom -e '(kill-emacs)'
                    ;;

                *)
                    $EMACSCLIENT -n --socket-name="$2" -e '(kill-emacs)'
                    ;;
            esac
        fi

    else
        echo 'usage: 0, 1 or 2 arguments'
        echo '  - 0: list running servers;'
        echo '  - 1: choose among "list, start, stop, kill"; "start" use "main" as server name;'
        echo '  - 2: "es start/stop/kill SERVER_NAME" as you specified.'
    fi
}

# =======================================================
# Function: emacs client (main & coding)
# =======================================================

# emacsclient: main
function em() {

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nc --socket-name=main
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nc --socket-name=main $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "main" server;'
        echo '  - 1: run emacsclient to open FILES.'
    fi
}

# emacsclient: coding
function ec() {

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nc --socket-name=coding
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nc --socket-name=coding $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nc" to "coding" server;'
        echo '  - 1: run emacsclient to open FILES.'
    fi
}

# emacsclient: tty (emacs in console)
function et() {

    if [[ $# -eq 0 ]]; then
        $EMACSCLIENT -nw --socket-name=tty
    elif [[ -n $1 ]]; then
        $EMACSCLIENT -nw --socket-name=tty $1
    else
        echo 'usage: 0 or 1 argument'
        echo '  - 0: connet "emacsclient -nw" to "tty" server;'
        echo '  - 1: run emacsclient to open FILES.'
    fi
}

function magit() {

    $EMACSCLIENT -nw --socket-name=tty -e "(magit)"
}
