set -o posix
set -o noglob
set -o nounset
set -o noclobber

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
}

tablap () {
    swaymsg input 9580:110:PenTablet  map_to_region 0 0 960 600
}

tabdesk () {
    swaymsg input 9580:110:PenTablet  map_to_region 0 0 1920 1200
}

gitpushall () {
    find ~ -maxdepth 2 -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. push
}

gitpullall () {
    find ~ -maxdepth 2 -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. pull
}

gitstatusall () {
    find ~ -maxdepth 2 -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. status
}

export LEDGER_FILE=$HOME/Projects/ledger/yc.ledger

newmail () {
    notmuch-mailmover; mbsync -a; notmuch new; notmuch tag +deleted -- folder:/Trash/
}

lap_en_display () {
    SWAYSOCK=$(find $XDG_RUNTIME_DIR -maxdepth 1 -name 'sway-ipc*' -type s) \
            swaymsg output eDP-1 enable
}

# w3m needs history file to exist to save browsing history
if ! [ -e $HOME/.w3m/history ]; then touch $HOME/.w3m/history; fi
