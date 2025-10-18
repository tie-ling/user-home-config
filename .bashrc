set -o posix
set -o noglob
set -o nounset
set -o noclobber

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
}

tab1080e () {
    swaymsg input 9580:110:PenTablet_Pen  map_to_region 384 0 1536 960
}

tab1080d () {
    swaymsg input 9580:110:PenTablet_Pen  map_to_region 0 0 1920 1080
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
    mbsync -a; notmuch new;
}

vncserver () {
    WAYLAND_DISPLAY=wayland-1 wayvnc --keyboard=yc --max-fps 60 --gpu localhost
}
# w3m needs history file to exist to save browsing history
if ! [ -e $HOME/.w3m/history ]; then touch $HOME/.w3m/history; fi
