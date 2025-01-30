# when HISTTIMEFORMAT is set;
# save timestamp (unix epoch) in history file;
# and display history in this format when $(history) is invoked
export HISTTIMEFORMAT="%m-%dT%H:%M "
# longer shell history than default (500)
export HISTFILESIZE=10000
export HISTSIZE=10000
# safe shell options
# see bash man page for details
set -o posix
set -o noglob
set -o nounset
set -o noclobber

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
}

tab1080 () {
    swaymsg input 9580:110:PenTablet_Pen  map_to_region 768 100 1152 648
}

web () {
    hugo --source $HOME/Web serve
}

gitpushall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. push
}

gitpullall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. pull
}


gitstatusall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. status
}

export LEDGER_FILE=$HOME/Documents/hledger/yc.hledger

newmail () {
    mbsync -a; notmuch new;
}

# w3m needs history file to exist to save browsing history
if ! [ -e $HOME/.w3m/history ]; then touch $HOME/.w3m/history; fi
