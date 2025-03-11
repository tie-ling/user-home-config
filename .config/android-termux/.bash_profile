# Use longer shell history
export HISTFILESIZE=10000
export HISTSIZE=10000


# save timestamp (unix epoch) in history file
export HISTTIMEFORMAT="%m-%dT%H:%M "

# safe shell options
# see bash man page for details
set -o posix
set -o noglob
set -o nounset
set -o noclobber

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
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

if ! [ -e $HOME/.w3m/history ]; then touch $HOME/.w3m/history; fi
